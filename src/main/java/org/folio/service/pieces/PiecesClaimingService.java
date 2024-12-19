package org.folio.service.pieces;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.HttpStatus;
import org.folio.rest.acq.model.Organization;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ClaimingCollection;
import org.folio.rest.jaxrs.model.ClaimingPieceResult;
import org.folio.rest.jaxrs.model.ClaimingResults;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceBatchStatusCollection;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.organization.OrganizationService;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManager;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.models.claiming.IntegrationDetailField.CLAIM_PIECE_IDS;
import static org.folio.models.claiming.IntegrationDetailField.EXPORT_TYPE_SPECIFIC_PARAMETERS;
import static org.folio.models.claiming.IntegrationDetailField.VENDOR_EDI_ORDERS_EXPORT_CONFIG;
import static org.folio.orders.utils.HelperUtils.DATA_EXPORT_SPRING_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CREATE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_EXECUTE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_CREATE_JOBS_AND_UPDATE_PIECES;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_FIND_PIECE_BY_ID;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_GROUP_PIECES_BY_VENDOR;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_RETRIEVE_CONFIG_ENTRIES;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY;
import static org.folio.rest.core.exceptions.ErrorCodes.UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS;
import static org.folio.rest.jaxrs.model.ClaimingPieceResult.Status.SUCCESS;

@Log4j2
@Service
@RequiredArgsConstructor
public class PiecesClaimingService {

  private static final String JOB_STATUS = "status";
  private static final String EXPORT_TYPE_CLAIMS = "CLAIMS";
  private static final String VENDOR_CODE_PARAMETER = "vendorCode";
  private static final String PIECE_ID_PARAMETER = "pieceId";

  private final ConfigurationEntriesCache configurationEntriesCache;
  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final OrganizationService organizationService;
  private final PieceUpdateFlowManager pieceUpdateFlowManager;
  private final RestClient restClient;

  /**
   * Sends claims by receiving pieces to be claimed, groups them by vendor,
   * updates piece statuses and finally creates jobs per vendor and associated integration details
   *
   * @param claimingCollection An array of pieces ids
   * @param requestContext     Headers to make HTTP or Kafka requests
   * @return Future of an array of claimingResults
   */
  public Future<ClaimingResults> sendClaims(ClaimingCollection claimingCollection, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(claimingCollection.getClaimingPieceIds())) {
      log.info("sendClaims:: Cannot send claims piece ids are empty - No claims are sent");
      throwHttpException(CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY, claimingCollection, HttpStatus.HTTP_BAD_REQUEST);
    }
    return configurationEntriesCache.loadConfiguration(DATA_EXPORT_SPRING_CONFIG_MODULE_NAME, requestContext)
      .compose(config -> {
        if (CollectionUtils.isEmpty(config.getMap())) {
          log.info("sendClaims:: Cannot retrieve config entries - No claims are sent");
          throwHttpException(CANNOT_RETRIEVE_CONFIG_ENTRIES, claimingCollection, HttpStatus.HTTP_BAD_REQUEST);
        }
        var pieceIds = claimingCollection.getClaimingPieceIds().stream().toList();
        log.info("sendClaims:: Received pieces to be claimed, pieceIds: {}", pieceIds);
        return groupPieceIdsByVendor(pieceIds, requestContext)
          .compose(pieceIdsByVendors -> {
            if (CollectionUtils.isEmpty(pieceIdsByVendors)) {
              log.info("sendClaims:: Cannot find pieces with late status to process - No claims are sent");
              throwHttpException(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS, claimingCollection, HttpStatus.HTTP_BAD_REQUEST);
            }
            pieceIdsByVendors.forEach((vendor, piecesByVendor) ->
              log.info("createVendorPiecePair:: Using pieces by vendor map, vendorId: {}, piecesByVendor: {}", vendor.getId(), piecesByVendor));
            var vendorWithoutIntegrationDetails = checkVendorIntegrationDetails(config, pieceIdsByVendors);
            if (Objects.nonNull(vendorWithoutIntegrationDetails)) {
              log.info("sendClaims:: Unable to generate claims because no claim integrations exist - No claims are sent");
              throwHttpExceptionOnMissingVendorIntegrationDetails(claimingCollection, vendorWithoutIntegrationDetails);
            }
            return createJobsByVendor(claimingCollection, config, pieceIdsByVendors, requestContext);
          });
      })
      .onFailure(t -> log.error("sendClaims:: Failed send claims: {}", JsonObject.mapFrom(claimingCollection).encodePrettily(), t));
  }

  private Future<Map<Organization, List<String>>> groupPieceIdsByVendor(List<String> pieceIds, RequestContext requestContext) {
    log.info("groupPieceIdsByVendor:: Grouping pieces by vendor, pieceIds count: {}", pieceIds.size());
    return pieceStorageService.getPiecesByIds(pieceIds, requestContext)
      .compose(pieces -> {
        if (CollectionUtils.isEmpty(pieces)) {
          log.info("groupPieceIdsByVendor:: No pieces are found by piece ids, pieceIds: {}", pieceIds);
          return Future.succeededFuture();
        }
        var uniquePiecePoLinePairs = pieces.stream()
          .filter(Objects::nonNull).filter(piece -> Objects.nonNull(piece.getId()) && Objects.nonNull(piece.getPoLineId()))
          .map(piece -> Pair.of(piece.getPoLineId(), piece.getId())).distinct()
          .toList();
        log.info("groupPieceIdsByVendor:: Prepared unique piece-poLine pairs, pairs: {}", uniquePiecePoLinePairs);
        return collectResultsOnSuccess(createPieceIdByVendorFutures(pieces, uniquePiecePoLinePairs, requestContext))
          .map(PiecesClaimingService::transformAndGroupPieceIdsByVendor);
      });
  }

  private List<Future<Pair<Organization, String>>> createPieceIdByVendorFutures(List<Piece> pieces, List<Pair<String, String>> uniquePiecePoLinePairs,
                                                                                RequestContext requestContext) {
    var pieceIdByVendorFutures = new ArrayList<Future<Pair<Organization, String>>>();
    uniquePiecePoLinePairs.forEach(piecePoLinePairs -> {
      var foundPiece = pieces.stream()
        .filter(Objects::nonNull).filter(piece -> Objects.nonNull(piece.getId())).filter(piece -> piece.getId().equals(piecePoLinePairs.getRight()))
        .findFirst().orElseThrow(() -> new NoSuchElementException(String.format(CANNOT_FIND_PIECE_BY_ID.getDescription(), piecePoLinePairs.getRight())));
      var pieceIdByVendorFuture = createVendorPiecePair(piecePoLinePairs, foundPiece, requestContext);
      if (Objects.nonNull(pieceIdByVendorFuture)) {
        pieceIdByVendorFutures.add(pieceIdByVendorFuture);
      }
    });
    return pieceIdByVendorFutures;
  }

  private Future<Pair<Organization, String>> createVendorPiecePair(Pair<String, String> piecePoLinePairs,
                                                                   Piece piece, RequestContext requestContext) {
    if (!piece.getReceivingStatus().equals(Piece.ReceivingStatus.LATE)) {
      log.info("createVendorPiecePair:: Ignoring processing of a piece not in LATE state, piece id: {}", piece.getId());
      return Future.succeededFuture();
    }
    return purchaseOrderLineService.getOrderLineById(piecePoLinePairs.getLeft(), requestContext)
      .compose(poLine -> purchaseOrderStorageService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
        .compose(purchaseOrder -> organizationService.getVendorById(purchaseOrder.getVendor(), requestContext)))
      .map(vendor -> {
        if (Objects.nonNull(vendor) && Boolean.TRUE.equals(vendor.getIsVendor())) {
          return Pair.of(vendor, piecePoLinePairs.getRight());
        }
        return null;
      });
  }

  private Organization checkVendorIntegrationDetails(JsonObject config, Map<Organization, List<String>> pieceIdsByVendors) {
    return pieceIdsByVendors.keySet().stream()
      .filter(vendor -> {
        var vendorIntegrationDetails = config.stream()
          .filter(configEntry -> isExportTypeClaimsAndCorrectVendorId(vendor.getId(), configEntry))
          .toList();
        log.info("checkVendorIntegrationDetails:: Found vendor integration details, vendorId: {}, integrationDetails: {}", vendor.getId(), vendorIntegrationDetails);
        return vendorIntegrationDetails.isEmpty();
      })
      .findFirst().orElse(null);
  }

  private boolean isExportTypeClaimsAndCorrectVendorId(String vendorId, Map.Entry<String, Object> configEntry) {
    return configEntry.getKey().startsWith(String.format("%s_%s", EXPORT_TYPE_CLAIMS, vendorId)) && Objects.nonNull(configEntry.getValue());
  }

  private static Map<Organization, List<String>> transformAndGroupPieceIdsByVendor(List<Pair<Organization, String>> piecesByVendorList) {
    return StreamEx.of(piecesByVendorList).distinct().filter(Objects::nonNull)
      .groupingBy(Pair::getKey, mapping(Pair::getValue, toList()));
  }

  private Future<ClaimingResults> createJobsByVendor(ClaimingCollection claimingCollection, JsonObject config,
                                                     Map<Organization, List<String>> pieceIdsByVendor, RequestContext requestContext) {
    log.info("createJobsByVendor:: Creating jobs by vendor, vendors by pieces count: {}", pieceIdsByVendor.size());
    if (CollectionUtils.isEmpty(pieceIdsByVendor)) {
      log.info("createJobsByVendor:: Cannot group pieces by vendor - No jobs were create or pieces processed");
      throwHttpException(CANNOT_GROUP_PIECES_BY_VENDOR, claimingCollection, HttpStatus.HTTP_NOT_FOUND);
    }
    return collectResultsOnSuccess(createUpdatePiecesAndJobFutures(claimingCollection, config, pieceIdsByVendor, requestContext))
      .map(updatedPieceLists -> {
        if (CollectionUtils.isEmpty(updatedPieceLists)) {
          log.info("createJobsByVendor:: Cannot create jobs and update pieces - No jobs were create or pieces processed");
          throwHttpException(CANNOT_CREATE_JOBS_AND_UPDATE_PIECES, claimingCollection, HttpStatus.HTTP_NOT_FOUND);
        }
        var successClaimingPieceResults = createSuccessClaimingResults(updatedPieceLists);
        log.info("createJobsByVendor:: Successfully processed pieces for claiming, count: {}", successClaimingPieceResults.size());
        var claimingResults = new ClaimingResults().withClaimingPieceResults(successClaimingPieceResults);
        log.debug("createJobsByVendor:: Returning claiming results, claimingResults: {}", JsonObject.mapFrom(claimingResults).encodePrettily());
        return claimingResults;
      });
  }

  private List<Future<List<String>>> createUpdatePiecesAndJobFutures(ClaimingCollection claimingCollection, JsonObject config,
                                                                     Map<Organization, List<String>> pieceIdsByVendor, RequestContext requestContext) {
    var updatePiecesAndJobFutures = new ArrayList<Future<List<String>>>();
    pieceIdsByVendor.forEach((vendor, pieceIds) -> config.stream()
      .filter(pieceIdsByVendorIdEntry -> isExportTypeClaimsAndCorrectVendorId(vendor.getId(), pieceIdsByVendorIdEntry))
      .forEach(configEntry -> {
        log.info("createUpdatePiecesAndJobFutures:: Preparing job integration detail for vendor, vendor id: {}, pieces: {}, job key: {}",
          vendor.getId(), pieceIds.size(), configEntry.getKey());
        updatePiecesAndJobFutures.add(updatePiecesAndCreateJob(claimingCollection, pieceIds, configEntry, requestContext).map(pieceIds));
      }));
    return updatePiecesAndJobFutures;
  }

  private List<ClaimingPieceResult> createSuccessClaimingResults(List<List<String>> updatedPieceLists) {
    return updatedPieceLists.stream().flatMap(Collection::stream).distinct()
      .map(pieceId -> new ClaimingPieceResult().withPieceId(pieceId).withStatus(SUCCESS))
      .toList();
  }

  private Future<Void> updatePiecesAndCreateJob(ClaimingCollection claimingCollection, List<String> pieceIds,
                                                Map.Entry<String, Object> configEntry, RequestContext requestContext) {
    log.info("updatePiecesAndCreateJob:: Updating pieces and creating a job, job key: {}, count: {}", configEntry.getKey(), pieceIds.size());
    return createJob(configEntry.getKey(), configEntry.getValue(), pieceIds, requestContext).map(pieceIds)
      .compose(v -> pieceUpdateFlowManager.updatePiecesStatuses(pieceIds, PieceBatchStatusCollection.ReceivingStatus.CLAIM_SENT,
        claimingCollection.getClaimingInterval(), claimingCollection.getInternalNote(), claimingCollection.getExternalNote(), requestContext));
  }

  private Future<Void> createJob(String configKey, Object configValue, List<String> pieceIds, RequestContext requestContext) {
    var integrationDetail = new JsonObject(String.valueOf(configValue));
    integrationDetail.getJsonObject(EXPORT_TYPE_SPECIFIC_PARAMETERS.getValue())
      .getJsonObject(VENDOR_EDI_ORDERS_EXPORT_CONFIG.getValue())
      .put(CLAIM_PIECE_IDS.getValue(), pieceIds);
    return restClient.post(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB), integrationDetail, Object.class, requestContext)
      .compose(response -> {
        var createdJob = new JsonObject(String.valueOf(response));
        log.info("createJob:: Created job, config key: {}, job status: {}", configKey, createdJob.getString(JOB_STATUS));
        return restClient.postEmptyResponse(resourcesPath(DATA_EXPORT_SPRING_EXECUTE_JOB), createdJob, requestContext)
          .onSuccess(v -> log.info("createJob:: Executed job, config key: {}", configKey));
      })
      .mapEmpty();
  }

  private void throwHttpExceptionOnMissingVendorIntegrationDetails(ClaimingCollection claimingCollection, Organization vendorWithoutIntegrationDetails) {
    var parameters = createPieceIdParameters(claimingCollection);
    parameters.add(new Parameter().withKey(VENDOR_CODE_PARAMETER).withValue(vendorWithoutIntegrationDetails.getCode()));
    var errors = List.of(new Error().withCode(UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS.getCode())
      .withMessage(String.format(UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS.getDescription(), vendorWithoutIntegrationDetails.getCode()))
      .withParameters(parameters));
    throw new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), new Errors().withErrors(errors).withTotalRecords(errors.size()));
  }

  private void throwHttpException(ErrorCodes errorCode, ClaimingCollection claimingCollection, HttpStatus httpStatus) {
    var errors = List.of(new Error().withCode(errorCode.getCode())
      .withMessage(errorCode.getDescription())
      .withParameters(createPieceIdParameters(claimingCollection)));
    throw new HttpException(httpStatus.toInt(), new Errors().withErrors(errors).withTotalRecords(errors.size()));
  }

  private ArrayList<Parameter> createPieceIdParameters(ClaimingCollection claimingCollection) {
    return claimingCollection.getClaimingPieceIds().stream()
      .map(pieceId -> new Parameter().withKey(PIECE_ID_PARAMETER).withValue(pieceId))
      .collect(Collectors.toCollection(ArrayList::new));
  }
}
