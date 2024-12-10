package org.folio.service.pieces;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ClaimingCollection;
import org.folio.rest.jaxrs.model.ClaimingPieceResult;
import org.folio.rest.jaxrs.model.ClaimingResults;
import org.folio.rest.jaxrs.model.Error;
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

import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.models.claiming.ClaimingError.CANNOT_CREATE_JOBS_AND_UPDATE_PIECES;
import static org.folio.models.claiming.ClaimingError.CANNOT_FIND_A_PIECE_BY_ID;
import static org.folio.models.claiming.ClaimingError.CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS;
import static org.folio.models.claiming.ClaimingError.CANNOT_GROUP_PIECES_BY_VENDOR_MESSAGE;
import static org.folio.models.claiming.ClaimingError.CANNOT_RETRIEVE_CONFIG_ENTRIES;
import static org.folio.models.claiming.ClaimingError.CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY;
import static org.folio.models.claiming.IntegrationDetailField.CLAIM_PIECE_IDS;
import static org.folio.models.claiming.IntegrationDetailField.EXPORT_TYPE_SPECIFIC_PARAMETERS;
import static org.folio.models.claiming.IntegrationDetailField.VENDOR_EDI_ORDERS_EXPORT_CONFIG;
import static org.folio.orders.utils.HelperUtils.DATA_EXPORT_SPRING_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CREATE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_EXECUTE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.ClaimingPieceResult.Status.FAILURE;
import static org.folio.rest.jaxrs.model.ClaimingPieceResult.Status.SUCCESS;

@Log4j2
@Service
@RequiredArgsConstructor
public class PiecesClaimingService {

  private static final String JOB_STATUS = "status";
  private static final String EXPORT_TYPE_CLAIMS = "CLAIMS";

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
   * @param claimingCollection An array of pieces ids
   * @param requestContext Headers to make HTTP or Kafka requests
   * @return Future of an array of claimingResults
   */
  public Future<ClaimingResults> sendClaims(ClaimingCollection claimingCollection, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(claimingCollection.getClaimingPieceIds())) {
      log.info("sendClaims:: No claims are sent, claiming piece ids are empty");
      return Future.succeededFuture(createEmptyClaimingResults(CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY.getValue()));
    }
    return configurationEntriesCache.loadConfiguration(DATA_EXPORT_SPRING_CONFIG_MODULE_NAME, requestContext)
      .compose(config -> {
        if (CollectionUtils.isEmpty(config.getMap())) {
          log.info("sendClaims:: No claims are sent, config has no entries");
          return Future.succeededFuture(createEmptyClaimingResults(CANNOT_RETRIEVE_CONFIG_ENTRIES.getValue()));
        }
        var pieceIds = claimingCollection.getClaimingPieceIds().stream().toList();
        log.info("sendClaims:: Received pieces to be claimed, pieceIds: {}", pieceIds);
        return groupPieceIdsByVendorId(pieceIds, requestContext)
          .compose(pieceIdsByVendorIds -> {
            if (CollectionUtils.isEmpty(pieceIdsByVendorIds)) {
              return Future.succeededFuture(createEmptyClaimingResults(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS.getValue()));
            }
            log.info("sendClaims:: Using pieces by vendor id map, map: {}", pieceIdsByVendorIds);
            return createJobsByVendor(config, pieceIdsByVendorIds, requestContext);
          });
      })
      .onFailure(t -> log.error("sendClaims:: Failed send claims: {}", JsonObject.mapFrom(claimingCollection).encodePrettily(), t));
  }

  private Future<Map<String, List<String>>> groupPieceIdsByVendorId(List<String> pieceIds, RequestContext requestContext) {
    log.info("groupPieceIdsByVendorId:: Grouping pieces by vendor, pieceIds count: {}", pieceIds.size());
    return pieceStorageService.getPiecesByIds(pieceIds, requestContext)
      .compose(pieces -> {
        if (CollectionUtils.isEmpty(pieces)) {
          log.info("groupPieceIdsByVendorId:: No pieces are found by piece ids, pieceIds: {}", pieceIds);
          return Future.succeededFuture();
        }
        var uniquePiecePoLinePairs = pieces.stream()
          .filter(Objects::nonNull).filter(piece -> Objects.nonNull(piece.getId()) && Objects.nonNull(piece.getPoLineId()))
          .map(piece -> Pair.of(piece.getPoLineId(), piece.getId())).distinct()
          .toList();
        log.info("groupPieceIdsByVendorId:: Prepared unique piece-poLine pairs, pairs: {}", uniquePiecePoLinePairs);
        return collectResultsOnSuccess(createPieceIdByVendorFutures(pieces, uniquePiecePoLinePairs, requestContext))
          .map(PiecesClaimingService::transformAndGroupPieceIdsByVendorId);
      });
  }

  private List<Future<Pair<String, String>>> createPieceIdByVendorFutures(List<Piece> pieces, List<Pair<String, String>> uniquePiecePoLinePairs,
                                                                          RequestContext requestContext) {
    var pieceIdByVendorIdFutures = new ArrayList<Future<Pair<String, String>>>();
    uniquePiecePoLinePairs.forEach(piecePoLinePairs -> {
      var foundPiece = pieces.stream()
        .filter(Objects::nonNull).filter(piece -> Objects.nonNull(piece.getId())).filter(piece -> piece.getId().equals(piecePoLinePairs.getRight()))
        .findFirst().orElseThrow(() -> new NoSuchElementException(String.format(CANNOT_FIND_A_PIECE_BY_ID.getValue(), piecePoLinePairs.getRight())));
      var pieceIdByVendorIdFuture = createVendorPiecePair(piecePoLinePairs, foundPiece, requestContext);
      if (Objects.nonNull(pieceIdByVendorIdFuture)) {
        pieceIdByVendorIdFutures.add(pieceIdByVendorIdFuture);
      }
    });
    return pieceIdByVendorIdFutures;
  }

  private Future<Pair<String, String>> createVendorPiecePair(Pair<String, String> piecePoLinePairs,
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
          return Pair.of(vendor.getId(), piecePoLinePairs.getRight());
        }
        return null;
      });
  }

  private static Map<String, List<String>> transformAndGroupPieceIdsByVendorId(List<Pair<String, String>> piecesByVendorList) {
    return StreamEx.of(piecesByVendorList).distinct().filter(Objects::nonNull)
      .groupingBy(Pair::getKey, mapping(Pair::getValue, toList()));
  }

  private Future<ClaimingResults> createJobsByVendor(JsonObject config, Map<String, List<String>> pieceIdsByVendorId,
                                                     RequestContext requestContext) {
    log.info("createJobsByVendor:: Creating jobs by vendor, vendors by pieces count: {}", pieceIdsByVendorId.size());
    if (CollectionUtils.isEmpty(pieceIdsByVendorId)) {
      log.info("createJobsByVendor:: No jobs are created, pieceIdsByVendorId is empty");
      return Future.succeededFuture(new ClaimingResults()
        .withClaimingPieceResults(createErrorClaimingResults(pieceIdsByVendorId, CANNOT_GROUP_PIECES_BY_VENDOR_MESSAGE.getValue())));
    }
    return collectResultsOnSuccess(createUpdatePiecesAndJobFutures(config, pieceIdsByVendorId, requestContext))
      .map(updatedPieceLists -> {
        if (CollectionUtils.isEmpty(updatedPieceLists)) {
          log.info("createJobsByVendor:: No pieces were processed for claiming");
          return new ClaimingResults().withClaimingPieceResults(createErrorClaimingResults(pieceIdsByVendorId, CANNOT_CREATE_JOBS_AND_UPDATE_PIECES.getValue()));
        }
        var successClaimingPieceResults = createSuccessClaimingResults(updatedPieceLists);
        log.info("createJobsByVendor:: Successfully processed pieces for claiming, count: {}", successClaimingPieceResults.size());
        var claimingResults = new ClaimingResults().withClaimingPieceResults(successClaimingPieceResults);
        log.debug("createJobsByVendor:: Returning claiming results, claimingResults: {}", JsonObject.mapFrom(claimingResults).encodePrettily());
        return claimingResults;
      });
  }

  private List<Future<List<String>>> createUpdatePiecesAndJobFutures(JsonObject config, Map<String, List<String>> pieceIdsByVendorId,
                                                                     RequestContext requestContext) {
    var updatePiecesAndJobFutures = new ArrayList<Future<List<String>>>();
    pieceIdsByVendorId.forEach((vendorId, pieceIds) -> config.stream()
      .filter(pieceIdsByVendorIdEntry -> isExportTypeClaimsAndCorrectVendorId(vendorId, pieceIdsByVendorIdEntry)
          && Objects.nonNull(pieceIdsByVendorIdEntry.getValue()))
      .forEach(pieceIdsByVendorIdEntry -> {
        log.info("createJobsByVendor:: Preparing job integration detail for vendor, vendor id: {}, pieces: {}, job key: {}",
          vendorId, pieceIds.size(), pieceIdsByVendorIdEntry.getKey());
        updatePiecesAndJobFutures.add(updatePiecesAndCreateJob(pieceIds, pieceIdsByVendorIdEntry, requestContext));
      }));
    return updatePiecesAndJobFutures;
  }

  private static ClaimingResults createEmptyClaimingResults(String message) {
    return new ClaimingResults().withClaimingPieceResults(List.of(new ClaimingPieceResult().withError(new Error().withMessage(message))));
  }

  private List<ClaimingPieceResult> createSuccessClaimingResults(List<List<String>> updatedPieceLists) {
    return updatedPieceLists.stream().flatMap(Collection::stream).distinct()
        .map(pieceId -> new ClaimingPieceResult().withPieceId(pieceId).withStatus(SUCCESS))
        .toList();
  }

  private List<ClaimingPieceResult> createErrorClaimingResults(Map<String, List<String>> pieceIdsByVendorId, String message) {
    return pieceIdsByVendorId.values().stream()
        .flatMap(Collection::stream)
        .map(pieceId -> new ClaimingPieceResult().withPieceId(pieceId).withStatus(FAILURE).withError(new Error().withMessage(message)))
        .toList();
  }

  private static boolean isExportTypeClaimsAndCorrectVendorId(String vendorId, Map.Entry<String, Object> pieceIdsByVendorIdEntry) {
    return pieceIdsByVendorIdEntry.getKey().startsWith(String.format("%s_%s", EXPORT_TYPE_CLAIMS, vendorId));
  }

  private Future<List<String>> updatePiecesAndCreateJob(List<String> pieceIds, Map.Entry<String, Object> pieceIdsByVendorIdEntry,
                                                        RequestContext requestContext) {
    log.info("updatePiecesAndCreateJob:: Updating pieces and creating a job, job key: {}, count: {}", pieceIdsByVendorIdEntry.getKey(), pieceIds.size());
    return pieceUpdateFlowManager.updatePiecesStatuses(pieceIds, PieceBatchStatusCollection.ReceivingStatus.CLAIM_SENT, requestContext)
        .compose(v -> createJob(pieceIdsByVendorIdEntry.getKey(), pieceIdsByVendorIdEntry.getValue(), pieceIds, requestContext).map(pieceIds));
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
}
