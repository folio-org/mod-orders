package org.folio.service.pieces;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.HttpStatus;
import org.folio.models.claiming.ClaimJob;
import org.folio.models.claiming.ClaimPiece;
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
import org.folio.service.caches.ExportConfigsCache;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.organization.OrganizationService;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManager;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.models.claiming.IntegrationDetailField.CLAIM_PIECE_IDS;
import static org.folio.models.claiming.IntegrationDetailField.CONFIG_ID;
import static org.folio.models.claiming.IntegrationDetailField.EXPORT_TYPE_SPECIFIC_PARAMETERS;
import static org.folio.models.claiming.IntegrationDetailField.JOB_ID;
import static org.folio.models.claiming.IntegrationDetailField.VENDOR_EDI_ORDERS_EXPORT_CONFIG;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CREATE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_FIND_PIECE_BY_ID;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_RETRIEVE_CONFIG_ENTRIES;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY;
import static org.folio.rest.core.exceptions.ErrorCodes.UNABLE_TO_GENERATE_CLAIMS_FOR_ORG_NO_INTEGRATION_DETAILS;
import static org.folio.rest.jaxrs.model.ClaimingPieceResult.Status.SUCCESS;
import static org.folio.rest.jaxrs.model.Piece.ReceivingStatus.LATE;
import static org.folio.rest.jaxrs.model.PieceBatchStatusCollection.ReceivingStatus.CLAIM_SENT;

@Log4j2
@Service
@RequiredArgsConstructor
public class PiecesClaimingService {

  private static final String EXPORT_TYPE_CLAIMS = "CLAIMS";
  private static final String VENDOR_CODE_PARAMETER = "vendorCode";
  private static final String PIECE_ID_PARAMETER = "pieceId";

  private final ExportConfigsCache exportConfigsCache;
  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final OrganizationService organizationService;
  private final PieceUpdateFlowManager pieceUpdateFlowManager;
  private final RestClient restClient;

  private record ClaimIntegrations(Map<String, ClaimJob> claimJobs, List<ClaimPiece> piecesWithoutIntegration) {
  }

  /**
   * Sends claims by receiving pieces to be claimed, finds the Claims integrations of each piece,
   * updates piece statuses and finally creates one job per integration with only the pieces it takes
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
    var pieceIds = claimingCollection.getClaimingPieceIds().stream().toList();
    log.info("sendClaims:: Received pieces to be claimed, pieceIds: {}", pieceIds);
    return loadClaimPieces(pieceIds, requestContext)
      .compose(claimPieces -> {
        if (CollectionUtils.isEmpty(claimPieces)) {
          log.info("sendClaims:: Cannot find pieces with late status to process - No claims are sent");
          throwHttpException(CANNOT_FIND_PIECES_WITH_LATE_STATUS_TO_PROCESS, claimingCollection, HttpStatus.HTTP_BAD_REQUEST);
        }
        return resolveIntegrations(claimPieces, requestContext)
          .compose(integrations -> validateIntegrations(claimingCollection, integrations, requestContext)
            .compose(v -> updatePiecesAndCreateJobs(claimingCollection, claimPieces, integrations.claimJobs(), requestContext)));
      })
      .onFailure(t -> log.error("sendClaims:: Failed send claims: {}", JsonObject.mapFrom(claimingCollection).encodePrettily(), t));
  }

  private Future<List<ClaimPiece>> loadClaimPieces(List<String> pieceIds, RequestContext requestContext) {
    log.info("loadClaimPieces:: Loading pieces to be claimed, pieceIds count: {}", pieceIds.size());
    return pieceStorageService.getPiecesByIds(pieceIds, requestContext)
      .compose(pieces -> {
        if (CollectionUtils.isEmpty(pieces)) {
          log.info("loadClaimPieces:: No pieces are found by piece ids, pieceIds: {}", pieceIds);
          return Future.succeededFuture(List.of());
        }
        var uniquePiecePoLinePairs = pieces.stream()
          .filter(Objects::nonNull).filter(piece -> Objects.nonNull(piece.getId()) && Objects.nonNull(piece.getPoLineId()))
          .map(piece -> Pair.of(piece.getPoLineId(), piece.getId())).distinct()
          .toList();
        log.info("loadClaimPieces:: Prepared unique piece-poLine pairs, pairs: {}", uniquePiecePoLinePairs);
        return collectResultsOnSuccess(createClaimPieceFutures(pieces, uniquePiecePoLinePairs, requestContext))
          .map(claimPieces -> claimPieces.stream().filter(Objects::nonNull).distinct().toList());
      });
  }

  private List<Future<ClaimPiece>> createClaimPieceFutures(List<Piece> pieces, List<Pair<String, String>> uniquePiecePoLinePairs,
                                                           RequestContext requestContext) {
    var claimPieceFutures = new ArrayList<Future<ClaimPiece>>();
    uniquePiecePoLinePairs.forEach(piecePoLinePairs -> {
      var foundPiece = pieces.stream()
        .filter(Objects::nonNull).filter(piece -> Objects.nonNull(piece.getId())).filter(piece -> piece.getId().equals(piecePoLinePairs.getRight()))
        .findFirst().orElseThrow(() -> new NoSuchElementException(String.format(CANNOT_FIND_PIECE_BY_ID.getDescription(), piecePoLinePairs.getRight())));
      claimPieceFutures.add(createClaimPiece(piecePoLinePairs, foundPiece, requestContext));
    });
    return claimPieceFutures;
  }

  private Future<ClaimPiece> createClaimPiece(Pair<String, String> piecePoLinePairs, Piece piece, RequestContext requestContext) {
    if (!piece.getReceivingStatus().equals(LATE)) {
      log.info("createClaimPiece:: Ignoring processing of a piece not in LATE state, piece id: {}", piece.getId());
      return Future.succeededFuture();
    }
    return purchaseOrderLineService.getOrderLineById(piecePoLinePairs.getLeft(), requestContext)
      .compose(poLine -> purchaseOrderStorageService.getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext)
        .compose(purchaseOrder -> organizationService.getVendorById(purchaseOrder.getVendor(), requestContext))
        .map(vendor -> {
          if (Objects.nonNull(vendor) && Boolean.TRUE.equals(vendor.getIsVendor())) {
            return new ClaimPiece(vendor, piecePoLinePairs.getRight(), poLine.getAcquisitionMethod());
          }
          return null;
        }));
  }

  private Future<ClaimIntegrations> resolveIntegrations(List<ClaimPiece> claimPieces, RequestContext requestContext) {
    var piecesByPoLineValues = claimPieces.stream()
      .collect(groupingBy(piece -> Pair.of(piece.vendor().getId(), piece.acquisitionMethod()), LinkedHashMap::new, toList()));
    log.info("resolveIntegrations:: Loading Claims integrations, vendor and acquisition method combinations: {}", piecesByPoLineValues.size());
    var configsFutures = new ArrayList<Future<Pair<List<ClaimPiece>, JsonObject>>>();
    piecesByPoLineValues.forEach((poLineValues, pieces) -> configsFutures.add(
      exportConfigsCache.loadClaimsConfigsForPoLine(poLineValues.getLeft(), poLineValues.getRight(), requestContext)
        .map(configs -> Pair.of(pieces, configs))));
    return collectResultsOnSuccess(configsFutures).map(PiecesClaimingService::groupPiecesByIntegration);
  }

  private static ClaimIntegrations groupPiecesByIntegration(List<Pair<List<ClaimPiece>, JsonObject>> piecesWithConfigs) {
    var claimJobs = new LinkedHashMap<String, ClaimJob>();
    var piecesWithoutIntegration = new ArrayList<ClaimPiece>();
    piecesWithConfigs.forEach(piecesWithConfig -> {
      var pieces = piecesWithConfig.getLeft();
      var configs = piecesWithConfig.getRight();
      if (configs.isEmpty()) {
        piecesWithoutIntegration.addAll(pieces);
      }
      // A piece goes to every integration returned for it
      configs.fieldNames().forEach(configName -> {
        var config = configs.getJsonObject(configName);
        var pieceIds = claimJobs.computeIfAbsent(config.getString(CONFIG_ID.getValue()), id -> new ClaimJob(config, new ArrayList<>())).pieceIds();
        pieces.forEach(piece -> pieceIds.add(piece.pieceId()));
      });
    });
    return new ClaimIntegrations(claimJobs, piecesWithoutIntegration);
  }

  private Future<Void> validateIntegrations(ClaimingCollection claimingCollection, ClaimIntegrations integrations, RequestContext requestContext) {
    if (integrations.piecesWithoutIntegration().isEmpty()) {
      return Future.succeededFuture();
    }
    log.info("validateIntegrations:: Pieces without Claims integration: {} - No claims are sent", integrations.piecesWithoutIntegration().size());
    var firstVendor = integrations.piecesWithoutIntegration().getFirst().vendor();
    if (!integrations.claimJobs().isEmpty()) {
      throwHttpExceptionOnMissingVendorIntegrationDetails(claimingCollection, firstVendor);
    }
    // Only when no piece got an integration can the tenant have none at all, which has its own error
    return exportConfigsCache.loadExportConfigs(EXPORT_TYPE_CLAIMS, requestContext)
      .compose(allConfigs -> {
        if (CollectionUtils.isEmpty(allConfigs.getMap())) {
          throwHttpException(CANNOT_RETRIEVE_CONFIG_ENTRIES, claimingCollection, HttpStatus.HTTP_BAD_REQUEST);
        }
        throwHttpExceptionOnMissingVendorIntegrationDetails(claimingCollection, firstVendor);
        return Future.succeededFuture();
      });
  }

  private Future<ClaimingResults> updatePiecesAndCreateJobs(ClaimingCollection claimingCollection, List<ClaimPiece> claimPieces,
                                                            Map<String, ClaimJob> claimJobs, RequestContext requestContext) {
    var pieceIds = claimPieces.stream().map(ClaimPiece::pieceId).distinct().toList();
    log.info("updatePiecesAndCreateJobs:: Updating pieces and creating jobs, pieces: {}, integrations: {}", pieceIds.size(), claimJobs.size());
    return updatePieces(claimingCollection, pieceIds, requestContext)
      .compose(v -> createJobs(claimJobs, requestContext))
      .map(v -> {
        var successClaimingPieceResults = createSuccessClaimingResults(pieceIds);
        log.info("updatePiecesAndCreateJobs:: Successfully processed pieces for claiming, count: {}", successClaimingPieceResults.size());
        var claimingResults = new ClaimingResults().withClaimingPieceResults(successClaimingPieceResults);
        log.debug("updatePiecesAndCreateJobs:: Returning claiming results, claimingResults: {}", JsonObject.mapFrom(claimingResults).encodePrettily());
        return claimingResults;
      });
  }

  private Future<List<Void>> createJobs(Map<String, ClaimJob> claimJobs, RequestContext requestContext) {
    var createJobFutures = new ArrayList<Future<Void>>();
    claimJobs.forEach((configId, claimJob) -> createJobFutures.add(createJob(configId, claimJob.config(), claimJob.pieceIds(), requestContext)));
    log.info("createJobs:: Creating one job per integration, total count: {}", createJobFutures.size());
    return collectResultsOnSuccess(createJobFutures);
  }

  private Future<Void> updatePieces(ClaimingCollection claimingCollection, List<String> pieceIds, RequestContext requestContext) {
    var claimingInterval = claimingCollection.getClaimingInterval();
    var internalNote = claimingCollection.getInternalNote();
    var externalNote = claimingCollection.getExternalNote();
    return pieceUpdateFlowManager.updatePiecesStatuses(pieceIds, CLAIM_SENT, claimingInterval, internalNote, externalNote, requestContext);
  }

  private Future<Void> createJob(String configKey, Object configValue, List<String> pieceIds, RequestContext requestContext) {
    log.info("createJob:: Creating a job, job key: {}, pieces: {}", configKey, pieceIds.size());
    var integrationDetail = new JsonObject(String.valueOf(configValue));
    var jobId = UUID.randomUUID().toString();
    integrationDetail.put(JOB_ID.getValue(), jobId)
      .getJsonObject(EXPORT_TYPE_SPECIFIC_PARAMETERS.getValue())
      .getJsonObject(VENDOR_EDI_ORDERS_EXPORT_CONFIG.getValue())
      .put(CLAIM_PIECE_IDS.getValue(), pieceIds);
    return restClient.post(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB), integrationDetail, Object.class, requestContext)
      .onSuccess(v -> log.info("createJob:: Created a job, job key: {}, pieces: {}", configKey, pieceIds.size()))
      .onFailure(t -> log.error("Failed to create a job", t))
      .mapEmpty();
  }

  private List<ClaimingPieceResult> createSuccessClaimingResults(List<String> pieceIds) {
    return pieceIds.stream()
      .map(pieceId -> new ClaimingPieceResult().withPieceId(pieceId).withStatus(SUCCESS))
      .toList();
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

  private List<Parameter> createPieceIdParameters(ClaimingCollection claimingCollection) {
    return claimingCollection.getClaimingPieceIds().stream()
      .map(pieceId -> new Parameter().withKey(PIECE_ID_PARAMETER).withValue(pieceId))
      .collect(Collectors.toCollection(ArrayList::new));
  }
}
