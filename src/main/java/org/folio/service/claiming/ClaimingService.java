package org.folio.service.claiming;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManager;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.DATA_EXPORT_SPRING_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CREATE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_EXECUTE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.ClaimingPieceResult.Status.FAILURE;
import static org.folio.rest.jaxrs.model.ClaimingPieceResult.Status.SUCCESS;

@Log4j2
@Service
public class ClaimingService {

  private static final Logger logger = LogManager.getLogger(ClaimingService.class);
  private static final String JOB_STATUS = "status";
  private static final String EXPORT_TYPE_CLAIMS = "CLAIMS";
  private static final String CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY = "Cannot send claims, piece ids are empty";
  private static final String CANNOT_RETRIEVE_CONFIG_ENTRIES = "Cannot retrieve config entries";
  private static final String CANNOT_GROUP_PIECES_BY_VENDOR_MESSAGE = "Cannot group pieces by vendor";
  private static final String CANNOT_CREATE_JOBS_AND_UPDATE_PIECES = "Cannot create jobs and update pieces";

  private final ConfigurationEntriesCache configurationEntriesCache;
  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final OrganizationService organizationService;
  private final PieceUpdateFlowManager pieceUpdateFlowManager;
  private final RestClient restClient;

  public ClaimingService(ConfigurationEntriesCache configurationEntriesCache, PieceStorageService pieceStorageService,
                         PurchaseOrderLineService purchaseOrderLineService, PurchaseOrderStorageService purchaseOrderStorageService,
                         OrganizationService organizationService, PieceUpdateFlowManager pieceUpdateFlowManager, RestClient restClient) {
    this.configurationEntriesCache = configurationEntriesCache;
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.organizationService = organizationService;
    this.pieceUpdateFlowManager = pieceUpdateFlowManager;
    this.restClient = restClient;
  }

  /**
   * Sends claims by receiving pieces to be claimed, groups them by vendor,
   * updates piece statuses and finally creates jobs per vendor and associated integration details
   * @param claimingCollection An array of pieces ids
   * @param requestContext Headers to make HTTP or Kafka requests
   * @return Future of an array of claimingResults
   */
  public Future<ClaimingResults> sendClaims(ClaimingCollection claimingCollection, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(claimingCollection.getClaimingPieceIds())) {
      logger.info("sendClaims:: No claims are sent, claiming piece ids are empty");
      return Future.succeededFuture(createEmptyClaimingResults(CANNOT_SEND_CLAIMS_PIECE_IDS_ARE_EMPTY));
    }
    return configurationEntriesCache.loadConfiguration(DATA_EXPORT_SPRING_CONFIG_MODULE_NAME, requestContext)
      .compose(config -> {
        if (CollectionUtils.isEmpty(config.getMap())) {
          logger.info("sendClaims:: No claims are sent, config has no entries");
          return Future.succeededFuture(createEmptyClaimingResults(CANNOT_RETRIEVE_CONFIG_ENTRIES));
        }
        var pieceIds = claimingCollection.getClaimingPieceIds().stream().toList();
        logger.info("sendClaims:: Received pieces to be claimed, pieceIds: {}", pieceIds);
        return groupPieceIdsByVendorId(pieceIds, requestContext)
          .compose(pieceIdsByVendorIds -> {
            if (CollectionUtils.isEmpty(pieceIdsByVendorIds)) {
              return Future.succeededFuture(createEmptyClaimingResults("Cannot find pieces with Expected status to process"));
            }
            return createJobsByVendor(config, pieceIdsByVendorIds, requestContext);
          });
      })
      .onFailure(t -> logger.error("sendClaims :: Failed send claims: {}", JsonObject.mapFrom(claimingCollection).encodePrettily(), t));
  }

  private Future<Map<String, List<String>>> groupPieceIdsByVendorId(List<String> pieceIds, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(pieceIds)) {
      logger.info("groupPieceIdsByVendorId:: No pieces are grouped by vendor, pieceIds is empty");
      return Future.succeededFuture();
    }
    return pieceStorageService.getPiecesByIds(pieceIds, requestContext)
      .compose(pieces -> {
        var uniquePiecePoLinePairs = pieces.stream()
          .map(piece -> Pair.of(piece.getPoLineId(), piece.getId())).distinct()
          .toList();
        var pieceIdByVendorIdFutures = new ArrayList<Future<Pair<String, String>>>();
        uniquePiecePoLinePairs.forEach(piecePoLinePairs -> {
          var pieceIdByVendorIdFuture = pieceStorageService.getPieceById(piecePoLinePairs.getRight(), requestContext)
            .compose(piece -> createVendorPiecePair(piecePoLinePairs, piece, requestContext));
          if (Objects.nonNull(pieceIdByVendorIdFuture)) {
            pieceIdByVendorIdFutures.add(pieceIdByVendorIdFuture);
          }
        });
        return collectResultsOnSuccess(pieceIdByVendorIdFutures)
          .map(ClaimingService::transformAndGroupPieceIdsByVendorId);
      });
  }

  private Future<Pair<String, String>> createVendorPiecePair(Pair<String, String> piecePoLinePairs, Piece piece, RequestContext requestContext) {
    if (Objects.nonNull(piece) && !piece.getReceivingStatus().equals(Piece.ReceivingStatus.EXPECTED)) {
      logger.info("createVendorPiecePair:: Ignoring processing of a piece not in expected state, piece id: {}", piece.getId());
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
      .groupingBy(Pair::getKey, mapping(Pair::getValue, collectingAndThen(toList(), lists -> StreamEx.of(lists).toList())));
  }

  private Future<ClaimingResults> createJobsByVendor(JsonObject config, Map<String, List<String>> pieceIdsByVendorId,
                                                     RequestContext requestContext) {
    if (CollectionUtils.isEmpty(pieceIdsByVendorId)) {
      logger.info("createJobsByVendor:: No jobs are created, pieceIdsByVendorId is empty");
      return Future.succeededFuture(new ClaimingResults().withClaimingPieceResults(createErrorClaimingResults(pieceIdsByVendorId, CANNOT_GROUP_PIECES_BY_VENDOR_MESSAGE)));
    }
    var updatePiecesAndJobFutures = new ArrayList<Future<List<String>>>();
    pieceIdsByVendorId.forEach((vendorId, pieceIds) -> config.stream()
      .filter(entry -> isExportTypeClaimsAndCorrectVendorId(vendorId, entry) && Objects.nonNull(entry.getValue()))
      .forEach(entry -> {
        logger.info("createJobsByVendor:: Preparing job integration detail for vendor, vendor id: {}, pieces: {}, job key: {}", vendorId, pieceIds.size(), entry.getKey());
        updatePiecesAndJobFutures.add(updatePiecesAndCreateJob(requestContext, pieceIds, entry));
      }));
    return collectResultsOnSuccess(updatePiecesAndJobFutures)
      .map(updatedPieceLists -> {
        if (CollectionUtils.isEmpty(updatedPieceLists)) {
          logger.info("createJobsByVendor:: No pieces were processes for claiming");
          return new ClaimingResults().withClaimingPieceResults(createErrorClaimingResults(pieceIdsByVendorId, CANNOT_CREATE_JOBS_AND_UPDATE_PIECES));
        }
        var successClaimingPieceResults = createSuccessClaimingResults(updatedPieceLists);
        logger.info("createJobsByVendor:: Successfully processed pieces for claiming, count: {}", successClaimingPieceResults.size());
        return new ClaimingResults().withClaimingPieceResults(successClaimingPieceResults);
      });
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

  private static boolean isExportTypeClaimsAndCorrectVendorId(String vendorId, Map.Entry<String, Object> entry) {
    return entry.getKey().startsWith(String.format("%s_%s", EXPORT_TYPE_CLAIMS, vendorId));
  }

  private Future<List<String>> updatePiecesAndCreateJob(RequestContext requestContext, List<String> pieceIds, Map.Entry<String, Object> entry) {
    logger.info("updatePiecesAndCreateJob:: Updating pieces and creating a job, job key: {}, count: {}", entry.getKey(), pieceIds.size());
    return pieceUpdateFlowManager.updatePiecesStatuses(pieceIds, PieceBatchStatusCollection.ReceivingStatus.CLAIM_SENT, requestContext).map(pieceIds)
        .compose(updatePieceIds -> createJob(entry.getKey(), entry.getValue(), requestContext).map(updatePieceIds));
  }

  private Future<Void> createJob(String configKey, Object configValue, RequestContext requestContext) {
    var integrationDetail = new JsonObject(String.valueOf(configValue));
    return restClient.post(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB), integrationDetail, Object.class, requestContext)
      .map(response -> {
        var createdJob = new JsonObject(String.valueOf(response));
        logger.info("createJob:: Created job, config key: {}, job status: {}", configKey, createdJob.getString(JOB_STATUS));
        return restClient.postEmptyResponse(resourcesPath(DATA_EXPORT_SPRING_EXECUTE_JOB), createdJob, requestContext)
          .onSuccess(v -> logger.info("createJob:: Executed job, config key: {}, job status: {}", configKey, createdJob.getString(JOB_STATUS)));
      })
      .mapEmpty();
  }
}
