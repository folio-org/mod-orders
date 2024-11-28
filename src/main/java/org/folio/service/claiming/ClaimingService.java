package org.folio.service.claiming;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.ClaimingHolder;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.ClaimingCollection;
import org.folio.rest.jaxrs.model.ClaimingPieceResult;
import org.folio.rest.jaxrs.model.ClaimingResults;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.organization.OrganizationService;
import org.folio.service.pieces.PieceStorageService;
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

@Log4j2
@Service
public class ClaimingService {

  private static final Logger logger = LogManager.getLogger(ClaimingService.class);
  private static final String CREATE_JOB = "/data-export-spring/jobs";
  private static final String EXECUTE_JOB = "/data-export-spring/jobs/send";
  private static final String JOB_STATUS = "status";

  private final ConfigurationEntriesCache configurationEntriesCache;
  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final OrganizationService organizationService;
  private final RestClient restClient;

  public ClaimingService(ConfigurationEntriesCache configurationEntriesCache, PieceStorageService pieceStorageService,
                         PurchaseOrderLineService purchaseOrderLineService, PurchaseOrderStorageService purchaseOrderStorageService,
                         OrganizationService organizationService, RestClient restClient) {
    this.configurationEntriesCache = configurationEntriesCache;
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.organizationService = organizationService;
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
    var claimingHolder = new ClaimingHolder();
    return configurationEntriesCache.loadConfiguration(DATA_EXPORT_SPRING_CONFIG_MODULE_NAME, requestContext)
      .compose(config -> {
        var pieceIds = claimingCollection.getClaimingPieceIds().stream().toList();
        logger.info("sendClaims:: Received pieces to be claimed, pieceIds: {}", pieceIds);
        return groupPieceIdsByVendorId(claimingHolder, pieceIds, requestContext)
          .compose(pieceIdsByVendorIds -> createJobsByVendor(claimingHolder, config, pieceIdsByVendorIds, requestContext));
      })
      .onFailure(t -> logger.error("sendClaims :: Failed send claims: {}",
        JsonObject.mapFrom(claimingCollection).encodePrettily(), t));
  }

  private Future<Map<String, List<String>>> groupPieceIdsByVendorId(ClaimingHolder claimingHolder, List<String> pieceIds, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(pieceIds)) {
      logger.info("groupPieceIdsByVendorId:: No pieces are grouped by vendor, pieceIds is empty");
      return Future.succeededFuture();
    }
    return pieceStorageService.getPiecesByIds(pieceIds, requestContext)
      .compose(pieces -> {
        claimingHolder.withPieces(pieces);
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

  private Future<ClaimingResults> createJobsByVendor(ClaimingHolder claimingHolder, JsonObject config, Map<String,
                                                     List<String>> pieceIdsByVendorId, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(pieceIdsByVendorId)) {
      logger.info("createJobsByVendor:: No jobs are created, pieceIdsByVendorId is empty");
      return Future.succeededFuture(new ClaimingResults());
    }
    var updatePiecesAndJobFutures = new ArrayList<Future<List<String>>>();
    pieceIdsByVendorId.forEach((vendorId, pieceIds) -> {
      logger.info("createJobsByVendor:: Preparing job integration detail for vendor, vendor id: {}, pieceIds: {}", vendorId, pieceIds);
      config.stream()
        .filter(entry -> entry.getKey().contains(vendorId))
        .forEach(entry -> {
          var updatePiecesAndJobFuture = updatePieces(claimingHolder, pieceIds, requestContext)
            .compose(updatePieceIds -> createJob(entry.getKey(), entry.getValue(), requestContext).map(updatePieceIds));
          updatePiecesAndJobFutures.add(updatePiecesAndJobFuture);
        });
    });
    return collectResultsOnSuccess(updatePiecesAndJobFutures)
      .map(updatedLists -> {
        var processedPieces = updatedLists.stream().flatMap(Collection::stream).distinct()
          .map(pieceId -> new ClaimingPieceResult().withPieceId(pieceId).withStatus(ClaimingPieceResult.Status.SUCCESS))
          .toList();
        logger.info("createJobsByVendor:: Processed pieces for claiming, count: {}", processedPieces.size());
        return new ClaimingResults().withClaimingPieceResults(processedPieces);
      });
  }

  private Future<List<String>> updatePieces(ClaimingHolder claimingHolder, List<String> pieceIds, RequestContext requestContext) {
    var piecesByVendorFutures = new ArrayList<Future<String>>();
    pieceIds.forEach(pieceId -> {
      var piece = claimingHolder.getPieces().stream()
        .filter(pieceFromStorage -> pieceFromStorage.getId().equals(pieceId))
        .findFirst().orElseThrow()
        .withReceivingStatus(Piece.ReceivingStatus.CLAIM_SENT);
      piecesByVendorFutures.add(pieceStorageService.updatePiece(piece, requestContext).map(pieceId));
    });
    return collectResultsOnSuccess(piecesByVendorFutures);
  }

  private Future<Void> createJob(String configKey, Object configValue, RequestContext requestContext) {
    var integrationDetail = new JsonObject(String.valueOf(configValue));
    return restClient.post(CREATE_JOB, integrationDetail, Object.class, requestContext)
      .map(response -> {
        var createdJob = new JsonObject(String.valueOf(response));
        logger.info("createJob:: Created job, config key: {}, job status: {}", configKey, createdJob.getString(JOB_STATUS));
        return restClient.postEmptyResponse(EXECUTE_JOB, createdJob, requestContext)
          .onSuccess(v -> logger.info("createJob:: Executed job, config key: {}, job status: {}", configKey, createdJob.getString(JOB_STATUS)));
      })
      .mapEmpty();
  }
}
