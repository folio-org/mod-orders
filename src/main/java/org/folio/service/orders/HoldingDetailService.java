package org.folio.service.orders;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.HoldingDetailAggregator;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingDetailResults;
import org.folio.rest.jaxrs.model.HoldingDetailResultsProperty;
import org.folio.rest.jaxrs.model.ItemsDetail;
import org.folio.rest.jaxrs.model.ItemsDetailCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PiecesDetail;
import org.folio.rest.jaxrs.model.PiecesDetailCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLinesDetail;
import org.folio.rest.jaxrs.model.PoLinesDetailCollection;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceStorageService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;

@Log4j2
public class HoldingDetailService {

  private final ConsortiumConfigurationService consortiumConfigurationService;
  private final ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PieceStorageService pieceStorageService;
  private final InventoryItemManager inventoryItemManager;

  public HoldingDetailService(ConsortiumConfigurationService consortiumConfigurationService,
                              ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever,
                              PurchaseOrderLineService purchaseOrderLineService,
                              PieceStorageService pieceStorageService,
                              InventoryItemManager inventoryItemManager) {
    this.consortiumConfigurationService = consortiumConfigurationService;
    this.consortiumUserTenantsRetriever = consortiumUserTenantsRetriever;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.pieceStorageService = pieceStorageService;
    this.inventoryItemManager = inventoryItemManager;
  }

  public Future<HoldingDetailResults> postOrdersHoldingDetail(List<String> holdingIds, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(holdingIds)) {
      log.info("postOrdersHoldingDetail:: No holding ids were passed");
      return Future.succeededFuture(new HoldingDetailResults());
    }
    return getUserTenantsIfNeeded(requestContext)
      .compose(userTenants -> {
        var aggregatorFutures = new ArrayList<Future<HoldingDetailAggregator>>();
        if (CollectionUtils.isNotEmpty(userTenants)) {
          userTenants.forEach(tenantId -> {
            var localRequestContext = createContextWithNewTenantId(requestContext, tenantId);
            aggregatorFutures.add(aggregateHoldingDetailByTenant(holdingIds, localRequestContext));
          });
        } else {
          aggregatorFutures.add(aggregateHoldingDetailByTenant(holdingIds, requestContext));
        }
        return collectResultsOnSuccess(aggregatorFutures);
      })
      .compose(aggregators -> {
        var holdingDetailResults = new HoldingDetailResults();

        holdingIds.forEach(holdingId -> {
          // Accumulate data from all aggregators for this holdingId
          var allPoLinesDetails = new ArrayList<PoLinesDetail>();
          var allPiecesDetails = new ArrayList<PiecesDetail>();
          var allItemsDetails = new ArrayList<ItemsDetail>();

          aggregators.forEach(aggregator -> {
            if (aggregator.getPoLinesByHoldingId().containsKey(holdingId)) {
              var poLinesDetails = aggregator.getPoLinesByHoldingId().get(holdingId).stream()
                .filter(Objects::nonNull)
                .map(poLine -> new PoLinesDetail().withId(poLine.getId())
                  .withCheckinItems(poLine.getCheckinItems()))
                .toList();
              allPoLinesDetails.addAll(poLinesDetails);
            }
            if (aggregator.getPiecesByHoldingId().containsKey(holdingId)) {
              var piecesDetails = aggregator.getPiecesByHoldingId().get(holdingId).stream()
                .filter(Objects::nonNull)
                .map(piece -> new PiecesDetail().withId(piece.getId())
                  .withPoLineId(piece.getPoLineId())
                  .withItemId(piece.getItemId())
                  .withTenantId(piece.getReceivingTenantId()))
                .toList();
              allPiecesDetails.addAll(piecesDetails);
            }
            if (aggregator.getItemsByHoldingId().containsKey(holdingId)) {
              var itemsDetails = aggregator.getItemsByHoldingId().get(holdingId).stream()
                .filter(Objects::nonNull)
                .map(item -> new ItemsDetail().withId(item.getString(ID))
                  .withTenantId(aggregator.getPieceTenantIdByItemId(item.getString(ID))))
                .toList();
              allItemsDetails.addAll(itemsDetails);
            }
          });

          // Build collections with accumulated data
          var poLinesDetailCollection = new PoLinesDetailCollection()
            .withPoLinesDetail(allPoLinesDetails)
            .withTotalRecords(allPoLinesDetails.size());
          var piecesDetailCollection = new PiecesDetailCollection()
            .withPiecesDetail(allPiecesDetails)
            .withTotalRecords(allPiecesDetails.size());
          var itemsDetailCollection = new ItemsDetailCollection()
            .withItemsDetail(allItemsDetails)
            .withTotalRecords(allItemsDetails.size());

          log.info("postOrdersHoldingDetail:: Prepared accumulated data for {} holding with poLines={}, pieces={}, items={} for tenants={}", holdingId,
            allPoLinesDetails.size(), allPiecesDetails.size(), allItemsDetails.size(),
            aggregators.stream().map(HoldingDetailAggregator::getTenant).filter(Objects::nonNull).toList());

          var holdingDetailProperty = new HoldingDetailResultsProperty()
            .withPoLinesDetailCollection(poLinesDetailCollection)
            .withPiecesDetailCollection(piecesDetailCollection)
            .withItemsDetailCollection(itemsDetailCollection);
          holdingDetailResults.withAdditionalProperty(holdingId, holdingDetailProperty);
        });

        return Future.succeededFuture(holdingDetailResults);
      })
      .recover(throwable -> {
        log.error("postOrdersHoldingDetail:: Error building holding detail results for holding ids={}", holdingIds, throwable);
        return Future.failedFuture(throwable);
      });
  }

  private Future<HoldingDetailAggregator> aggregateHoldingDetailByTenant(List<String> holdingIds, RequestContext requestContext) {
    var aggregator = new HoldingDetailAggregator();
    aggregator.setTenant(TenantTool.tenantId(requestContext.getHeaders()));

    log.info("aggregateHoldingDetailByTenant:: Aggregating poLines, pieces and items for tenant={}", aggregator.getTenant());

    // Aggregate poLines, pieces, and items independently in parallel
    var poLinesFuture = purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext)
      .map(poLines -> {
        log.info("aggregateHoldingDetailByTenant:: Found {} poLines by holding ids={}", poLines.size(), holdingIds);
        var poLinesByHoldingIds = groupPoLinesByHoldingId(holdingIds, poLines);
        aggregator.setPoLinesByHoldingId(poLinesByHoldingIds);
        return null;
      })
      .recover(throwable -> {
        log.warn("aggregateHoldingDetailByTenant:: Failed to retrieve poLines for holding ids={}", holdingIds, throwable);
        aggregator.setPoLinesByHoldingId(Collections.emptyMap());
        return Future.succeededFuture(null);
      });

    var piecesFuture = pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext)
      .map(pieces -> {
        log.info("aggregateHoldingDetailByTenant:: Found {} pieces by holding ids={}", pieces.size(), holdingIds);
        var piecesByHoldingId = groupPiecesByHoldingId(pieces);
        aggregator.setPiecesByHoldingId(piecesByHoldingId);
        return null;
      })
      .recover(throwable -> {
        log.warn("aggregateHoldingDetailByTenant:: Failed to retrieve pieces for holding ids={}", holdingIds, throwable);
        aggregator.setPiecesByHoldingId(Collections.emptyMap());
        return Future.succeededFuture(null);
      });

    var itemsFuture = inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext)
      .map(items -> {
        log.info("aggregateHoldingDetailByTenant:: Found {} items by holding ids={}", items.size(), holdingIds);
        var itemsByHoldingId = groupItemsByHoldingId(items);
        aggregator.setItemsByHoldingId(itemsByHoldingId);
        return null;
      })
      .recover(throwable -> {
        log.warn("aggregateHoldingDetailByTenant:: Failed to retrieve items for holding ids={}", holdingIds, throwable);
        aggregator.setItemsByHoldingId(Collections.emptyMap());
        return Future.succeededFuture(null);
      });

    return Future.all(poLinesFuture, piecesFuture, itemsFuture)
      .map(v -> aggregator);
  }

  protected Future<List<String>> getUserTenantsIfNeeded(RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> {
        if (consortiumConfiguration.isEmpty()) {
          return Future.succeededFuture(Collections.emptyList());
        }
        var configuration = consortiumConfiguration.get();

        // Always change to central tenant when it comes to checking if Central Ordering is enabled
        var localRequestContext = createContextWithNewTenantId(requestContext, configuration.centralTenantId());
        return consortiumConfigurationService.isCentralOrderingEnabled(localRequestContext)
          .compose(enabled -> {
            if (Boolean.FALSE.equals(enabled)) {
              log.info("getUserTenantsIfNeeded:: Central ordering is disabled or not configured");
              return Future.succeededFuture(Collections.emptyList());
            }
            return consortiumUserTenantsRetriever.getUserTenants(configuration.consortiumId(), configuration.centralTenantId(), requestContext);
          });
      })
      .recover(throwable -> {
        log.warn("getUserTenantsIfNeeded:: Failed to retrieve user tenants, falling back to current tenant only", throwable);
        return Future.succeededFuture(Collections.emptyList());
      });
  }

  private Map<String, List<PoLine>> groupPoLinesByHoldingId(List<String> holdingIds, List<PoLine> poLines) {
    if (CollectionUtils.isEmpty(poLines)) {
      return Collections.emptyMap();
    }
    var holdingIdSet = new HashSet<>(holdingIds);
    return poLines.stream()
      .filter(Objects::nonNull)
      .filter(poLine -> CollectionUtils.isNotEmpty(poLine.getLocations()))
      .flatMap(poLine -> poLine.getLocations().stream()
        .filter(location -> Objects.nonNull(location.getHoldingId()))
        .filter(location -> holdingIdSet.contains(location.getHoldingId()))
        .map(location -> Map.entry(location.getHoldingId(), poLine)))
      .distinct()
      .collect(Collectors.groupingBy(Map.Entry::getKey, Collectors.mapping(Map.Entry::getValue, Collectors.toList())));
  }

  private Map<String, List<Piece>> groupPiecesByHoldingId(List<Piece> pieces) {
    if (CollectionUtils.isEmpty(pieces)) {
      return Collections.emptyMap();
    }
    return pieces.stream()
      .filter(Objects::nonNull)
      .filter(piece -> Objects.nonNull(piece.getHoldingId()))
      .collect(Collectors.groupingBy(Piece::getHoldingId));
  }

  private Map<String, List<JsonObject>> groupItemsByHoldingId(List<JsonObject> items) {
    if (CollectionUtils.isEmpty(items)) {
      return Collections.emptyMap();
    }
    return items.stream()
      .filter(Objects::nonNull)
      .filter(item -> Objects.nonNull(item.getString(ITEM_HOLDINGS_RECORD_ID)))
      .collect(Collectors.groupingBy(item -> item.getString(ITEM_HOLDINGS_RECORD_ID)));
  }
}
