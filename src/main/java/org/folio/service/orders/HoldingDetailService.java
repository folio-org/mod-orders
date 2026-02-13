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
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceStorageService;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;

@Log4j2
public class HoldingDetailService {

  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PieceStorageService pieceStorageService;
  private final InventoryItemManager inventoryItemManager;

  public HoldingDetailService(PurchaseOrderLineService purchaseOrderLineService,
                              PieceStorageService pieceStorageService,
                              InventoryItemManager inventoryItemManager) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.pieceStorageService = pieceStorageService;
    this.inventoryItemManager = inventoryItemManager;
  }

  public Future<HoldingDetailResults> postOrdersHoldingDetail(List<String> holdingIds, RequestContext requestContext) {
    if (CollectionUtils.isEmpty(holdingIds)) {
      log.info("postOrdersHoldingDetail:: No holding ids were passed");
      return Future.succeededFuture(new HoldingDetailResults());
    }
    var aggregator = new HoldingDetailAggregator();

    // Retrieve poLines, pieces, and items independently in parallel
    var poLinesFuture = purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext)
      .map(poLines -> {
        log.info("postOrdersHoldingDetail:: Found {} poLines by holding ids={}", poLines.size(), holdingIds);
        var poLinesByHoldingIds = groupedPoLinesByHoldingId(holdingIds, poLines);
        aggregator.setPoLinesByHoldingId(poLinesByHoldingIds);
        return null;
      })
      .recover(throwable -> {
        log.warn("postOrdersHoldingDetail:: Failed to retrieve poLines for holding ids={}", holdingIds, throwable);
        aggregator.setPoLinesByHoldingId(Collections.emptyMap());
        return Future.succeededFuture(null);
      });

    var piecesFuture = pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext)
      .map(pieces -> {
        log.info("postOrdersHoldingDetail:: Found {} pieces by holding ids={}", pieces.size(), holdingIds);
        var piecesByHoldingId = groupPiecesByHoldingId(pieces);
        aggregator.setPiecesByHoldingId(piecesByHoldingId);
        return null;
      })
      .recover(throwable -> {
        log.warn("postOrdersHoldingDetail:: Failed to retrieve pieces for holding ids={}", holdingIds, throwable);
        aggregator.setPiecesByHoldingId(Collections.emptyMap());
        return Future.succeededFuture(null);
      });

    var itemsFuture = inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext)
      .map(items -> {
        log.info("postOrdersHoldingDetail:: Found {} items by holding ids={}", items.size(), holdingIds);
        var itemsByHoldingId = groupItemsByHoldingId(items);
        aggregator.setItemsByHoldingId(itemsByHoldingId);
        return null;
      })
      .recover(throwable -> {
        log.warn("postOrdersHoldingDetail:: Failed to retrieve items for holding ids={}", holdingIds, throwable);
        aggregator.setItemsByHoldingId(Collections.emptyMap());
        return Future.succeededFuture(null);
      });

    return Future.all(poLinesFuture, piecesFuture, itemsFuture)
      .compose(v -> {
        var holdingDetailResults = new HoldingDetailResults();
        holdingIds.forEach(holdingId -> {
          var poLinesDetailCollection = new PoLinesDetailCollection();
          var piecesDetailCollection = new PiecesDetailCollection();
          var itemsDetailCollection = new ItemsDetailCollection();

          if (aggregator.getPoLinesByHoldingId().containsKey(holdingId)) {
            var poLinesDetails = aggregator.getPoLinesByHoldingId().get(holdingId).stream()
              .filter(Objects::nonNull)
              .map(poLine -> new PoLinesDetail().withId(poLine.getId()))
              .toList();
            poLinesDetailCollection.withPoLinesDetail(poLinesDetails)
              .withTotalRecords(poLinesDetails.size());
          }
          if (aggregator.getPiecesByHoldingId().containsKey(holdingId)) {
            var piecesDetails = aggregator.getPiecesByHoldingId().get(holdingId).stream()
              .filter(Objects::nonNull)
              .map(piece -> new PiecesDetail().withId(piece.getId())
                .withPoLineId(piece.getPoLineId())
                .withItemId(piece.getItemId())
                .withTenantId(piece.getReceivingTenantId()))
              .toList();
            piecesDetailCollection.withPiecesDetail(piecesDetails)
              .withTotalRecords(piecesDetails.size());
          }
          if (aggregator.getItemsByHoldingId().containsKey(holdingId)) {
            var itemsDetails = aggregator.getItemsByHoldingId().get(holdingId).stream()
              .filter(Objects::nonNull)
              .map(item -> new ItemsDetail().withId(item.getString(ID))
                .withTenantId(aggregator.getPieceTenantIdByItemId(item.getString(ID))))
              .toList();
            itemsDetailCollection.withItemsDetail(itemsDetails)
              .withTotalRecords(itemsDetails.size());
          }
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

  private Map<String, List<PoLine>> groupedPoLinesByHoldingId(List<String> holdingIds, List<PoLine> poLines) {
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
