package org.folio.service.orders;

import io.vertx.core.json.JsonObject;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingSummary;
import org.folio.rest.jaxrs.model.HoldingSummaryCollection;
import org.folio.rest.jaxrs.model.OrderCloseReason;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.pieces.PieceStorageService;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toMap;

public class HoldingsSummaryService {

  private PurchaseOrderStorageService purchaseOrderStorageService;
  private PurchaseOrderLineService purchaseOrderLineService;
  private PieceStorageService pieceStorageService;

  public HoldingsSummaryService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
    PieceStorageService pieceStorageService) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.pieceStorageService = pieceStorageService;
  }

  public CompletableFuture<HoldingSummaryCollection> getHoldingsSummary(String holdingId, RequestContext requestContext) {
    var queryForPiece = String.format("?query=holdingId==%s", holdingId);
    var queryForLines = String.format("?query=locations=\"holdingId\" : \"%s\"", holdingId);

    return pieceStorageService.getPieces(Integer.MAX_VALUE, 0, queryForPiece, requestContext)
      .thenCompose(piecesCollection -> {
        List<String> lineIds = piecesCollection.getPieces().stream()
          .map(Piece::getPoLineId)
          .collect(Collectors.toList());

        if (!lineIds.isEmpty()) {
          StringBuffer poIds = new StringBuffer();
          poIds.append(lineIds.get(0));
          lineIds.stream().skip(1).forEach(id -> poIds.append(" or ").append(id));

          return CompletableFuture.completedFuture(String.format(queryForLines + " or id==(%s)", poIds));
        }
        return CompletableFuture.completedFuture(queryForLines);})
      .thenCompose(query -> purchaseOrderLineService.getOrderLines(query, 0, Integer.MAX_VALUE, requestContext)
        .thenCompose(lines -> {
          var purchaseOrderIds = lines.stream()
            .map(PoLine::getPurchaseOrderId)
            .collect(Collectors.toList());
          if (lines.isEmpty()) {
            return CompletableFuture.completedFuture((new HoldingSummaryCollection().withTotalRecords(0)));
          } else {
            return purchaseOrderStorageService.getPurchaseOrdersByIds(purchaseOrderIds, requestContext)
              .thenCompose(orders -> buildHoldingSummaries(orders, lines))
              .thenApply(hs -> new HoldingSummaryCollection().withHoldingSummaries(hs).withTotalRecords(hs.size()));
          }
        }));
  }

  private CompletableFuture<List<HoldingSummary>> buildHoldingSummaries(List<PurchaseOrder> orders, List<PoLine> lines) {
    var ordersById = orders.stream()
      .collect(toMap(PurchaseOrder::getId, Function.identity()));

    var holdingSummaryElement = lines.stream()
      .map(poLine -> buildHoldingSummaryElement(poLine, ordersById.get(poLine.getPurchaseOrderId())))
      .collect(Collectors.toList());

    return CompletableFuture.completedFuture(holdingSummaryElement);
  }

  private HoldingSummary buildHoldingSummaryElement(PoLine poLine, PurchaseOrder purchaseOrder) {
    return new HoldingSummary().withPoLineId(poLine.getId())
      .withPoLineNumber(poLine.getPoLineNumber())
      .withOrderStatus(HoldingSummary.OrderStatus.fromValue(purchaseOrder.getWorkflowStatus().value()))
      .withOrderCloseReason(mapCloseReason(purchaseOrder))
      .withOrderType(HoldingSummary.OrderType.fromValue(purchaseOrder.getOrderType().toString()))
      .withPolReceiptStatus(HoldingSummary.PolReceiptStatus.fromValue(poLine.getReceiptStatus().toString()))
      .withOrderSentDate(purchaseOrder.getDateOrdered());
  }

  private OrderCloseReason mapCloseReason(PurchaseOrder purchaseOrder) {
    return purchaseOrder.getCloseReason() != null ? JsonObject.mapFrom(purchaseOrder.getCloseReason()).mapTo(OrderCloseReason.class) : null;
  }
}
