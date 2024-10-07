package org.folio.service.orders;

import static java.util.stream.Collectors.toMap;

import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.folio.orders.utils.QueryUtils;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingSummary;
import org.folio.rest.jaxrs.model.HoldingSummaryCollection;
import org.folio.rest.jaxrs.model.OrderCloseReason;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class HoldingsSummaryService {

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final PieceStorageService pieceStorageService;

  public HoldingsSummaryService(PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderLineService purchaseOrderLineService,
    PieceStorageService pieceStorageService) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.pieceStorageService = pieceStorageService;
  }

  public Future<HoldingSummaryCollection> getHoldingsSummary(String holdingId, RequestContext requestContext) {
    var queryForPiece = String.format("?query=holdingId==%s", holdingId);
    var queryForLines = String.format("?query=locations=\"holdingId\" : \"%s\"", holdingId);

    return pieceStorageService.getAllPieces(queryForPiece, requestContext)
      .map(piecesCollection -> {
        Set<String> lineIds = piecesCollection.getPieces().stream()
          .map(Piece::getPoLineId)
          .collect(Collectors.toSet());
        if (!lineIds.isEmpty()) {
          return String.format("%s or %s", queryForLines, QueryUtils.convertIdsToCqlQuery(lineIds));
        }
        return queryForLines;})
      .compose(query -> purchaseOrderLineService.getOrderLines(query, 0, Integer.MAX_VALUE, requestContext)
        .compose(lines -> {
          var purchaseOrderIds = lines.stream()
            .map(PoLine::getPurchaseOrderId)
            .collect(Collectors.toList());
          if (lines.isEmpty()) {
            return Future.succeededFuture((new HoldingSummaryCollection().withTotalRecords(0)));
          } else {
            return purchaseOrderStorageService.getPurchaseOrdersByIds(purchaseOrderIds, requestContext)
              .compose(orders -> buildHoldingSummaries(orders, lines))
              .map(hs -> new HoldingSummaryCollection().withHoldingSummaries(hs).withTotalRecords(hs.size()));
          }
        }));
  }

  private Future<List<HoldingSummary>> buildHoldingSummaries(List<PurchaseOrder> orders, List<PoLine> lines) {
    var ordersById = orders.stream()
      .collect(toMap(PurchaseOrder::getId, Function.identity()));

    var holdingSummaryElement = lines.stream()
      .map(poLine -> buildHoldingSummaryElement(poLine, ordersById.get(poLine.getPurchaseOrderId())))
      .collect(Collectors.toList());

    return Future.succeededFuture(holdingSummaryElement);
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
