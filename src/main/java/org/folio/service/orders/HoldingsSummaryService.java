package org.folio.service.orders;

import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingSummary;
import org.folio.rest.jaxrs.model.HoldingSummaryCollection;
import org.folio.rest.jaxrs.model.OrderCloseReason;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toMap;

public class HoldingsSummaryService {

  private PurchaseOrderService purchaseOrderService;

  private PurchaseOrderLineService purchaseOrderLineService;

  public HoldingsSummaryService(PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService) {
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<HoldingSummaryCollection> getHoldingsSummary(String holdingId, RequestContext requestContext) {
    var query = String.format("?query=locations=\"holdingId\" : \"%s\"", holdingId);
    return purchaseOrderLineService.getOrderLines(query, 0, Integer.MAX_VALUE, requestContext)
      .thenCompose(lines -> {
        var purchaseOrderIds = lines.stream()
          .map(PoLine::getPurchaseOrderId)
          .collect(Collectors.toList());
        if (lines.isEmpty()) {
          return CompletableFuture.completedFuture((new HoldingSummaryCollection().withTotalRecords(0)));
        } else {
          return purchaseOrderService.getPurchaseOrdersByIds(purchaseOrderIds, requestContext)
            .thenCompose(orders -> buildHoldingSummaries(orders, lines))
            .thenApply(hs -> new HoldingSummaryCollection().withHoldingSummaries(hs).withTotalRecords(hs.size()));
        }
      });
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
      .withPolReceiptStatus(HoldingSummary.PolReceiptStatus.fromValue(poLine.getReceiptStatus().toString()));
  }

  private OrderCloseReason mapCloseReason(PurchaseOrder purchaseOrder) {
    return purchaseOrder.getCloseReason() != null ? JsonObject.mapFrom(purchaseOrder.getCloseReason()).mapTo(OrderCloseReason.class) : null;
  }
}
