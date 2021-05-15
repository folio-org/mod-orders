package org.folio.service.orders;

import static java.util.stream.Collectors.toMap;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingSummary;
import org.folio.rest.jaxrs.model.HoldingSummaryCollection;
import org.folio.rest.jaxrs.model.OrderCloseReason;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.json.JsonObject;

public class HoldingsSummaryService {


  @Autowired
  private PurchaseOrderService purchaseOrderService;
  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;

  public HoldingsSummaryService(PurchaseOrderService purchaseOrderService) {
    this.purchaseOrderService = purchaseOrderService;
  }

  public CompletionStage<HoldingSummaryCollection> getHoldingsSummary(String holdingId, RequestContext requestContext) {
    var query = String.format("locations.holdingId==%s", holdingId);
    return purchaseOrderLineService.getOrderLines(query, 0, Integer.MAX_VALUE, requestContext)
      .thenCompose(lines -> {
        var lineIds = lines.stream()
          .map(PoLine::getId)
          .collect(Collectors.toList());

        return purchaseOrderService.getPurchaseOrdersByPoLineIds(lineIds, requestContext)
          .thenCompose(orders -> buildHoldingSummaries(orders, lines))
          .thenApply(hs -> new HoldingSummaryCollection().withHoldingSummaries(hs).withTotalRecords(hs.size()));
      });
  }

  private CompletionStage<List<HoldingSummary>> buildHoldingSummaries(List<PurchaseOrder> orders, List<PoLine> lines) {
    var ordersById = orders.stream()
      .collect(toMap(PurchaseOrder::getId, Function.identity()));

    var holdingSummaryElement = lines.stream()
      .map(poLine -> buildHoldingSummaryElement(poLine, ordersById.get(poLine.getId())))
      .collect(Collectors.toList());

    return CompletableFuture.completedFuture(holdingSummaryElement);
  }

  private HoldingSummary buildHoldingSummaryElement(PoLine poLine, PurchaseOrder purchaseOrder) {
    return new HoldingSummary().withPoLineId(poLine.getId())
      .withPoLineNumber(poLine.getPoLineNumber())
      .withOrderStatus(HoldingSummary.OrderStatus.fromValue(purchaseOrder.getWorkflowStatus().value()))
      .withOrderCloseReason(JsonObject.mapFrom(purchaseOrder.getCloseReason()).mapTo(OrderCloseReason.class))
      .withOrderType(HoldingSummary.OrderType.fromValue(purchaseOrder.getOrderType().toString()))
      .withPolReceiptStatus(HoldingSummary.PolReceiptStatus.fromValue(poLine.getReceiptStatus().toString()));
  }
}
