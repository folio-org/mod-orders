package org.folio.service.orders;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;

import io.vertx.core.json.JsonObject;

public class CompositePurchaseOrderService {

  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;

  public CompositePurchaseOrderService(PurchaseOrderService purchaseOrderService,
      PurchaseOrderLineService purchaseOrderLineService) {
    this.purchaseOrderService = purchaseOrderService;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }


  public CompletableFuture<CompositePurchaseOrder> getCompositeOrderByPoLineId(String poLineId, RequestContext requestContext) {
    return purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
                .thenCompose(poLine -> getCompositeOrderById(poLine.getPurchaseOrderId(), requestContext));
  }

  public CompletableFuture<CompositePurchaseOrder> getCompositeOrderById(String orderId, RequestContext requestContext) {
    return purchaseOrderService.getPurchaseOrderById(orderId, requestContext)
      .thenCompose(purchaseOrder -> purchaseOrderLineService.getCompositePoLinesByOrderId( orderId, requestContext)
      .thenApply(poLines -> convertToCompositePurchaseOrder(JsonObject.mapFrom(purchaseOrder)).withCompositePoLines(poLines)));
  }
}
