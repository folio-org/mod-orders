package org.folio.service.orders.lines.update;

import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.service.orders.PurchaseOrderLineService;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

public class OrderLinePatchOperationService {
  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;

  private final OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver;

  private final PurchaseOrderLineService purchaseOrderLineService;

  public OrderLinePatchOperationService(RestClient restClient, OrderLinePatchOperationHandlerResolver orderLinePatchOperationHandlerResolver,
      PurchaseOrderLineService purchaseOrderLineService) {
    this.restClient = restClient;
    this.orderLinePatchOperationHandlerResolver = orderLinePatchOperationHandlerResolver;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<Void> patch(String lineId, PatchOrderLineRequest request, RequestContext requestContext){
    CompletableFuture<Void> future = new CompletableFuture<>();

    purchaseOrderLineService.getOrderLineById(lineId, requestContext)
        .thenAccept(poLine ->     {
          OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder
            = new OrderLineUpdateInstanceHolder()
            .withPathOrderLineRequest(request)
            .withStoragePoLine(poLine);

          PatchOperationHandler patchOperationHandler = orderLinePatchOperationHandlerResolver.resolve(request.getOperation());
          patchOperationHandler.handle(orderLineUpdateInstanceHolder, requestContext)
              .thenCompose(v -> patchOrderLine(orderLineUpdateInstanceHolder, lineId, requestContext))
              .thenAccept(v -> future.complete(null))
              .exceptionally(t -> {
                future.completeExceptionally(t);
                return null;
              });
        }).exceptionally(t -> {
          future.completeExceptionally(t);
          return null;
        });

    return future;
  }

  private CompletableFuture<Void> patchOrderLine(OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder,
       String lineId, RequestContext requestContext) {
    if (Objects.nonNull(orderLineUpdateInstanceHolder.getStoragePatchOrderLineRequest())) {
      RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
      return restClient.patch(requestEntry, orderLineUpdateInstanceHolder.getStoragePatchOrderLineRequest(), requestContext);
    }
    return CompletableFuture.completedFuture(null);
  }
}
