package org.folio.service.orders.lines.update;

import java.util.Objects;

import lombok.extern.log4j.Log4j2;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CreateInventoryType;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.orders.PurchaseOrderLineService;

import io.vertx.core.Future;

@Log4j2
public class OrderLinePatchOperationService {

  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;

  private final OrderLineUpdateInstanceStrategyResolver orderLineUpdateInstanceStrategyResolver;

  private final PurchaseOrderLineService purchaseOrderLineService;

  private final InventoryInstanceManager inventoryInstanceManager;

  public OrderLinePatchOperationService(RestClient restClient,
                                        OrderLineUpdateInstanceStrategyResolver orderLineUpdateInstanceStrategyResolver,
                                        PurchaseOrderLineService purchaseOrderLineService,
                                        InventoryInstanceManager inventoryInstanceManager) {
    this.restClient = restClient;
    this.orderLineUpdateInstanceStrategyResolver = orderLineUpdateInstanceStrategyResolver;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryInstanceManager = inventoryInstanceManager;
  }

  public Future<Void> patch(String lineId, PatchOrderLineRequest request, RequestContext requestContext) {
    log.info("patch:: Start patching operation: {} for poLineId: {}", request.getOperation(), lineId);
    var newInstanceId = request.getReplaceInstanceRef().getNewInstanceId();
    return inventoryInstanceManager.createShadowInstanceIfNeeded(newInstanceId, requestContext)
      .compose(v -> purchaseOrderLineService.getOrderLineById(lineId, requestContext))
      .compose(poLine -> patchOrderLine(request, poLine, requestContext))
      .onSuccess(v -> log.info("patch:: Successfully patched operation: {} for poLineId: {}", request.getOperation(), lineId))
      .onFailure(e -> log.error("Failed to patch operation: {} for poLineId: {}", request.getOperation(), lineId, e))
      .mapEmpty();
  }

  private Future<PoLine> patchOrderLine(PatchOrderLineRequest request, PoLine poLine, RequestContext requestContext) {
    var orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
      .withPathOrderLineRequest(request)
      .withStoragePoLine(poLine);

    return handleUpdateInstance(orderLineUpdateInstanceHolder, requestContext)
      .compose(v -> sendPatchOrderLineRequest(orderLineUpdateInstanceHolder, poLine.getId(), requestContext))
      .compose(v -> purchaseOrderLineService.getOrderLineById(poLine.getId(), requestContext));
  }

  public Future<Void> handleUpdateInstance(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    return updateInstanceForPhysical(holder, requestContext)
      .compose(v -> updateInstanceForEresource(holder, requestContext));
  }

  private Future<Void> updateInstanceForPhysical(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    log.info("updateInstanceForPhysical:: Updating instance for physical type with poLineId: '{}'", holder.getStoragePoLine().getId());
    var physical = PoLineCommonUtil.getPhysical(holder.getStoragePoLine());
    if (physical == null) {
      return Future.succeededFuture();
    }
    return orderLineUpdateInstanceStrategyResolver.resolve(CreateInventoryType.fromValue(physical.getCreateInventory().value()))
      .updateInstance(holder, requestContext);
  }

  private Future<Void> updateInstanceForEresource(OrderLineUpdateInstanceHolder holder, RequestContext requestContext) {
    log.info("updateInstanceForEresource:: Updating instance for eresource type with poLineId: '{}'", holder.getStoragePoLine().getId());
    var eresource = PoLineCommonUtil.getEresource(holder.getStoragePoLine());
    if (eresource == null) {
      return Future.succeededFuture();
    }
    return orderLineUpdateInstanceStrategyResolver.resolve(CreateInventoryType.fromValue(eresource.getCreateInventory().value()))
      .updateInstance(holder, requestContext);
  }

  private Future<Void> sendPatchOrderLineRequest(OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder, String lineId,
                                                 RequestContext requestContext) {
    log.debug("sendPatchOrderLineRequest:: sending patch request for poLineId: {}", lineId);
    StoragePatchOrderLineRequest storagePatchOrderLineRequest = orderLineUpdateInstanceHolder.getStoragePatchOrderLineRequest();
    if (Objects.nonNull(storagePatchOrderLineRequest)) {
      RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(lineId);
      return restClient.patch(requestEntry, storagePatchOrderLineRequest, requestContext);
    }
    return Future.succeededFuture();
  }

}
