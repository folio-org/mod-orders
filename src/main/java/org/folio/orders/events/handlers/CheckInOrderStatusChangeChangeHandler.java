package org.folio.orders.events.handlers;

import static org.folio.helper.CheckinHelper.IS_ITEM_ORDER_CLOSED_PRESENT;

import org.folio.helper.PurchaseOrderHelper;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;


@Component("checkInOrderStatusChangeHandler")
public class CheckInOrderStatusChangeChangeHandler extends AbstractOrderStatusHandler {

  @Autowired
  public CheckInOrderStatusChangeChangeHandler(Vertx vertx, EncumbranceService encumbranceService,
          PurchaseOrderStorageService purchaseOrderStorageService, PurchaseOrderHelper purchaseOrderHelper) {
    super(vertx.getOrCreateContext(), encumbranceService, purchaseOrderStorageService, purchaseOrderHelper);
  }

  @Override
  protected boolean isOrdersStatusChangeSkip(PurchaseOrder purchaseOrder, JsonObject ordersPayload){
    boolean orderItemOrderClosedPresent = ordersPayload.getBoolean(IS_ITEM_ORDER_CLOSED_PRESENT);
    return  (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.PENDING)
          || (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.CLOSED && orderItemOrderClosedPresent) ;
  }
}
