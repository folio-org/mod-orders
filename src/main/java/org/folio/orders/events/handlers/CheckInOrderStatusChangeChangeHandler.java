package org.folio.orders.events.handlers;

import static org.folio.rest.impl.CheckinHelper.IS_ITEM_ORDER_CLOSED_PRESENT;

import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;


@Component("checkInOrderStatusChangeHandler")
public class CheckInOrderStatusChangeChangeHandler extends AbstractOrderStatusHandler {

  @Autowired
  public CheckInOrderStatusChangeChangeHandler(Vertx vertx) {
    super(vertx.getOrCreateContext());
  }

  @Override
  protected boolean isOrdersStatusChangeSkip(PurchaseOrder purchaseOrder, JsonObject ordersPayload){
    boolean orderItemOrderClosedPresent = ordersPayload.getBoolean(IS_ITEM_ORDER_CLOSED_PRESENT);
    return  (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.PENDING)
          || (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.CLOSED && orderItemOrderClosedPresent) ;
  }
}
