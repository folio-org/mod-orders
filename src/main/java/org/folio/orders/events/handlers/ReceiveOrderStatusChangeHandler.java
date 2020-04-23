package org.folio.orders.events.handlers;

import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

@Component("orderStatusHandler")
public class ReceiveOrderStatusChangeHandler extends AbstractOrderStatusHandler {

  @Autowired
  public ReceiveOrderStatusChangeHandler(Vertx vertx) {
    super(vertx);
  }

  @Override
  protected boolean isOrdersStatusChangeSkip(PurchaseOrder purchaseOrder, JsonObject ordersPayload){
    return  (purchaseOrder.getWorkflowStatus() == PurchaseOrder.WorkflowStatus.PENDING);
  }
}
