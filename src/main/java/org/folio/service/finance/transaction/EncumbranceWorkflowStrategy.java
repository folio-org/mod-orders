package org.folio.service.finance.transaction;

import java.util.List;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

public interface EncumbranceWorkflowStrategy {

  default Future<List<EncumbranceRelationsHolder>> prepareProcessEncumbrancesAndValidate(
      CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage, RequestContext requestContext) {
    return Future.succeededFuture();
  }

  Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
    RequestContext requestContext);

  OrderWorkflowType getStrategyName();
}
