package org.folio.service.finance.transaction;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

public interface EncumbranceWorkflowStrategy {

  default CompletableFuture<Void> prepareProcessEncumbrancesAndValidate(CompositePurchaseOrder compPO,
      CompositePurchaseOrder poAndLinesFromStorage, RequestContext requestContext) {
    return CompletableFuture.completedFuture(null);
  }
  CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
    RequestContext requestContext);
  OrderWorkflowType getStrategyName();
}
