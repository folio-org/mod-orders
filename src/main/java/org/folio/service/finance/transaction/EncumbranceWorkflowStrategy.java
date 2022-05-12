package org.folio.service.finance.transaction;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

public interface EncumbranceWorkflowStrategy {

  default CompletableFuture<List<EncumbranceRelationsHolder>> prepareProcessEncumbrancesAndValidate(
      CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage, RequestContext requestContext) {
    return CompletableFuture.completedFuture(null);
  }
  CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
    RequestContext requestContext);
  OrderWorkflowType getStrategyName();
}
