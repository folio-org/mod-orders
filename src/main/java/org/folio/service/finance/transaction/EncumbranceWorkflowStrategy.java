package org.folio.service.finance.transaction;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.orders.OrderWorkflowType;

public interface EncumbranceWorkflowStrategy {

    CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, RequestContext requestContext);
    OrderWorkflowType getStrategyName();
}
