package org.folio.service.orders.managers;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;

public interface OrderWorkflowManager {
    CompletableFuture<Void> process(CompositePurchaseOrder compPO, RequestContext requestContext);
}
