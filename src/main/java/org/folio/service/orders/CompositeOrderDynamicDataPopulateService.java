package org.folio.service.orders;

import java.util.concurrent.CompletableFuture;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.core.models.RequestContext;

public interface CompositeOrderDynamicDataPopulateService {
    CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder, RequestContext requestContext);
}
