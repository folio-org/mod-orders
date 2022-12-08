package org.folio.service.orders;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.core.models.RequestContext;

import io.vertx.core.Future;

public interface CompositeOrderDynamicDataPopulateService {
    Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder retrieveHolder, RequestContext requestContext);
}
