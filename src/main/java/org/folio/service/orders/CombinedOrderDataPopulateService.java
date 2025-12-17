package org.folio.service.orders;

import java.util.Set;
import java.util.stream.Collectors;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.core.models.RequestContext;

import io.vertx.core.Future;

public class CombinedOrderDataPopulateService implements CompositeOrderDynamicDataPopulateService {

  private final CompositeOrderRetrieveHolderBuilder holderBuilder;
  private final Set<CompositeOrderDynamicDataPopulateService> populateServices;

  public CombinedOrderDataPopulateService(CompositeOrderRetrieveHolderBuilder holderBuilder,
                                          Set<CompositeOrderDynamicDataPopulateService> populateServices) {
    this.holderBuilder = holderBuilder;
    this.populateServices = populateServices;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder retrieveHolder, RequestContext requestContext) {
    return holderBuilder.withCurrentFiscalYear(retrieveHolder, requestContext)
      .compose(holder -> populateAllDynamicData(holder, requestContext));
  }

  private Future<CompositeOrderRetrieveHolder> populateAllDynamicData(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    return Future.join(populateServices.stream()
      .map(service -> service.populate(holder, requestContext))
      .collect(Collectors.toList()))
      .map(aVoid -> holder);
  }
}
