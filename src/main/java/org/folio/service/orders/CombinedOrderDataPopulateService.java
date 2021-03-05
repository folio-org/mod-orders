package org.folio.service.orders;

import java.util.Set;
import java.util.concurrent.CompletableFuture;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.core.models.RequestContext;

public class CombinedOrderDataPopulateService implements CompositeOrderDynamicDataPopulate {

  private final CompositeOrderRetrieveHolderBuilder holderBuilder;
  private final Set<CompositeOrderDynamicDataPopulate> populateServices;

  public CombinedOrderDataPopulateService(CompositeOrderRetrieveHolderBuilder holderBuilder,
                                          Set<CompositeOrderDynamicDataPopulate> populateServices) {
    this.holderBuilder = holderBuilder;
    this.populateServices = populateServices;
  }

  @Override
  public CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder retrieveHolder,
      RequestContext requestContext) {
    return holderBuilder.withCurrentFiscalYear(retrieveHolder, requestContext)
      .thenCompose(holder -> populateAllDynamicData(holder, requestContext));
  }

  private CompletableFuture<CompositeOrderRetrieveHolder> populateAllDynamicData(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    return CompletableFuture.allOf(populateServices.stream()
      .map(service -> service.populate(holder, requestContext))
      .toArray(CompletableFuture[]::new))
      .thenApply(aVoid -> holder);
  }
}
