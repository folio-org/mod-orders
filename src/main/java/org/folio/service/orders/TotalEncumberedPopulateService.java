package org.folio.service.orders;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.models.RequestContext;

public class TotalEncumberedPopulateService implements CompositeOrderDynamicDataPopulateService {

  @Override
  public CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {

    return Optional.of(holder)
      .map(CompositeOrderRetrieveHolder::getFiscalYear)
      .map(s -> withTotalEncumbered(holder, requestContext))
      .orElseGet(() -> CompletableFuture.completedFuture(holder.withTotalEncumbered(0d)));
  }

  public CompletableFuture<CompositeOrderRetrieveHolder> withTotalEncumbered(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {

    return CompletableFuture.completedFuture(holder.withTotalEncumbered(HelperUtils.getTransactionsTotal(holder.getCurrentEncumbrances())));

  }

}
