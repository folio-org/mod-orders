package org.folio.service.orders;

import java.util.Objects;
import java.util.concurrent.CompletionException;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.FiscalYearService;

import io.vertx.core.Future;

public class CompositeOrderRetrieveHolderBuilder {
  private final FiscalYearService fiscalYearService;

  public CompositeOrderRetrieveHolderBuilder(FiscalYearService fiscalYearService) {
    this.fiscalYearService = fiscalYearService;
  }

  public Future<CompositeOrderRetrieveHolder> withCurrentFiscalYear(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    return holder.getOrder()
      .getPoLines()
      .stream()
      .flatMap(poLine -> poLine.getFundDistribution()
        .stream())
      .map(FundDistribution::getFundId)
      .findFirst()
      .map(fundId -> fiscalYearService.getCurrentFiscalYearByFundId(fundId, requestContext)
        .map(holder::withFiscalYear)
         .otherwise(t -> {
          Throwable cause = Objects.nonNull(t.getCause()) ? t.getCause() : t;
          if (cause instanceof HttpException && ((HttpException) cause).getCode() == 404) {
            return holder;
          }
          throw new CompletionException(cause);
        }))
      .orElseGet(() ->  Future.succeededFuture(holder));
  }

}
