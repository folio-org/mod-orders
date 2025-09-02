package org.folio.service.orders;

import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.FiscalYear;
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
    return getFiscalYear(holder, requestContext)
      .map(fiscalYear -> {
        if (fiscalYear != null) {
          holder.withFiscalYearId(fiscalYear.getId());
          holder.withFiscalYearCurrency(fiscalYear.getCurrency());
        }
        return holder;
      })
      .otherwise(t -> handleFiscalYearError(holder, t));
  }

  private Future<FiscalYear> getFiscalYear(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
    if (StringUtils.isNotBlank(holder.getFiscalYearId())) {
      return fiscalYearService.getFiscalYearById(holder.getFiscalYearId(), requestContext);
    }

    if (holder.getOrder() == null || CollectionUtils.isEmpty(holder.getOrder().getPoLines())) {
      return Future.succeededFuture(null);
    }

    return holder.getOrder()
      .getPoLines()
      .stream()
      .flatMap(poLine ->  poLine.getFundDistribution().stream())
      .filter(Objects::nonNull)
      .map(FundDistribution::getFundId)
      .filter(StringUtils::isNotBlank)
      .findFirst()
      .map(fundId -> fiscalYearService.getCurrentFiscalYearByFundId(fundId, requestContext))
      .orElseGet(() -> Future.succeededFuture(null));
  }

  private CompositeOrderRetrieveHolder handleFiscalYearError(CompositeOrderRetrieveHolder holder, Throwable t) {
    Throwable cause = Objects.nonNull(t.getCause()) ? t.getCause() : t;
    if (cause instanceof HttpException && ((HttpException) cause).getCode() == 404) {
      return holder;
    }
    throw new RuntimeException(cause);
  }

}
