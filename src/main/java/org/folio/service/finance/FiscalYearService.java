package org.folio.service.finance;

import static org.folio.rest.core.exceptions.ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletionException;

import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

import io.vertx.core.Future;

public class FiscalYearService {

  private static final String CURRENT_FISCAL_YEAR = "/finance/ledgers/{id}/current-fiscal-year";

  private final RestClient restClient;
  private final FundService fundService;


  public FiscalYearService(RestClient restClient, FundService fundService) {
    this.restClient = restClient;
    this.fundService = fundService;
  }

  public Future<FiscalYear> getCurrentFiscalYear(String ledgerId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(CURRENT_FISCAL_YEAR).withId(ledgerId);
    return restClient.get(requestEntry, FiscalYear.class, requestContext)
      .recover(t -> {
        Throwable cause = Objects.nonNull(t.getCause()) ? t.getCause() : t;
        if (isFiscalYearNotFound(cause)) {
          List<Parameter> parameters = Collections.singletonList(new Parameter().withValue(ledgerId)
            .withKey("ledgerId"));
          throw new HttpException(404, CURRENT_FISCAL_YEAR_NOT_FOUND.toError()
            .withParameters(parameters));
        }
        throw new CompletionException(cause);
      });
  }

  public Future<FiscalYear> getCurrentFiscalYearByFundId(String fundId, RequestContext requestContext) {
    return fundService.retrieveFundById(fundId, requestContext)
      .compose(fund -> getCurrentFiscalYear(fund.getLedgerId(), requestContext));
  }

  private boolean isFiscalYearNotFound(Throwable t) {
    return t instanceof HttpException && ((HttpException) t).getCode() == 404;
  }
}
