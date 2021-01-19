package org.folio.service.finance;

import static org.folio.orders.utils.ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

public class FiscalYearService {

  private static final String ENDPOINT = "/finance/ledgers/{id}/current-fiscal-year";
  
  private final RestClient restClient;


  public FiscalYearService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<FiscalYear> getCurrentFiscalYear(String ledgerId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withId(ledgerId);
    return restClient.get(requestEntry, requestContext, FiscalYear.class)
      .exceptionally(t -> {
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

  private boolean isFiscalYearNotFound(Throwable t) {
    return t instanceof HttpException && ((HttpException) t).getCode() == 404;
  }
}
