package org.folio.service.exchange;


import static org.folio.orders.utils.ResourcePathResolver.FINANCE_EXCHANGE_RATE;

import java.util.concurrent.CompletableFuture;

import org.folio.orders.utils.ResourcePathResolver;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;


public class FinanceExchangeRateService {
  private static final String ENDPOINT = ResourcePathResolver.resourcesPath(FINANCE_EXCHANGE_RATE);
  private final RestClient restClient;

  public FinanceExchangeRateService() {
    this.restClient = new RestClient();
  }

  public CompletableFuture<ExchangeRate> getExchangeRate(String from, String to, RequestContext requestContext) {
    var cf = new CompletableFuture<ExchangeRate>();
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQueryParameter("from", from).withQueryParameter("to", to);
    restClient.get(requestEntry, ExchangeRate.class, requestContext)
      .map(value -> cf.complete(value))
      .otherwise(ex -> cf.completeExceptionally(ex));
    return cf;
  }

}
