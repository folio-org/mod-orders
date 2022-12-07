package org.folio.service.exchange;

import static org.folio.orders.utils.ResourcePathResolver.FINANCE_EXCHANGE_RATE;

import org.folio.orders.utils.ResourcePathResolver;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;

public class FinanceExchangeRateService {
  private static final String ENDPOINT = ResourcePathResolver.resourcesPath(FINANCE_EXCHANGE_RATE);
  private final RestClient restClient;

  public FinanceExchangeRateService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<ExchangeRate> getExchangeRate(String from, String to, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT)
      .withQueryParameter("from", from)
      .withQueryParameter("to", to);
    return restClient.get(requestEntry, ExchangeRate.class, requestContext);
  }

}
