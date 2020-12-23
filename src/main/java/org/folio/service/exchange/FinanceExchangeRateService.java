package org.folio.service.exchange;


import static org.folio.orders.utils.ResourcePathResolver.FINANCE_EXCHANGE_RATE;

import java.util.concurrent.CompletableFuture;
import org.folio.orders.utils.ResourcePathResolver;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;


public class FinanceExchangeRateService {
  private static final String ROE_QUERY_PARAMS = ResourcePathResolver.resourcesPath(FINANCE_EXCHANGE_RATE) + "?from=%s&to=%s";
  private final RestClient exchangeRateRestClient;

  public FinanceExchangeRateService(RestClient exchangeRateRestClient) {
    this.exchangeRateRestClient = exchangeRateRestClient;
  }

  public CompletableFuture<ExchangeRate> getExchangeRate(String from, String to, RequestContext requestContext) {
    String roeQuery = String.format(ROE_QUERY_PARAMS, from, to);
    return exchangeRateRestClient.get(roeQuery, requestContext, ExchangeRate.class);
  }

}
