package org.folio.utils;

import static io.vertx.core.Future.succeededFuture;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.folio.rest.core.models.RequestContext;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.exchange.CacheableExchangeRateService;

public class CurrencyConversionMockHelper {

  private final ConfigurationEntriesCache configurationEntriesCache;
  private final CacheableExchangeRateService cacheableExchangeRateService;
  private final RequestContext requestContext;

  public CurrencyConversionMockHelper(ConfigurationEntriesCache configurationEntriesCache,
                                      CacheableExchangeRateService cacheableExchangeRateService,
                                      RequestContext requestContext) {
    this.configurationEntriesCache = configurationEntriesCache;
    this.cacheableExchangeRateService = cacheableExchangeRateService;
    this.requestContext = requestContext;
  }

  public void mockExchangeRateProviderResolver(String fromCurrency, String toCurrency, Double exchangeRateAmount) {
    when(cacheableExchangeRateService.getExchangeRate(any(), any(), any(), any()))
      .thenReturn(succeededFuture(new org.folio.rest.acq.model.finance.ExchangeRate()
        .withFrom(fromCurrency).withTo(toCurrency).withExchangeRate(exchangeRateAmount)));
    when(configurationEntriesCache.getSystemCurrency(requestContext))
      .thenReturn(succeededFuture(fromCurrency));
  }
}
