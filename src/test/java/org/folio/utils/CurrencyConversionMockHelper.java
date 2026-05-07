package org.folio.utils;

import static io.vertx.core.Future.succeededFuture;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.exchange.CacheableExchangeRateService;

public class CurrencyConversionMockHelper {

  private final CacheableExchangeRateService cacheableExchangeRateService;

  public CurrencyConversionMockHelper(CommonSettingsCache commonSettingsCache,
                                      CacheableExchangeRateService cacheableExchangeRateService, String systemCurrency, RequestContext requestContext) {
    this.cacheableExchangeRateService = cacheableExchangeRateService;
    when(commonSettingsCache.getSystemCurrency(requestContext))
      .thenReturn(succeededFuture(systemCurrency));
  }

  public void mockExchangeRateProviderResolver(String fromCurrency, String toCurrency, Double exchangeRateAmount) {
    var result = succeededFuture(new ExchangeRate()
      .withFrom(fromCurrency)
      .withTo(toCurrency)
      .withExchangeRate(exchangeRateAmount));
    when(cacheableExchangeRateService.getExchangeRate(eq(fromCurrency), eq(toCurrency), any(), any())).thenReturn(result);
    when(cacheableExchangeRateService.getExchangeRate(eq(fromCurrency), eq(toCurrency), any(), any(), any())).thenReturn(result);
  }

  public void mockCustomExchangeRate(String fromCurrency, String toCurrency) {
    when(cacheableExchangeRateService.getExchangeRate(eq(fromCurrency), eq(toCurrency), any(Double.class), any(), any()))
      .thenAnswer(invocation -> {
        Double rate = invocation.getArgument(2);
        ExchangeRate.OperationMode operationMode = invocation.getArgument(3);
        return succeededFuture(new ExchangeRate()
          .withFrom(fromCurrency)
          .withTo(toCurrency)
          .withExchangeRate(rate)
          .withOperationMode(operationMode));
      });
  }
}
