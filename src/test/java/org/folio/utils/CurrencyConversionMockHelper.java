package org.folio.utils;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.service.exchange.ExchangeRateProviderResolver.RATE_KEY;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import javax.money.Monetary;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.exchange.FinanceApiExchangeRateProvider;
import org.folio.service.exchange.ManualCurrencyConversion;
import org.folio.service.exchange.ManualExchangeRateProvider;
import org.javamoney.moneta.spi.DefaultNumberValue;
import org.mockito.Mockito;

public class CurrencyConversionMockHelper {

  private final ConfigurationEntriesCache configurationEntriesCache;
  private final ExchangeRateProviderResolver exchangeRateProviderResolver;
  private final RequestContext requestContext;

  public CurrencyConversionMockHelper(ConfigurationEntriesCache configurationEntriesCache,
                                      ExchangeRateProviderResolver exchangeRateProviderResolver, RequestContext requestContext) {
    this.configurationEntriesCache = configurationEntriesCache;
    this.exchangeRateProviderResolver = exchangeRateProviderResolver;
    this.requestContext = requestContext;
  }

  public enum ExchangeRateProviderMode {
    MANUAL,
    FINANCE_API
  }

  public void mockExchangeRateProviderResolver(ExchangeRateProviderMode exchangeRateProviderMode, String fromCurrency, String toCurrency, Double exchangeRateAmount) {
    doReturn(succeededFuture(fromCurrency)).when(configurationEntriesCache).getSystemCurrency(requestContext);

    ExchangeRateProvider exchangeRateProvider;
    ManualCurrencyConversion manualCurrencyConversion;
    if (exchangeRateProviderMode == ExchangeRateProviderMode.MANUAL) {
      ConversionQuery conversionQuery = ConversionQueryBuilder.of().setBaseCurrency(fromCurrency).setTermCurrency(toCurrency).set(RATE_KEY, exchangeRateAmount).build();
      exchangeRateProvider = Mockito.mock(ManualExchangeRateProvider.class);
      manualCurrencyConversion = new ManualCurrencyConversion(conversionQuery, exchangeRateProvider, ConversionContext.of(), ManualCurrencyConversion.OperationMode.DIVIDE);
    } else if (exchangeRateProviderMode == ExchangeRateProviderMode.FINANCE_API) {
      ConversionQuery conversionQuery = ConversionQueryBuilder.of().setBaseCurrency(fromCurrency).setTermCurrency(toCurrency).build();
      exchangeRateProvider = Mockito.mock(FinanceApiExchangeRateProvider.class);
      manualCurrencyConversion = new ManualCurrencyConversion(conversionQuery, exchangeRateProvider, ConversionContext.of());
    } else {
      throw new IllegalStateException("Unsupported ExchangeRateProviderMode");
    }

    ExchangeRate exchangeRate = mock(ExchangeRate.class);

    doReturn(exchangeRateProvider).when(exchangeRateProviderResolver).resolve(any(ConversionQuery.class), eq(requestContext), any(ManualCurrencyConversion.OperationMode.class));
    doReturn(manualCurrencyConversion).when(exchangeRateProvider).getCurrencyConversion(any(ConversionQuery.class));
    doReturn(exchangeRate).when(exchangeRateProvider).getExchangeRate(any(ConversionQuery.class));

    when(exchangeRate.getContext()).thenReturn(ConversionContext.of());
    when(exchangeRate.getCurrency()).thenReturn(Monetary.getCurrency(toCurrency));
    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency(fromCurrency));
    when(exchangeRate.getFactor()).thenReturn(new DefaultNumberValue(exchangeRateAmount));
  }
}
