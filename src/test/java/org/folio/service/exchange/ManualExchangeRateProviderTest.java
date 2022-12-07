package org.folio.service.exchange;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ProviderContext;

import org.junit.jupiter.api.Test;
public class ManualExchangeRateProviderTest {

  @Test
  public void getContext() {
    ManualExchangeRateProvider manualExchangeRateProvider = new ManualExchangeRateProvider();
    ProviderContext context = manualExchangeRateProvider.getContext();
    assertThat(context, is(notNullValue()));
  }

  @Test
  public void getExchangeRateWithCurrencyConversion() {
    ManualExchangeRateProvider manualExchangeRateProvider = new ManualExchangeRateProvider();

    String baseCurrency = "USD";
    String termCurrency = "EUR";

    ConversionQuery conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(baseCurrency)
      .setTermCurrency(termCurrency)
      .set("factor", 1.0d)
      .build();

    // TODO: UNCOMMENT
    // ExchangeRate exchangeRate = manualExchangeRateProvider.getExchangeRate(conversionQuery);

/*    assertThat(exchangeRate, is(notNullValue()));
    assertThat(baseCurrency, is(exchangeRate.getBaseCurrency().toString()));
    assertThat(termCurrency, is(exchangeRate.getCurrency().toString()));
    assertThat(exchangeRate.getFactor().doubleValue(), is(1.0d));*/
  }

  @Test
  public void getCurrencyConversion() {
    ManualExchangeRateProvider manualExchangeRateProvider = new ManualExchangeRateProvider();

    String baseCurrency = "USD";
    String termCurrency = "EUR";

    ConversionQuery conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(baseCurrency)
      .setTermCurrency(termCurrency)
      .set("factor", 1.0d)
      .build();

    CurrencyConversion currencyConversion = manualExchangeRateProvider.getCurrencyConversion(conversionQuery);
    assertThat(currencyConversion.getExchangeRateProvider(), is(notNullValue()));
    assertThat(currencyConversion.getCurrency(), is(notNullValue()));
  }
}
