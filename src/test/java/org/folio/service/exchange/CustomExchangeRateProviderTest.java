package org.folio.service.exchange;

import org.folio.CopilotGenerated;
import org.folio.rest.core.exceptions.HttpException;
import org.javamoney.moneta.spi.DefaultNumberValue;
import org.junit.jupiter.api.Test;

import javax.money.convert.ConversionQueryBuilder;

import static org.folio.service.exchange.CustomExchangeRateProvider.RATE_KEY;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@CopilotGenerated(partiallyGenerated = true, model = "o3-mini")
public class CustomExchangeRateProviderTest {

  @Test
  public void testShouldReturnExchangeRateFromRateProvider() {
    var query = ConversionQueryBuilder.of()
      .setBaseCurrency("USD")
      .setTermCurrency("EUR")
      .set(RATE_KEY, 2d)
      .build();

    var exchangeRate = new CustomExchangeRateProvider().getExchangeRate(query);

    assertEquals("USD", exchangeRate.getBaseCurrency().getCurrencyCode());
    assertEquals("EUR", exchangeRate.getCurrency().getCurrencyCode());
    assertEquals(new DefaultNumberValue(2d).doubleValue(), exchangeRate.getFactor().doubleValue(), 0);
  }

  @Test
  void testShouldThrowExceptionWhenRateNotProvided() {
    var query = ConversionQueryBuilder.of()
      .setBaseCurrency("USD")
      .setTermCurrency("EUR")
      .build();
    var provider = new CustomExchangeRateProvider();
    var exception = assertThrows(HttpException.class, () -> provider.getExchangeRate(query));

    assertTrue(exception.getMessage().contains("Rate must be provided"),
      "Exception message should mention that the rate is required");
  }

  @Test
  void testGetContextReturnsValidProviderContext() {
    var provider = new CustomExchangeRateProvider();
    var context = provider.getContext();
    assertNotNull(context);
    assertEquals("CUSTOM", context.getProviderName());
  }

  @Test
  void testGetCurrencyConversionReturnsConversion() {
    var query = ConversionQueryBuilder.of()
      .setBaseCurrency("USD")
      .setTermCurrency("EUR")
      .set(RATE_KEY, 2d)
      .build();
    var provider = new CustomExchangeRateProvider();
    var conversion = provider.getCurrencyConversion(query);

    assertNotNull(conversion, "The conversion returned should not be null");
  }

  @Test
  void testGetExchangeRateReturnsValidExchangeRate() {
    var query = ConversionQueryBuilder.of()
      .setBaseCurrency("USD")
      .setTermCurrency("EUR")
      .set(RATE_KEY, 1.5d)
      .build();
    var provider = new CustomExchangeRateProvider();
    var exchangeRate = provider.getExchangeRate(query);

    assertNotNull(exchangeRate, "ExchangeRate should not be null");
    // Use the correct method name getBaseCurrency instead of getBase
    assertEquals("USD", exchangeRate.getBaseCurrency().getCurrencyCode());
    assertEquals("EUR", exchangeRate.getCurrency().getCurrencyCode());
    // Validate the exchange rate factor.
    assertEquals(1.5d, exchangeRate.getFactor().numberValueExact(Double.class));
  }

  @Test
  void testCurrencyConversionConversionContext() {
    var query = ConversionQueryBuilder.of()
      .setBaseCurrency("USD")
      .setTermCurrency("JPY")
      .set(RATE_KEY, 110d)
      .build();
    var provider = new CustomExchangeRateProvider();
    var conversion = provider.getCurrencyConversion(query);

    assertNotNull(conversion, "Conversion should not be null");

    // Since CurrencyConversion does not expose a conversion context,
    // verify the provider context separately.
    var context = provider.getContext();

    assertEquals("CUSTOM", context.getProviderName());
  }
}
