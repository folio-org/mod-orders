package org.folio.service.exchange;

import org.folio.CopilotGenerated;
import org.javamoney.moneta.Money;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.money.Monetary;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.CurrencyConversionException;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;
import javax.money.convert.RateType;
import org.folio.rest.acq.model.finance.ExchangeRate.OperationMode;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@CopilotGenerated(model = "Claude Sonnet 3.5")
public class ManualCurrencyConversionTest {

  private ExchangeRateProvider rateProvider;
  private ConversionQuery conversionQuery;
  private ConversionContext conversionContext;
  private ExchangeRate exchangeRate;

  @BeforeEach
  void setUp() {
    rateProvider = mock(ExchangeRateProvider.class);
    conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(Monetary.getCurrency("USD"))
      .setTermCurrency(Monetary.getCurrency("EUR"))
      .build();
    conversionContext = ConversionContext.of(RateType.ANY);
    exchangeRate = mock(ExchangeRate.class);

    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency("USD"));
    when(exchangeRate.getCurrency()).thenReturn(Monetary.getCurrency("EUR"));
    when(exchangeRate.getFactor()).thenReturn(Money.of(0.9, "USD").getNumber());
    when(exchangeRate.getContext()).thenReturn(ConversionContext.of(RateType.ANY));
  }

  @Test
  void testGetExchangeRate() {
    var amount = Money.of(10, "USD");
    when(rateProvider.getExchangeRate(any(ConversionQuery.class))).thenReturn(exchangeRate);

    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext);
    var rate = conversion.getExchangeRate(amount);

    assertNotNull(rate);
    assertEquals(exchangeRate, rate);
  }

  @Test
  void testGetExchangeRateProvider() {
    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext);
    assertEquals(rateProvider, conversion.getExchangeRateProvider());
  }

  @Test
  void testWithConversionContext() {
    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext);
    var newContext = ConversionContext.of(RateType.DEFERRED);
    var newConversion = conversion.with(newContext);

    assertNotNull(newConversion);
    assertNotSame(conversion, newConversion);
  }

  @Test
  void testApplyMultiplyMode() {
    var amount = Money.of(10, "USD");
    when(rateProvider.getExchangeRate(any(ConversionQuery.class))).thenReturn(exchangeRate);

    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext, OperationMode.MULTIPLY);
    var result = conversion.apply(amount);

    assertNotNull(result);
    assertEquals("EUR", result.getCurrency().getCurrencyCode());
  }

  @Test
  void testApplyDivideModeWithSameCurrency() {
    var amount = Money.of(10, "EUR");
    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext, OperationMode.DIVIDE);

    var result = conversion.apply(amount);

    assertEquals(amount, result);
  }

  @Test
  void testApplyDivideModeWithDifferentCurrency() {
    var amount = Money.of(10, "USD");
    when(rateProvider.getExchangeRate(any(ConversionQuery.class))).thenReturn(exchangeRate);

    var contextWithScale = ConversionContext.of(RateType.ANY).toBuilder().set("exchangeRateScale", 2).build();
    when(exchangeRate.getContext()).thenReturn(contextWithScale);

    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext, OperationMode.DIVIDE);
    var result = conversion.apply(amount);

    assertNotNull(result);
    assertEquals("EUR", result.getCurrency().getCurrencyCode());
  }

  @Test
  void testApplyDivideModeWithNullExchangeRate() {
    var amount = Money.of(10, "USD");
    when(rateProvider.getExchangeRate(any(ConversionQuery.class))).thenReturn(null);

    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext, OperationMode.DIVIDE);

    assertThrows(CurrencyConversionException.class, () -> conversion.apply(amount));
  }

  @Test
  void testApplyDivideModeWithCurrencyMismatch() {
    var amount = Money.of(10, "GBP");
    when(rateProvider.getExchangeRate(any(ConversionQuery.class))).thenReturn(exchangeRate);
    when(exchangeRate.getBaseCurrency()).thenReturn(Monetary.getCurrency("USD")); // Different from amount currency

    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext, OperationMode.DIVIDE);

    assertThrows(CurrencyConversionException.class, () -> conversion.apply(amount));
  }

  @Test
  void testApplyDivideModeWithNegativeScale() {
    var amount = Money.of(10, "USD");
    when(rateProvider.getExchangeRate(any(ConversionQuery.class))).thenReturn(exchangeRate);
    var contextWithNegativeScale = ConversionContext.of(RateType.ANY).toBuilder().set("exchangeRateScale", -1).build();
    when(exchangeRate.getContext()).thenReturn(contextWithNegativeScale);

    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext, OperationMode.DIVIDE);
    var result = conversion.apply(amount);

    assertNotNull(result);
    assertEquals("EUR", result.getCurrency().getCurrencyCode());
  }

  @Test
  void testApplyDivideModeWithNullScale() {
    var amount = Money.of(10, "USD");
    when(rateProvider.getExchangeRate(any(ConversionQuery.class))).thenReturn(exchangeRate);
    var contextWithoutScale = ConversionContext.of(RateType.ANY);
    when(exchangeRate.getContext()).thenReturn(contextWithoutScale);

    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext, OperationMode.DIVIDE);
    var result = conversion.apply(amount);

    assertNotNull(result);
    assertEquals("EUR", result.getCurrency().getCurrencyCode());
  }

  @Test
  void testToString() {
    var conversion = new ManualCurrencyConversion(conversionQuery, rateProvider, conversionContext);
    var str = conversion.toString();
    assertTrue(str.contains("CurrencyConversion"));
  }
}
