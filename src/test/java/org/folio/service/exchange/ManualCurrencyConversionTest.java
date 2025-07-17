package org.folio.service.exchange;

import static org.folio.rest.acq.model.finance.ExchangeRate.OperationMode.DIVIDE;
import static org.folio.rest.acq.model.finance.ExchangeRate.OperationMode.MULTIPLY;
import static org.folio.service.exchange.CustomExchangeRateProvider.RATE_KEY;

import javax.money.Monetary;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQueryBuilder;

import org.folio.rest.acq.model.finance.ExchangeRate;
import org.javamoney.moneta.Money;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

public class ManualCurrencyConversionTest {

  @ParameterizedTest
  @EnumSource(ExchangeRate.OperationMode.class)
  void testApplyWithOperationModes(ExchangeRate.OperationMode operationMode) {
    var totalAmount = 10;
    var fromCurrency = "USD";
    var toCurrency = "AUD";

    var provider = new CustomExchangeRateProvider(operationMode);
    var query = ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .set(RATE_KEY, operationMode == MULTIPLY ? 1.58d : 0.63d)
      .build();
    var conversion = provider.getCurrencyConversion(query);

    var totalAmountBeforeConversion = Money.of(totalAmount, fromCurrency).with(conversion).with(Monetary.getDefaultRounding());
    var manualCurrencyConversion = new ManualCurrencyConversion(query, provider, ConversionContext.of(), operationMode);
    var totalAmountAfterConversion = manualCurrencyConversion.apply(totalAmountBeforeConversion).getNumber();

    System.out.println(totalAmountAfterConversion.doubleValue());

    Assertions.assertTrue(totalAmountAfterConversion.doubleValue() > 0);
  }

  @Test
  void testApplyWithSameCurrencies() {
    var totalAmount = 10d;
    var fromCurrency = "USD";
    var toCurrency = "USD";

    var provider = new CustomExchangeRateProvider();
    var query = ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .build();
    var conversion = provider.getCurrencyConversion(query);

    var totalAmountBeforeConversion = Money.of(totalAmount, fromCurrency).with(conversion).with(Monetary.getDefaultRounding());
    var manualCurrencyConversion = new ManualCurrencyConversion(query, provider, ConversionContext.of(), DIVIDE);
    var totalAmountAfterConversion = manualCurrencyConversion.apply(totalAmountBeforeConversion).getNumber();

    Assertions.assertEquals(totalAmount, totalAmountAfterConversion.doubleValue());
  }
}
