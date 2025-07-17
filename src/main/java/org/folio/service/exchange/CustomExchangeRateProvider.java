package org.folio.service.exchange;

import org.folio.rest.core.exceptions.HttpException;
import org.javamoney.moneta.convert.ExchangeRateBuilder;
import org.javamoney.moneta.spi.DefaultNumberValue;

import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;
import javax.money.convert.ProviderContext;
import javax.money.convert.ProviderContextBuilder;
import javax.money.convert.RateType;
import org.folio.rest.acq.model.finance.ExchangeRate.OperationMode;

public class CustomExchangeRateProvider implements ExchangeRateProvider {

  private static final ProviderContext CONTEXT = ProviderContextBuilder.of("CUSTOM", RateType.DEFERRED, RateType.ANY)
    .set("providerDescription", "Custom exchange rate provider")
    .build();
  public static final String RATE_KEY = "factor";

  private final OperationMode operationMode;

  public CustomExchangeRateProvider() {
    this.operationMode = OperationMode.MULTIPLY;
  }

  public CustomExchangeRateProvider(OperationMode operationMode) {
    this.operationMode = operationMode;
  }

  @Override
  public ProviderContext getContext() {
    return CONTEXT;
  }

  @Override
  public ExchangeRate getExchangeRate(ConversionQuery conversionQuery) {
    var builder = new ExchangeRateBuilder(ConversionContext.of());
    builder.setBase(conversionQuery.getBaseCurrency());
    builder.setTerm(conversionQuery.getCurrency());
    if (conversionQuery.get(RATE_KEY, Double.class) == null) {
      throw new HttpException(400, "Rate must be provided in provider : " + this.getClass().getSimpleName());
    }
    builder.setFactor(DefaultNumberValue.of(conversionQuery.get(RATE_KEY, Double.class)));

    return builder.build();
  }

  @Override
  public CurrencyConversion getCurrencyConversion(ConversionQuery conversionQuery) {
    return new ManualCurrencyConversion(conversionQuery, this, ConversionContext.of(this.getContext().getProviderName(), RateType.ANY), operationMode);
  }
}
