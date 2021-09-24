package org.folio.service.exchange;


import static org.folio.service.exchange.ExchangeRateProviderResolver.RATE_KEY;

import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;
import javax.money.convert.ProviderContext;
import javax.money.convert.ProviderContextBuilder;
import javax.money.convert.RateType;

import org.folio.rest.core.exceptions.HttpException;
import org.javamoney.moneta.convert.ExchangeRateBuilder;
import org.javamoney.moneta.spi.DefaultNumberValue;

public class ManualExchangeRateProvider implements ExchangeRateProvider {
  private static final ProviderContext CONTEXT;

  static {
    CONTEXT = ProviderContextBuilder.of("TRE", RateType.DEFERRED, RateType.ANY).set("providerDescription", "ThunderJet Manual Exchange Rate Service").build();
  }

  @Override
  public ProviderContext getContext() {
    return CONTEXT;
  }

  @Override
  public ExchangeRate getExchangeRate(ConversionQuery conversionQuery) {
    ExchangeRateBuilder builder = new ExchangeRateBuilder(ConversionContext.of());
    builder.setBase(conversionQuery.getBaseCurrency());
    builder.setTerm(conversionQuery.getCurrency());
    if (conversionQuery.get(RATE_KEY, Double.class) == null) {
      throw new HttpException(500, "Rate must be provided in provider : " + this.getClass().getSimpleName());
    }
    builder.setFactor(DefaultNumberValue.of(conversionQuery.get(RATE_KEY, Double.class)));
    return builder.build();
  }

  @Override
  public CurrencyConversion getCurrencyConversion(ConversionQuery conversionQuery) {
    return new ManualCurrencyConversion(conversionQuery, this, ConversionContext.of(this.getContext().getProviderName(), RateType.ANY));
  }
}
