package org.folio.service.exchange;

import javax.money.MonetaryAmount;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;

import org.javamoney.moneta.spi.AbstractCurrencyConversion;

public class ManualCurrencyConversion extends AbstractCurrencyConversion {
  private final ExchangeRateProvider rateProvider;
  private final ConversionQuery conversionQuery;

  public ManualCurrencyConversion(ConversionQuery conversionQuery, ExchangeRateProvider rateProvider, ConversionContext conversionContext) {
    super(conversionQuery.getCurrency(), conversionContext);
    this.conversionQuery = conversionQuery;
    this.rateProvider = rateProvider;
  }

  @Override
  public ExchangeRate getExchangeRate(MonetaryAmount amount) {
    return this.rateProvider.getExchangeRate(ConversionQueryBuilder.of(this.conversionQuery).setBaseCurrency(amount.getCurrency()).build());
  }

  @Override
  public ExchangeRateProvider getExchangeRateProvider() {
    return this.rateProvider;
  }

  @Override
  public CurrencyConversion with(ConversionContext conversionContext) {
    return new ManualCurrencyConversion(this.conversionQuery, this.rateProvider, conversionContext);
  }

  @Override
  public String toString() {
    return "CurrencyConversion [MonetaryAmount -> MonetaryAmount; provider=" + this.rateProvider + ", context=" + this.getContext() + ", termCurrency=" + this.getCurrency() + ']';
  }

}
