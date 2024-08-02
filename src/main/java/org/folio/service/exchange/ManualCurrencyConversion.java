package org.folio.service.exchange;

import java.util.Objects;
import javax.money.MonetaryAmount;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.CurrencyConversionException;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;

import org.javamoney.moneta.function.MonetaryOperators;
import org.javamoney.moneta.spi.AbstractCurrencyConversion;

public class ManualCurrencyConversion extends AbstractCurrencyConversion {
  private final ExchangeRateProvider rateProvider;
  private final ConversionQuery conversionQuery;
  private final OperationMode operationMode;

  public ManualCurrencyConversion(ConversionQuery conversionQuery, ExchangeRateProvider rateProvider, ConversionContext conversionContext) {
    super(conversionQuery.getCurrency(), conversionContext);
    this.conversionQuery = conversionQuery;
    this.rateProvider = rateProvider;
    this.operationMode = OperationMode.MULTIPLY;
  }

  public ManualCurrencyConversion(ConversionQuery conversionQuery, ExchangeRateProvider rateProvider, ConversionContext conversionContext,
                                  OperationMode operationMode) {
    super(conversionQuery.getCurrency(), conversionContext);
    this.conversionQuery = conversionQuery;
    this.rateProvider = rateProvider;
    this.operationMode = operationMode;
  }

  public enum OperationMode {
    MULTIPLY,
    DIVIDE
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
  public MonetaryAmount apply(MonetaryAmount amount) {
    // Add support for reverse conversion for cases when just a single manual exchange rate is set in the POL
    // i.e. ECB dynamic rates would be: AUD -> USD will use ~0.66, USD -> AUD will use ~1.55
    // but in case of manual exchange rate this second exchange rate is not provided
    if (this.operationMode == OperationMode.MULTIPLY) {
      return super.apply(amount);
    }
    if (super.getCurrency().equals(Objects.requireNonNull(amount).getCurrency())) {
      return amount;
    } else {
      var rate = this.getExchangeRate(amount);
      if (Objects.isNull(rate) || !amount.getCurrency().equals(rate.getBaseCurrency())) {
        throw new CurrencyConversionException(amount.getCurrency(), super.getCurrency(), null);
      }
      var factor = this.roundFactor(amount, rate.getFactor());
      var scale = rate.getContext().get("exchangeRateScale", Integer.class);
      amount = amount.divide(factor).getFactory().setCurrency(rate.getCurrency()).create();
      return Objects.nonNull(scale) && scale >= 0 ? amount.with(MonetaryOperators.rounding(scale)) : amount;
    }
  }

  @Override
  public String toString() {
    return "CurrencyConversion [MonetaryAmount -> MonetaryAmount; provider=" + this.rateProvider + ", context=" + this.getContext() + ", termCurrency=" + this.getCurrency() + ']';
  }

}
