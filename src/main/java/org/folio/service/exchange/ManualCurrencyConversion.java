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
import org.folio.rest.acq.model.finance.ExchangeRate.OperationMode;

public class ManualCurrencyConversion extends AbstractCurrencyConversion {

  private static final String EXCHANGE_RATE_SCALE = "exchangeRateScale";

  private final ExchangeRateProvider rateProvider;
  private final ConversionQuery conversionQuery;
  private final OperationMode operationMode;

  public ManualCurrencyConversion(ConversionQuery conversionQuery, ExchangeRateProvider rateProvider,
                                  ConversionContext conversionContext) {
    this(conversionQuery, rateProvider, conversionContext, OperationMode.MULTIPLY);
  }

  public ManualCurrencyConversion(ConversionQuery conversionQuery, ExchangeRateProvider rateProvider,
                                  ConversionContext conversionContext, OperationMode operationMode) {
    super(conversionQuery.getCurrency(), conversionContext);
    this.conversionQuery = conversionQuery;
    this.rateProvider = rateProvider;
    this.operationMode = operationMode;
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

  /**
   * Apply conversion calculation using either MULTIPLY (default) or DIVIDE operation modes.
   * The default operation mode MULTIPLY applies multiplication operation on the total amount before conversion using
   * either of the two exchange rates, e.g. AUD->USD ECB/IMF exchange rate is 0.66 while for USD->AUD it is 1.536
   * The newer DIVIDE operation mode adds support for reverse conversion for cases when just a single, manual
   * exchange rate is present (set in the POL), and so in this case the only way is to convert back
   * by the same exchange rate in a division operation.
   *
   * @param amount Before conversion
   * @return amount After conversion
   */
  @Override
  public MonetaryAmount apply(MonetaryAmount amount) {
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
      var scale = rate.getContext().get(EXCHANGE_RATE_SCALE, Integer.class);
      amount = amount.divide(factor).getFactory().setCurrency(rate.getCurrency()).create();
      return Objects.nonNull(scale) && scale >= 0 ? amount.with(MonetaryOperators.rounding(scale)) : amount;
    }
  }

  @Override
  public String toString() {
    return "CurrencyConversion [MonetaryAmount -> MonetaryAmount"
      + "; provider=" + this.rateProvider
      + ", context=" + this.getContext()
      + ", baseCurrency=" + this.conversionQuery.getBaseCurrency()
      + ", termCurrency=" + this.getCurrency()
      + ']';
  }
}
