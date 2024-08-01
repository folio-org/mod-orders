package org.folio.service.exchange;

import static org.javamoney.moneta.convert.ExchangeRateType.ECB;
import static org.javamoney.moneta.convert.ExchangeRateType.IDENTITY;

import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;
import javax.money.convert.MonetaryConversions;
import javax.money.convert.ProviderContext;
import javax.money.convert.ProviderContextBuilder;
import javax.money.convert.RateType;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.models.RequestContext;
import org.folio.spring.SpringContextUtil;
import org.javamoney.moneta.convert.ExchangeRateBuilder;
import org.javamoney.moneta.spi.DefaultNumberValue;

public class FinanceApiExchangeRateProvider implements ExchangeRateProvider {

  private final Logger logger = LogManager.getLogger();

  private static final ProviderContext CONTEXT;

  static {
    CONTEXT = ProviderContextBuilder.of("FRE", RateType.DEFERRED, RateType.ANY).set("providerDescription", "ThunderJet Finance API Exchange Rate Service").build();
  }

  public FinanceApiExchangeRateProvider(RequestContext requestContext) {
    SpringContextUtil.autowireDependencies(this, requestContext.getContext());
  }

  @Override
  public ProviderContext getContext() {
    return CONTEXT;
  }

  @Override
  public ExchangeRate getExchangeRate(ConversionQuery conversionQuery) {
    var exchangeRate = getExchangeRateFromService(conversionQuery);
    logger.info("getExchangeRateFromService:: exchangeRate: {}", exchangeRate.getExchangeRate());

    ExchangeRateBuilder builder = new ExchangeRateBuilder(ConversionContext.of());
    builder.setBase(conversionQuery.getBaseCurrency());
    builder.setTerm(conversionQuery.getCurrency());
    builder.setFactor(DefaultNumberValue.of(exchangeRate.getExchangeRate()));
    return builder.build();
  }

  private org.folio.rest.acq.model.finance.ExchangeRate getExchangeRateFromService(ConversionQuery conversionQuery) {
      double exchangeRate = MonetaryConversions.getExchangeRateProvider(IDENTITY, ECB)
        .getExchangeRate(conversionQuery.getBaseCurrency(), conversionQuery.getCurrency())
        .getFactor()
        .doubleValue();

      return new org.folio.rest.acq.model.finance.ExchangeRate()
        .withFrom(conversionQuery.getBaseCurrency().getCurrencyCode())
        .withTo(conversionQuery.getCurrency().getCurrencyCode())
        .withExchangeRate(exchangeRate);
  }
  @Override
  public CurrencyConversion getCurrencyConversion(ConversionQuery conversionQuery) {
    return new ManualCurrencyConversion(conversionQuery, this, ConversionContext.of(this.getContext().getProviderName(), RateType.ANY));
  }
}
