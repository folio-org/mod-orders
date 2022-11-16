package org.folio.service.exchange;

import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;
import javax.money.convert.ProviderContext;
import javax.money.convert.ProviderContextBuilder;
import javax.money.convert.RateType;

import org.folio.rest.core.models.RequestContext;
import org.folio.spring.SpringContextUtil;
import org.javamoney.moneta.convert.ExchangeRateBuilder;
import org.javamoney.moneta.spi.DefaultNumberValue;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Future;

public class FinanceApiExchangeRateProvider implements ExchangeRateProvider {
  private static final ProviderContext CONTEXT;
  private final RequestContext requestContext;

  @Autowired
  private FinanceExchangeRateService financeExchangeRateService;

  static {
    CONTEXT = ProviderContextBuilder.of("FRE", RateType.DEFERRED, RateType.ANY).set("providerDescription", "ThunderJet Finance API Exchange Rate Service").build();
  }

  public FinanceApiExchangeRateProvider(RequestContext requestContext) {
    SpringContextUtil.autowireDependencies(this, requestContext.getContext());
    this.requestContext = requestContext;
  }

  @Override
  public ProviderContext getContext() {
    return CONTEXT;
  }

  @Override
  public ExchangeRate getExchangeRate(ConversionQuery conversionQuery) {
    return getExchangeRateFromService(conversionQuery)
      .map(exchangeRate -> {
        ExchangeRateBuilder builder = new ExchangeRateBuilder(ConversionContext.of());
        builder.setBase(conversionQuery.getBaseCurrency());
        builder.setTerm(conversionQuery.getCurrency());
        builder.setFactor(DefaultNumberValue.of(exchangeRate.getExchangeRate()));
        return builder.build();
      }).result();
  }

  private Future<org.folio.rest.acq.model.finance.ExchangeRate> getExchangeRateFromService(ConversionQuery conversionQuery) {
    var baseCurrencyCode = conversionQuery.getBaseCurrency().getCurrencyCode();
    var conversionCurrencyCode = conversionQuery.getCurrency().getCurrencyCode();
    return Future.succeededFuture()
      .compose(v -> financeExchangeRateService.getExchangeRate(baseCurrencyCode,conversionCurrencyCode, requestContext));
  }
  @Override
  public CurrencyConversion getCurrencyConversion(ConversionQuery conversionQuery) {
    return new ManualCurrencyConversion(conversionQuery, this, ConversionContext.of(this.getContext().getProviderName(), RateType.ANY));
  }
}
