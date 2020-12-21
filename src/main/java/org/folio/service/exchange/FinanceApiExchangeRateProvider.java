package org.folio.service.exchange;

import java.util.concurrent.CompletableFuture;

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
      .thenApply(exchangeRate -> {
        ExchangeRateBuilder builder = new ExchangeRateBuilder(ConversionContext.of());
        builder.setBase(conversionQuery.getBaseCurrency());
        builder.setTerm(conversionQuery.getCurrency());
        builder.setFactor(DefaultNumberValue.of(exchangeRate.getExchangeRate()));
        return builder.build();
      }).join();
  }

  private CompletableFuture<org.folio.rest.acq.model.finance.ExchangeRate> getExchangeRateFromService(ConversionQuery conversionQuery) {
    return CompletableFuture.supplyAsync(() -> financeExchangeRateService
      .getExchangeRate(conversionQuery.getBaseCurrency().getCurrencyCode(), conversionQuery.getCurrency().getCurrencyCode(),
        requestContext).join());
  }
  @Override
  public CurrencyConversion getCurrencyConversion(ConversionQuery conversionQuery) {
    return new ManualCurrencyConversion(conversionQuery, this, ConversionContext.of(this.getContext().getProviderName(), RateType.ANY));
  }
}
