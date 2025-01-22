package org.folio.service.exchange;

import lombok.extern.log4j.Log4j2;
import org.javamoney.moneta.spi.AbstractRateProvider;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ProviderContext;
import javax.money.convert.ProviderContextBuilder;
import javax.money.convert.RateType;

import static org.folio.service.exchange.FolioExchangeRateType.FOLIO_HISTORY;

@Log4j2
public class FolioHistoryCurrencyRateProvider extends AbstractRateProvider {

  private static final ProviderContext CONTEXT = ProviderContextBuilder.of(FOLIO_HISTORY.get(), RateType.HISTORIC)
    .set("providerDescription", FOLIO_HISTORY.getDescription())
    .build();

  public FolioHistoryCurrencyRateProvider() {
    super(CONTEXT);
  }

  @Override
  public ProviderContext getContext() {
    return CONTEXT;
  }

  @Override
  public ExchangeRate getExchangeRate(ConversionQuery conversionQuery) {
    return null;
  }
}
