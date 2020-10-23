package org.folio.service.exchange;

import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import java.util.Optional;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ExchangeRateProvider;
import org.folio.rest.core.models.RequestContext;

public class ExchangeRateProviderResolver {
  private final Logger logger = LoggerFactory.getLogger(this.getClass());
  public static final String RATE_KEY = "factor";

  public ExchangeRateProvider resolve(ConversionQuery conversionQuery, RequestContext requestContext){
    ExchangeRateProvider exchangeRateProvider = Optional.ofNullable(conversionQuery)
            .map(query -> query.get(RATE_KEY, Double.class))
            .map(rate -> (ExchangeRateProvider) new ManualExchangeRateProvider())
            .orElseGet(() -> new FinanceApiExchangeRateProvider(requestContext));
    logger.debug("Created ExchangeRateProvider name: {}", exchangeRateProvider.getContext().getProviderName());
    return exchangeRateProvider;
  }
}
