package org.folio.service.exchange;

import java.util.Optional;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ExchangeRateProvider;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.models.RequestContext;

public class ExchangeRateProviderResolver {
  private final Logger logger = LogManager.getLogger();
  public static final String RATE_KEY = "factor";


  public ExchangeRateProvider resolve(ConversionQuery conversionQuery, RequestContext requestContext) {
    return resolve(conversionQuery, requestContext, ManualExchangeRateProvider.OperationMode.MULTIPLY);
  }

  public ExchangeRateProvider resolve(ConversionQuery conversionQuery, RequestContext requestContext, ManualExchangeRateProvider.OperationMode operationMode) {
    ExchangeRateProvider exchangeRateProvider = Optional.ofNullable(conversionQuery)
            .map(query -> query.get(RATE_KEY, Double.class))
            .map(rate -> (ExchangeRateProvider) new ManualExchangeRateProvider(operationMode))
            .orElseGet(() -> new FinanceApiExchangeRateProvider(requestContext));
    logger.info("Created ExchangeRateProvider name: {}, operationMode: {}", exchangeRateProvider.getContext().getProviderName(), operationMode);
    return exchangeRateProvider;
  }
}
