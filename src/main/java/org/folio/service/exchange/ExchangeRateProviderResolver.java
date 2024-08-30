package org.folio.service.exchange;

import java.util.Optional;

import javax.money.convert.ConversionQuery;
import javax.money.convert.ExchangeRateProvider;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.models.RequestContext;

public class ExchangeRateProviderResolver {

  public static final String RATE_KEY = "factor";

  private final Logger logger = LogManager.getLogger();

  public ExchangeRateProvider resolve(ConversionQuery conversionQuery, RequestContext requestContext) {
    return resolve(conversionQuery, requestContext, ManualCurrencyConversion.OperationMode.MULTIPLY);
  }

  public ExchangeRateProvider resolve(ConversionQuery conversionQuery, RequestContext requestContext, ManualCurrencyConversion.OperationMode operationMode) {
    ExchangeRateProvider exchangeRateProvider = Optional.ofNullable(conversionQuery)
            .map(query -> query.get(RATE_KEY, Double.class))
            .map(rate -> (ExchangeRateProvider) new ManualExchangeRateProvider(operationMode))
            .orElseGet(() -> new FinanceApiExchangeRateProvider(requestContext));
    logger.info("resolve:: exchangeRateProvider name: {}, operationMode: {}", exchangeRateProvider.getContext().getProviderName(), operationMode);
    return exchangeRateProvider;
  }
}
