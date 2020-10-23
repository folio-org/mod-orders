package org.folio.config;

import org.folio.rest.core.RestClient;
import org.folio.service.exchange.FinanceExchangeRateService;
import org.springframework.context.annotation.Bean;

public class ServicesConfiguration {
  @Bean
  FinanceExchangeRateService rateOfExchangeService(RestClient exchangeRateRestClient) {
    return new FinanceExchangeRateService(exchangeRateRestClient);
  }
}
