package org.folio.config;

import static org.folio.orders.utils.ResourcePathResolver.FINANCE_EXCHANGE_RATE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import org.folio.rest.core.RestClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RestClientsConfiguration {

  @Bean
  RestClient exchangeRateRestClient() {
    return new RestClient(resourcesPath(FINANCE_EXCHANGE_RATE));
  }
}
