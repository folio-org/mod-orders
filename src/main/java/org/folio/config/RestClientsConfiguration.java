package org.folio.config;

import static org.folio.orders.utils.ResourcePathResolver.FINANCE_EXCHANGE_RATE;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.TRANSACTIONS_ENDPOINT;
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

  @Bean
  RestClient fundRestClient() {
    return new RestClient(resourcesPath(FUNDS));
  }

  @Bean
  RestClient orderLinesRestClient() {
    return new RestClient(resourcesPath(PO_LINES));
  }

  @Bean
  RestClient purchaseOrderRestClient() {
    return new RestClient(resourcesPath(PURCHASE_ORDER));
  }

  @Bean
  RestClient transactionRestClient() {
    return new RestClient(resourcesPath(TRANSACTIONS_ENDPOINT));
  }
}
