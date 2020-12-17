package org.folio.config;

import org.folio.rest.core.RestClient;
import org.folio.service.exchange.FinanceExchangeRateService;
import org.folio.service.finance.FundService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.orders.RolloverOrderService;
import org.springframework.context.annotation.Bean;

public class ServicesConfiguration {
  @Bean
  FinanceExchangeRateService rateOfExchangeService(RestClient exchangeRateRestClient) {
    return new FinanceExchangeRateService(exchangeRateRestClient);
  }

  @Bean
  PurchaseOrderService purchaseOrderService(RestClient purchaseOrderRestClient) {
    return new PurchaseOrderService(purchaseOrderRestClient);
  }

  @Bean
  PurchaseOrderLineService purchaseOrderLineService(RestClient orderLinesRestClient) {
    return new PurchaseOrderLineService(orderLinesRestClient);
  }

  @Bean
  FundService fundService(RestClient fundRestClient) {
    return new FundService(fundRestClient);
  }

  @Bean
  RolloverOrderService rolloverOrderService(FundService fundService, PurchaseOrderService purchaseOrderService,
                                            PurchaseOrderLineService purchaseOrderLineService, RestClient transactionRestClient) {
    return new RolloverOrderService(fundService, purchaseOrderService, purchaseOrderLineService, transactionRestClient);
  }


}
