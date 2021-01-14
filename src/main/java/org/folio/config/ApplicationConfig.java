package org.folio.config;

import org.folio.dao.PrefixDAO;
import org.folio.dao.PrefixHttpDAO;
import org.folio.dao.PurchaseOrderDAO;
import org.folio.dao.PurchaseOrderHttpDAO;
import org.folio.dao.ReasonForClosureDAO;
import org.folio.dao.ReasonForClosureHttpDAO;
import org.folio.dao.SuffixDAO;
import org.folio.dao.SuffixHttpDAO;
import org.folio.service.PrefixService;
import org.folio.service.ReasonForClosureService;
import org.folio.service.SuffixService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

@Configuration
@ComponentScan({ "org.folio" })
@Import({ RestClientsConfiguration.class, ServicesConfiguration.class})
public class ApplicationConfig {

  @Bean
  public SuffixDAO suffixDAO() {
    return new SuffixHttpDAO();
  }

  @Bean
  public PrefixDAO prefixDAO() {
    return new PrefixHttpDAO();
  }

  @Bean
  public ReasonForClosureDAO reasonForClosureDAO() {
    return new ReasonForClosureHttpDAO();
  }

  @Bean
  public PurchaseOrderDAO purchaseOrderDAO() {
    return new PurchaseOrderHttpDAO();
  }

  @Bean
  public SuffixService suffixService() {
    return new SuffixService(restClient, purchaseOrderService);
  }

  @Bean
  public PrefixService prefixService() {
    return new PrefixService(restClient, purchaseOrderService);
  }

  @Bean
  public ReasonForClosureService reasonForClosureService() {
    return new ReasonForClosureService(restClient);
  }

  @Bean
  ExchangeRateProviderResolver exchangeRateProviderResolver() {
    return new ExchangeRateProviderResolver();
  }
}
