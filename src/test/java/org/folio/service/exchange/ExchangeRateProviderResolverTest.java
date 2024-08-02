package org.folio.service.exchange;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;

import io.vertx.core.Context;
import io.vertx.junit5.VertxExtension;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.ExchangeRate;
import javax.money.convert.ExchangeRateProvider;
import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

@ExtendWith(VertxExtension.class)
public class ExchangeRateProviderResolverTest {

  @Autowired
  private ExchangeRateProviderResolver exchangeRateProviderResolver;

  private final Context ctx = getFirstContextFromVertx(getVertx());

  @Mock
  private Map<String, String> okapiHeadersMock;

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ContextConfiguration.class);
  }

  @Test
  void testResolveWithDynamicExchangeRate() {
    var fromCurrency = "USD";
    var toCurrency = "AUD";

    var conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .build();

    var exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);

    Assertions.assertNotNull(exchangeRateProvider);

    ExchangeRate exchangeRate = exchangeRateProvider.getExchangeRate(conversionQuery);

    Assertions.assertNotNull(exchangeRate);
    Assertions.assertNotNull(exchangeRateProvider.getCurrencyConversion(conversionQuery));

    Assertions.assertEquals(fromCurrency, exchangeRate.getBaseCurrency().toString());
    Assertions.assertEquals(toCurrency, exchangeRate.getCurrency().toString());
    Assertions.assertNotNull(exchangeRate.getFactor());
  }

  @Test
  void testResolveWithManualExchangeRate() {
    var fromCurrency = "USD";
    var toCurrency = "AUD";
    var exchangeRateValue = 0.66d;

    var conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .set(ExchangeRateProviderResolver.RATE_KEY, exchangeRateValue)
      .build();

    var exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);

    Assertions.assertNotNull(exchangeRateProvider);

    var exchangeRate = exchangeRateProvider.getExchangeRate(conversionQuery);

    Assertions.assertNotNull(exchangeRate);
    Assertions.assertNotNull(exchangeRateProvider.getCurrencyConversion(conversionQuery));

    Assertions.assertEquals(fromCurrency, exchangeRate.getBaseCurrency().toString());
    Assertions.assertEquals(toCurrency, exchangeRate.getCurrency().toString());
    Assertions.assertEquals(exchangeRateValue, exchangeRate.getFactor().doubleValue());
  }

  @Test
  void testResolveWithNullConversionQuery() {
    ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(null, requestContext);

    Assertions.assertNotNull(exchangeRateProvider);
  }

  private static class ContextConfiguration {
    @Bean
    ExchangeRateProviderResolver exchangeRateProviderResolver() {
      return new ExchangeRateProviderResolver();
    }
  }
}
