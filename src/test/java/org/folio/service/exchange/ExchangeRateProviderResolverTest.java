package org.folio.service.exchange;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.isVerticleNotDeployed;

import io.vertx.core.Context;
import io.vertx.junit5.VertxExtension;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import javax.money.convert.ConversionQuery;
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
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
class ExchangeRateProviderResolverTest {

  @InjectMocks
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
  }

  @Test
  void testResolveWithDynamicExchangeRate() {
    String fromCurrency = "USD";
    String toCurrency = "AUD";

    ConversionQuery conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .build();

    ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);

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
    String fromCurrency = "USD";
    String toCurrency = "AUD";
    Double exchangeRateValue = 0.66d;

    ConversionQuery conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .set(ExchangeRateProviderResolver.RATE_KEY, exchangeRateValue)
      .build();

    ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);

    Assertions.assertNotNull(exchangeRateProvider);

    ExchangeRate exchangeRate = exchangeRateProvider.getExchangeRate(conversionQuery);

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

}
