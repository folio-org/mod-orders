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
import javax.money.Monetary;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;

import org.javamoney.moneta.Money;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

@ExtendWith(VertxExtension.class)
public class ManualCurrencyConversionTest {

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

  @ParameterizedTest
  @EnumSource(ManualCurrencyConversion.OperationMode.class)
  void testApplyWithOperationModes(ManualCurrencyConversion.OperationMode operationMode) {
    var totalAmount = 6.51d;
    var fromCurrency = "USD";
    var toCurrency = "AUD";

    var manualExchangeRateProvider = new ManualExchangeRateProvider();

    ConversionQuery conversionQuery;
    if (operationMode == ManualCurrencyConversion.OperationMode.MULTIPLY) {
      conversionQuery = ConversionQueryBuilder.of()
        .setBaseCurrency(fromCurrency)
        .setTermCurrency(toCurrency)
        .build();
    } else {
      conversionQuery = ConversionQueryBuilder.of()
        .setBaseCurrency(fromCurrency)
        .setTermCurrency(toCurrency)
        .set(ExchangeRateProviderResolver.RATE_KEY, 0.66d)
        .build();
    }

    var exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext, operationMode);
    var currencyConversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
    var totalAmountBeforeConversion = Money.of(totalAmount, fromCurrency).with(currencyConversion).with(Monetary.getDefaultRounding());
    var manualCurrencyConversion = new ManualCurrencyConversion(conversionQuery, manualExchangeRateProvider, ConversionContext.of(), operationMode);
    var totalAmountAfterConversion = manualCurrencyConversion.apply(totalAmountBeforeConversion).getNumber();

    Assertions.assertTrue(totalAmountAfterConversion.doubleValue() > 0);
  }

  @Test
  void testApplyWithSameCurrencies() {
    var totalAmount = 10d;
    var fromCurrency = "USD";
    var toCurrency = "USD";

    var manualExchangeRateProvider = new ManualExchangeRateProvider();

    var conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .build();

    var exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
    var currencyConversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
    var totalAmountBeforeConversion = Money.of(totalAmount, fromCurrency).with(currencyConversion).with(Monetary.getDefaultRounding());
    var manualCurrencyConversion = new ManualCurrencyConversion(conversionQuery, manualExchangeRateProvider, ConversionContext.of(), ManualCurrencyConversion.OperationMode.DIVIDE);
    var totalAmountAfterConversion = manualCurrencyConversion.apply(totalAmountBeforeConversion).getNumber();

    Assertions.assertEquals(totalAmount, totalAmountAfterConversion.doubleValue());
  }

  private static class ContextConfiguration {
    @Bean
    ExchangeRateProviderResolver exchangeRateProviderResolver() {
      return new ExchangeRateProviderResolver();
    }
  }
}
