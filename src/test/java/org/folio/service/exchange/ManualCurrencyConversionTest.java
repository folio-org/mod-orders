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
import javax.money.MonetaryAmount;
import javax.money.convert.ConversionContext;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;
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
  @EnumSource(ManualExchangeRateProvider.OperationMode.class)
  void testApplyWithOperationModes(ManualExchangeRateProvider.OperationMode operationMode) {
    Double totalAmount = 6.51d;
    String fromCurrency = "USD";
    String toCurrency = "AUD";

    ManualExchangeRateProvider manualExchangeRateProvider = new ManualExchangeRateProvider();
    ConversionQuery conversionQuery;
    if (operationMode == ManualExchangeRateProvider.OperationMode.MULTIPLY) {
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

    ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
    CurrencyConversion currencyConversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
    MonetaryAmount totalAmountBeforeConversion = Money.of(totalAmount, fromCurrency).with(currencyConversion).with(Monetary.getDefaultRounding());
    ManualCurrencyConversion manualCurrencyConversion = new ManualCurrencyConversion(conversionQuery, manualExchangeRateProvider, ConversionContext.of(), operationMode);
    Number totalAmountAfterConversion = manualCurrencyConversion.apply(totalAmountBeforeConversion).getNumber();

    Assertions.assertTrue(totalAmountAfterConversion.doubleValue() > 0);
  }

  @Test
  void testApplyWithSameCurrencies() {
    Double totalAmount = 10d;
    String fromCurrency = "USD";
    String toCurrency = "USD";

    ManualExchangeRateProvider manualExchangeRateProvider = new ManualExchangeRateProvider();
    ConversionQuery conversionQuery = ConversionQueryBuilder.of()
      .setBaseCurrency(fromCurrency)
      .setTermCurrency(toCurrency)
      .build();

    ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
    CurrencyConversion currencyConversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
    MonetaryAmount totalAmountBeforeConversion = Money.of(totalAmount, fromCurrency).with(currencyConversion).with(Monetary.getDefaultRounding());
    ManualCurrencyConversion manualCurrencyConversion = new ManualCurrencyConversion(conversionQuery, manualExchangeRateProvider, ConversionContext.of(), ManualExchangeRateProvider.OperationMode.DIVIDE);
    Number totalAmountAfterConversion = manualCurrencyConversion.apply(totalAmountBeforeConversion).getNumber();

    Assertions.assertEquals(totalAmount, totalAmountAfterConversion.doubleValue());
  }

  private static class ContextConfiguration {
    @Bean
    ExchangeRateProviderResolver exchangeRateProviderResolver() {
      return new ExchangeRateProviderResolver();
    }
  }
}
