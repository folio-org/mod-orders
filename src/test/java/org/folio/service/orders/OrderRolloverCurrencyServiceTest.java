package org.folio.service.orders;

import static java.util.Collections.singletonList;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.mockito.Mockito.mock;

import io.vertx.core.Context;
import io.vertx.junit5.VertxExtension;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;
import org.folio.ApiTestSuite;
import org.folio.dao.FailedLedgerRolloverPoLineDao;
import org.folio.models.PoLineEncumbrancesHolder;
import org.folio.rest.acq.model.finance.Encumbrance;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.FundService;
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.transaction.TransactionService;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

@ExtendWith(VertxExtension.class)
public class OrderRolloverCurrencyServiceTest {

  @Autowired
  private OrderRolloverService orderRolloverService;

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

  private static Stream<Arguments> testAmountWithConversionArgs() {
    return Stream.of(
      Arguments.of("USD", "USD", 10d, 10d, 10d, null, 100d),
      Arguments.of("USD", "AUD", 10d, 6.51d, 10d, null, 100d),
      Arguments.of("AUD", "USD", 6.51d, 10d, 6.51d, null, 100d),
      Arguments.of("USD", "UZS", 10d, 9d, 10d, 0.9d, 100d)
    );
  }

  @ParameterizedTest
  @MethodSource("testAmountWithConversionArgs")
  void testAmountWithConversion(String fromCurrency, String toCurrency, Double poLineEstimatedPrice, Double initialAmountEncumbered, Double totalAmount, Double exchangeRate, Double fundAllocation) {
    var purchaseOrderId = UUID.randomUUID().toString();
    var poLineId = UUID.randomUUID().toString();
    var transactionId = UUID.randomUUID().toString();
    var encumbranceId = UUID.randomUUID().toString();
    var fundId = UUID.randomUUID().toString();

    var fundDistribution = new FundDistribution()
      .withFundId(fundId)
      .withValue(fundAllocation)
      .withDistributionType(DistributionType.PERCENTAGE);
    var cost = new Cost()
      .withCurrency(toCurrency)
      .withPoLineEstimatedPrice(poLineEstimatedPrice);
    if (Objects.nonNull(exchangeRate)) {
      cost.setExchangeRate(exchangeRate);
    }

    var poLine = new PoLine()
      .withId(encumbranceId)
      .withId(poLineId)
      .withPurchaseOrderId(purchaseOrderId)
      .withCost(cost)
      .withFundDistribution(singletonList(fundDistribution));

    var encumbrance = new Encumbrance()
      .withSourcePurchaseOrderId(purchaseOrderId)
      .withSourcePoLineId(poLineId)
      .withOrderType(Encumbrance.OrderType.ONE_TIME)
      .withInitialAmountEncumbered(initialAmountEncumbered);
    var transaction = new Transaction()
      .withId(transactionId)
      .withFromFundId(fundId)
      .withCurrency(fromCurrency)
      .withEncumbrance(encumbrance);

    var currencyConversion = orderRolloverService.retrieveCurrencyConversion(fromCurrency, poLine.getCost(), requestContext);

    var holder = new PoLineEncumbrancesHolder(poLine)
      .withEncumbrances(singletonList(transaction))
      .withCurrencyConversion(currencyConversion)
      .withSystemCurrency(fromCurrency);

    var totalAmountAfterConversion = orderRolloverService.calculateTotalInitialAmountEncumbered(holder);

    if (Objects.nonNull(exchangeRate)) {
      Assertions.assertEquals(BigDecimal.valueOf(totalAmount), totalAmountAfterConversion);
    } else {
      Assertions.assertNotNull(totalAmountAfterConversion);
      Assertions.assertTrue(totalAmount <= Math.round(totalAmountAfterConversion.doubleValue()));
    }
  }

  private static class ContextConfiguration {
    @Bean
    FundService fundService() {
      return mock(FundService.class);
    }

    @Bean
    PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean
    TransactionService transactionService() {
      return mock(TransactionService.class);
    }

    @Bean
    ConfigurationEntriesCache configurationEntriesCache() {
      return mock(ConfigurationEntriesCache.class);
    }

    @Bean
    ExchangeRateProviderResolver exchangeRateProviderResolver() {
      // Using a real bean for currency conversion purposes
      return new ExchangeRateProviderResolver();
    }

    @Bean
    LedgerRolloverProgressService ledgerRolloverProgressService() {
      return mock(LedgerRolloverProgressService.class);
    }

    @Bean
    LedgerRolloverErrorService ledgerRolloverErrorService() {
      return mock(LedgerRolloverErrorService.class);
    }

    @Bean
    FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao() {
      return mock(FailedLedgerRolloverPoLineDao.class);
    }

    @Bean
    OrderRolloverService orderRolloverService(FundService fundService,
                                              PurchaseOrderLineService purchaseOrderLineService,
                                              TransactionService transactionService,
                                              ConfigurationEntriesCache configurationEntriesCache,
                                              ExchangeRateProviderResolver exchangeRateProviderResolver,
                                              LedgerRolloverProgressService ledgerRolloverProgressService,
                                              LedgerRolloverErrorService ledgerRolloverErrorService,
                                              FailedLedgerRolloverPoLineDao failedLedgerRolloverPoLineDao) {
      return new OrderRolloverService(fundService,
        purchaseOrderLineService,
        transactionService,
        configurationEntriesCache,
        exchangeRateProviderResolver,
        ledgerRolloverProgressService,
        ledgerRolloverErrorService,
        failedLedgerRolloverPoLineDao
      );
    }
  }

}
