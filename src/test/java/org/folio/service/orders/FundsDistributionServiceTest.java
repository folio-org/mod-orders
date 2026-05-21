package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.List;

import javax.money.MonetaryAmount;
import javax.money.convert.CurrencyConversion;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.FundsDistributionService;
import org.folio.service.exchange.ManualCurrencyConversion;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import static org.mockito.Mockito.mock;
import javax.money.CurrencyUnit;
import javax.money.Monetary;
import org.javamoney.moneta.Money;
import static javax.money.Monetary.getDefaultRounding;

public class FundsDistributionServiceTest {

  private FundsDistributionService distributionService = new FundsDistributionService();

  @Test
  void shouldDistributeFunds() {
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction3 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction4 = new Transaction().withEncumbrance(new Encumbrance());
    FundDistribution fundDistribution1 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(30d);
    FundDistribution fundDistribution2 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(30d);
    FundDistribution fundDistribution3 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(30d);
    FundDistribution fundDistribution4 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(10d);
    PoLine poLine = new PoLine()
        .withFundDistribution(List.of(fundDistribution1, fundDistribution2, fundDistribution3, fundDistribution4))
        .withCost(new Cost()
                      .withListUnitPrice(1.94)
                      .withQuantityPhysical(1)
                      .withCurrency("USD"));
    CurrencyConversion conversion = Mockito.mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));


    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction1)
        .withFundDistribution(fundDistribution1)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction2)
        .withFundDistribution(fundDistribution2)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder3 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction3)
        .withFundDistribution(fundDistribution3)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder4 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction4)
        .withFundDistribution(fundDistribution4)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3, holder4);
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    assertEquals(0.58, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(0.58, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(0.58, resultHolders.get(2).getNewEncumbrance().getAmount());
    assertEquals(0.2, resultHolders.get(3).getNewEncumbrance().getAmount());
  }

  @Test
  void shouldSubtractExtraPennyFromEncumbrancesRelatedToAmountFundDistributions() {
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction3 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction4 = new Transaction().withEncumbrance(new Encumbrance());
    FundDistribution fundDistribution1 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.AMOUNT).withValue(0.40);
    FundDistribution fundDistribution2 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(30d);
    FundDistribution fundDistribution3 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(30d);
    FundDistribution fundDistribution4 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.AMOUNT).withValue(0.39);
    PoLine poLine = new PoLine()
        .withFundDistribution(List.of(fundDistribution1, fundDistribution2, fundDistribution3, fundDistribution4))
        .withCost(new Cost()
                      .withListUnitPrice(1.99)
                      .withQuantityPhysical(1)
                      .withCurrency("USD"));
    CurrencyConversion conversion = Mockito.mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));

    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction1)
        .withFundDistribution(fundDistribution1)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction2)
        .withFundDistribution(fundDistribution2)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder3 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction3)
        .withFundDistribution(fundDistribution3)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder4 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction4)
        .withFundDistribution(fundDistribution4)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3, holder4);
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    assertEquals(0.4, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(0.6, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(0.6, resultHolders.get(2).getNewEncumbrance().getAmount());
    assertEquals(0.39, resultHolders.get(3).getNewEncumbrance().getAmount());
  }

  @Test
  void shouldDistributeFundsAfterConversion() {
    double exchangeRate = 1.99;
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction3 = new Transaction().withEncumbrance(new Encumbrance());
    FundDistribution fundDistribution1 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(30d);
    FundDistribution fundDistribution2 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(30d);
    FundDistribution fundDistribution3 = new FundDistribution().withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(40d);
    PoLine poLine = new PoLine()
        .withFundDistribution(List.of(fundDistribution1, fundDistribution2, fundDistribution3))
        .withCost(new Cost()
                      .withListUnitPrice(1d)
                      .withQuantityPhysical(1)
                      .withCurrency("USD"));
    CurrencyConversion conversion = Mockito.mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> {
      MonetaryAmount amount = invocation.getArgument(0);
      return amount.multiply(exchangeRate);
    });

    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction1)
        .withFundDistribution(fundDistribution1)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction2)
        .withFundDistribution(fundDistribution2)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder3 = new EncumbranceRelationsHolder()
        .withPoLine(poLine)
        .withNewEncumbrance(transaction3)
        .withFundDistribution(fundDistribution3)
        .withCurrency("USD")
        .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3);
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    assertEquals(0.59, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(0.6, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(0.8, resultHolders.get(2).getNewEncumbrance().getAmount());
  }

    @Test
  void testDistributeFundsShouldUseFixedAmountsWhenDistributionTypeIsAmount() {
    // TestMate-b939619a2e1dc436abd7d563ec379ad4
    // Given
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Cost cost = new Cost()
      .withCurrency("USD")
      .withListUnitPrice(100.0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(25.0);
    Encumbrance encumbrance = new Encumbrance()
      .withAmountExpended(0.0)
      .withAmountCredited(0.0)
      .withAmountAwaitingPayment(0.0);
    Transaction transaction = new Transaction()
      .withEncumbrance(encumbrance);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution)
      .withNewEncumbrance(transaction)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    Transaction resultTransaction = resultHolders.get(0).getNewEncumbrance();
    assertEquals(25.0, resultTransaction.getEncumbrance().getInitialAmountEncumbered());
    assertEquals(25.0, resultTransaction.getAmount());
    assertEquals(100.0, resultHolders.get(0).getPoLine().getCost().getPoLineEstimatedPrice());
  }

    @Test
  void testDistributeFundsShouldAdjustForRoundingRemainderByAddingToPercentageDistributions() {
    // TestMate-f9f7fc72cd8231b613bd9dd88ce94e1f
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Cost cost = new Cost()
      .withCurrency("USD")
      .withListUnitPrice(1.00)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fd1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(33.33);
    FundDistribution fd2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(33.33);
    FundDistribution fd3 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(33.33);
    poLine.setFundDistribution(List.of(fd1, fd2, fd3));
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fd1)
      .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fd2)
      .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder3 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fd3)
      .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    // Expected total is 1.00. 33.33% of 1.00 is 0.33.
    // 0.33 + 0.33 + 0.33 = 0.99. Remainder is +0.01.
    // Remainder is positive, so the iterator starts from the end (holder3).
    assertEquals(0.33, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(0.33, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(0.34, resultHolders.get(2).getNewEncumbrance().getAmount());
    assertEquals(0.33, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.33, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.34, resultHolders.get(2).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    double totalEncumbered = resultHolders.stream()
      .mapToDouble(h -> h.getNewEncumbrance().getAmount())
      .sum();
    assertEquals(1.00, totalEncumbered, 0.0001);
    assertEquals(1.00, resultHolders.get(0).getPoLine().getCost().getPoLineEstimatedPrice());
  }

    @Test
void testDistributeFundsShouldSubtractRoundingRemainderWhenCalculatedTotalExceedsExpected() {
  // TestMate-4598a1a226ee1a2e5ec8c29091c6b5f1
  // Given
  CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
  when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
  Cost cost = new Cost()
    .withCurrency("USD")
    .withListUnitPrice(1.00)
    .withQuantityPhysical(1);
  PoLine poLine = new PoLine()
    .withCost(cost);
  // 50.005% of 1.00 is 0.50005. Standard rounding (HALF_EVEN/HALF_UP) for USD (2 digits) results in 0.51.
  // 0.51 + 0.51 = 1.02. Expected total is 1.00. Remainder is -0.02.
  // When remainder is negative, the iterator starts from the beginning (index 0).
  FundDistribution fd1 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(50.005);
  FundDistribution fd2 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(50.005);
  poLine.setFundDistribution(List.of(fd1, fd2));
  EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
    .withPoLine(poLine)
    .withFundDistribution(fd1)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
    .withCurrency("USD")
    .withPoLineToFyConversion(conversion);
  EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
    .withPoLine(poLine)
    .withFundDistribution(fd2)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
    .withCurrency("USD")
    .withPoLineToFyConversion(conversion);
  List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
  // When
  List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
  // Then
  assertEquals(1.00, resultHolders.get(0).getPoLine().getCost().getPoLineEstimatedPrice());
  // Initial rounded sum: 0.51 + 0.51 = 1.02. Remainder -0.02.
  // Smallest unit is -0.01.
  // Iterator starts at index 0.
  // Holder 1: 0.51 + (-0.01) = 0.50. Remainder becomes -0.01.
  // Holder 2: 0.51 + (-0.01) = 0.50. Remainder becomes 0.
  assertEquals(0.50, resultHolders.get(0).getNewEncumbrance().getAmount());
  assertEquals(0.50, resultHolders.get(1).getNewEncumbrance().getAmount());
  assertEquals(0.50, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  assertEquals(0.50, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  double totalEncumbered = resultHolders.stream()
    .mapToDouble(h -> h.getNewEncumbrance().getAmount())
    .sum();
  assertEquals(1.00, totalEncumbered, 0.0001);
}

    @Test
  void testDistributeFundsShouldCalculateEffectiveAmountBySubtractingExpendedAndAwaitingPayment() {
    // TestMate-3537db648ea6bec780238fc600053bf7
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    CurrencyConversion conversion = mock(CurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Cost cost = new Cost()
      .withCurrency("USD")
      .withListUnitPrice(100.0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    Encumbrance encumbrance = new Encumbrance()
      .withAmountExpended(20.0)
      .withAmountAwaitingPayment(10.0)
      .withAmountCredited(5.0);
    Transaction transaction = new Transaction()
      .withEncumbrance(encumbrance);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution)
      .withNewEncumbrance(transaction)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    Transaction resultTransaction = resultHolders.get(0).getNewEncumbrance();
    Encumbrance resultEncumbrance = resultTransaction.getEncumbrance();
    // Initial (100.0) - Expended (20.0) + Credited (5.0) - AwaitingPayment (10.0) = 75.0
    assertEquals(100.0, resultEncumbrance.getInitialAmountEncumbered());
    assertEquals(75.0, resultTransaction.getAmount());
    assertEquals(100.0, resultHolders.get(0).getPoLine().getCost().getPoLineEstimatedPrice());
  }

    @Test
  void testDistributeFundsShouldHandleCurrencyConversionWhenPoLineAndFiscalYearCurrenciesDiffer() {
    // TestMate-94a2b2bc47ecd47e24d5b116a3af6453
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    CurrencyUnit usdCurrency = Monetary.getCurrency("USD");
    double exchangeRate = 1.1;
    CurrencyConversion conversion = mock(CurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> {
      MonetaryAmount amount = invocation.getArgument(0);
      return Money.of(amount.getNumber().doubleValue() * exchangeRate, usdCurrency);
    });
    Cost cost = new Cost()
      .withCurrency("EUR")
      .withListUnitPrice(100.0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    Transaction transaction = new Transaction()
      .withEncumbrance(new Encumbrance()
        .withAmountExpended(0.0)
        .withAmountCredited(0.0)
        .withAmountAwaitingPayment(0.0));
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution)
      .withNewEncumbrance(transaction)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    EncumbranceRelationsHolder resultHolder = resultHolders.get(0);
    Transaction resultTransaction = resultHolder.getNewEncumbrance();
    // 100.0 EUR * 1.1 = 110.0 USD
    assertEquals(110.0, resultTransaction.getEncumbrance().getInitialAmountEncumbered());
    assertEquals(110.0, resultTransaction.getAmount());
    assertEquals(100.0, resultHolder.getPoLine().getCost().getPoLineEstimatedPrice());
  }

    @Test
  void testDistributeFundsShouldFilterOutHoldersWithNullPoLine() {
    // TestMate-c40ba8427eec13af84c6e6af3d6f92e2
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    CurrencyConversion conversion = mock(CurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    // Valid Holder Setup
    Cost cost = new Cost()
      .withCurrency("USD")
      .withListUnitPrice(100.0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fd = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    Transaction transactionA = new Transaction()
      .withEncumbrance(new Encumbrance()
        .withAmountExpended(0.0)
        .withAmountCredited(0.0)
        .withAmountAwaitingPayment(0.0));
    
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fd)
      .withNewEncumbrance(transactionA)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    // Invalid Holder Setup (Null PoLine)
    Transaction transactionB = new Transaction()
      .withAmount(0.0)
      .withEncumbrance(new Encumbrance()
        .withInitialAmountEncumbered(0.0));
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
      .withNewEncumbrance(transactionB);
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    // Holder A should be processed
    assertEquals(100.0, holderA.getNewEncumbrance().getAmount());
    assertEquals(100.0, holderA.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    
    // Holder B should be ignored (values remain unchanged)
    assertEquals(0.0, holderB.getNewEncumbrance().getAmount());
    assertEquals(0.0, holderB.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    
    // The list size remains the same as filtering happens during processing, not on the returned collection
    assertEquals(2, resultHolders.size());
  }

    @Test
  void testDistributeFundsShouldHandleNegativeEstimatedPrice() {
    // TestMate-9250da82dffede2293b4180696e0edde
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    CurrencyConversion conversion = mock(CurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Cost cost = new Cost()
      .withCurrency("USD")
      .withListUnitPrice(-100.0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fd1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    FundDistribution fd2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    poLine.setFundDistribution(List.of(fd1, fd2));
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fd1)
      .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fd2)
      .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    // The PoLineEstimatedPrice is calculated as -100.0
    assertEquals(-100.0, resultHolders.get(0).getPoLine().getCost().getPoLineEstimatedPrice());
    // Based on FinanceUtils.calculateEncumbranceEffectiveAmount, the effective amount (Transaction.amount)
    // is floored at 0.0 if the calculation (initial - expended + credited - awaiting) is negative.
    // Initial amount is -50.0, so the effective amount must be 0.0.
    assertEquals(0.0, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(-50.0, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.0, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(-50.0, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    double totalAmount = resultHolders.stream()
      .mapToDouble(h -> h.getNewEncumbrance().getAmount())
      .sum();
    assertEquals(0.0, totalAmount, 0.0001);
    double totalInitialEncumbered = resultHolders.stream()
      .mapToDouble(h -> h.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered())
      .sum();
    assertEquals(-100.0, totalInitialEncumbered, 0.0001);
  }

    @Test
  void testDistributeFundsShouldSkipAdjustmentWhenAllDistributionsAreAmountType() {
    // TestMate-f67959340606de93512de75cf3840b75
    // Given
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Cost cost = new Cost()
      .withCurrency("USD")
      .withListUnitPrice(100.0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(99.99);
    Encumbrance encumbrance = new Encumbrance()
      .withAmountExpended(0.0)
      .withAmountCredited(0.0)
      .withAmountAwaitingPayment(0.0);
    Transaction transaction = new Transaction()
      .withEncumbrance(encumbrance);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution)
      .withNewEncumbrance(transaction)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    EncumbranceRelationsHolder resultHolder = resultHolders.get(0);
    Transaction resultTransaction = resultHolder.getNewEncumbrance();
    assertEquals(1, resultHolders.size());
    assertEquals(99.99, resultTransaction.getEncumbrance().getInitialAmountEncumbered(), 0.0001);
    assertEquals(99.99, resultTransaction.getAmount(), 0.0001);
    assertEquals(100.0, resultHolder.getPoLine().getCost().getPoLineEstimatedPrice(), 0.0001);
  }

    @Test
  void testDistributeFundsShouldHandleZeroEstimatedPrice() {
    // TestMate-025592f8516c1eaf37ce59a4e7495a55
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    CurrencyConversion conversion = mock(CurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Cost cost = new Cost()
      .withCurrency("USD")
      .withListUnitPrice(0.0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    Transaction transaction = new Transaction()
      .withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution)
      .withNewEncumbrance(transaction)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(1, resultHolders.size());
    EncumbranceRelationsHolder resultHolder = resultHolders.get(0);
    Transaction resultTransaction = resultHolder.getNewEncumbrance();
    Encumbrance resultEncumbrance = resultTransaction.getEncumbrance();
    assertEquals(0.0, resultEncumbrance.getInitialAmountEncumbered(), 0.0001);
    assertEquals(0.0, resultTransaction.getAmount(), 0.0001);
    assertEquals(0.0, resultHolder.getPoLine().getCost().getPoLineEstimatedPrice(), 0.0001);
  }

    @Test
void testDistributeFundsShouldGroupHoldersByPoLineAndProcessEachGroup() {
  // TestMate-ef59d7a6e8d347002cc6e3683f96f0c1
  // Given
  FundsDistributionService distributionService = new FundsDistributionService();
  CurrencyConversion conversion = mock(CurrencyConversion.class);
  when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
  PoLine poLineA = new PoLine()
    .withCost(new Cost()
      .withCurrency("USD")
      .withListUnitPrice(100.0)
      .withQuantityPhysical(1));
  FundDistribution fd1 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(50.0);
  FundDistribution fd2 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(50.0);
  poLineA.setFundDistribution(List.of(fd1, fd2));
  PoLine poLineB = new PoLine()
    .withCost(new Cost()
      .withCurrency("USD")
      .withListUnitPrice(200.0)
      .withQuantityPhysical(1));
  FundDistribution fd3 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(50.0);
  FundDistribution fd4 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(50.0);
  poLineB.setFundDistribution(List.of(fd3, fd4));
  EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
    .withPoLine(poLineA)
    .withFundDistribution(fd1)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
    .withCurrency("USD")
    .withPoLineToFyConversion(conversion);
  EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
    .withPoLine(poLineA)
    .withFundDistribution(fd2)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
    .withCurrency("USD")
    .withPoLineToFyConversion(conversion);
  EncumbranceRelationsHolder holder3 = new EncumbranceRelationsHolder()
    .withPoLine(poLineB)
    .withFundDistribution(fd3)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
    .withCurrency("USD")
    .withPoLineToFyConversion(conversion);
  EncumbranceRelationsHolder holder4 = new EncumbranceRelationsHolder()
    .withPoLine(poLineB)
    .withFundDistribution(fd4)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()))
    .withCurrency("USD")
    .withPoLineToFyConversion(conversion);
  List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3, holder4);
  // When
  List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
  // Then
  assertEquals(100.0, poLineA.getCost().getPoLineEstimatedPrice(), 0.0001);
  assertEquals(50.0, holder1.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered(), 0.0001);
  assertEquals(50.0, holder1.getNewEncumbrance().getAmount(), 0.0001);
  assertEquals(50.0, holder2.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered(), 0.0001);
  assertEquals(50.0, holder2.getNewEncumbrance().getAmount(), 0.0001);
  assertEquals(200.0, poLineB.getCost().getPoLineEstimatedPrice(), 0.0001);
  assertEquals(100.0, holder3.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered(), 0.0001);
  assertEquals(100.0, holder3.getNewEncumbrance().getAmount(), 0.0001);
  assertEquals(100.0, holder4.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered(), 0.0001);
  assertEquals(100.0, holder4.getNewEncumbrance().getAmount(), 0.0001);
}

}
