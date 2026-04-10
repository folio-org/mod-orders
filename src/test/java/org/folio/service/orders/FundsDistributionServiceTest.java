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
  void testDistributeFundsWhenRemainderIsPositiveShouldAddPennyToPercentageDistributions() {
    // TestMate-c8f9558408c7013680b84042627143ca
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    String currency = "USD";
    Cost cost = new Cost()
      .withListUnitPrice(0.05)
      .withQuantityPhysical(1)
      .withCurrency(currency);
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(33.0);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(33.0);
    PoLine poLine = new PoLine()
      .withCost(cost)
      .withFundDistribution(List.of(fundDistribution1, fundDistribution2));
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution1)
      .withNewEncumbrance(transaction1)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution2)
      .withNewEncumbrance(transaction2)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(2, resultHolders.size());
    // 33% of 0.05 is 0.0165. Rounded to 0.02.
    // Total calculated: 0.02 + 0.02 = 0.04.
    // Remainder: 0.05 - 0.04 = +0.01.
    // For positive remainder, iterator moves backwards, so the last holder gets the penny.
    assertEquals(0.02, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(0.02, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.03, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(0.03, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
void testDistributeFundsWhenEncumbranceHasExistingTransactionsShouldCalculateEffectiveAmount() {
  // TestMate-b1c7467dbaaa239509cfcb71ea49ad82
  // Given
  String currency = "USD";
  double listUnitPrice = 100.0;
  double amountExpended = 20.0;
  double amountCredited = 5.0;
  double amountAwaitingPayment = 10.0;
  double expectedEffectiveAmount = 75.0;
  CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
  when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
  Encumbrance encumbrance = new Encumbrance()
    .withAmountExpended(amountExpended)
    .withAmountCredited(amountCredited)
    .withAmountAwaitingPayment(amountAwaitingPayment);
  Transaction newEncumbrance = new Transaction()
    .withEncumbrance(encumbrance)
    .withCurrency(currency);
  Cost cost = new Cost()
    .withListUnitPrice(listUnitPrice)
    .withQuantityPhysical(1)
    .withCurrency(currency);
  FundDistribution fundDistribution = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(100.0);
  PoLine poLine = new PoLine()
    .withCost(cost)
    .withFundDistribution(List.of(fundDistribution));
  EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
    .withPoLine(poLine)
    .withNewEncumbrance(newEncumbrance)
    .withFundDistribution(fundDistribution)
    .withCurrency(currency)
    .withPoLineToFyConversion(conversion);
  List<EncumbranceRelationsHolder> holders = List.of(holder);
  // When
  List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
  // Then
  assertEquals(1, resultHolders.size());
  Transaction resultTransaction = resultHolders.get(0).getNewEncumbrance();
  assertEquals(expectedEffectiveAmount, resultTransaction.getAmount());
  assertEquals(listUnitPrice, resultTransaction.getEncumbrance().getInitialAmountEncumbered());
}

    @Test
  void testDistributeFundsWhenMultiplePoLinesExistShouldGroupAndProcessSeparately() {
    // TestMate-27dddea5a550328c5b1f16bb2c321318
    // Given
    String currency = "USD";
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    // PoLine A Setup (Price 1.00, 50/50 Distribution)
    PoLine poLineA = new PoLine()
      .withId("poLineA")
      .withCost(new Cost()
        .withListUnitPrice(1.0)
        .withQuantityPhysical(1)
        .withCurrency(currency));
    FundDistribution fundDistributionA1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    FundDistribution fundDistributionA2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    Transaction transactionA1 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transactionA2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holderA1 = new EncumbranceRelationsHolder()
      .withPoLine(poLineA)
      .withFundDistribution(fundDistributionA1)
      .withNewEncumbrance(transactionA1)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holderA2 = new EncumbranceRelationsHolder()
      .withPoLine(poLineA)
      .withFundDistribution(fundDistributionA2)
      .withNewEncumbrance(transactionA2)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    // PoLine B Setup (Price 2.00, 50/50 Distribution)
    PoLine poLineB = new PoLine()
      .withId("poLineB")
      .withCost(new Cost()
        .withListUnitPrice(2.0)
        .withQuantityPhysical(1)
        .withCurrency(currency));
    FundDistribution fundDistributionB1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    FundDistribution fundDistributionB2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    Transaction transactionB1 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transactionB2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holderB1 = new EncumbranceRelationsHolder()
      .withPoLine(poLineB)
      .withFundDistribution(fundDistributionB1)
      .withNewEncumbrance(transactionB1)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holderB2 = new EncumbranceRelationsHolder()
      .withPoLine(poLineB)
      .withFundDistribution(fundDistributionB2)
      .withNewEncumbrance(transactionB2)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holderA1, holderA2, holderB1, holderB2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(4, resultHolders.size());
    // Verification of PoLine A Distributions
    assertEquals(0.50, holderA1.getNewEncumbrance().getAmount());
    assertEquals(0.50, holderA1.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.50, holderA2.getNewEncumbrance().getAmount());
    assertEquals(0.50, holderA2.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(1.00, poLineA.getCost().getPoLineEstimatedPrice());
    // Verification of PoLine B Distributions
    assertEquals(1.00, holderB1.getNewEncumbrance().getAmount());
    assertEquals(1.00, holderB1.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(1.00, holderB2.getNewEncumbrance().getAmount());
    assertEquals(1.00, holderB2.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(2.00, poLineB.getCost().getPoLineEstimatedPrice());
  }

    @Test
void testDistributeFundsWhenEffectiveAmountIsNegativeShouldFloorToZero() {
  // TestMate-9b71c08c426fa0b2ca246661ee372f40
  // Given
  FundsDistributionService distributionService = new FundsDistributionService();
  String currency = "USD";
  double listUnitPrice = 10.0;
  double amountExpended = 50.0;
  double expectedEffectiveAmount = 0.0;
  Cost cost = new Cost()
    .withListUnitPrice(listUnitPrice)
    .withQuantityPhysical(1)
    .withCurrency(currency);
  FundDistribution fundDistribution = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(100.0);
  PoLine poLine = new PoLine()
    .withCost(cost)
    .withFundDistribution(List.of(fundDistribution));
  Encumbrance encumbrance = new Encumbrance()
    .withAmountExpended(amountExpended)
    .withAmountCredited(0.0)
    .withAmountAwaitingPayment(0.0);
  Transaction newEncumbrance = new Transaction()
    .withEncumbrance(encumbrance)
    .withCurrency(currency);
  CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
  when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
  EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
    .withPoLine(poLine)
    .withNewEncumbrance(newEncumbrance)
    .withFundDistribution(fundDistribution)
    .withCurrency(currency)
    .withPoLineToFyConversion(conversion);
  List<EncumbranceRelationsHolder> holders = List.of(holder);
  // When
  List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
  // Then
  assertEquals(1, resultHolders.size());
  Transaction resultTransaction = resultHolders.get(0).getNewEncumbrance();
  assertEquals(expectedEffectiveAmount, resultTransaction.getAmount());
  assertEquals(listUnitPrice, resultTransaction.getEncumbrance().getInitialAmountEncumbered());
}

    @Test
  void testDistributeFundsWhenRemainderIsZeroShouldNotModifyInitialAmounts() {
    // TestMate-e8db04b3cb2bbc2585e956a5bda18304
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    String currency = "USD";
    double listUnitPrice = 100.0;
    double distributionValue = 50.0;
    double expectedAmount = 50.0;
    Cost cost = new Cost()
      .withListUnitPrice(listUnitPrice)
      .withQuantityPhysical(1)
      .withCurrency(currency);
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(distributionValue);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(distributionValue);
    PoLine poLine = new PoLine()
      .withCost(cost)
      .withFundDistribution(List.of(fundDistribution1, fundDistribution2));
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution1)
      .withNewEncumbrance(transaction1)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution2)
      .withNewEncumbrance(transaction2)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(2, resultHolders.size());
    assertEquals(listUnitPrice, poLine.getCost().getPoLineEstimatedPrice());
    assertEquals(expectedAmount, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(expectedAmount, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(expectedAmount, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(expectedAmount, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void testDistributeFundsWhenPoLineHasNegativePriceShouldDistributeCorrectly() {
    // TestMate-9af4d33d954e5f6a263f1d69623bb403
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    String currency = "USD";
    double negativePrice = -100.0;
    Cost cost = new Cost()
      .withListUnitPrice(negativePrice)
      .withQuantityPhysical(1)
      .withCurrency(currency);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution1)
      .withNewEncumbrance(transaction1)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution2)
      .withNewEncumbrance(transaction2)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(2, resultHolders.size());
    assertEquals(negativePrice, poLine.getCost().getPoLineEstimatedPrice());
    // The 'amount' (effective amount) is floored at 0.0 by FinanceUtils.calculateEncumbranceEffectiveAmount
    assertEquals(0.0, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(-50.0, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.0, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(-50.0, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void testDistributeFundsWhenOnlyAmountDistributionsExistShouldNotApplyRemainder() {
    // TestMate-c4a8a21021ba71a59a57dd0985dc281a
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    String currency = "USD";
    double listUnitPrice = 1.00;
    double distributionAmount = 0.99;
    Cost cost = new Cost()
      .withListUnitPrice(listUnitPrice)
      .withQuantityPhysical(1)
      .withCurrency(currency);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(distributionAmount);
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Transaction transaction = new Transaction()
      .withEncumbrance(new Encumbrance())
      .withCurrency(currency);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution)
      .withNewEncumbrance(transaction)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(1, resultHolders.size());
    Transaction resultTransaction = resultHolders.get(0).getNewEncumbrance();
    
    // Verify that the remainder (1.00 - 0.99 = 0.01) was NOT applied to the AMOUNT distribution
    assertEquals(distributionAmount, resultTransaction.getAmount());
    assertEquals(distributionAmount, resultTransaction.getEncumbrance().getInitialAmountEncumbered());
  }

}
