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
import java.util.UUID;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static javax.money.Monetary.getDefaultRounding;
import javax.money.CurrencyUnit;

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
  void shouldAdjustLastPercentageDistributionWhenRemainderIsPositive() {
    // TestMate-c5da30319178df515d8a1cfae2a149ac
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(33.33d);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(33.33d);
    FundDistribution fundDistribution3 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(33.33d);
    Cost cost = new Cost()
      .withListUnitPrice(1.00d)
      .withQuantityPhysical(1)
      .withCurrency("USD");
    PoLine poLine = new PoLine()
      .withCost(cost)
      .withFundDistribution(List.of(fundDistribution1, fundDistribution2, fundDistribution3));
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction3 = new Transaction().withEncumbrance(new Encumbrance());
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
    // When
    distributionService.distributeFunds(holders);
    // Then
    assertEquals(0.33, holder1.getNewEncumbrance().getAmount());
    assertEquals(0.33, holder1.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.33, holder2.getNewEncumbrance().getAmount());
    assertEquals(0.33, holder2.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.34, holder3.getNewEncumbrance().getAmount());
    assertEquals(0.34, holder3.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void shouldDistributeMultiplePennyRemainderAcrossMultiplePercentageDistributions() {
    // TestMate-51b0644f74e63c9095b7aa52aa46711f
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    String currency = "USD";
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(24.5d);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(24.5d);
    FundDistribution fundDistribution3 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(24.5d);
    FundDistribution fundDistribution4 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(24.5d);
    Cost cost = new Cost()
      .withListUnitPrice(1.00d)
      .withQuantityPhysical(1)
      .withCurrency(currency);
    PoLine poLine = new PoLine()
      .withCost(cost)
      .withFundDistribution(List.of(fundDistribution1, fundDistribution2, fundDistribution3, fundDistribution4));
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction3 = new Transaction().withEncumbrance(new Encumbrance());
    Transaction transaction4 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction1)
      .withFundDistribution(fundDistribution1)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction2)
      .withFundDistribution(fundDistribution2)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder3 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction3)
      .withFundDistribution(fundDistribution3)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    EncumbranceRelationsHolder holder4 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction4)
      .withFundDistribution(fundDistribution4)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3, holder4);
    // When
    distributionService.distributeFunds(holders);
    // Then
    assertEquals(0.25, holder1.getNewEncumbrance().getAmount());
    assertEquals(0.25, holder1.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.25, holder2.getNewEncumbrance().getAmount());
    assertEquals(0.25, holder2.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.25, holder3.getNewEncumbrance().getAmount());
    assertEquals(0.25, holder3.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.25, holder4.getNewEncumbrance().getAmount());
    assertEquals(0.25, holder4.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void shouldCalculateEffectiveAmountConsideringFinancialActivity() {
    // TestMate-184ba4e9c2a73eb90bfdba7250b50075
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    String currency = "USD";
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(100.0);
    Cost cost = new Cost()
      .withCurrency(currency)
      .withListUnitPrice(100.0)
      .withQuantityPhysical(1);
    PoLine poLine = new PoLine()
      .withCost(cost)
      .withFundDistribution(List.of(fundDistribution));
    Encumbrance encumbrance = new Encumbrance()
      .withAmountExpended(20.0)
      .withAmountCredited(5.0)
      .withAmountAwaitingPayment(10.0);
    Transaction transaction = new Transaction()
      .withEncumbrance(encumbrance);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution)
      .withNewEncumbrance(transaction)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder);
    // When
    distributionService.distributeFunds(holders);
    // Then
    assertEquals(100.0, holder.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(75.0, holder.getNewEncumbrance().getAmount());
  }

    @Test
  void shouldIsolateDistributionsForMultiplePoLines() {
    // TestMate-26c4b4fdd303de06c865088f80e4b1ae
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    // PoLine A Setup
    FundDistribution fundDistributionA = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    PoLine poLineA = new PoLine()
      .withId(UUID.fromString("00000000-0000-0000-0000-00000000000a").toString())
      .withCost(new Cost()
        .withListUnitPrice(10.0)
        .withQuantityPhysical(1)
        .withCurrency("USD"))
      .withFundDistribution(List.of(fundDistributionA));
    Transaction transactionA = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
      .withPoLine(poLineA)
      .withNewEncumbrance(transactionA)
      .withFundDistribution(fundDistributionA)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    // PoLine B Setup
    FundDistribution fundDistributionB = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    PoLine poLineB = new PoLine()
      .withId(UUID.fromString("00000000-0000-0000-0000-00000000000b").toString())
      .withCost(new Cost()
        .withListUnitPrice(20.0)
        .withQuantityPhysical(1)
        .withCurrency("USD"))
      .withFundDistribution(List.of(fundDistributionB));
    Transaction transactionB = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
      .withPoLine(poLineB)
      .withNewEncumbrance(transactionB)
      .withFundDistribution(fundDistributionB)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB);
    // When
    distributionService.distributeFunds(holders);
    // Then
    assertEquals(10.0, holderA.getNewEncumbrance().getAmount());
    assertEquals(10.0, holderA.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(20.0, holderB.getNewEncumbrance().getAmount());
    assertEquals(20.0, holderB.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void shouldSkipHoldersWithNullPoLine() {
    // TestMate-6f27768b42d22f7a7f997da60ac931d5
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    String currency = "USD";
    Cost cost = new Cost()
      .withListUnitPrice(100.0)
      .withQuantityPhysical(1)
      .withCurrency(currency);
    PoLine poLine = new PoLine()
      .withCost(cost);
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withFundDistribution(fundDistribution)
      .withNewEncumbrance(transaction1)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(null)
      .withNewEncumbrance(transaction2);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(2, resultHolders.size());
    assertEquals(100.0, holder1.getNewEncumbrance().getAmount());
    assertEquals(100.0, holder1.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertNull(holder2.getNewEncumbrance().getAmount());
    assertEquals(0.0, holder2.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void shouldNotAdjustRemainderWhenNoPercentageDistributionsExist() {
    // TestMate-a8ded29e8d19a6e3d24cac1ba2d43ba1
    // Given
    FundsDistributionService distributionService = new FundsDistributionService();
    String currency = "USD";
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    Cost cost = new Cost()
      .withListUnitPrice(1.00d)
      .withQuantityPhysical(1)
      .withCurrency(currency);
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(0.49d);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(0.50d);
    PoLine poLine = new PoLine()
      .withCost(cost)
      .withFundDistribution(List.of(fundDistribution1, fundDistribution2));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction1)
      .withFundDistribution(fundDistribution1)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction2)
      .withFundDistribution(fundDistribution2)
      .withCurrency(currency)
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    distributionService.distributeFunds(holders);
    // Then
    assertEquals(0.49, holder1.getNewEncumbrance().getAmount());
    assertEquals(0.49, holder1.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(0.50, holder2.getNewEncumbrance().getAmount());
    assertEquals(0.50, holder2.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
void shouldHandleZeroEstimatedPrice() {
  // TestMate-72756123ca5649fc65dab1073e97870a
  // Given
  FundsDistributionService distributionService = new FundsDistributionService();
  String currency = "USD";
  Cost cost = new Cost()
    .withListUnitPrice(0.0)
    .withQuantityPhysical(1)
    .withCurrency(currency);
  FundDistribution fundDistribution1 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(50.0);
  FundDistribution fundDistribution2 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(50.0);
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
  assertThat(resultHolders, hasSize(2));
  assertThat(resultHolders.get(0).getNewEncumbrance().getAmount(), is(0.0));
  assertThat(resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered(), is(0.0));
  assertThat(resultHolders.get(1).getNewEncumbrance().getAmount(), is(0.0));
  assertThat(resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered(), is(0.0));
}

    @Test
void shouldAdjustByOneUnitForCurrenciesWithNoDecimals() {
  // TestMate-eb666695baf9474f295382c3bde9d573
  // Given
  String currencyCode = "JPY";
  CurrencyUnit jpyCurrency = mock(CurrencyUnit.class);
  when(jpyCurrency.getCurrencyCode()).thenReturn(currencyCode);
  when(jpyCurrency.getDefaultFractionDigits()).thenReturn(0);
  CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
  when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
  Cost cost = new Cost()
    .withListUnitPrice(100.0)
    .withQuantityPhysical(1)
    .withCurrency(currencyCode);
  PoLine poLine = new PoLine()
    .withId(UUID.fromString("00000000-0000-0000-0000-000000000001").toString())
    .withCost(cost);
  FundDistribution fd1 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(33.3);
  FundDistribution fd2 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(33.3);
  FundDistribution fd3 = new FundDistribution()
    .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
    .withValue(33.3);
  EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
    .withPoLine(poLine)
    .withFundDistribution(fd1)
    .withCurrency(currencyCode)
    .withPoLineToFyConversion(conversion)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()));
  EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
    .withPoLine(poLine)
    .withFundDistribution(fd2)
    .withCurrency(currencyCode)
    .withPoLineToFyConversion(conversion)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()));
  EncumbranceRelationsHolder holder3 = new EncumbranceRelationsHolder()
    .withPoLine(poLine)
    .withFundDistribution(fd3)
    .withCurrency(currencyCode)
    .withPoLineToFyConversion(conversion)
    .withNewEncumbrance(new Transaction().withEncumbrance(new Encumbrance()));
  List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2, holder3);
  // When
  distributionService.distributeFunds(holders);
  // Then
  // 100 JPY total. 33.3% of 100 = 33.3, rounded to 33 JPY.
  // Sum = 33 + 33 + 33 = 99. Remainder = 1.
  // Remainder is positive, so iterator starts from the end (holder3).
  assertEquals(33.0, holder1.getNewEncumbrance().getAmount());
  assertEquals(33.0, holder1.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  assertEquals(33.0, holder2.getNewEncumbrance().getAmount());
  assertEquals(33.0, holder2.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  assertEquals(34.0, holder3.getNewEncumbrance().getAmount());
  assertEquals(34.0, holder3.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
}

}
