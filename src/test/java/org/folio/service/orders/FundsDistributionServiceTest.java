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
  void testDistributeFundsShouldApplyEncumbranceEffectiveAmountLogic() {
    // TestMate-d1a7149293eeaa19199bd85d0bf85d7d
    // Given
    double listUnitPrice = 100.0;
    double amountExpended = 20.0;
    double amountCredited = 5.0;
    double amountAwaitingPayment = 10.0;
    double expectedEffectiveAmount = 75.0; // 100 - 20 + 5 - 10
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    FundDistribution fundDistribution = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    PoLine poLine = new PoLine()
      .withFundDistribution(List.of(fundDistribution))
      .withCost(new Cost()
        .withListUnitPrice(listUnitPrice)
        .withQuantityPhysical(1)
        .withCurrency("USD"));
    Encumbrance encumbrance = new Encumbrance()
      .withAmountExpended(amountExpended)
      .withAmountCredited(amountCredited)
      .withAmountAwaitingPayment(amountAwaitingPayment);
    Transaction transaction = new Transaction()
      .withEncumbrance(encumbrance);
    EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction)
      .withFundDistribution(fundDistribution)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(List.of(holder));
    // Then
    Transaction resultTransaction = resultHolders.get(0).getNewEncumbrance();
    assertEquals(expectedEffectiveAmount, resultTransaction.getAmount());
    assertEquals(listUnitPrice, resultTransaction.getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void testDistributeFundsWhenMultiplePoLinesShouldProcessIndependently() {
    // TestMate-ad304b3f8149bd28be8beb6e48640780
    // Given
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    FundDistribution fundDistributionA = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    PoLine poLineA = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withFundDistribution(List.of(fundDistributionA))
      .withCost(new Cost()
        .withListUnitPrice(10.0)
        .withQuantityPhysical(1)
        .withCurrency("USD"));
    Transaction transactionA = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holderA = new EncumbranceRelationsHolder()
      .withPoLine(poLineA)
      .withNewEncumbrance(transactionA)
      .withFundDistribution(fundDistributionA)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    FundDistribution fundDistributionB = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    PoLine poLineB = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withFundDistribution(List.of(fundDistributionB))
      .withCost(new Cost()
        .withListUnitPrice(20.0)
        .withQuantityPhysical(1)
        .withCurrency("USD"));
    Transaction transactionB = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holderB = new EncumbranceRelationsHolder()
      .withPoLine(poLineB)
      .withNewEncumbrance(transactionB)
      .withFundDistribution(fundDistributionB)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holderA, holderB);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(2, resultHolders.size());
    EncumbranceRelationsHolder resultA = resultHolders.stream()
      .filter(h -> h.getPoLine().getId().equals(poLineA.getId()))
      .findFirst()
      .get();
    assertEquals(10.0, resultA.getNewEncumbrance().getAmount());
    assertEquals(10.0, resultA.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    EncumbranceRelationsHolder resultB = resultHolders.stream()
      .filter(h -> h.getPoLine().getId().equals(poLineB.getId()))
      .findFirst()
      .get();
    assertEquals(20.0, resultB.getNewEncumbrance().getAmount());
    assertEquals(20.0, resultB.getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void testDistributeFundsWhenHolderHasNoPoLineShouldFilterOut() {
    // TestMate-6daa86207f3f74b803d553c8c4ed057a
    // Given
    double listUnitPrice = 100.0;
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100.0);
    PoLine poLine1 = new PoLine()
      .withFundDistribution(List.of(fundDistribution1))
      .withCost(new Cost()
        .withListUnitPrice(listUnitPrice)
        .withQuantityPhysical(1)
        .withCurrency("USD"));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine1)
      .withNewEncumbrance(transaction1)
      .withFundDistribution(fundDistribution1)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    
    // Initialize holder2 with a null PoLine. 
    // Explicitly set initial state to 0.0 to match assertions, as generated POJOs default to null.
    Transaction transaction2 = new Transaction()
      .withAmount(0.0)
      .withEncumbrance(new Encumbrance().withInitialAmountEncumbered(0.0));
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(null)
      .withNewEncumbrance(transaction2)
      .withCurrency("USD");
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(2, resultHolders.size());
    // Verify valid holder was processed
    assertEquals(100.0, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(100.0, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    
    // Verify invalid holder was ignored. 
    // Since it was filtered out, the values remain at their initial state (0.0) rather than being updated by the logic.
    assertEquals(0.0, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(0.0, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void testDistributeFundsWhenRemainderIsZeroShouldNotAdjustPercentageDistributions() {
    // TestMate-cb6ba41bbeb7a60af033359e4c3cfa98
    // Given
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(50.0);
    PoLine poLine = new PoLine()
      .withFundDistribution(List.of(fundDistribution1, fundDistribution2))
      .withCost(new Cost()
        .withListUnitPrice(10.0)
        .withQuantityPhysical(1)
        .withCurrency("USD"));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction1)
      .withFundDistribution(fundDistribution1)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction2)
      .withFundDistribution(fundDistribution2)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(2, resultHolders.size());
    assertEquals(5.0, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(5.0, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(5.0, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(5.0, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

    @Test
  void testDistributeFundsWhenAllDistributionsAreAmountTypeShouldNotAdjustRemainder() {
    // TestMate-1b0cd6f29fbe89ee0fa0e03244e2ece2
    // Given
    double listUnitPrice = 10.00;
    double distributionValue = 4.99;
    CurrencyConversion conversion = mock(ManualCurrencyConversion.class);
    when(conversion.apply(any(MonetaryAmount.class))).thenAnswer(invocation -> invocation.getArgument(0));
    FundDistribution fundDistribution1 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(distributionValue);
    FundDistribution fundDistribution2 = new FundDistribution()
      .withDistributionType(FundDistribution.DistributionType.AMOUNT)
      .withValue(distributionValue);
    PoLine poLine = new PoLine()
      .withFundDistribution(List.of(fundDistribution1, fundDistribution2))
      .withCost(new Cost()
        .withListUnitPrice(listUnitPrice)
        .withQuantityPhysical(1)
        .withCurrency("USD"));
    Transaction transaction1 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder1 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction1)
      .withFundDistribution(fundDistribution1)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    Transaction transaction2 = new Transaction().withEncumbrance(new Encumbrance());
    EncumbranceRelationsHolder holder2 = new EncumbranceRelationsHolder()
      .withPoLine(poLine)
      .withNewEncumbrance(transaction2)
      .withFundDistribution(fundDistribution2)
      .withCurrency("USD")
      .withPoLineToFyConversion(conversion);
    List<EncumbranceRelationsHolder> holders = List.of(holder1, holder2);
    // When
    List<EncumbranceRelationsHolder> resultHolders = distributionService.distributeFunds(holders);
    // Then
    assertEquals(2, resultHolders.size());
    assertEquals(distributionValue, resultHolders.get(0).getNewEncumbrance().getAmount());
    assertEquals(distributionValue, resultHolders.get(0).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
    assertEquals(distributionValue, resultHolders.get(1).getNewEncumbrance().getAmount());
    assertEquals(distributionValue, resultHolders.get(1).getNewEncumbrance().getEncumbrance().getInitialAmountEncumbered());
  }

}
