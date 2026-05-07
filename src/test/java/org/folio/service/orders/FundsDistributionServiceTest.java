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

}
