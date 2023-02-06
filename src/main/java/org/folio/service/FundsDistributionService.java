package org.folio.service;

import static javax.money.Monetary.getDefaultRounding;
import static org.folio.orders.utils.HelperUtils.calculateEncumbranceEffectiveAmount;

import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.convert.CurrencyConversion;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryFunctions;
import org.javamoney.moneta.function.MonetaryOperators;

public class FundsDistributionService {

  public <T extends EncumbranceRelationsHolder> List<T> distributeFunds(List<T> holders) {
    Map<CompositePoLine, List<EncumbranceRelationsHolder>> lineHoldersMap = holders.stream()
        .filter(holder -> Objects.nonNull(holder.getPoLine()))
        .collect(Collectors.groupingBy(EncumbranceRelationsHolder::getPoLine));

    lineHoldersMap.forEach((poLine, encumbranceRelationsHolders) -> {
      poLine.getCost().setPoLineEstimatedPrice(HelperUtils.calculateEstimatedPrice(poLine.getCost()).getNumber().doubleValue());
      CurrencyUnit poLineCurrency = Monetary.getCurrency(poLine.getCost().getCurrency());
      CurrencyConversion conversion = encumbranceRelationsHolders.stream()
          .map(EncumbranceRelationsHolder::getPoLineToFyConversion).findFirst().get();

      MonetaryAmount expectedTotal = Money.of(poLine.getCost().getPoLineEstimatedPrice(), poLineCurrency)
          .with(conversion)
          .with(getDefaultRounding());
      MonetaryAmount calculatedTotal = encumbranceRelationsHolders.stream()
          .map(EncumbranceRelationsHolder::getFundDistribution)
          .map(fundDistribution -> getDistributionAmount(fundDistribution, expectedTotal, poLineCurrency, conversion))
          .reduce((money, money2) -> Money.from(MonetaryFunctions.sum(money, money2)))
          .orElseGet(() -> Money.zero(poLineCurrency));

      MonetaryAmount remainder = expectedTotal.abs()
          .subtract(calculatedTotal.abs());
      int remainderSignum = remainder.signum();
      MonetaryAmount smallestUnit = getSmallestUnit(expectedTotal, remainderSignum);

      for (ListIterator<EncumbranceRelationsHolder> iterator = getIterator(encumbranceRelationsHolders, remainderSignum); isIteratorHasNext(iterator, remainderSignum);) {

        final EncumbranceRelationsHolder holder = iteratorNext(iterator, remainderSignum);
          CurrencyUnit fyCurrency = Monetary.getCurrency(holder.getCurrency());
          MonetaryAmount initialAmount = getDistributionAmount(holder.getFundDistribution(), expectedTotal, poLineCurrency, conversion);

          if (FundDistribution.DistributionType.PERCENTAGE.equals(holder.getFundDistribution().getDistributionType()) && !remainder.isZero()) {
            initialAmount = initialAmount.add(smallestUnit);
            remainder = remainder.abs().subtract(smallestUnit.abs()).multiply(remainderSignum);
          }

          MonetaryAmount expended = Optional.of(holder).map(EncumbranceRelationsHolder::getNewEncumbrance).map(Transaction::getEncumbrance).map(Encumbrance::getAmountExpended).map(aDouble -> Money.of(aDouble, fyCurrency))
              .orElse(Money.zero(fyCurrency));
          MonetaryAmount awaitingPayment = Optional.of(holder).map(EncumbranceRelationsHolder::getNewEncumbrance).map(Transaction::getEncumbrance).map(Encumbrance::getAmountAwaitingPayment)
              .map(aDouble -> Money.of(aDouble, fyCurrency)).orElse(Money.zero(fyCurrency));
          MonetaryAmount amount = calculateEncumbranceEffectiveAmount(initialAmount, expended, awaitingPayment, fyCurrency);

          holder.getNewEncumbrance().setAmount(amount.getNumber().doubleValue());
          holder.getNewEncumbrance().getEncumbrance().setInitialAmountEncumbered(initialAmount.getNumber().doubleValue());

      }
    });
    return holders;
  }


  private MonetaryAmount getDistributionAmount(FundDistribution fundDistribution, MonetaryAmount total, CurrencyUnit poLineCurrency,
                                               CurrencyConversion conversion) {
    if (fundDistribution.getDistributionType() == FundDistribution.DistributionType.AMOUNT) {
      return Money.of(fundDistribution.getValue(), poLineCurrency).with(conversion).with(getDefaultRounding());
    }
    return total.with(MonetaryOperators.percent(fundDistribution.getValue())).with(getDefaultRounding());
  }

  private MonetaryAmount getSmallestUnit(MonetaryAmount expectedAdjustmentValue, int remainderSignum) {
    CurrencyUnit currencyUnit = expectedAdjustmentValue.getCurrency();
    int decimalPlaces = currencyUnit.getDefaultFractionDigits();
    int smallestUnitSignum = expectedAdjustmentValue.signum() * remainderSignum;
    return Money.of(1 / Math.pow(10, decimalPlaces), currencyUnit).multiply(smallestUnitSignum);
  }

  private ListIterator<EncumbranceRelationsHolder> getIterator(List<EncumbranceRelationsHolder> holders, int remainder) {
    return remainder > 0 ? holders.listIterator(holders.size()) : holders.listIterator();
  }

  private boolean isIteratorHasNext(ListIterator<EncumbranceRelationsHolder> iterator, int remainder) {
    return remainder > 0 ? iterator.hasPrevious() : iterator.hasNext();
  }

  private EncumbranceRelationsHolder iteratorNext(ListIterator<EncumbranceRelationsHolder> iterator, int remainder) {
    return remainder > 0 ? iterator.previous() : iterator.next();
  }
}
