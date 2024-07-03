package org.folio.service.finance.budget;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.rest.core.exceptions.ErrorCodes.FUND_CANNOT_BE_PAID;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import javax.money.MonetaryAmount;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryFunctions;

public class BudgetRestrictionService {

  public void checkEncumbranceRestrictions(List<? extends EncumbranceRelationsHolder> dataHolders) {

    Map<Budget, List<EncumbranceRelationsHolder>> budgetHoldersMap = dataHolders.stream()
        .filter(EncumbranceRelationsHolder::getRestrictEncumbrance)
        .collect(groupingBy(EncumbranceRelationsHolder::getBudget));

    Map<String, String> fundHoldersMap = dataHolders.stream()
        .filter(EncumbranceRelationsHolder::getRestrictEncumbrance)
        .map(EncumbranceRelationsHolder::getFundDistribution)
        .filter(fd -> fd != null && fd.getFundId() != null && fd.getCode() != null)
        .distinct()
        .collect(Collectors.toMap(FundDistribution::getFundId, FundDistribution::getCode,
          (fundEntityKey, fundEntityDupKey) -> fundEntityKey));

    List<String> failedFundIds = budgetHoldersMap.entrySet()
        .stream()
        .filter(entry -> Objects.nonNull(entry.getKey().getAllowableEncumbrance()))
        .filter(entry -> {
          MonetaryAmount newEncumberedAmount = calculateNewEncumberedAmount(entry.getValue());
          return isRemainingAmountExceed(entry.getKey(), newEncumberedAmount);
        })
        .map(Map.Entry::getKey)
        .map(Budget::getFundId)
        .collect(toList());

    if (!failedFundIds.isEmpty()) {
      Parameter parameter = new Parameter().withKey(FUNDS)
          .withValue(failedFundIds.stream().map(fundHoldersMap::get).collect(toList()).toString());
      throw new HttpException(422, FUND_CANNOT_BE_PAID.toError()
          .withParameters(Collections.singletonList(parameter)));
    }
  }

  private boolean isRemainingAmountExceed(Budget budget, MonetaryAmount newEncumberedAmount) {
    // im check
    // [remaining amount we can encumber] = (totalFunding * allowableEncumbrance) - unavailable
    // where unavailable = awaitingPayment + encumbered + expenditure
    CurrencyUnit currency = newEncumberedAmount.getCurrency();
    Money totalFundings = Money.of(budget.getTotalFunding(), currency);

    BigDecimal allowableEncumbrance = BigDecimal.valueOf(budget.getAllowableEncumbrance())
        .movePointLeft(2);
    Money unavailable = Money.of(budget.getUnavailable(), currency);
    Money totalAmountCanBeExpended = totalFundings.multiply(allowableEncumbrance);
    Money amountCanBeEncumbered = totalAmountCanBeExpended.subtract(unavailable);


    return newEncumberedAmount.isGreaterThan(amountCanBeEncumbered);
  }

  private MonetaryAmount calculateNewEncumberedAmount(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    CurrencyUnit currency = Monetary.getCurrency(encumbranceRelationsHolders.get(0).getCurrency());
    return encumbranceRelationsHolders.stream()
        .map(holder -> {
          MonetaryAmount newTransactionAmount = Money.of(holder.getNewEncumbrance().getAmount(), holder.getCurrency());

          MonetaryAmount existingTransactionAmount = Optional.ofNullable(holder.getOldEncumbrance())
              .map(transaction -> Money.of(transaction.getAmount(), transaction.getCurrency()))
              .orElseGet(() -> Money.zero(Monetary.getCurrency(holder.getCurrency())));

          return newTransactionAmount.subtract(existingTransactionAmount);
        })
        .reduce(MonetaryFunctions::sum)
        .orElseGet(() -> Money.zero(currency));
  }

}
