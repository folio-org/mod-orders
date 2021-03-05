package org.folio.service.finance.budget;

import static java.util.stream.Collectors.flatMapping;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.folio.orders.utils.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.orders.utils.ResourcePathResolver.BUDGETS;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.money.CurrencyUnit;
import javax.money.MonetaryAmount;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.LedgerService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryFunctions;

public class BudgetRestrictionService {

  private final BudgetService budgetService;
  private final LedgerService ledgerService;

  public BudgetRestrictionService(BudgetService budgetService, LedgerService ledgerService) {
    this.budgetService = budgetService;
    this.ledgerService = ledgerService;
  }

  public CompletableFuture<Void> checkEncumbranceRestrictions(List<Fund> funds, List<Transaction> encumbrances, RequestContext requestContext) {
    Map<String, List<Transaction>> trsGroupedByFindId = encumbrances.stream().collect(groupingBy(Transaction::getFromFundId));
    Map<String, List<Transaction>> trsGroupedByLedgerId = funds.stream()
            .collect(groupingBy(Fund::getLedgerId, HashMap::new, flatMapping(fund -> trsGroupedByFindId.get(fund.getId()).stream(), toList())));
    List<String> fundIds = funds.stream().map(Fund::getId).collect(toList());
    return budgetService.getBudgets(fundIds, requestContext)
      .thenCombine(ledgerService.getLedgersByIds(trsGroupedByLedgerId.keySet(), requestContext), (budgets, ledgers) -> {
        ledgers.stream()
                .filter(ledger -> Boolean.TRUE.equals(ledger.getRestrictEncumbrance()))
                .forEach(ledger -> checkEnoughMoneyForTransactions(trsGroupedByLedgerId.get(ledger.getId()), budgets));
        return null;
      });
  }

  private void checkEnoughMoneyForTransactions(List<Transaction> encumbrances, List<Budget> budgets) {
    Set<String> fundIds = encumbrances.stream()
      .map(Transaction::getFromFundId)
      .collect(Collectors.toSet());

    Map<String, Budget> fundIdBudgetMap = budgets.stream()
      .filter(budget -> fundIds.contains(budget.getFundId()))
      .collect(toMap(Budget::getFundId, Function.identity()));

    Map<Budget, List<Transaction>> trsGroupedByBudget = encumbrances.stream()
      .collect(groupingBy(transaction -> fundIdBudgetMap.get(transaction.getFromFundId())));

    checkEnoughMoneyInBudgets(trsGroupedByBudget);
  }

  private Collector<Transaction, ?, MonetaryAmount> sumTransactionAmounts(String currency) {
    return mapping(tx -> Money.of(tx.getAmount(), tx.getCurrency()),
        Collectors.reducing(Money.of(0, currency), MonetaryFunctions::sum));
  }

  public void checkEnoughMoneyInBudgets(Map<Budget, List<Transaction>> trsGroupedByBudget) {
    List<String> failedBudgets = new ArrayList<>();
    trsGroupedByBudget.forEach((budget, encumbrances) -> {
      if (!encumbrances.isEmpty() && budget.getAllowableEncumbrance() != null) {

          String currency = encumbrances.get(0).getCurrency();
          Map<String, MonetaryAmount> transactionAmountsByFunds = encumbrances.stream()
                  .collect(groupingBy(Transaction::getFromFundId, sumTransactionAmounts(currency)));

        MonetaryAmount transactionAmount = transactionAmountsByFunds.get(budget.getFundId());
        MonetaryAmount remainingAmount = getBudgetRemainingAmountForEncumbrance(budget, transactionAmount.getCurrency());
        if (transactionAmount.isGreaterThan(remainingAmount)) {
          failedBudgets.add(budget.getId());
        }

      }
    });
    if (!failedBudgets.isEmpty()) {
      throw new HttpException(422, FUND_CANNOT_BE_PAID.toError()
              .withAdditionalProperty(BUDGETS, failedBudgets));
    }

  }

  /**
   * Calculates remaining amount for encumbrance
   * [remaining amount] = (allocated + netTransfers) * allowableEncumbered - (encumbered + awaitingPayment + expended)
   *
   * @param budget processed budget
   * @return remaining amount for encumbrance
   */

  public Money getBudgetRemainingAmountForEncumbrance(Budget budget, CurrencyUnit systemCurrency) {
    Money allocated = Money.of(budget.getAllocated(), systemCurrency);
    // get allowableEncumbered converted from percentage value
    BigDecimal allowableEncumbered = BigDecimal.valueOf(budget.getAllowableEncumbrance())
      .movePointLeft(2);

    Money netTransfers = Money.of(budget.getNetTransfers(), systemCurrency);
    Money encumbered = Money.of(budget.getEncumbered(), systemCurrency);
    Money awaitingPayment = Money.of(budget.getAwaitingPayment(), systemCurrency);
    Money expended = Money.of(budget.getExpenditures(), systemCurrency);

    Money totalFunding = allocated.add(netTransfers);
    Money unavailable = encumbered.add(awaitingPayment).add(expended);

    return totalFunding.multiply(allowableEncumbered).subtract(unavailable);
  }
}
