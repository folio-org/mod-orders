package org.folio.rest.impl;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.BUDGET_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.orders.utils.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.FUND_ID;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.ResourcePathResolver.BUDGETS;
import static org.folio.orders.utils.ResourcePathResolver.ENCUMBRANCES;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.orders.utils.ResourcePathResolver.LEDGERS;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TRANSACTION_SUMMARIES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.function.BiConsumer;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.money.MonetaryAmount;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.BudgetCollection;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.LedgerCollection;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryFunctions;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

public class FinanceHelper extends AbstractHelper {
  private static final String ENCUMBRANCE_POST_ENDPOINT = resourcesPath(ENCUMBRANCES) + "?lang=%s";
  private static final String GET_CURRENT_FISCAL_YEAR_BY_ID = "/finance/ledgers/%s/current-fiscal-year?lang=%s";
  private static final String GET_FUNDS_WITH_SEARCH_PARAMS = resourcesPath(FUNDS) + SEARCH_PARAMS;
  private static final String GET_BUDGETS_WITH_SEARCH_PARAMS = resourcesPath(BUDGETS) + SEARCH_PARAMS;
  private static final String GET_LEDGERS_WITH_SEARCH_PARAMS = resourcesPath(LEDGERS) + SEARCH_PARAMS;
  private static final String QUERY_EQUALS = "&query=";
  private String systemCurrency;

  FinanceHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * Creates Encumbrance records associated with given PO line and updates PO line with corresponding links.
   *
   * @return CompletableFuture with void on success.
   */
  CompletableFuture<Void> handleEncumbrances(CompositePurchaseOrder compPo) {
    List<Pair<Transaction, FundDistribution>> encumbranceDistributionPairs = buildEncumbrances(compPo);
    List<Transaction> encumbrances = encumbranceDistributionPairs.stream()
      .map(Pair::getLeft)
      .collect(toList());
    return retrieveSystemCurrency()
      .thenCompose(v -> prepareEncumbrances(encumbrances))
      .thenCompose(v -> createSummary(compPo.getId(), encumbrances.size()))
      .thenCompose(summaryId -> createEncumbrances(encumbranceDistributionPairs));
  }

  private CompletableFuture<Void> retrieveSystemCurrency() {
    return getSystemCurrency().thenAccept(currency -> systemCurrency = currency);
  }


  private Collector<Transaction, ?, MonetaryAmount> sumTransactionAmounts() {
    return Collectors.mapping(tx -> Money.of(tx.getAmount(), systemCurrency),
      Collectors.reducing(Money.of(0, systemCurrency), MonetaryFunctions::sum));
  }


  private CompletableFuture<String> createSummary(String id, int number) {
    OrderTransactionSummary summary = new OrderTransactionSummary()
      .withId(id)
      .withNumTransactions(number);
    return createRecordInStorage(JsonObject.mapFrom(summary), resourcesPath(ORDER_TRANSACTION_SUMMARIES));
  }

  private CompletableFuture<Void> createEncumbrances(List<Pair<Transaction, FundDistribution>> encumbranceDistributionPairs) {
    return VertxCompletableFuture.allOf(ctx, encumbranceDistributionPairs.stream()
      .map(pair -> createRecordInStorage(JsonObject.mapFrom(pair.getLeft()), String.format(ENCUMBRANCE_POST_ENDPOINT, lang))
        .thenAccept(id -> pair.getValue().setEncumbrance(id))
        .exceptionally(fail -> {
          checkForCustomTransactionError(fail);
          throw new CompletionException(fail);
        }))
      .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> prepareEncumbrances(List<Transaction> encumbrances) {
    Map<String, List<Transaction>> groupedByFund = encumbrances.stream()
      .collect(groupingBy(Transaction::getFromFundId));
    return groupByLedgerIds(groupedByFund)
      .thenCompose(trsGroupedByLedgerId -> checkEncumbranceRestrictions(trsGroupedByLedgerId, groupedByFund)
        .thenApply(v -> trsGroupedByLedgerId))
      .thenCompose(trsGroupedByLedgerId -> VertxCompletableFuture.allOf(ctx,
        trsGroupedByLedgerId.entrySet()
          .stream()
          .map(entry -> getCurrentFiscalYear(entry.getKey()).thenAccept(fiscalYear -> updateEncumbrances(entry, fiscalYear)))
          .collect(toList())
          .toArray(new CompletableFuture[0])));
  }

  private CompletableFuture<Void> checkEncumbranceRestrictions(Map<String, List<Transaction>> trsGroupedByLedgerId,
      Map<String, List<Transaction>> groupedByFund) {
    return getBudgets(groupedByFund).thenCombine(getLedgersByIds(new ArrayList<>(trsGroupedByLedgerId.keySet())),
        (budgets, ledgers) -> {
          trsGroupedByLedgerId.forEach((ledgerId, transactions) -> {
            Ledger processedLedger = ledgers.stream()
              .filter(ledger -> ledger.getId().equals(ledgerId))
              .findFirst()
              .orElseThrow(() -> new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), LEDGER_NOT_FOUND_FOR_TRANSACTION.toError()));

            verifyBudgetsForEncumbrancesAreActive(budgets, transactions);

            if (processedLedger.getRestrictEncumbrance()) {
              checkEnoughMoneyForTransactions(transactions, budgets);
            }
          });
          return null;
        });
  }

  private void verifyBudgetsForEncumbrancesAreActive(List<Budget> budgets, List<Transaction> transactions) {
    transactions.forEach(tr -> budgets.stream()
      .filter(budget -> budget.getFundId().equals(tr.getFromFundId()))
      .filter(budget -> budget.getBudgetStatus() == Budget.BudgetStatus.ACTIVE)
      .findFirst()
      .orElseThrow(() -> new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), BUDGET_IS_INACTIVE.toError())));
  }

  private void checkEnoughMoneyForTransactions(List<Transaction> encumbrances, List<Budget> budgets) {
    Map<String, MonetaryAmount> transactionAmountsByFunds = encumbrances.stream()
      .collect(groupingBy(Transaction::getFromFundId, sumTransactionAmounts()));
    checkEnoughMoneyInBudgets(budgets, transactionAmountsByFunds);
  }

  private void checkEnoughMoneyInBudgets(List<Budget> budgets, Map<String, MonetaryAmount> transactionAmountsByFunds) {
    List<String> failedBudgets = new ArrayList<>();
    budgets.stream()
      .filter(budget -> budget.getAllowableEncumbrance() != null)
      .forEach(budget -> {
        MonetaryAmount remainingAmount = getBudgetRemainingAmountForEncumbrance(budget);
        MonetaryAmount transactionAmount = transactionAmountsByFunds.get(budget.getFundId());
        if (transactionAmount.isGreaterThan(remainingAmount)) {
          failedBudgets.add(budget.getId());
        }
      });
    if (!failedBudgets.isEmpty()) {
      throw new HttpException(422, FUND_CANNOT_BE_PAID.toError().withAdditionalProperty(BUDGETS, failedBudgets));
    }
  }

  private void updateEncumbrances(Map.Entry<String, List<Transaction>> entry, FiscalYear fiscalYear) {
    entry.getValue()
      .forEach(transaction -> transaction.withFiscalYearId(fiscalYear.getId())
        .withCurrency(StringUtils.isNotEmpty(fiscalYear.getCurrency()) ? fiscalYear.getCurrency() : systemCurrency));
  }

  private CompletableFuture<Map<String, List<Transaction>>> groupByLedgerIds(Map<String, List<Transaction>> groupedByFund) {
    return getFunds(groupedByFund).thenApply(lists -> lists.stream()
      .flatMap(Collection::stream)
      .collect(HashMap::new, accumulator(groupedByFund), Map::putAll));
  }

  private CompletableFuture<List<List<Fund>>> getFunds(Map<String, List<Transaction>> groupedByFund) {
    return collectResultsOnSuccess(StreamEx.ofSubLists(new ArrayList<>(groupedByFund.entrySet()), MAX_IDS_FOR_GET_RQ)
      .map(entries -> entries.stream()
        .map(Map.Entry::getKey)
        .distinct()
        .collect(toList()))
      .map(this::getFundsByIds)
      .toList());
  }

  private CompletableFuture<List<List<Budget>>> getBudgetsByChunks(Map<String, List<Transaction>> groupedByFund) {
    return collectResultsOnSuccess(StreamEx.ofSubLists(new ArrayList<>(groupedByFund.entrySet()), MAX_IDS_FOR_GET_RQ)
      .map(entries -> entries.stream()
        .map(Map.Entry::getKey)
        .distinct()
        .collect(toList()))
      .map(this::getBudgetsByFundIds)
      .toList());
  }



  private CompletableFuture<List<Budget>> getBudgets(Map<String, List<Transaction>> groupedByFund) {
    return getBudgetsByChunks(groupedByFund).thenApply(lists -> lists.stream()
      .flatMap(Collection::stream)
      .collect(Collectors.toList()));
  }


  private BiConsumer<HashMap<String, List<Transaction>>, Fund> accumulator(Map<String, List<Transaction>> groupedByFund) {
    return (map, fund) -> map.merge(fund.getLedgerId(), groupedByFund.get(fund.getId()), (transactions, transactions2) -> {
      transactions.addAll(transactions2);
      return transactions;
    });
  }

  private CompletableFuture<List<Fund>> getFundsByIds(List<String> ids) {
    String query = convertIdsToCqlQuery(ids);
    String queryParam = QUERY_EQUALS + encodeQuery(query, logger);
    String endpoint = String.format(GET_FUNDS_WITH_SEARCH_PARAMS, MAX_IDS_FOR_GET_RQ, 0, queryParam, lang);

    return HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(entries -> entries.mapTo(FundCollection.class))
      .thenApply(fundCollection -> {
        if (ids.size() == fundCollection.getFunds().size()) {
          return fundCollection.getFunds();
        }
        String missingIds = String.join(", ", CollectionUtils.subtract(ids, fundCollection.getFunds().stream().map(Fund::getId).collect(toList())));
        throw new HttpException(400, FUNDS_NOT_FOUND.toError().withParameters(Collections.singletonList(new Parameter().withKey("funds").withValue(missingIds))));
      });
  }

  private CompletableFuture<List<Budget>> getBudgetsByFundIds(List<String> ids) {
    String query = convertIdsToCqlQuery(ids, FUND_ID);
    String queryParam = QUERY_EQUALS + encodeQuery(query, logger);
    String endpoint = String.format(GET_BUDGETS_WITH_SEARCH_PARAMS, MAX_IDS_FOR_GET_RQ, 0, queryParam, lang);

    return HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(entries -> VertxCompletableFuture.supplyBlockingAsync(ctx, () -> entries.mapTo(BudgetCollection.class)))
      .thenApply(budgetCollection -> {
        if (ids.size() == budgetCollection.getBudgets().size()) {
          return budgetCollection.getBudgets();
        }
        String missingIds = String.join(", ", CollectionUtils.subtract(ids, budgetCollection.getBudgets().stream().map(Budget::getId).collect(toList())));
        throw new HttpException(400, BUDGET_NOT_FOUND_FOR_TRANSACTION.toError().withParameters(Collections.singletonList(new Parameter().withKey("budgets").withValue(missingIds))));
      });
  }

  private CompletableFuture<List<Ledger>> getLedgersByIds(List<String> ledgerIds) {
    String query = convertIdsToCqlQuery(ledgerIds, ID);
    String queryParam = QUERY_EQUALS + encodeQuery(query, logger);
    String endpoint = String.format(GET_LEDGERS_WITH_SEARCH_PARAMS, MAX_IDS_FOR_GET_RQ, 0, queryParam, lang);

    return HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(entries -> VertxCompletableFuture.supplyBlockingAsync(ctx, () -> entries.mapTo(LedgerCollection.class)))
      .thenApply(ledgerCollection -> {
        if (ledgerIds.size() == ledgerCollection.getLedgers().size()) {
          return ledgerCollection.getLedgers();
        }
        String missingIds = String.join(", ", CollectionUtils.subtract(ledgerIds, ledgerCollection.getLedgers().stream().map(Ledger::getId).collect(toList())));
        throw new HttpException(400, LEDGER_NOT_FOUND_FOR_TRANSACTION.toError().withParameters(Collections.singletonList(new Parameter().withKey("ledgers").withValue(missingIds))));
      });
  }

  private List<Pair<Transaction, FundDistribution>>  buildEncumbrances(CompositePurchaseOrder compPo) {

    return compPo.getCompositePoLines()
      .stream()
      .flatMap(poLine -> poLine.getFundDistribution()
        .stream()
        .map(fundDistribution -> new MutablePair<>(buildEncumbrance(fundDistribution, poLine, compPo), fundDistribution)))
      .collect(toList());
  }

  private CompletableFuture<FiscalYear> getCurrentFiscalYear(String ledgerId) {
    String endpoint = String.format(GET_CURRENT_FISCAL_YEAR_BY_ID, ledgerId, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger).thenApply(entry -> entry.mapTo(FiscalYear.class))
      .exceptionally(t -> {
        if (isFiscalYearNotFound(t)) {
          List<Parameter> parameters = Collections.singletonList(new Parameter().withValue(ledgerId)
            .withKey("ledgerId"));
          throw new HttpException(400, CURRENT_FISCAL_YEAR_NOT_FOUND.toError()
            .withParameters(parameters));
        }
        throw new CompletionException(t.getCause());
      });
  }

  private boolean isFiscalYearNotFound(Throwable t) {
    return t.getCause() instanceof HttpException && ((HttpException) t.getCause()).getCode() == 404;
  }

  private Transaction buildEncumbrance(FundDistribution distribution, CompositePoLine poLine, CompositePurchaseOrder compPo) {
    MonetaryAmount estimatedPrice = calculateEstimatedPrice(poLine.getCost());
    Transaction transaction = new Transaction();
    transaction.setTransactionType(Transaction.TransactionType.ENCUMBRANCE);
    transaction.setFromFundId(distribution.getFundId());
    transaction.setSource(Transaction.Source.USER);
    transaction.setAmount(calculateAmountEncumbered(distribution, estimatedPrice));
    transaction.setCurrency(systemCurrency);

    transaction.setEncumbrance(new Encumbrance());
    transaction.getEncumbrance()
      .setSourcePoLineId(poLine.getId());
    transaction.getEncumbrance()
      .setSourcePurchaseOrderId(compPo.getId());
    transaction.getEncumbrance()
      .setReEncumber(compPo.getReEncumber());
    transaction.getEncumbrance()
      .setSubscription(compPo.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING);
    transaction.getEncumbrance()
      .setStatus(Encumbrance.Status.UNRELEASED);
    transaction.getEncumbrance()
      .setOrderType(Encumbrance.OrderType.fromValue(compPo.getOrderType()
        .value()));
    transaction.getEncumbrance()
      .setInitialAmountEncumbered(transaction.getAmount());

    return transaction;
  }

  private double calculateAmountEncumbered(FundDistribution distribution, MonetaryAmount estimatedPrice) {
    if (distribution.getDistributionType() == FundDistribution.DistributionType.PERCENTAGE) {
      return estimatedPrice.with(MonetaryOperators.percent(distribution.getValue()))
        .with(MonetaryOperators.rounding())
        .getNumber()
        .doubleValue();
    }
    return distribution.getValue();
  }

  public void checkForCustomTransactionError(Throwable fail) {
    if (fail.getCause().getMessage().contains(BUDGET_NOT_FOUND_FOR_TRANSACTION.getDescription())) {
      throw new CompletionException(new HttpException(422, BUDGET_NOT_FOUND_FOR_TRANSACTION));
    } else if (fail.getCause().getMessage().contains(LEDGER_NOT_FOUND_FOR_TRANSACTION.getDescription())) {
      throw new CompletionException(new HttpException(422, LEDGER_NOT_FOUND_FOR_TRANSACTION));
    } else if (fail.getCause().getMessage().contains(BUDGET_IS_INACTIVE.getDescription())) {
      throw new CompletionException(new HttpException(422, BUDGET_IS_INACTIVE));
    } else if (fail.getCause().getMessage().contains(FUND_CANNOT_BE_PAID.getDescription())) {
      throw new CompletionException(new HttpException(422, FUND_CANNOT_BE_PAID));
    } else {
      throw new CompletionException(fail.getCause());
    }
  }

  /**
   * Calculates remaining amount for encumbrance
   * [remaining amount] = (allocated * allowableEncumbered) - (encumbered + awaitingPayment + expended)
   *
   * @param budget     processed budget
   * @return remaining amount for encumbrance
   */
  private Money getBudgetRemainingAmountForEncumbrance(Budget budget) {
    Money allocated = Money.of(budget.getAllocated(), systemCurrency);
    // get allowableEncumbered converted from percentage value
    BigDecimal allowableEncumbered = BigDecimal.valueOf(budget.getAllowableEncumbrance()).movePointLeft(2);

    Money encumbered = Money.of(budget.getEncumbered(), systemCurrency);
    Money awaitingPayment = Money.of(budget.getAwaitingPayment(), systemCurrency);
    Money expenditures = Money.of(budget.getExpenditures(), systemCurrency);

    return allocated.multiply(allowableEncumbered)
      .subtract(encumbered.add(awaitingPayment.add(expenditures)));
  }
}
