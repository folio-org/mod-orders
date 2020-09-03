package org.folio.helper;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static org.folio.orders.utils.ErrorCodes.BUDGET_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.orders.utils.ErrorCodes.INACTIVE_EXPENSE_CLASS;
import static org.folio.orders.utils.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.ResourcePathResolver.BUDGETS;
import static org.folio.orders.utils.ResourcePathResolver.BUDGET_EXPENSE_CLASSES;
import static org.folio.orders.utils.ResourcePathResolver.CURRENT_BUDGET;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.orders.utils.ResourcePathResolver.LEDGERS;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TRANSACTION_SUMMARIES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.function.BiConsumer;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.money.MonetaryAmount;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.LineFundId;
import org.folio.models.PoLineFundHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.BudgetExpenseClassCollection;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.LedgerCollection;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Tags;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.TransactionService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryFunctions;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Context;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

public class FinanceHelper extends AbstractHelper {
  private static final String GET_CURRENT_ACTIVE_BUDGET_BY_FUND_ID = resourcesPath(CURRENT_BUDGET) + "?lang=%s&status=Active";
  private static final String GET_CURRENT_FISCAL_YEAR_BY_ID = "/finance/ledgers/%s/current-fiscal-year?lang=%s";
  private static final String GET_FUNDS_WITH_SEARCH_PARAMS = resourcesPath(FUNDS) + SEARCH_PARAMS;
  private static final String GET_BUDGET_EXPENSE_CLASSES_QUERY = resourcesPath(BUDGET_EXPENSE_CLASSES) + SEARCH_PARAMS;
  private static final String GET_LEDGERS_WITH_SEARCH_PARAMS = resourcesPath(LEDGERS) + SEARCH_PARAMS;
  private static final String QUERY_EQUALS = "&query=";
  private static final String ENCUMBRANCE_CRITERIA = "transactionType==Encumbrance";
  private static final String AND = " and ";

  private final TransactionService transactionService;

  private String systemCurrency;

  public FinanceHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
    transactionService = new TransactionService(okapiHeaders, ctx, lang);
  }

  public FinanceHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang
          , TransactionService transactionService) {
    super(httpClient, okapiHeaders, ctx, lang);
    this.transactionService = transactionService;
  }

  /**
   * Creates Encumbrance records associated with given PO line and updates PO line with corresponding links.
   *
   * @return CompletableFuture with void on success.
   */
  public CompletableFuture<List<EncumbranceRelationsHolder>> buildNewEncumbrances(CompositePurchaseOrder compPO,
      List<CompositePoLine> compositePoLines, List<Transaction> storeEncumbrances) {

    List<PoLineFundHolder> poLineFundHolders = buildNewEncumbrancesHolders(compositePoLines, storeEncumbrances);
    if (!poLineFundHolders.isEmpty()) {
      List<EncumbranceRelationsHolder> encumbranceHolders = buildEncumbrances(poLineFundHolders, compPO);
      return prepareEncumbrances(encumbranceHolders);
    }
    return CompletableFuture.completedFuture(Collections.emptyList());
  }

  public CompletableFuture<List<Transaction>> getOrderEncumbrances(String orderId) {
    return transactionService.getTransactions(Integer.MAX_VALUE, 0, buildEncumbranceOrderQuery(orderId))
              .thenApply(TransactionCollection::getTransactions);
  }

  public CompletableFuture<List<Transaction>> getPoLineEncumbrances(String polineId) {
    return transactionService.getTransactions(Integer.MAX_VALUE, 0, buildEncumbranceByPolineQuery(polineId))
      .thenApply(TransactionCollection::getTransactions);
  }

  public List<Transaction> makeEncumbrancesPending(List<Transaction> encumbrances) {
    encumbrances.forEach(encumbrance -> {
      encumbrance.setAmount(0d);
      encumbrance.getEncumbrance().setInitialAmountEncumbered(0d);
      encumbrance.getEncumbrance().setStatus(Encumbrance.Status.PENDING);
    });
    return encumbrances;
  }

  public void updateEncumbrance(FundDistribution fundDistribution, CompositePoLine poLine, Transaction trEncumbrance) {
    MonetaryAmount estimatedPrice = calculateEstimatedPrice(poLine.getCost());
    trEncumbrance.setAmount(calculateAmountEncumbered(fundDistribution, estimatedPrice));
    trEncumbrance.setCurrency(systemCurrency);
    trEncumbrance.setExpenseClassId(fundDistribution.getExpenseClassId());
    if (Objects.nonNull(poLine.getTags())) {
      trEncumbrance.setTags(new Tags().withTagList(poLine.getTags().getTagList()));
    }
    Encumbrance encumbrance = trEncumbrance.getEncumbrance();
    encumbrance.setStatus(Encumbrance.Status.UNRELEASED);
    encumbrance.setInitialAmountEncumbered(trEncumbrance.getAmount());
  }

  public Encumbrance buildEncumbranceWithOrderFields(CompositePurchaseOrder compPo) {
    Encumbrance encumbrance = new Encumbrance();
    encumbrance.setSourcePurchaseOrderId(compPo.getId());
    encumbrance.setReEncumber(compPo.getReEncumber());
    encumbrance.setSubscription(compPo.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING);
    encumbrance.setOrderType(Encumbrance.OrderType.fromValue(compPo.getOrderType().value()));
    return encumbrance;
  }

  private CompletableFuture<Void> retrieveSystemCurrency() {
    return getSystemCurrency().thenAccept(currency -> systemCurrency = currency);
  }


  private Collector<Transaction, ?, MonetaryAmount> sumTransactionAmounts() {
    return Collectors.mapping(tx -> Money.of(tx.getAmount(), systemCurrency),
      Collectors.reducing(Money.of(0, systemCurrency), MonetaryFunctions::sum));
  }

  public CompletableFuture<Void> updateOrderTransactionSummary(String orderId, int number) {
    OrderTransactionSummary summary = new OrderTransactionSummary()
      .withId(orderId)
      .withNumTransactions(number);
    return handleUpdateRequest(resourceByIdPath(ORDER_TRANSACTION_SUMMARIES, orderId), summary);
  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> prepareEncumbrances(List<EncumbranceRelationsHolder> encumbranceHolders) {
    Map<String, List<Transaction>> groupedByFund = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getTransaction)
      .collect(groupingBy(Transaction::getFromFundId));
    return retrieveSystemCurrency()
      .thenCompose(v -> groupByLedgerIds(groupedByFund))
      .thenCompose(trsGroupedByLedgerId -> checkEncumbranceRestrictions(trsGroupedByLedgerId, groupedByFund)
        .thenApply(v -> trsGroupedByLedgerId))
      .thenCompose(trsGroupedByLedgerId -> allOf(ctx,
        trsGroupedByLedgerId.entrySet()
          .stream()
          .map(entry -> getCurrentFiscalYear(entry.getKey()).thenAccept(fiscalYear -> buildEncumbrancesForUpdate(entry, fiscalYear)))
          .collect(toList())
          .toArray(new CompletableFuture[0])))
      .thenApply(v -> encumbranceHolders);
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

  private void buildEncumbrancesForUpdate(Map.Entry<String, List<Transaction>> entry, FiscalYear fiscalYear) {
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
      .map(this::fetchBudgetsByFundIds)
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
        throw new HttpException(404, FUNDS_NOT_FOUND.toError().withParameters(Collections.singletonList(new Parameter().withKey("funds").withValue(missingIds))));
      });
  }

  public CompletableFuture<List<Budget>> fetchBudgetsByFundIds(List<String> fundIds) {
    List<CompletableFuture<Budget>> futureList = fundIds.stream()
      .distinct()
      .map(this::getActiveBudgetByFundId)
      .collect(toList());

    return VertxCompletableFuture.allOf(ctx, futureList.toArray(new CompletableFuture[0]))
      .thenApply(v -> futureList.stream().map(CompletableFuture::join).collect(Collectors.toList()));
  }

  private CompletableFuture<Budget> getActiveBudgetByFundId(String fundId) {
    String endpoint = String.format(GET_CURRENT_ACTIVE_BUDGET_BY_FUND_ID, fundId, lang);

    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger).thenApply(entries -> entries.mapTo(Budget.class))
      .exceptionally(t -> {
        if (t.getCause() instanceof HttpException) {
          throw new HttpException(404, BUDGET_NOT_FOUND_FOR_TRANSACTION
            .toError().withParameters(Collections.singletonList(new Parameter().withKey("fund").withValue(fundId))));
        }
        throw new CompletionException(t.getCause());
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
        throw new HttpException(404, LEDGER_NOT_FOUND_FOR_TRANSACTION.toError().withParameters(Collections.singletonList(new Parameter().withKey("ledgers").withValue(missingIds))));
      });
  }

  private List<EncumbranceRelationsHolder> buildEncumbrances(List<PoLineFundHolder> holders, CompositePurchaseOrder compPO) {
    return holders.stream()
                  .map(holder -> {
                    Encumbrance encumbranceSkeleton = buildEncumbranceWithOrderFields(compPO);
                    Transaction encumbrance = buildEncumbrance(holder.getFundDistribution(), holder.getPoLine(), encumbranceSkeleton);
                    return new EncumbranceRelationsHolder(encumbrance, holder);
                  })
                  .collect(toList());
  }

  private void updateEncumbrancesWithPolFields(List<EncumbranceRelationsHolder> holders) {
    holders.forEach(this::updateEncumbranceWithPolFields);
  }

  private void updateEncumbranceWithPolFields(EncumbranceRelationsHolder holder) {
      PoLineFundHolder poLineFundHolder = holder.getPoLineFundHolder();
      updateEncumbrance(poLineFundHolder.getFundDistribution(), poLineFundHolder.getPoLine(), holder.getTransaction());
  }


  private CompletableFuture<FiscalYear> getCurrentFiscalYear(String ledgerId) {
    String endpoint = String.format(GET_CURRENT_FISCAL_YEAR_BY_ID, ledgerId, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger).thenApply(entry -> entry.mapTo(FiscalYear.class))
      .exceptionally(t -> {
        if (isFiscalYearNotFound(t)) {
          List<Parameter> parameters = Collections.singletonList(new Parameter().withValue(ledgerId).withKey("ledgerId"));
          throw new HttpException(404, CURRENT_FISCAL_YEAR_NOT_FOUND.toError().withParameters(parameters));
        }
        throw new CompletionException(t.getCause());
      });
  }

  private boolean isFiscalYearNotFound(Throwable t) {
    return t.getCause() instanceof HttpException && ((HttpException) t.getCause()).getCode() == 404;
  }

  private Transaction buildEncumbrance(FundDistribution distribution, CompositePoLine poLine, Encumbrance encumbrance) {
    MonetaryAmount estimatedPrice = calculateEstimatedPrice(poLine.getCost());
    Transaction transaction = new Transaction();
    transaction.setTransactionType(Transaction.TransactionType.ENCUMBRANCE);
    transaction.setFromFundId(distribution.getFundId());
    transaction.setSource(Transaction.Source.PO_LINE);
    transaction.setAmount(calculateAmountEncumbered(distribution, estimatedPrice));
    transaction.setCurrency(systemCurrency);
    transaction.setExpenseClassId(distribution.getExpenseClassId());
    if (Objects.nonNull(poLine.getTags())) {
      transaction.setTags(new Tags().withTagList(poLine.getTags().getTagList()));
    }

    encumbrance.setSourcePoLineId(poLine.getId());
    encumbrance.setStatus(Encumbrance.Status.UNRELEASED);
    encumbrance.setInitialAmountEncumbered(transaction.getAmount());
    transaction.setEncumbrance(encumbrance);
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

  private String buildEncumbranceOrderQuery(String orderId) {
    return ENCUMBRANCE_CRITERIA
                + AND + "encumbrance.sourcePurchaseOrderId==" + orderId
                    + AND + "encumbrance.status <> " + Encumbrance.Status.RELEASED;
  }

  private String buildEncumbranceByPolineQuery(String polineId) {
    return ENCUMBRANCE_CRITERIA
      + AND + "encumbrance.sourcePoLineId==" + polineId
      + AND + "encumbrance.status <> " + Encumbrance.Status.RELEASED;
  }


  private Map<LineFundId, PoLineFundHolder> convertToLineFundMap(List<CompositePoLine> lines) {
    Map<LineFundId, PoLineFundHolder> resultMap = new HashMap<>();
    lines.stream()
         .flatMap(line -> line.getFundDistribution().stream().map(fund -> new PoLineFundHolder(line, fund)))
         .collect(toList())
         .forEach(poLineFundHolder -> resultMap.put(poLineFundHolder.getId(), poLineFundHolder));
    return resultMap;
  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> buildEncumbrancesForUpdate(List<CompositePoLine> compPoLines
                              , List<Transaction> storeEncumbrances) {
    List<EncumbranceRelationsHolder> holders = buildUpdateEncumbranceHolders(compPoLines, storeEncumbrances);
    if (!holders.isEmpty()) {
      updateEncumbrancesWithPolFields(holders);
      return prepareEncumbrances(holders);
    }
    return CompletableFuture.completedFuture(Collections.emptyList());
  }

  public List<EncumbranceRelationsHolder> buildUpdateEncumbranceHolders(List<CompositePoLine> poLines
                          , List<Transaction> orderEncumbrancesInStorage) {
    List<EncumbranceRelationsHolder> holders = new ArrayList<>();
    if (!CollectionUtils.isEmpty(orderEncumbrancesInStorage)) {
      poLines.stream()
        .filter(line -> !CollectionUtils.isEmpty(line.getFundDistribution()))
        .forEach(line ->
          line.getFundDistribution().forEach(fund ->
            orderEncumbrancesInStorage.stream()
              .filter(encumbrance -> encumbrance.getEncumbrance().getSourcePoLineId().equals(line.getId())
                                        && encumbrance.getFromFundId().equals(fund.getFundId()))
              .findAny()
              .ifPresent(encumbranceTr -> {
                double amountBeforeUpdate = encumbranceTr.getAmount();
                double initialAmountBeforeUpdate = encumbranceTr.getEncumbrance().getInitialAmountEncumbered();
                EncumbranceRelationsHolder holder = new EncumbranceRelationsHolder()
                                                              .withPoLineFund(new PoLineFundHolder(line, fund))
                                                              .withTransaction(encumbranceTr);
                updateEncumbranceWithPolFields(holder);
                double updatedAmount = encumbranceTr.getAmount();
                double updatedInitialAmount = encumbranceTr.getEncumbrance().getInitialAmountEncumbered();
                if (Double.compare(amountBeforeUpdate, updatedAmount) != 0
                        || (Double.compare(initialAmountBeforeUpdate, updatedInitialAmount) != 0)) {
                  holders.add(holder);
                }
              })
          )
        );
    }
    return holders;
  }

  public List<PoLineFundHolder> buildNewEncumbrancesHolders(List<CompositePoLine> poLines,
      List<Transaction> encumbrancesInStorage) {
    List<PoLineFundHolder> holders = new ArrayList<>();
    if (!CollectionUtils.isEmpty(encumbrancesInStorage)) {

      Map<String, List<Transaction>> groupedByFund = encumbrancesInStorage.stream()
        .collect(groupingBy(Transaction::getFromFundId));
      poLines.stream()
        .filter(line -> !CollectionUtils.isEmpty(line.getFundDistribution()))
        .forEach(line ->
          line.getFundDistribution().forEach(fund -> {
              boolean isFundForUpdate = Optional.ofNullable(groupedByFund.get(fund.getFundId()))
                                                .map(encumbrances -> encumbrances.stream().anyMatch(encumbr -> line.getId().equals(encumbr.getEncumbrance().getSourcePoLineId())))
                                                .orElse(false);
              if (!isFundForUpdate) {
                encumbrancesInStorage.stream()
                  .filter(encumbrance -> (!line.getId().equals(encumbrance.getEncumbrance().getSourcePoLineId()))
                          || (encumbrance.getEncumbrance().getSourcePoLineId().equals(line.getId())
                                        && !encumbrance.getFromFundId().equals(fund.getFundId())))
                  .findAny()
                  .ifPresent(encumbrance -> {
                    PoLineFundHolder holder = new PoLineFundHolder(line, fund);
                    holders.add(holder);
                  });
              }
            }
          )
        );
    }
    else {
      poLines.stream()
        .filter(line -> !CollectionUtils.isEmpty(line.getFundDistribution()))
        .forEach(line ->
          line.getFundDistribution().forEach(fund -> {
            PoLineFundHolder holder = new PoLineFundHolder(line, fund);
            holders.add(holder);
          })
        );
    }
    return holders;
  }

  public List<Transaction> findNeedReleaseEncumbrances(List<CompositePoLine> compositePoLines,
      List<Transaction> orderEncumbrancesInStorage) {
    List<Transaction> encumbrancesForRelease = new ArrayList<>();
    Map<LineFundId, PoLineFundHolder> poLineFundHolderMap = convertToLineFundMap(compositePoLines);
    if (!CollectionUtils.isEmpty(orderEncumbrancesInStorage)) {
      orderEncumbrancesInStorage.forEach(encumbrance -> {
        LineFundId lineFundId = new LineFundId(encumbrance.getEncumbrance().getSourcePoLineId(), encumbrance.getFromFundId());
        PoLineFundHolder poLineFundHolder = poLineFundHolderMap.get(lineFundId);
        if (poLineFundHolder == null) {
          encumbrancesForRelease.add(encumbrance);
        }
      });
    }
    return encumbrancesForRelease;
  }


  public CompletableFuture<Void> updateTransactions(List<Transaction> transactions) {
    return transactionService.updateTransactions(transactions);
  }

  public CompletableFuture<Void> releaseEncumbrances(List<Transaction> encumbrances) {
    return transactionService.releaseEncumbrances(encumbrances);
  }

  public CompletableFuture<String> createOrderTransactionSummary(String orderId, int number) {
    return transactionService.createOrderTransactionSummary(orderId, number);
  }

  CompletionStage<Void> releasePoLineEncumbrances(String lineId) {
    return getPoLineEncumbrances(lineId).thenCompose(trs -> {
      if (!trs.isEmpty()) {
        return updateOrderTransactionSummary(trs.get(0).getEncumbrance().getSourcePurchaseOrderId(), trs.size())
          .thenCompose(v -> transactionService.releaseEncumbrances(trs));
      }
      return CompletableFuture.completedFuture(null);
    });
  }

  CompletionStage<Void> releaseOrderEncumbrances(String orderId) {
    return getOrderEncumbrances(orderId).thenCompose(trs -> {
      if (!trs.isEmpty()) {
        return updateOrderTransactionSummary(orderId, trs.size())
          .thenCompose(v -> transactionService.releaseEncumbrances(trs));
      }
      return CompletableFuture.completedFuture(null);
    });
  }

  public CompletableFuture<Void> validateExpenseClasses(List<CompositePoLine> poLines) {
    List<FundDistribution> fundDistributionsWithExpenseClasses = poLines.stream()
      .flatMap(poLine -> poLine.getFundDistribution().stream())
      .filter(fundDistribution -> Objects.nonNull(fundDistribution.getExpenseClassId()))
      .collect(toList());

    return allOf(ctx, fundDistributionsWithExpenseClasses.stream()
      .map(this::checkExpenseClassIsActiveByFundDistribution)
      .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> checkExpenseClassIsActiveByFundDistribution(FundDistribution fundDistribution) {
    String query = String.format("budget.fundId==%s and budget.budgetStatus==Active and status==Inactive and expenseClassId==%s",
      fundDistribution.getFundId(), fundDistribution.getExpenseClassId());
    String queryParam = QUERY_EQUALS + encodeQuery(query, logger);
    String endpoint = String.format(GET_BUDGET_EXPENSE_CLASSES_QUERY, MAX_IDS_FOR_GET_RQ, 0, queryParam, lang);

    return HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(entries -> entries.mapTo(BudgetExpenseClassCollection.class))
      .thenAccept(budgetExpenseClasses -> {
        if (budgetExpenseClasses.getTotalRecords() > 0) {
          throw new HttpException(400, INACTIVE_EXPENSE_CLASS.toError()
            .withParameters(Arrays.asList(
              new Parameter()
                .withKey("fundId").withValue(fundDistribution.getFundId()),
              new Parameter()
                .withKey("expenseClassId").withValue(fundDistribution.getExpenseClassId())
            )));
        }
      });
  }
}
