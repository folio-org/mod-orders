package org.folio.service.finance;

import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.BUDGET_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.orders.utils.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.ResourcePathResolver.BUDGETS;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.function.BiConsumer;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.money.CurrencyUnit;
import javax.money.MonetaryAmount;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.models.LineFundId;
import org.folio.models.PoLineFundHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Tags;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryFunctions;
import org.javamoney.moneta.function.MonetaryOperators;

public class EncumbranceService {

  private static final String ENCUMBRANCE_CRITERIA = "transactionType==Encumbrance";
  public static final String AND = " and ";
  public static final String FUND_CODE = "fundCode";
  public static final String EXPENSE_CLASS_NAME = "expenseClassName";

  private final TransactionService transactionService;
  private final TransactionSummariesService transactionSummariesService;
  private final ExchangeRateProviderResolver exchangeRateProviderResolver;
  private final FundService fundService;
  private final LedgerService ledgerService;
  private final BudgetService budgetService;
  private final FiscalYearService fiscalYearService;
  private final ConfigurationEntriesService configurationEntriesService;

  public EncumbranceService(TransactionService transactionService,
                            TransactionSummariesService transactionSummariesService,
                            ExchangeRateProviderResolver exchangeRateProviderResolver,
                            FundService fundService,
                            LedgerService ledgerService,
                            BudgetService budgetService,
                            FiscalYearService fiscalYearService,
                            ConfigurationEntriesService configurationEntriesService) {
    this.transactionService = transactionService;
    this.transactionSummariesService = transactionSummariesService;
    this.exchangeRateProviderResolver = exchangeRateProviderResolver;
    this.fundService = fundService;
    this.ledgerService = ledgerService;
    this.budgetService = budgetService;
    this.fiscalYearService = fiscalYearService;
    this.configurationEntriesService = configurationEntriesService;
  }


  CompletableFuture<Void> createOrUpdateEncumbrances(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
    return createEncumbrances(holder.getEncumbrancesForCreate(), requestContext)
            .thenCompose(v -> releaseEncumbrances(holder.getEncumbrancesForRelease(), requestContext))
            .thenCompose(v -> updateEncumbrances(holder, requestContext));
  }

  public CompletableFuture<Void> createEncumbrances(List<EncumbranceRelationsHolder> relationsHolders, RequestContext requestContext) {
    return CompletableFuture.allOf(relationsHolders.stream()
            .map(holder -> transactionService.createTransaction(holder.getTransaction(), requestContext)
                    .thenAccept(transaction -> {
                      PoLineFundHolder poLineFundHolder = holder.getPoLineFundHolder();
                      poLineFundHolder.getFundDistribution().setEncumbrance(transaction.getId());
                    })
                    .exceptionally(fail -> {
                      checkCustomTransactionError(fail);
                      throw new CompletionException(fail);
                    })
            )
            .toArray(CompletableFuture[]::new)
    );
  }

  public CompletionStage<Void> updateEncumbrancesOrderStatus(String orderId, CompositePurchaseOrder.WorkflowStatus orderStatus, RequestContext requestContext) {
    return getOrderEncumbrances(orderId, requestContext)
            .thenCompose(encumbrs -> {
              if (isEncumbrancesOrderStatusUpdateNeeded(orderStatus, encumbrs)) {
                return transactionSummariesService.updateOrderTransactionSummary(orderId, encumbrs.size(), requestContext)
                        .thenApply(v -> {
                          syncEncumbrancesOrderStatus(orderStatus, encumbrs);
                          return encumbrs;
                        })
                        .thenCompose(transactions -> transactionService.updateTransactions(transactions, requestContext));
              }
              return CompletableFuture.completedFuture(null);
            });
  }

  private void syncEncumbrancesOrderStatus(CompositePurchaseOrder.WorkflowStatus workflowStatus,
                                           List<Transaction> encumbrances) {
    Encumbrance.OrderStatus orderStatus = Encumbrance.OrderStatus.fromValue(workflowStatus.value());
    encumbrances.forEach(encumbrance -> encumbrance.getEncumbrance().setOrderStatus(orderStatus));
  }


  private boolean isEncumbrancesOrderStatusUpdateNeeded(CompositePurchaseOrder.WorkflowStatus orderStatus, List<Transaction> encumbrs) {
    return !CollectionUtils.isEmpty(encumbrs) && !orderStatus.value().equals(encumbrs.get(0).getEncumbrance().getOrderStatus().value());
  }


  private CompletionStage<Void> updateEncumbrances(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
    List<Transaction> encumbrances = holder.getEncumbrancesForUpdate().stream()
            .map(EncumbranceRelationsHolder::getTransaction)
            .collect(toList());
    return updateEncumbrances(encumbrances, requestContext);
  }


  /**
   * Creates Encumbrance records associated with given PO line and updates PO line with corresponding links.
   *
   * @return CompletableFuture with void on success.
   */
  public CompletableFuture<List<EncumbranceRelationsHolder>> buildNewEncumbrances(CompositePurchaseOrder compPO,
      List<CompositePoLine> compositePoLines, List<Transaction> storeEncumbrances, RequestContext requestContext) {

    List<PoLineFundHolder> poLineFundHolders = buildNewEncumbrancesHolders(compositePoLines, storeEncumbrances);
    if (!poLineFundHolders.isEmpty()) {
      List<EncumbranceRelationsHolder> encumbranceHolders = buildEncumbrances(poLineFundHolders, compPO);
      return prepareEncumbrances(encumbranceHolders, requestContext)
        .thenApply(holders -> {
          convertTransactionAmountToFYCurrency(holders, requestContext);
          return holders;
        });
    }
    return CompletableFuture.completedFuture(Collections.emptyList());
  }

  private void convertTransactionAmountToFYCurrency(List<EncumbranceRelationsHolder> holders, RequestContext requestContext) {
    holders.forEach(holder -> {
      Transaction transaction = holder.getTransaction();
      String transactionCurrency = transaction.getCurrency();
      String poLineCurrency = holder.getPoLineFundHolder().getPoLine().getCost().getCurrency();

      if (!poLineCurrency.equals(transactionCurrency)) {
        Money amount = Money.of(transaction.getAmount(), poLineCurrency);
        ConversionQuery conversionQuery = ConversionQueryBuilder.of().setBaseCurrency(poLineCurrency)
          .setTermCurrency(transactionCurrency).build();
        ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver
          .resolve(conversionQuery, requestContext);
        CurrencyConversion conversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
        double convertedAmount = amount.with(conversion).getNumber().doubleValue();
        holder.getTransaction().setAmount(convertedAmount);
        holder.getTransaction().getEncumbrance().setInitialAmountEncumbered(convertedAmount);
      }
    });
  }

  public CompletableFuture<List<Transaction>> getOrderEncumbrances(String orderId, RequestContext requestContext) {
    return transactionService.getTransactions(buildEncumbranceOrderQuery(orderId), 0, Integer.MAX_VALUE, requestContext)
              .thenApply(TransactionCollection::getTransactions);
  }

  public CompletableFuture<List<Transaction>> getPoLineEncumbrances(String poLineId, RequestContext requestContext) {
    return transactionService.getTransactions(buildEncumbranceByPoLineQuery(poLineId), 0, Integer.MAX_VALUE, requestContext)
      .thenApply(TransactionCollection::getTransactions);
  }

  public CompletableFuture<List<Transaction>> getPoLinesEncumbrances(List<CompositePoLine> poLines, RequestContext requestContext) {
    List<String> poLineIds = poLines.stream().map(CompositePoLine::getId).collect(toList());
    return transactionService.getTransactions(poLineIds, requestContext);
  }

  public void updateEncumbrance(FundDistribution fundDistribution, CompositePoLine poLine, Transaction trEncumbrance) {
    MonetaryAmount estimatedPrice = calculateEstimatedPrice(poLine.getCost());
    trEncumbrance.setAmount(calculateAmountEncumbered(fundDistribution, estimatedPrice));
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

  private Collector<Transaction, ?, MonetaryAmount> sumTransactionAmounts(String currency) {
    return mapping(tx -> Money.of(tx.getAmount(), tx.getCurrency()),
      Collectors.reducing(Money.of(0, currency), MonetaryFunctions::sum));
  }


  public CompletableFuture<List<EncumbranceRelationsHolder>> prepareEncumbrances(List<EncumbranceRelationsHolder> encumbranceHolders,
                                                                                 RequestContext requestContext) {

    Map<String, List<Transaction>> groupedByFund = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getTransaction)
      .collect(groupingBy(Transaction::getFromFundId));
    return configurationEntriesService.getSystemCurrency(requestContext).thenCompose(systemCurrency -> groupTransactionsByLedgerIds(groupedByFund, requestContext)
      .thenCompose(trsGroupedByLedgerId -> allOf(trsGroupedByLedgerId.entrySet()
        .stream()
        .map(entry -> fiscalYearService.getCurrentFiscalYear(entry.getKey(), requestContext)
                .thenAccept(fiscalYear -> buildEncumbrancesForUpdate(entry, fiscalYear, systemCurrency)))
        .collect(toList())
        .toArray(new CompletableFuture[0]))
              .thenCompose(vVoid -> checkEncumbranceRestrictions(trsGroupedByLedgerId, groupedByFund, requestContext)))
      .thenApply(v -> encumbranceHolders));
  }

  private CompletableFuture<Void> checkEncumbranceRestrictions(Map<String, List<Transaction>> trsGroupedByLedgerId,
      Map<String, List<Transaction>> groupedByFund, RequestContext requestContext) {
    return budgetService.getBudgets(groupedByFund.keySet(), requestContext).thenCombine(ledgerService.getLedgersByIds(trsGroupedByLedgerId.keySet(), requestContext),
        (budgets, ledgers) -> {
          verifyBudgetsForEncumbrancesAreActive(budgets);
          trsGroupedByLedgerId.forEach((ledgerId, transactions) -> {
            Ledger processedLedger = ledgers.stream()
              .filter(ledger -> ledger.getId().equals(ledgerId))
              .findFirst()
              .orElseThrow(() -> new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), LEDGER_NOT_FOUND_FOR_TRANSACTION.toError()));

            if (Boolean.TRUE.equals(processedLedger.getRestrictEncumbrance())) {
              checkEnoughMoneyForTransactions(transactions, budgets);
            }
          });
          return null;
        });
  }

  public void verifyBudgetsForEncumbrancesAreActive(List<Budget> budgets) {
    if (budgets.stream().anyMatch(budget -> budget.getBudgetStatus() != Budget.BudgetStatus.ACTIVE)) {
      throw  new HttpException(HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt(), BUDGET_IS_INACTIVE.toError());
    }
  }

  private void checkEnoughMoneyForTransactions(List<Transaction> encumbrances, List<Budget> budgets) {
    if (!encumbrances.isEmpty()) {
      String currency = encumbrances.get(0).getCurrency();
      Map<String, MonetaryAmount> remainingAmountsByFunds = encumbrances.stream()
              .collect(groupingBy(Transaction::getFromFundId, sumTransactionAmounts(currency)));
      Set<String> fundIds = encumbrances.stream().map(Transaction::getFromFundId).collect(Collectors.toSet());

      List<Budget> relatedBudgets = budgets.stream().filter(budget -> fundIds.contains(budget.getFundId())).collect(toList());

      checkEnoughMoneyInBudgets(relatedBudgets, remainingAmountsByFunds);
    }
  }

  private void checkEnoughMoneyInBudgets(List<Budget> budgets, Map<String, MonetaryAmount> transactionAmountsByFunds) {
    List<String> failedBudgets = new ArrayList<>();
    budgets.stream()
      .filter(budget -> budget.getAllowableEncumbrance() != null)
      .forEach(budget -> {
        MonetaryAmount transactionAmount = transactionAmountsByFunds.get(budget.getFundId());
        MonetaryAmount remainingAmount = getBudgetRemainingAmountForEncumbrance(budget, transactionAmount.getCurrency());
        if (transactionAmount.isGreaterThan(remainingAmount)) {
          failedBudgets.add(budget.getId());
        }
      });
    if (!failedBudgets.isEmpty()) {
      throw new HttpException(422, FUND_CANNOT_BE_PAID.toError().withAdditionalProperty(BUDGETS, failedBudgets));
    }
  }

  public CompletableFuture<Void> releaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    encumbrances.forEach(transaction -> transaction.getEncumbrance().setStatus(Encumbrance.Status.RELEASED));
    return transactionService.updateTransactions(encumbrances, requestContext);
  }

  private void buildEncumbrancesForUpdate(Map.Entry<String, List<Transaction>> entry, FiscalYear fiscalYear, String systemCurrency) {
    entry.getValue()
      .forEach(transaction -> transaction.withFiscalYearId(fiscalYear.getId())
        .withCurrency(chooseCurrency(fiscalYear, systemCurrency)));
  }

  private String chooseCurrency(FiscalYear fiscalYear, String systemCurrency) {
    return StringUtils.isNotEmpty(fiscalYear.getCurrency()) ? fiscalYear.getCurrency() : systemCurrency;
  }

  private CompletableFuture<Map<String, List<Transaction>>> groupTransactionsByLedgerIds(Map<String, List<Transaction>> groupedByFund, RequestContext requestContext) {
    return getFunds(groupedByFund, requestContext).thenApply(lists -> lists.stream()
            .collect(HashMap::new, accumulator(groupedByFund), Map::putAll));
  }

  private CompletableFuture<List<Fund>> getFunds(Map<String, List<Transaction>> groupedByFund, RequestContext requestContext) {
    Collection<String> ids = groupedByFund.keySet();
    return fundService.getAllFunds(ids, requestContext).thenApply(funds -> funds);
  }

  private BiConsumer<HashMap<String, List<Transaction>>, Fund> accumulator(Map<String, List<Transaction>> groupedByFund) {
    return (map, fund) -> map.merge(fund.getLedgerId(), groupedByFund.get(fund.getId()), (transactions, transactions2) -> {
      transactions.addAll(transactions2);
      return transactions;
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


  private Transaction buildEncumbrance(FundDistribution distribution, CompositePoLine poLine, Encumbrance encumbrance) {
    MonetaryAmount estimatedPrice = calculateEstimatedPrice(poLine.getCost());
    Transaction transaction = new Transaction();
    transaction.setTransactionType(Transaction.TransactionType.ENCUMBRANCE);
    transaction.setFromFundId(distribution.getFundId());
    transaction.setSource(Transaction.Source.PO_LINE);
    transaction.setAmount(calculateAmountEncumbered(distribution, estimatedPrice));
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
  private Money getBudgetRemainingAmountForEncumbrance(Budget budget, CurrencyUnit systemCurrency) {
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

  private String buildEncumbranceByPoLineQuery(String polineId) {
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

  public CompletableFuture<List<EncumbranceRelationsHolder>> buildEncumbrancesForUpdate(List<CompositePoLine> compPoLines,
                                                                                        List<Transaction> storeEncumbrances, RequestContext requestContext) {
    List<EncumbranceRelationsHolder> holders = buildUpdateEncumbranceHolders(compPoLines, storeEncumbrances);
    if (!holders.isEmpty()) {
      updateEncumbrancesWithPolFields(holders);
      return prepareEncumbrances(holders, requestContext).thenApply(holdersParam -> {
        convertTransactionAmountToFYCurrency(holdersParam, requestContext);
        return holdersParam;
      });
    }
    return CompletableFuture.completedFuture(Collections.emptyList());
  }

  public List<EncumbranceRelationsHolder> buildUpdateEncumbranceHolders(List<CompositePoLine> poLines,
                                                                        List<Transaction> orderEncumbrancesInStorage) {
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
                                                .map(encumbrances -> encumbrances.stream().anyMatch(encumbrance -> line.getId().equals(encumbrance.getEncumbrance().getSourcePoLineId())))
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

  public CompletableFuture<Void> updateEncumbrances(List<Transaction> transactions, RequestContext requestContext) {
    return transactionService.updateTransactions(transactions, requestContext);
  }

  public CompletableFuture<Void> deletePoLineEncumbrances(String lineId, RequestContext requestContext) {
    return getPoLineEncumbrances(lineId, requestContext)
      .thenCompose(encumbrances -> releaseOrderEncumbrances(encumbrances, requestContext)
        .thenCompose(vVoid -> transactionService.deleteTransactions(encumbrances, requestContext)));
  }

  public CompletableFuture<Void> deleteOrderEncumbrances(String orderId, RequestContext requestContext) {
    return getOrderEncumbrances(orderId, requestContext)
            .thenCompose(encumbrances -> releaseOrderEncumbrances(encumbrances, requestContext)
              .thenCompose(vVoid -> transactionService.deleteTransactions(encumbrances, requestContext)));
  }

  private CompletionStage<Void> releaseOrderEncumbrances(List<Transaction> trs, RequestContext requestContext) {
    if (!trs.isEmpty()) {
      String orderId = trs.get(0).getEncumbrance().getSourcePurchaseOrderId();
      return transactionSummariesService.updateOrderTransactionSummary(orderId, trs.size(), requestContext)
              .thenCompose(v -> releaseEncumbrances(trs, requestContext));
    }
    return CompletableFuture.completedFuture(null);
  }

  protected void checkCustomTransactionError(Throwable fail) {
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

}
