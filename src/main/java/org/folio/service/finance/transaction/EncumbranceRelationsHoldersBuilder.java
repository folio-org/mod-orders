package org.folio.service.finance.transaction;

import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Tags;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Ongoing;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.budget.BudgetService;

import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.folio.orders.utils.HelperUtils.getConversionQuery;

public class EncumbranceRelationsHoldersBuilder {

  private final EncumbranceService encumbranceService;
  private final FundService fundService;
  private final FiscalYearService fiscalYearService;
  private final ExchangeRateProviderResolver exchangeRateProviderResolver;
  private final BudgetService budgetService;
  private final LedgerService ledgerService;

  public EncumbranceRelationsHoldersBuilder(EncumbranceService encumbranceService, FundService fundService,
                                            FiscalYearService fiscalYearService,
                                            ExchangeRateProviderResolver exchangeRateProviderResolver, BudgetService budgetService,
                                            LedgerService ledgerService) {
    this.encumbranceService = encumbranceService;
    this.fundService = fundService;
    this.fiscalYearService = fiscalYearService;
    this.exchangeRateProviderResolver = exchangeRateProviderResolver;
    this.budgetService = budgetService;
    this.ledgerService = ledgerService;
  }

  public List<EncumbranceRelationsHolder> buildBaseHolders(CompositePurchaseOrder compPO) {
    return compPO.getCompositePoLines()
      .stream()
      .flatMap(poLine -> poLine.getFundDistribution()
        .stream()
        .map(fundDistribution -> new EncumbranceRelationsHolder().withFundDistribution(fundDistribution)
          .withPoLine(poLine)
          .withPurchaseOrder(compPO)))
      .map(this::buildBaseHolder)
      .collect(Collectors.toList());
  }

  private EncumbranceRelationsHolder buildBaseHolder(EncumbranceRelationsHolder holder) {
    Encumbrance encumbrance = new Encumbrance().withSourcePoLineId(holder.getPoLineId())
      .withSourcePurchaseOrderId(holder.getOrderId())
      .withOrderType(Encumbrance.OrderType.fromValue(holder.getOrderType()
        .value()))
      .withReEncumber(holder.getReEncumber())
      .withSubscription(Optional.ofNullable(holder.getOngoing())
        .map(Ongoing::getIsSubscription)
        .orElse(false));
    Transaction transaction = new Transaction().withTransactionType(Transaction.TransactionType.ENCUMBRANCE)
      .withFromFundId(holder.getFundId())
      .withExpenseClassId(holder.getExpenseClassId())
      .withSource(Transaction.Source.PO_LINE)
      .withEncumbrance(encumbrance);
    org.folio.rest.jaxrs.model.Tags tags = holder.getPoLine()
      .getTags();
    if (Objects.nonNull(tags)) {
      transaction.setTags(new Tags().withTagList(tags.getTagList()));
    }
    return holder.withNewEncumbrance(transaction);
  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> withExistingTransactions(
      List<EncumbranceRelationsHolder> encumbranceHolders, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    if (poAndLinesFromStorage == null)
      return CompletableFuture.completedFuture(encumbranceHolders);
    List<String> transactionIds = poAndLinesFromStorage.getCompositePoLines().stream()
      .flatMap(poLine -> poLine.getFundDistribution().stream().map(FundDistribution::getEncumbrance))
      .filter(Objects::nonNull)
      .collect(Collectors.toList());
    if (transactionIds.isEmpty())
      return CompletableFuture.completedFuture(encumbranceHolders);
    return encumbranceService.getEncumbrancesByIds(transactionIds, requestContext)
      .thenApply(transactions -> {
        mapHoldersToTransactions(encumbranceHolders, transactions);
        return withToBeReleasedHolders(encumbranceHolders, transactions);
      });
  }

  public List<EncumbranceRelationsHolder> withKnownTransactions(
      List<EncumbranceRelationsHolder> encumbranceHolders,
      List<Transaction> transactions) {
    mapHoldersToTransactions(encumbranceHolders, transactions);
    return withToBeReleasedHolders(encumbranceHolders, transactions);
  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> withBudgets(List<EncumbranceRelationsHolder> encumbranceHolders,
      RequestContext requestContext) {
    List<String> fundIds = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getFundId)
      .filter(Objects::nonNull)
      .distinct()
      .collect(toList());
    if (fundIds.isEmpty()) {
      return CompletableFuture.completedFuture(encumbranceHolders);
    }
    return budgetService.getBudgets(fundIds, requestContext)
      .thenApply(budgets -> mapHoldersToBudgets(budgets, encumbranceHolders));
  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> withLedgersData(List<EncumbranceRelationsHolder> encumbranceHolders,
      RequestContext requestContext) {
    List<String> fundIds = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getFundId)
      .filter(Objects::nonNull)
      .distinct()
      .collect(toList());

    if (fundIds.isEmpty()) {
      return CompletableFuture.completedFuture(encumbranceHolders);
    }

    return fundService.getAllFunds(fundIds, requestContext)
      .thenApply(funds -> populateLedgerIds(funds, encumbranceHolders))
      .thenCompose(holders -> {
        List<String> ledgerIds = encumbranceHolders.stream()
          .map(EncumbranceRelationsHolder::getLedgerId)
          .distinct()
          .collect(toList());
        return ledgerService.getLedgersByIds(ledgerIds, requestContext);
      })
      .thenApply(ledgers -> mapRestrictEncumbranceToHolders(ledgers, encumbranceHolders));

  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> withFiscalYearData(List<EncumbranceRelationsHolder> encumbranceHolders,
      RequestContext requestContext) {
    return encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getBudget)
      .map(Budget::getFiscalYearId)
      .findFirst()
      .map(fiscalYearId -> fiscalYearService.getFiscalYearById(fiscalYearId, requestContext)
        .thenApply(fiscalYear -> {
          encumbranceHolders.forEach(holder -> holder.withCurrentFiscalYearId(fiscalYear.getId())
            .withCurrency(fiscalYear.getCurrency()));
          return encumbranceHolders;
        }))
      .orElseGet(() -> CompletableFuture.completedFuture(encumbranceHolders));

  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> withConversion(List<EncumbranceRelationsHolder> encumbranceHolders,
                                                                            RequestContext requestContext) {
    return encumbranceHolders.stream().map(EncumbranceRelationsHolder::getCurrency)
        .filter(Objects::nonNull).findFirst().map(transactionCurrency -> FolioVertxCompletableFuture.supplyBlockingAsync(requestContext.getContext(), () ->  {

          Map<String, List<EncumbranceRelationsHolder>> currencyHolderMap = encumbranceHolders.stream().filter(holder -> Objects.nonNull(holder.getPoLine())).collect(groupingBy(holder -> holder.getPoLine().getCost().getCurrency()));
          currencyHolderMap.forEach((poLineCurrency, encumbranceRelationsHolders) -> {
            Double exchangeRate = encumbranceRelationsHolders.stream()
                .map(EncumbranceRelationsHolder::getPoLine)
                .map(CompositePoLine::getCost)
                .map(Cost::getExchangeRate)
                .filter(Objects::nonNull)
                .findFirst()
                .orElse(null);
            ConversionQuery conversionQuery = getConversionQuery(exchangeRate, poLineCurrency, transactionCurrency);
            ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
            CurrencyConversion conversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
            encumbranceRelationsHolders.forEach(holder -> holder.withPoLineToFyConversion(conversion));
          });
          return encumbranceHolders;
        }))
        .orElseGet(() -> CompletableFuture.completedFuture(encumbranceHolders));
  }

  private List<EncumbranceRelationsHolder> populateLedgerIds(List<Fund> funds,
      List<EncumbranceRelationsHolder> encumbranceHolders) {
    Map<String, String> idFundMap = funds.stream()
      .collect(toMap(Fund::getId, Fund::getLedgerId));
    encumbranceHolders.stream()
      .filter(holder -> Objects.nonNull(holder.getFundId()))
      .forEach(holder -> holder.withLedgerId(idFundMap.get(holder.getFundId())));
    return encumbranceHolders;
  }

  private List<EncumbranceRelationsHolder> mapRestrictEncumbranceToHolders(List<Ledger> ledgers,
      List<EncumbranceRelationsHolder> encumbranceHolders) {
    Map<String, Ledger> idLedgerMap = ledgers.stream()
      .collect(toMap(Ledger::getId, Function.identity()));
    encumbranceHolders.stream()
      .filter(holder -> Objects.nonNull(holder.getLedgerId()))
      .forEach(holder -> {
        Ledger ledger = idLedgerMap.get(holder.getLedgerId());
        holder.withRestrictEncumbrances(Optional.ofNullable(ledger)
          .map(Ledger::getRestrictEncumbrance)
          .orElse(false));
      });
    return encumbranceHolders;
  }

  private List<EncumbranceRelationsHolder> mapHoldersToBudgets(List<Budget> budgets,
      List<EncumbranceRelationsHolder> encumbranceHolders) {
    Map<String, Budget> fundIdBudgetMap = budgets.stream()
      .collect(toMap(Budget::getFundId, Function.identity()));
    encumbranceHolders.forEach(holder -> holder.withBudget(fundIdBudgetMap.get(holder.getFundId())));
    return encumbranceHolders;
  }

  public List<EncumbranceRelationsHolder> withToBeReleasedHolders(List<EncumbranceRelationsHolder> encumbranceHolders,
      List<Transaction> transactionsFromStorage) {

    List<EncumbranceRelationsHolder> toBeReleasedHolders = transactionsFromStorage.stream()
      .filter(transaction -> encumbranceHolders.stream().allMatch(holder -> !transaction.getEncumbrance().getSourcePoLineId().equals(holder.getPoLineId())
          || !transaction.getFromFundId().equals(holder.getFundId())
          || !Objects.equals(transaction.getExpenseClassId(), holder.getFundDistribution().getExpenseClassId())))
      .map(this::buildToBeReleasedHolder)
      .collect(toList());
    encumbranceHolders.addAll(toBeReleasedHolders);
    return encumbranceHolders;
  }

  private EncumbranceRelationsHolder buildToBeReleasedHolder(Transaction transaction) {
    return new EncumbranceRelationsHolder().withOldEncumbrance(transaction);
  }

  private void mapHoldersToTransactions(List<EncumbranceRelationsHolder> encumbranceHolders, List<Transaction> existingTransactions) {

    encumbranceHolders.forEach(holder -> existingTransactions.stream()
          .filter(encumbrance -> encumbrance.getEncumbrance().getSourcePoLineId().equals(holder.getPoLineId())
              && encumbrance.getFromFundId().equals(holder.getFundId())
              && Objects.equals(encumbrance.getExpenseClassId(), holder.getFundDistribution().getExpenseClassId()))
          .findAny()
          .ifPresent(existingTransaction -> {
            holder.withOldEncumbrance(existingTransaction);
            Transaction newTransaction = holder.getNewEncumbrance();
            newTransaction.setId(existingTransaction.getId());
            newTransaction.getEncumbrance()
                .withAmountExpended(existingTransaction.getEncumbrance().getAmountExpended())
                .withAmountAwaitingPayment(existingTransaction.getEncumbrance().getAmountAwaitingPayment());
          }));
  }

  private CompletableFuture<List<EncumbranceRelationsHolder>> prepareEncumbranceRelationsHolder(CompositePurchaseOrder compPO,
                                                                                                CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = buildBaseHolders(compPO);
    return withBudgets(encumbranceRelationsHolders, requestContext)
      .thenCompose(holders -> withLedgersData(holders, requestContext))
      .thenCompose(holders -> withFiscalYearData(holders, requestContext))
      .thenCompose(holders -> withConversion(holders, requestContext))
      .thenCompose(holders -> withExistingTransactions(holders, poFromStorage, requestContext));
  }

  public CompletableFuture<Map<String, List<CompositePoLine>>> retrieveMapFiscalYearsWithCompPOLines(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
                                                                               RequestContext requestContext) {
    return prepareEncumbranceRelationsHolder(compPO, poAndLinesFromStorage, requestContext)
      .thenApply(erhList -> erhList.stream().collect(groupingBy(EncumbranceRelationsHolder::getCurrentFiscalYearId,
        mapping(EncumbranceRelationsHolder::getPoLine, toList()))));
  }

  public List<Transaction> retrieveTransactionCollection(List<List<Transaction>> transactionCollections) {
    List<Transaction> allTransactions = new ArrayList<>();
    transactionCollections.forEach(transCollection -> allTransactions.addAll(transCollection));
    return allTransactions;
  }
}
