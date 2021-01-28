package org.folio.service.orders;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.MonetaryOperator;
import javax.money.convert.ConversionQuery;
import javax.money.convert.ConversionQueryBuilder;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;

import org.folio.models.ReEncumbranceHolder;
import org.folio.orders.utils.AsyncUtil;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.BudgetService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.RolloverRetrieveService;
import org.folio.service.finance.TransactionService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.json.JsonObject;

public class ReEncumbranceHoldersBuilder {

  private static final String GET_ORDER_TRANSACTIONS_QUERY_TEMPLATE = "fiscalYearId==%s AND encumbrance.sourcePurchaseOrderId==%s";

  private final BudgetService budgetService;
  private final LedgerService ledgerService;
  private final FundService fundService;
  private final ExchangeRateProviderResolver exchangeRateProviderResolver;
  private final FiscalYearService fiscalYearService;
  private final RolloverRetrieveService rolloverRetrieveService;
  private final TransactionService transactionService;

  public ReEncumbranceHoldersBuilder(BudgetService budgetService, LedgerService ledgerService, FundService fundService,
      ExchangeRateProviderResolver exchangeRateProviderResolver, FiscalYearService fiscalYearService,
      RolloverRetrieveService rolloverRetrieveService, TransactionService transactionService) {
    this.budgetService = budgetService;
    this.ledgerService = ledgerService;
    this.fundService = fundService;
    this.exchangeRateProviderResolver = exchangeRateProviderResolver;
    this.fiscalYearService = fiscalYearService;
    this.rolloverRetrieveService = rolloverRetrieveService;
    this.transactionService = transactionService;
  }

  public List<ReEncumbranceHolder> buildReEncumbranceHoldersWithOrdersData(CompositePurchaseOrder compPO) {
    return compPO.getCompositePoLines()
      .stream()
      .flatMap(compositePoLine -> compositePoLine.getFundDistribution()
        .stream()
        .map(fundDistribution -> new ReEncumbranceHolder().withFundDistribution(fundDistribution)
          .withPoLine(compositePoLine)
          .withPurchaseOrder(compPO)))
      .collect(toList());
  }

  public CompletableFuture<List<ReEncumbranceHolder>> withFunds(List<ReEncumbranceHolder> holders, RequestContext requestContext) {
    if (holders.isEmpty()) {
      return CompletableFuture.completedFuture(holders);
    }

    var fundIds = holders.stream()
            .map(ReEncumbranceHolder::getFundId)
            .distinct()
            .collect(toList());

    return fundService.getFunds(fundIds, requestContext)
            .thenApply(funds -> withFunds(holders, funds));
  }

  public CompletableFuture<List<ReEncumbranceHolder>> withLedgers(List<ReEncumbranceHolder> holders,
      RequestContext requestContext) {
    List<String> ledgerIds = holders.stream()
      .map(ReEncumbranceHolder::getFund)
      .filter(Objects::nonNull)
      .map(Fund::getLedgerId)
      .distinct()
      .collect(toList());
    if (ledgerIds.isEmpty()) {
      return CompletableFuture.completedFuture(holders);
    }
    return ledgerService.getLedgersByIds(ledgerIds, requestContext)
      .thenApply(ledgers -> {
        Map<String, Ledger> idLedgerMap = ledgers.stream()
          .collect(toMap(Ledger::getId, Function.identity()));
        return holders.stream()
                .map(holder -> {
                  String ledgerId = Optional.ofNullable(holder.getFund()).map(Fund::getLedgerId).orElse(null);
                  return holder.withLedger(idLedgerMap.get(ledgerId));
                })
                .collect(toList());

      });
  }

  public CompletableFuture<List<ReEncumbranceHolder>> withCurrentFiscalYear(List<ReEncumbranceHolder> holders,
                                                                            RequestContext requestContext) {
    return holders.stream()
            .map(ReEncumbranceHolder::getFund)
            .filter(Objects::nonNull)
            .map(Fund::getLedgerId)
            .findFirst().map(ledgerId -> fiscalYearService.getCurrentFiscalYear(ledgerId, requestContext)
              .thenApply(fiscalYear -> holders.stream().map(holder -> holder.withCurrentFiscalYear(fiscalYear)).collect(toList())))
            .orElseGet(() -> CompletableFuture.completedFuture(holders));

  }

  public CompletableFuture<List<ReEncumbranceHolder>> withBudgets(List<ReEncumbranceHolder> holders,
      RequestContext requestContext) {
    List<String> fundIds = holders.stream()
      .map(ReEncumbranceHolder::getFund)
      .filter(Objects::nonNull)
      .map(Fund::getId)
      .distinct()
      .collect(toList());
    if (fundIds.isEmpty()) {
      return CompletableFuture.completedFuture(holders);
    }
    return budgetService.fetchBudgetsByFundIds(fundIds, requestContext)
      .thenApply(budgets -> {
        Map<String, Budget> idBudgetMap = budgets.stream()
          .collect(toMap(Budget::getFundId, Function.identity()));
        return holders.stream()
                .map(holder -> holder.withBudget(idBudgetMap.get(Optional.of(holder)
                        .map(ReEncumbranceHolder::getFund)
                        .map(Fund::getId)
                        .orElse(null))))
                .collect(toList());
      });
  }

  public CompletableFuture<List<ReEncumbranceHolder>> withRollovers(List<ReEncumbranceHolder> holders,
      RequestContext requestContext) {
    List<String> ledgerIds = holders.stream()
      .map(ReEncumbranceHolder::getFund)
      .filter(Objects::nonNull)
      .map(Fund::getLedgerId)
      .distinct()
      .collect(toList());
    Optional<String> fiscalYearId = holders.stream()
            .map(ReEncumbranceHolder::getCurrentFiscalYear)
            .filter(Objects::nonNull)
            .map(FiscalYear::getId)
            .findFirst();
    if (ledgerIds.isEmpty() || fiscalYearId.isEmpty()) {
      return CompletableFuture.completedFuture(holders);
    }
    return rolloverRetrieveService.getLedgerFyRollovers(fiscalYearId.get(), ledgerIds, requestContext)
      .thenApply(rollovers -> withRollovers(rollovers, holders));
  }

  public CompletableFuture<List<ReEncumbranceHolder>> withConversion(List<ReEncumbranceHolder> reEncumbranceHolders,
      RequestContext requestContext) {
    Optional<CurrencyUnit> fyCurrency = reEncumbranceHolders.stream()
            .map(ReEncumbranceHolder::getCurrentFiscalYear)
            .filter(Objects::nonNull)
            .map(FiscalYear::getCurrency)
            .map(currency -> Monetary.getCurrency(currency))
            .findFirst();
    if (fyCurrency.isEmpty()) {
      return CompletableFuture.completedFuture(reEncumbranceHolders);
    }
    return AsyncUtil.executeBlocking(requestContext.getContext(), false, () -> {

      Map<CurrencyUnit, List<ReEncumbranceHolder>> currencyHoldersMap = reEncumbranceHolders.stream()
        .collect(groupingBy(reEncumbranceHolder -> Monetary.getCurrency(reEncumbranceHolder.getPoLine()
          .getCost()
          .getCurrency())));
      currencyHoldersMap.forEach((poLineCurrency, holders) -> {
        ConversionQuery conversionQuery = ConversionQueryBuilder.of()
          .setBaseCurrency(poLineCurrency)
          .setTermCurrency(fyCurrency.get())
          .build();
        ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
        CurrencyConversion conversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
        holders.forEach(holder -> holder.withPoLineToFyConversion(conversion));
      });
      return reEncumbranceHolders;
    });

  }

  public CompletableFuture<List<ReEncumbranceHolder>> withEncumbrances(List<ReEncumbranceHolder> holders,
      RequestContext requestContext) {
    return holders.stream()
      .filter(reEncumbranceHolder -> Objects.nonNull(reEncumbranceHolder.getRollover()))
      .findFirst()
      .map(reEncumbranceHolder -> withFromTransactions(holders, requestContext)
        .thenCompose(h -> withToTransactions(holders, requestContext)))
      .orElseGet(() -> CompletableFuture.completedFuture(holders));

  }

  public List<ReEncumbranceHolder> withEncumbranceRollover(List<ReEncumbranceHolder> holders) {
    return holders.stream().map(this::withEncumbranceRollover).collect(toList());
  }

  private List<ReEncumbranceHolder> withFunds(List<ReEncumbranceHolder> reEncumbranceHolders, List<Fund> funds) {
    Map<String, Fund> idFundMap = funds.stream()
            .collect(toMap(Fund::getId, Function.identity()));
    return reEncumbranceHolders.stream().map(holder -> holder.withFund(idFundMap.get(holder.getFundId()))).collect(toList());
  }

  private List<ReEncumbranceHolder> withRollovers(List<LedgerFiscalYearRollover> rollovers,
      List<ReEncumbranceHolder> holders) {

    Map<String, LedgerFiscalYearRollover> ledgerIdRolloverMap = rollovers.stream()
      .collect(toMap(LedgerFiscalYearRollover::getLedgerId, Function.identity()));

    return holders.stream().map(holder -> {
      LedgerFiscalYearRollover rollover = Optional.of(holder).map(ReEncumbranceHolder::getFund)
      .map(Fund::getLedgerId).map(ledgerIdRolloverMap::get).orElse(null);
      return holder.withRollover(rollover);
    }).collect(toList());
  }

  private CompletableFuture<List<ReEncumbranceHolder>> withToTransactions(List<ReEncumbranceHolder> holders,
      RequestContext requestContext) {
    ReEncumbranceHolder reEncumbranceHolder = holders.get(0);
    String toQuery = String.format(GET_ORDER_TRANSACTIONS_QUERY_TEMPLATE, reEncumbranceHolder.getRollover()
      .getToFiscalYearId(),
        reEncumbranceHolder.getPurchaseOrder()
          .getId());

    return transactionService.getTransactions(toQuery, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(TransactionCollection::getTransactions)
      .thenApply(transactions -> {
        populateExistingToFYEncumbrances(holders, transactions);
        populateNonExistingToFYEncumbrances(holders);
        return holders;
      });
  }

  private void populateNonExistingToFYEncumbrances(List<ReEncumbranceHolder> holders) {
    holders.stream()
      .filter(holder -> Objects.isNull(holder.getToFYEncumbrance()))
      .forEach(this::buildToFyEncumbrance);
  }

  private void populateExistingToFYEncumbrances(List<ReEncumbranceHolder> holders, List<Transaction> transactions) {
    holders.forEach(holder -> transactions.stream()
      .filter(transaction -> isTransactionRelatedToHolder(holder, transaction))
      .findFirst()
      .ifPresent(holder::withToFYEncumbrance));
  }

  private void buildToFyEncumbrance(ReEncumbranceHolder holder) {
    Optional.ofNullable(holder.getEncumbranceRollover())
      .ifPresent(encumbranceRollover -> {
        Transaction fromEncumbrance = holder.getFromFYEncumbrance();
        Transaction toEncumbrance = JsonObject.mapFrom(fromEncumbrance)
          .mapTo(Transaction.class)
                .withFiscalYearId(holder.getRollover().getToFiscalYearId())
          .withId(null);
        toEncumbrance.getEncumbrance()
          .withStatus(Encumbrance.Status.UNRELEASED)
          .withAmountAwaitingPayment(0d)
          .withAmountExpended(0d);

        MonetaryOperator percentOperator = MonetaryOperators.percent(encumbranceRollover.getIncreaseBy());
        MonetaryAmount amount;
        if (encumbranceRollover.getBasedOn() == EncumbranceRollover.BasedOn.REMAINING) {
          amount = Money.of(fromEncumbrance.getAmount(), fromEncumbrance.getCurrency());
        } else {
          amount = Money.of(fromEncumbrance.getEncumbrance()
            .getAmountExpended(), fromEncumbrance.getCurrency());
        }
        amount = amount.add(amount.with(percentOperator));
        toEncumbrance.setAmount(amount.getNumber()
          .doubleValue());
        toEncumbrance.getEncumbrance()
          .setInitialAmountEncumbered(toEncumbrance.getAmount());
        holder.withToFYEncumbrance(toEncumbrance);
      });
  }

  private CompletableFuture<List<ReEncumbranceHolder>> withFromTransactions(List<ReEncumbranceHolder> holders,
      RequestContext requestContext) {
    return holders.stream()
      .filter(holder -> Objects.nonNull(holder.getRollover()))
      .findFirst()
      .map(holder -> String.format(GET_ORDER_TRANSACTIONS_QUERY_TEMPLATE, holder.getRollover()
        .getFromFiscalYearId(),
          holder.getPurchaseOrder()
            .getId()))
      .map(fromQuery -> transactionService.getTransactions(fromQuery, 0, Integer.MAX_VALUE, requestContext)
        .thenApply(TransactionCollection::getTransactions)
        .thenApply(transactions -> {
          var idTransactionMap = transactions.stream().collect(toMap(Transaction::getId, Function.identity()));
          return holders.stream()
                  .map(holder -> holder.withFromFYEncumbrance(idTransactionMap.get(holder.getFundDistribution().getEncumbrance())))
                  .collect(toList());
        }))
      .orElseGet(() -> CompletableFuture.completedFuture(holders));
  }

  private boolean isTransactionRelatedToHolder(ReEncumbranceHolder holder, Transaction transaction) {
    return Objects.equals(transaction.getFromFundId(), holder.getFundId()) && Objects.equals(transaction.getExpenseClassId(), holder.getFundDistribution()
            .getExpenseClassId()) && Objects.equals(transaction.getEncumbrance().getSourcePoLineId(), holder.getPoLine()
            .getId());
  }

  private ReEncumbranceHolder withEncumbranceRollover(ReEncumbranceHolder reEncumbranceHolder) {
    CompositePurchaseOrder purchaseOrder = reEncumbranceHolder.getPurchaseOrder();
    return reEncumbranceHolder.getRollover()
      .getEncumbrancesRollover()
      .stream()
      .filter(er -> (er.getOrderType() == EncumbranceRollover.OrderType.ONE_TIME
          && purchaseOrder.getOrderType() == CompositePurchaseOrder.OrderType.ONE_TIME)
          || (er.getOrderType() == EncumbranceRollover.OrderType.ONGOING
              && purchaseOrder.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING && !purchaseOrder.getOngoing()
                .getIsSubscription())
          || (er.getOrderType() == EncumbranceRollover.OrderType.ONGOING_SUBSCRIPTION
              && purchaseOrder.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING && purchaseOrder.getOngoing()
                .getIsSubscription()))
      .findFirst()
      .map(reEncumbranceHolder::withEncumbranceRollover)
      .orElse(reEncumbranceHolder);
  }
}
