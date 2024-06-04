package org.folio.service.orders;

import static java.math.MathContext.DECIMAL64;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.ReEncumbranceHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.service.FundsDistributionService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.FinanceHoldersBuilder;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.budget.BudgetService;
import org.folio.service.finance.rollover.LedgerRolloverService;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.Money;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ReEncumbranceHoldersBuilder extends FinanceHoldersBuilder {

  private static final String GET_ORDER_TRANSACTIONS_QUERY_TEMPLATE = "fiscalYearId==%s AND encumbrance.sourcePurchaseOrderId==%s";

  private final LedgerRolloverService ledgerRolloverService;
  private final TransactionService transactionService;
  private final FundsDistributionService fundsDistributionService;

  public ReEncumbranceHoldersBuilder(BudgetService budgetService, LedgerService ledgerService, FundService fundService,
      ExchangeRateProviderResolver exchangeRateProviderResolver, FiscalYearService fiscalYearService,
      LedgerRolloverService ledgerRolloverService, TransactionService transactionService,
      FundsDistributionService fundsDistributionService) {
    super(fundService, fiscalYearService, exchangeRateProviderResolver, budgetService, ledgerService);
    this.ledgerRolloverService = ledgerRolloverService;
    this.transactionService = transactionService;
    this.fundsDistributionService = fundsDistributionService;
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

  public Future<List<ReEncumbranceHolder>> withRollovers(List<ReEncumbranceHolder> holders, RequestContext requestContext) {
    List<String> ledgerIds = holders.stream()
      .map(ReEncumbranceHolder::getLedgerId)
      .filter(Objects::nonNull)
      .distinct()
      .collect(toList());
    Optional<String> fiscalYearId = holders.stream()
            .map(ReEncumbranceHolder::getCurrentFiscalYearId)
            .filter(Objects::nonNull)
            .findFirst();
    if (ledgerIds.isEmpty() || fiscalYearId.isEmpty()) {
      return Future.succeededFuture(holders);
    }
    return ledgerRolloverService.getLedgerFyRollovers(fiscalYearId.get(), ledgerIds, requestContext)
      .map(rollovers -> withRollovers(rollovers, holders));
  }

  @Override
  protected Future<Void> withConversion(List<? extends EncumbranceRelationsHolder> encumbranceHolders,
      RequestContext requestContext) {
    List<ReEncumbranceHolder> reEncumbranceHolders = encumbranceHolders.stream()
      .map(h -> (ReEncumbranceHolder)h)
      .toList();
    Optional<String> fyCurrency = reEncumbranceHolders.stream()
      .map(ReEncumbranceHolder::getCurrency)
      .filter(Objects::nonNull)
      .findFirst();
    if (fyCurrency.isEmpty()) {
      return Future.succeededFuture(null);
    }
    return requestContext.getContext().executeBlocking(() -> {
      Map<String, List<ReEncumbranceHolder>> currencyHoldersMap = reEncumbranceHolders.stream()
        .collect(groupingBy(holder -> holder.getPoLine().getCost().getCurrency()));
      currencyHoldersMap.forEach((poLineCurrency, holders) -> {
        Double exchangeRate = holders.stream()
          .map(ReEncumbranceHolder::getPoLine)
          .map(CompositePoLine::getCost)
          .map(Cost::getExchangeRate)
          .filter(Objects::nonNull)
          .findFirst().orElse(null);
        ConversionQuery poLineToFYConversionQuery = HelperUtils.getConversionQuery(exchangeRate, poLineCurrency, fyCurrency.get());
        ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(poLineToFYConversionQuery, requestContext);
        CurrencyConversion poLineToFYConversion = exchangeRateProvider.getCurrencyConversion(poLineToFYConversionQuery);
        exchangeRate = poLineToFYConversion.getExchangeRate(Money.of(0d, poLineCurrency)).getFactor().doubleValue();

        double reverseRate = BigDecimal.ONE.divide(BigDecimal.valueOf(exchangeRate), DECIMAL64).doubleValue();

        ConversionQuery fyToPoLineConversionQuery = HelperUtils.getConversionQuery(reverseRate, fyCurrency.get(), poLineCurrency);
        CurrencyConversion fyToPoLineConversion = exchangeRateProvider.getCurrencyConversion(fyToPoLineConversionQuery);
        holders.forEach(holder -> holder.withPoLineToFyConversion(poLineToFYConversion).withFyToPoLineConversion(fyToPoLineConversion));
      });
      return null;
    });
  }

  public Future<List<ReEncumbranceHolder>> withPreviousFyEncumbrances(List<ReEncumbranceHolder> holders,
                                                                                 RequestContext requestContext) {
    return holders.stream()
      .filter(reEncumbranceHolder -> Objects.nonNull(reEncumbranceHolder.getRollover()))
      .findFirst()
      .map(reEncumbranceHolder -> populateFromFyEncumbrances(holders, requestContext))
      .orElseGet(() ->  Future.succeededFuture(holders));

  }

  public Future<List<ReEncumbranceHolder>> withToEncumbrances(List<ReEncumbranceHolder> holders,
                                                                         RequestContext requestContext) {
    return holders.stream()
      .filter(reEncumbranceHolder -> Objects.nonNull(reEncumbranceHolder.getRollover()))
      .findFirst()
      .map(reEncumbranceHolder -> withToTransactions(holders, requestContext))
      .orElseGet(() ->  Future.succeededFuture(holders));

  }

  public List<ReEncumbranceHolder> withEncumbranceRollover(List<ReEncumbranceHolder> holders) {
    return holders.stream().map(this::withEncumbranceRollover).collect(toList());
  }

  private List<ReEncumbranceHolder> withRollovers(List<LedgerFiscalYearRollover> rollovers,
      List<ReEncumbranceHolder> holders) {

    Map<String, LedgerFiscalYearRollover> ledgerIdRolloverMap = rollovers.stream()
      .collect(toMap(LedgerFiscalYearRollover::getLedgerId, Function.identity()));

    return holders.stream().map(holder -> {
      LedgerFiscalYearRollover rollover = Optional.of(holder)
      .map(ReEncumbranceHolder::getLedgerId).map(ledgerIdRolloverMap::get).orElse(null);
      return holder.withRollover(rollover);
    }).collect(toList());
  }

  private Future<List<ReEncumbranceHolder>> withToTransactions(List<ReEncumbranceHolder> holders,
      RequestContext requestContext) {
    ReEncumbranceHolder reEncumbranceHolder = holders.get(0);
    String toQuery = String.format(GET_ORDER_TRANSACTIONS_QUERY_TEMPLATE, reEncumbranceHolder.getRollover()
      .getToFiscalYearId(),
        reEncumbranceHolder.getPurchaseOrder()
          .getId());

    return transactionService.getTransactions(toQuery, requestContext)
      .map(transactions -> {
        populateExistingToFYEncumbrances(holders, transactions);
        populateNonExistingToFYEncumbrances(holders);
        return holders;
      });
  }

  private void populateNonExistingToFYEncumbrances(List<ReEncumbranceHolder> holders) {
    List<ReEncumbranceHolder> holdersWithNewEncumbrances = holders.stream()
      .filter(holder -> Objects.isNull(holder.getNewEncumbrance()))
        .map(holder -> {
          buildToFyEncumbrance(holder);
          return holder;
        })
        .collect(toList());
    fundsDistributionService.distributeFunds(holdersWithNewEncumbrances);

  }

  private void populateExistingToFYEncumbrances(List<ReEncumbranceHolder> holders, List<Transaction> transactions) {
    holders.forEach(holder -> transactions.stream()
      .filter(transaction -> isTransactionRelatedToHolder(holder, transaction))
      .findFirst()
      .ifPresent(holder::withNewEncumbrance));
  }

  private void buildToFyEncumbrance(ReEncumbranceHolder holder) {
    Optional.ofNullable(holder.getEncumbranceRollover())
      .ifPresent(encumbranceRollover -> {
        Transaction fromEncumbrance = holder.getPreviousFyEncumbrance();
        Transaction toEncumbrance = JsonObject.mapFrom(fromEncumbrance).mapTo(Transaction.class)
                                              .withFiscalYearId(holder.getRollover().getToFiscalYearId())
                                              .withId(null);
        toEncumbrance.getEncumbrance()
          .withStatus(Encumbrance.Status.UNRELEASED)
          .withAmountAwaitingPayment(0d)
          .withAmountExpended(0d);

        holder.withNewEncumbrance(toEncumbrance);
      });
  }

  private Future<List<ReEncumbranceHolder>> populateFromFyEncumbrances(List<ReEncumbranceHolder> holders,
                                                                                  RequestContext requestContext) {
    return holders.stream()
      .filter(holder -> Objects.nonNull(holder.getRollover()))
      .findFirst()
      .map(holder -> String.format(GET_ORDER_TRANSACTIONS_QUERY_TEMPLATE, holder.getRollover()
        .getFromFiscalYearId(), holder.getPurchaseOrder().getId()))
      .map(fromQuery -> transactionService.getTransactions(fromQuery, requestContext)
        .map(transactions -> {
          var idTransactionMap = transactions.stream().collect(toMap(Transaction::getId, Function.identity()));
          return holders.stream()
                  .map(holder -> holder.withPreviousFyEncumbrance(idTransactionMap.get(holder.getFundDistribution().getEncumbrance())))
                  .collect(toList());
        }))
      .orElseGet(() ->  Future.succeededFuture(holders));
  }

  private boolean isTransactionRelatedToHolder(ReEncumbranceHolder holder, Transaction transaction) {
    return Objects.equals(transaction.getFromFundId(), holder.getFundId()) && Objects.equals(transaction.getExpenseClassId(), holder.getFundDistribution()
            .getExpenseClassId()) && Objects.equals(transaction.getEncumbrance().getSourcePoLineId(), holder.getPoLine()
            .getId());
  }

  private ReEncumbranceHolder withEncumbranceRollover(ReEncumbranceHolder reEncumbranceHolder) {
    CompositePurchaseOrder purchaseOrder = reEncumbranceHolder.getPurchaseOrder();
    return Optional.ofNullable(reEncumbranceHolder.getRollover())
      .map(LedgerFiscalYearRollover::getEncumbrancesRollover)
      .flatMap(encumbranceRollovers -> encumbranceRollovers.stream()
        .filter(er -> (er.getOrderType() == EncumbranceRollover.OrderType.ONE_TIME
            && purchaseOrder.getOrderType() == CompositePurchaseOrder.OrderType.ONE_TIME)
            || (er.getOrderType() == EncumbranceRollover.OrderType.ONGOING
                && purchaseOrder.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING && !purchaseOrder.getOngoing()
                  .getIsSubscription())
            || (er.getOrderType() == EncumbranceRollover.OrderType.ONGOING_SUBSCRIPTION
                && purchaseOrder.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING && purchaseOrder.getOngoing()
                  .getIsSubscription()))
        .findFirst())
      .map(reEncumbranceHolder::withEncumbranceRollover)
      .orElse(reEncumbranceHolder);
  }
}
