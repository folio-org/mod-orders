package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.folio.orders.utils.HelperUtils.getConversionQuery;
import static org.folio.rest.core.exceptions.ErrorCodes.MULTIPLE_FISCAL_YEARS;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;

import io.vertx.core.json.JsonObject;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Metadata;
import org.folio.rest.acq.model.finance.Tags;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Ongoing;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.budget.BudgetService;

import io.vertx.core.Future;

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

  public Future<List<EncumbranceRelationsHolder>> withExistingTransactions(List<EncumbranceRelationsHolder> encumbranceHolders,
      CompositePurchaseOrder poAndLinesFromStorage, RequestContext requestContext) {
    if (poAndLinesFromStorage == null) {
      return Future.succeededFuture(encumbranceHolders);
    }

    List<String> transactionIds = poAndLinesFromStorage.getCompositePoLines().stream()
      .flatMap(poLine -> poLine.getFundDistribution().stream().map(FundDistribution::getEncumbrance))
      .filter(Objects::nonNull)
      .collect(Collectors.toList());
    if (transactionIds.isEmpty()) {
      return Future.succeededFuture(encumbranceHolders);
    }

    return encumbranceService.getEncumbrancesByIds(transactionIds, requestContext)
      .map(transactions -> {
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

  public Future<List<EncumbranceRelationsHolder>> withBudgets(List<EncumbranceRelationsHolder> encumbranceHolders, RequestContext requestContext) {
    List<String> fundIds = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getFundId)
      .filter(Objects::nonNull)
      .distinct()
      .collect(toList());
    if (fundIds.isEmpty()) {
      return Future.succeededFuture(encumbranceHolders);
    }
    return budgetService.getBudgets(fundIds, requestContext)
      .map(budgets -> mapHoldersToBudgets(budgets, encumbranceHolders));
  }

  public Future<List<EncumbranceRelationsHolder>> withLedgersData(List<EncumbranceRelationsHolder> encumbranceHolders, RequestContext requestContext) {
    List<String> fundIds = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getFundId)
      .filter(Objects::nonNull)
      .distinct()
      .collect(toList());

    if (fundIds.isEmpty()) {
      return Future.succeededFuture(encumbranceHolders);
    }

    return fundService.getAllFunds(fundIds, requestContext)
      .map(funds -> populateLedgerIds(funds, encumbranceHolders))
      .compose(holders -> {
        List<String> ledgerIds = encumbranceHolders.stream()
          .map(EncumbranceRelationsHolder::getLedgerId)
          .distinct()
          .collect(toList());
        return ledgerService.getLedgersByIds(ledgerIds, requestContext);
      })
      .map(ledgers -> mapRestrictEncumbranceToHolders(ledgers, encumbranceHolders));

  }

  public Future<List<EncumbranceRelationsHolder>> withFiscalYearData(List<EncumbranceRelationsHolder> encumbranceHolders, RequestContext requestContext) {
    if (encumbranceHolders.isEmpty())
      return Future.succeededFuture(encumbranceHolders);
    List<String> fiscalYearIds = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getBudget)
      .map(Budget::getFiscalYearId)
      .distinct()
      .collect(toList());
    if (fiscalYearIds.size() > 1) {
      List<Parameter> parameters = List.of(
        new Parameter().withKey("fiscalYearIds").withValue(fiscalYearIds.toString()),
        new Parameter().withKey("poId").withValue(encumbranceHolders.get(0).getPurchaseOrder().getId())
      );
      throw new HttpException(422, MULTIPLE_FISCAL_YEARS.toError().withParameters(parameters));
    }
    String fiscalYearId = fiscalYearIds.get(0);
    return fiscalYearService.getFiscalYearById(fiscalYearId, requestContext)
      .map(fiscalYear -> {
        encumbranceHolders.forEach(holder -> holder.withCurrentFiscalYearId(fiscalYear.getId())
          .withCurrency(fiscalYear.getCurrency()));
        return encumbranceHolders;
      });
  }

  public Future<List<EncumbranceRelationsHolder>> withConversion(List<EncumbranceRelationsHolder> encumbranceHolders, RequestContext requestContext) {
    return encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getCurrency)
      .filter(Objects::nonNull)
      .findFirst()
      .map(transactionCurrency -> requestContext.getContext()
        .<List<EncumbranceRelationsHolder>>executeBlocking(execBlockingFuture -> {
          Map<String, List<EncumbranceRelationsHolder>> currencyHolderMap = encumbranceHolders.stream()
            .filter(holder -> Objects.nonNull(holder.getPoLine()))
            .collect(groupingBy(holder -> holder.getPoLine().getCost().getCurrency()));

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
          execBlockingFuture.complete(encumbranceHolders);
        }))
      .orElseGet(() -> Future.succeededFuture(encumbranceHolders));
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
      .filter(transaction -> encumbranceHolders.stream()
        .noneMatch(holder -> holder.getOldEncumbrance() != null && transaction.getId().equals(holder.getOldEncumbrance().getId())))
      .map(tr -> buildToBeReleasedHolder(tr, encumbranceHolders))
      .toList();
    encumbranceHolders.addAll(toBeReleasedHolders);
    return encumbranceHolders;
  }

  private boolean encumbranceHolderMatchWithoutId(Transaction encumbrance, EncumbranceRelationsHolder holder) {
    return encumbrance.getEncumbrance().getSourcePoLineId().equals(holder.getPoLineId())
      && encumbrance.getFromFundId().equals(holder.getFundId())
      && Objects.equals(encumbrance.getExpenseClassId(), holder.getFundDistribution().getExpenseClassId());
  }

  private Optional<Transaction> findBestMatch(EncumbranceRelationsHolder holder, List<Transaction> transactions) {
    // Match by fundId/expenseClassId first and by encumbrance id last
    // (to avoid a conflict if 2 expense classes are switched between fund distributions of the same amount)
    Optional<Transaction> fundEcMatch = transactions.stream().filter(tr -> encumbranceHolderMatchWithoutId(tr, holder)).findFirst();
    if (fundEcMatch.isPresent()) {
      if (holder.getFundDistribution().getEncumbrance() != null) {
        // update the encumbrance id in the po line fund distribution
        holder.getFundDistribution().setEncumbrance(fundEcMatch.get().getId());
      }
      return fundEcMatch;
    }
    String id = holder.getFundDistribution().getEncumbrance();
    return transactions.stream().filter(tr -> tr.getId().equals(id)).findAny();
  }

  private EncumbranceRelationsHolder buildToBeReleasedHolder(Transaction transaction,
      List<EncumbranceRelationsHolder> encumbranceHolders) {
    // find the transaction reference in the fund distributions, to be able to remove it if the transaction is deleted
    FundDistribution fd = encumbranceHolders.stream()
      .filter(erh -> erh.getFundDistribution() != null &&
        transaction.getId().equals(erh.getFundDistribution().getEncumbrance()))
      .findFirst()
      .map(EncumbranceRelationsHolder::getFundDistribution)
      .orElse(null);
    return new EncumbranceRelationsHolder()
      .withOldEncumbrance(transaction)
      .withFundDistribution(fd);
  }

  private void mapHoldersToTransactions(List<EncumbranceRelationsHolder> encumbranceHolders,
      List<Transaction> existingTransactions) {
    encumbranceHolders.forEach(holder -> findBestMatch(holder, existingTransactions)
      .ifPresent(existingTransaction -> {
        holder.withOldEncumbrance(existingTransaction);
        Transaction newTransaction = holder.getNewEncumbrance();
        newTransaction.setId(existingTransaction.getId());
        newTransaction.setVersion(existingTransaction.getVersion());
        newTransaction.setMetadata(JsonObject.mapFrom(existingTransaction.getMetadata()).mapTo(Metadata.class));
        newTransaction.getEncumbrance()
          .withAmountExpended(existingTransaction.getEncumbrance().getAmountExpended())
          .withAmountAwaitingPayment(existingTransaction.getEncumbrance().getAmountAwaitingPayment());
        if (existingTransaction.getEncumbrance().getStatus() == Encumbrance.Status.RELEASED)
          newTransaction.getEncumbrance().setStatus(Encumbrance.Status.RELEASED);
      }));
  }

  public Future<List<EncumbranceRelationsHolder>> prepareEncumbranceRelationsHolder(CompositePurchaseOrder compPO,
      CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = buildBaseHolders(compPO);
    return withBudgets(encumbranceRelationsHolders, requestContext)
      .compose(holders -> withLedgersData(holders, requestContext))
      .compose(holders -> withFiscalYearData(holders, requestContext))
      .compose(holders -> withConversion(holders, requestContext))
      .compose(holders -> withExistingTransactions(holders, poFromStorage, requestContext));
  }

  public Future<Map<String, List<CompositePoLine>>> retrieveMapFiscalYearsWithCompPOLines(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
                                                                               RequestContext requestContext) {
    return prepareEncumbranceRelationsHolder(compPO, poAndLinesFromStorage, requestContext)
      .map(erhList -> erhList.stream().filter(erh-> Objects.nonNull(erh.getCurrentFiscalYearId())).collect(groupingBy(EncumbranceRelationsHolder::getCurrentFiscalYearId,
        mapping(EncumbranceRelationsHolder::getPoLine, toList()))));
  }

}
