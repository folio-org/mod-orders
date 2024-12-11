package org.folio.service.finance.transaction;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Metadata;
import org.folio.rest.acq.model.finance.Tags;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Ongoing;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.FinanceHoldersBuilder;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.LedgerService;
import org.folio.service.finance.budget.BudgetService;

public class EncumbranceRelationsHoldersBuilder extends FinanceHoldersBuilder {

  private final EncumbranceService encumbranceService;

  public EncumbranceRelationsHoldersBuilder(EncumbranceService encumbranceService, FundService fundService,
                                            FiscalYearService fiscalYearService, ExchangeRateProviderResolver exchangeRateProviderResolver,
                                            BudgetService budgetService, LedgerService ledgerService) {
    super(fundService, fiscalYearService, exchangeRateProviderResolver, budgetService, ledgerService);
    this.encumbranceService = encumbranceService;
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
        withToBeReleasedHolders(encumbranceHolders, transactions);
        return encumbranceHolders;
      });
  }

  public void withKnownTransactions(List<EncumbranceRelationsHolder> encumbranceHolders, List<Transaction> transactions) {
    mapHoldersToTransactions(encumbranceHolders, transactions);
    withToBeReleasedHolders(encumbranceHolders, transactions);
  }

  private void withToBeReleasedHolders(List<EncumbranceRelationsHolder> encumbranceHolders,
                                       List<Transaction> transactionsFromStorage) {

    List<EncumbranceRelationsHolder> toBeReleasedHolders = transactionsFromStorage.stream()
      .filter(transaction -> encumbranceHolders.stream()
        .noneMatch(holder -> holder.getOldEncumbrance() != null && transaction.getId().equals(holder.getOldEncumbrance().getId())))
      .map(tr -> buildToBeReleasedHolder(tr, encumbranceHolders))
      .toList();
    encumbranceHolders.addAll(toBeReleasedHolders);
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
          .withAmountCredited(existingTransaction.getEncumbrance().getAmountCredited())
          .withAmountAwaitingPayment(existingTransaction.getEncumbrance().getAmountAwaitingPayment());
        if (existingTransaction.getEncumbrance().getStatus() == Encumbrance.Status.RELEASED)
          newTransaction.getEncumbrance().setStatus(Encumbrance.Status.RELEASED);
      }));
  }

  public Future<List<EncumbranceRelationsHolder>> prepareEncumbranceRelationsHolder(CompositePurchaseOrder compPO,
                                                                                    CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    List<EncumbranceRelationsHolder> holders = buildBaseHolders(compPO);
    return withFinances(holders, requestContext)
      .compose(v -> withExistingTransactions(holders, poFromStorage, requestContext));
  }

  public Future<Map<String, List<CompositePoLine>>> retrieveMapFiscalYearsWithCompPOLines(CompositePurchaseOrder compPO,
                                                                                          CompositePurchaseOrder poAndLinesFromStorage,
                                                                                          RequestContext requestContext) {
    return prepareEncumbranceRelationsHolder(compPO, poAndLinesFromStorage, requestContext)
      .map(erhList -> erhList.stream()
        .filter(erh -> erh.getCurrentFiscalYearId() != null)
        .collect(Collectors.groupingBy(
          EncumbranceRelationsHolder::getCurrentFiscalYearId,
          Collectors.mapping(
            EncumbranceRelationsHolder::getPoLine,
            Collectors.collectingAndThen(
              Collectors.toList(),
              list -> list.stream().distinct().toList()
            )
          )
        ))
      );
  }

}
