package org.folio.service.finance.transaction;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.finance.WorkflowStatusName;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class OpenToPendingEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final TransactionSummariesService transactionSummariesService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;

  public OpenToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
      TransactionSummariesService transactionSummariesService,
      EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder) {
    this.encumbranceService = encumbranceService;
    this.transactionSummariesService = transactionSummariesService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;

  }

    @Override
    public CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, RequestContext requestContext) {
      return getOrderEncumbrances(compPO, requestContext)
                .thenApply(this::makeEncumbrancesPending)
                .thenCompose(transactions -> transactionSummariesService.updateOrderTransactionSummary(compPO.getId(), transactions.size(), requestContext)
                    .thenApply(vVoid -> transactions))
                .thenCompose(transactions -> encumbranceService.updateEncumbrances(transactions, requestContext));
    }

    private List<Transaction> makeEncumbrancesPending(List<Transaction> encumbrances) {
        encumbrances.forEach(encumbrance -> {
            encumbrance.setAmount(0d);
            encumbrance.getEncumbrance().setInitialAmountEncumbered(0d);
            encumbrance.getEncumbrance().setStatus(Encumbrance.Status.PENDING);
            encumbrance.getEncumbrance().setOrderStatus(Encumbrance.OrderStatus.PENDING);
        });
        return encumbrances;
    }

  @Override
  public WorkflowStatusName getStrategyName() {
    return WorkflowStatusName.OPEN_TO_PENDING;
  }

  public CompletableFuture<List<EncumbranceRelationsHolder>> prepareEncumbranceRelationsHolder(CompositePurchaseOrder compPO,
      RequestContext requestContext) {
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return encumbranceRelationsHoldersBuilder.withBudgets(encumbranceRelationsHolders, requestContext)
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withLedgersData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withFiscalYearData(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withConversion(holders, requestContext))
      .thenCompose(holders -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, requestContext));
  }

  public CompletableFuture<List<Transaction>> getOrderEncumbrances(CompositePurchaseOrder compPo, RequestContext requestContext) {
    return prepareEncumbranceRelationsHolder(compPo, requestContext)
      .thenApply(ehList -> ehList.stream().collect(groupingBy(EncumbranceRelationsHolder::getCurrentFiscalYearId,
      mapping(EncumbranceRelationsHolder::getPoLine, toList()))))
      .thenCompose(poLinesByCurrentFy -> getEncumbrancesByPoLinesFromCurrentFy(poLinesByCurrentFy, requestContext))
      .thenApply(trs -> trs.stream().flatMap(Collection::stream).collect(Collectors.toList()));
  }

  public CompletableFuture<List<List<Transaction>>> getEncumbrancesByPoLinesFromCurrentFy(
    Map<String, List<CompositePoLine>> polinesByFy, RequestContext requestContext) {
    return collectResultsOnSuccess(polinesByFy.entrySet()
      .stream()
      .map(entry -> encumbranceService.getCurrentPoLinesEncumbrances(entry.getValue(), entry.getKey(), requestContext))
      .collect(toList()));
  }
}
