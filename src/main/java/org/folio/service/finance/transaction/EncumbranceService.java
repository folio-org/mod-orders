package org.folio.service.finance.transaction;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Tags;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.javamoney.moneta.function.MonetaryOperators;

import javax.money.MonetaryAmount;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;

import static java.util.Objects.requireNonNullElse;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_IS_INACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.rest.core.exceptions.ErrorCodes.ERROR_REMOVING_INVOICE_LINE_ENCUMBRANCES;
import static org.folio.rest.core.exceptions.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.rest.core.exceptions.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class EncumbranceService {

  private static final String ENCUMBRANCE_CRITERIA = "transactionType==Encumbrance";
  public static final String AND = " and ";
  public static final String FUND_CODE = "fundCode";
  public static final String EXPENSE_CLASS_NAME = "expenseClassName";
  private static final Logger logger = LogManager.getLogger();

  private final TransactionService transactionService;
  private final TransactionSummariesService transactionSummariesService;
  private final InvoiceLineService invoiceLineService;
  private final OrderInvoiceRelationService orderInvoiceRelationService;
  private final FiscalYearService fiscalYearService;

  public EncumbranceService(TransactionService transactionService,
                            TransactionSummariesService transactionSummariesService,
                            InvoiceLineService invoiceLineService,
                            OrderInvoiceRelationService orderInvoiceRelationService,
                            FiscalYearService fiscalYearService) {
    this.transactionService = transactionService;
    this.transactionSummariesService = transactionSummariesService;
    this.invoiceLineService = invoiceLineService;
    this.orderInvoiceRelationService = orderInvoiceRelationService;
    this.fiscalYearService = fiscalYearService;
  }


  public CompletableFuture<Void> createOrUpdateEncumbrances(EncumbrancesProcessingHolder holder, RequestContext requestContext) {

    if (holder.getAllEncumbrancesQuantity() == 0)
      return completedFuture(null);
    return transactionSummariesService.createOrUpdateOrderTransactionSummary(holder, requestContext)
        .thenCompose(v -> createEncumbrances(holder.getEncumbrancesForCreate(), requestContext))
        .thenCompose(v -> releaseEncumbrances(holder.getEncumbrancesForRelease(), requestContext))
        .thenCompose(v -> unreleaseEncumbrances(holder.getEncumbrancesForUnrelease(), requestContext))
        .thenCompose(v -> updateEncumbrances(holder, requestContext))
        .thenCompose(v -> deleteEncumbrances(holder.getEncumbrancesForDelete(), requestContext));
  }

  public CompletableFuture<Void> createEncumbrances(List<EncumbranceRelationsHolder> relationsHolders, RequestContext requestContext) {
    List<CompletableFuture<Void>> futureList = new ArrayList<>();
    CompletableFuture<Void> future = completedFuture(null);
    for (EncumbranceRelationsHolder holder : relationsHolders) {
      future = future.thenCompose(v -> transactionService.createTransaction(holder.getNewEncumbrance(), requestContext)
        .thenAccept(transaction -> holder.getFundDistribution().setEncumbrance(transaction.getId()))
        .exceptionally(fail -> {
          checkCustomTransactionError(fail);
          throw new CompletionException(fail);
        }));
      futureList.add(future);
    }
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), futureList.toArray(new CompletableFuture[0]));
  }

  public CompletionStage<Void> updateEncumbrancesOrderStatus(CompositePurchaseOrder compPo, RequestContext requestContext) {
    return getOrderUnreleasedEncumbrances(compPo.getId(), requestContext)
            .thenCompose(encumbrs -> {
              if (isEncumbrancesOrderStatusUpdateNeeded(compPo.getWorkflowStatus(), encumbrs)) {
                return transactionSummariesService.updateOrderTransactionSummary(compPo.getId(), encumbrs.size(), requestContext)
                        .thenApply(v -> {
                          syncEncumbrancesOrderStatus(compPo.getWorkflowStatus(), encumbrs);
                          return encumbrs;
                        })
                        .thenCompose(transactions -> transactionService.updateTransactions(transactions, requestContext));
              }
              return completedFuture(null);
            });
  }

  public CompletableFuture<Void> updateOrderTransactionSummary(CompositePoLine compOrderLine, List<Transaction> encumbrs, RequestContext requestContext) {
    return transactionSummariesService.updateOrderTransactionSummary(compOrderLine.getPurchaseOrderId(), encumbrs.size(), requestContext);
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
            .map(EncumbranceRelationsHolder::getNewEncumbrance)
            .collect(toList());
    return updateEncumbrances(encumbrances, requestContext);
  }

  public CompletableFuture<List<Transaction>> getOrderEncumbrances(String orderId, RequestContext requestContext) {
    return transactionService.getTransactions(buildEncumbrancesByOrderQuery(orderId), requestContext);
  }

  public CompletableFuture<List<Transaction>> getOrderUnreleasedEncumbrances(String orderId, RequestContext requestContext) {
    return transactionService.getTransactions(buildUnreleasedEncumbrancesByOrderQuery(orderId), requestContext);
  }

  public CompletableFuture<List<Transaction>> getOrderEncumbrancesToUnrelease(CompositePurchaseOrder compPO,
                                                                              Map<String, List<CompositePoLine>> mapFiscalYearWithCompPOLines,
                                                                              RequestContext requestContext) {
    // Check invoice line's releaseEncumbrance value for each order line
    List<CompletableFuture<List<Transaction>>> futures =
      compPO.getCompositePoLines()
        .stream()
        .map(poLine -> getPoLineEncumbrancesToUnrelease(poLine, mapFiscalYearWithCompPOLines, requestContext))
        .collect(toList());
    return collectResultsOnSuccess(futures)
      .thenApply(listOfLists -> listOfLists.stream().flatMap(List::stream).collect(toList()));
  }

  public CompletableFuture<List<Transaction>> getPoLineEncumbrances(String poLineId, RequestContext requestContext) {
    return transactionService.getTransactions(buildEncumbrancesByPoLineQuery(poLineId), requestContext);
  }

  public CompletableFuture<List<Transaction>> getPoLineUnreleasedEncumbrances(String poLineId, RequestContext requestContext) {
    return transactionService.getTransactions(buildUnreleasedEncumbrancesByPoLineQuery(poLineId), requestContext);
  }

  public CompletableFuture<List<Transaction>> getPoLineReleasedEncumbrances(CompositePoLine poLine, RequestContext requestContext) {
    String currentFiscalYear = poLine.getFundDistribution().stream()
      .findFirst()
      .map(FundDistribution::getFundId)
      .orElse("");

    return fiscalYearService.getCurrentFiscalYearByFundId(currentFiscalYear, requestContext)
      .thenCompose(fiscalYear -> transactionService.getTransactions(buildReleasedEncumbranceByPoLineQuery(poLine.getId(), fiscalYear.getId()), requestContext));
  }

  public CompletableFuture<List<Transaction>> getEncumbrancesByIds(List<String> transactionIds, RequestContext requestContext) {
    return transactionService.getTransactionsByIds(transactionIds, requestContext);
  }

  public CompletableFuture<List<Transaction>> getCurrentPoLinesEncumbrances(List<CompositePoLine> poLines, String fiscalYearId, RequestContext requestContext) {
    String searchCriteria = "fiscalYearId==" + fiscalYearId;
    List<String> poLineIds = poLines.stream().map(CompositePoLine::getId).collect(toList());
    return transactionService.getTransactionsByPoLinesIds(poLineIds, searchCriteria, requestContext);
  }

  public CompletableFuture<List<Transaction>> getEncumbrancesByPoLinesFromCurrentFy(
      Map<String, List<CompositePoLine>> poLinesByFy, RequestContext requestContext) {
    return collectResultsOnSuccess(poLinesByFy.entrySet().stream()
        .map(entry -> getCurrentPoLinesEncumbrances(entry.getValue(), entry.getKey(), requestContext))
        .collect(toList()))
      .thenApply(encumbrances -> encumbrances.stream().flatMap(Collection::stream).collect(toList()));
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

  public CompletableFuture<Void> releaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    encumbrances.forEach(transaction -> transaction.getEncumbrance().setStatus(Encumbrance.Status.RELEASED));
    return transactionService.updateTransactions(encumbrances, requestContext);
  }

  public CompletableFuture<Void> unreleaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    encumbrances.forEach(transaction -> transaction.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED));
    return transactionService.updateTransactions(encumbrances, requestContext);
  }

  public CompletableFuture<Void> updateEncumbrances(List<Transaction> transactions, RequestContext requestContext) {
    return transactionService.updateTransactions(transactions, requestContext);
  }

  public CompletableFuture<Void> deleteEncumbrances(List<EncumbranceRelationsHolder> holders, RequestContext requestContext) {
    // before deleting encumbrances, set the fund distribution encumbrance to null if a new encumbrance has not been created
    // (as with pending->pending)
    holders.stream()
      .filter(erh -> erh.getFundDistribution() != null &&
        erh.getOldEncumbrance().getId().equals(erh.getFundDistribution().getEncumbrance()))
      .forEach(erh -> erh.getFundDistribution().setEncumbrance(null));
    List<Transaction> transactions = holders.stream().map(EncumbranceRelationsHolder::getOldEncumbrance).collect(toList());
    if (transactions.size() == 0)
      return completedFuture(null);
    return transactionService.deleteTransactions(transactions, requestContext)
      .thenCompose(v -> deleteEncumbranceLinksInInvoiceLines(transactions, requestContext));
  }

  public CompletableFuture<Void> deletePoLineEncumbrances(PoLine poline, RequestContext requestContext) {
    return getPoLineEncumbrances(poline.getId(), requestContext)
      .thenCompose(encumbrances -> transactionSummariesService.updateOrderTransactionSummary(poline.getPurchaseOrderId(),
          encumbrances.size(), requestContext)
        .thenCompose(v -> releaseEncumbrances(encumbrances, requestContext))
        .thenCompose(ok -> transactionService.deleteTransactions(encumbrances, requestContext)));
  }

  public CompletableFuture<Void> deleteOrderEncumbrances(String orderId, RequestContext requestContext) {
    return getOrderEncumbrances(orderId, requestContext)
      .thenCompose(encumbrances -> transactionSummariesService.updateOrderTransactionSummary(orderId, encumbrances.size(), requestContext)
        .thenCompose(v -> releaseEncumbrances(encumbrances, requestContext))
        .thenCompose(v -> transactionService.deleteTransactions(encumbrances, requestContext)));
  }

  public static double calculateAmountEncumbered(FundDistribution distribution, MonetaryAmount estimatedPrice) {
    if (distribution.getDistributionType() == FundDistribution.DistributionType.PERCENTAGE) {
      return estimatedPrice.with(MonetaryOperators.percent(distribution.getValue()))
        .with(MonetaryOperators.rounding())
        .getNumber()
        .doubleValue();
    }
    return distribution.getValue();
  }

  public CompletableFuture<Void> deleteEncumbranceLinksInInvoiceLines(List<Transaction> transactions,
      RequestContext requestContext) {
    String orderId = transactions.get(0).getEncumbrance().getSourcePurchaseOrderId();
    return orderInvoiceRelationService.isOrderLinkedToAnInvoice(orderId, requestContext)
      .thenCompose(linked -> {
        if (!linked)
          return completedFuture(null);
        List<String> poLineIds = transactions.stream()
          .map(tr -> tr.getEncumbrance().getSourcePoLineId())
          .distinct()
          .collect(toList());
        List<String> transactionIds = transactions.stream().map(Transaction::getId).collect(toList());
        return invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext)
          .thenCompose(invoiceLines -> invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds,
            requestContext));
      })
      .exceptionally(t -> {
        Throwable cause = requireNonNullElse(t.getCause(), t);
        String message = String.format(ERROR_REMOVING_INVOICE_LINE_ENCUMBRANCES.getDescription(), cause.getMessage());
        logger.error(message);
        throw new HttpException(500, message);
      });
  }

  private String buildEncumbrancesByOrderQuery(String orderId) {
    return ENCUMBRANCE_CRITERIA
      + AND + "encumbrance.sourcePurchaseOrderId==" + orderId;
  }

  private String buildUnreleasedEncumbrancesByOrderQuery(String orderId) {
    return ENCUMBRANCE_CRITERIA
                + AND + "encumbrance.sourcePurchaseOrderId==" + orderId
                    + AND + "encumbrance.status <> " + Encumbrance.Status.RELEASED;
  }

  private String buildEncumbrancesByPoLineQuery(String polineId) {
    return ENCUMBRANCE_CRITERIA + AND + "encumbrance.sourcePoLineId==" + polineId;
  }

  private String buildUnreleasedEncumbrancesByPoLineQuery(String polineId) {
    return ENCUMBRANCE_CRITERIA
      + AND + "encumbrance.sourcePoLineId==" + polineId
      + AND + "encumbrance.status <> " + Encumbrance.Status.RELEASED;
  }

  private String buildReleasedEncumbranceByPoLineQuery(String poLineId, String fiscalYearId) {
    return ENCUMBRANCE_CRITERIA
      + AND + "encumbrance.sourcePoLineId == " + poLineId
      + AND + "encumbrance.status == " + Encumbrance.Status.RELEASED
      + AND + "fiscalYearId == " + fiscalYearId;
  }

  private CompletableFuture<List<Transaction>> getPoLineEncumbrancesToUnrelease(CompositePoLine poLine,
                                                                                Map<String, List<CompositePoLine>> mapFiscalYearWithCompPOLines,
                                                                                RequestContext requestContext) {
    final String[] currentFiscalYearId = new String[1];
    for (Map.Entry<String, List<CompositePoLine>> entry : mapFiscalYearWithCompPOLines.entrySet()) {
      for (CompositePoLine pLine : entry.getValue()) {
        if (poLine.getId().equals(pLine.getId())) {
          currentFiscalYearId[0] = entry.getKey();
          break;
        }
      }
    }
    if (currentFiscalYearId[0] == null) {
        Error error = ErrorCodes.CURRENT_FISCAL_YEAR_ID_NOT_FOUND.toError();
        List<Parameter> parameters = Collections.singletonList(new Parameter().withKey("poLineNumber")
                                           .withValue(poLine.getPoLineNumber()));
      throw new HttpException(404, error.withParameters(parameters));
    }
    return hasNotInvoiceLineWithReleaseEncumbrance(poLine, requestContext)
      .thenCompose(hasNotInvoiceLineWithReleaseEncumbrance -> {
        if (hasNotInvoiceLineWithReleaseEncumbrance)
          return completedFuture(Collections.emptyList());
        return transactionService.getTransactions(
          buildReleasedEncumbranceByPoLineQuery(poLine.getId(), currentFiscalYearId[0]), requestContext);
      });
  }

  private CompletableFuture<Boolean> hasNotInvoiceLineWithReleaseEncumbrance(CompositePoLine poLine, RequestContext requestContext) {
    return invoiceLineService.retrieveInvoiceLines("poLineId == " + poLine.getId() + AND + "releaseEncumbrance == true", requestContext)
      .thenApply(invoiceLines -> !invoiceLines.isEmpty())
      .exceptionally(t -> {
        Throwable cause = t.getCause();
        // ignore 404 errors as mod-invoice may be unavailable
        if (cause instanceof HttpException && ((HttpException)cause).getCode() == HttpStatus.HTTP_NOT_FOUND.toInt())
          return false;
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
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
