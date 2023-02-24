package org.folio.service.finance.transaction;

import static java.util.Objects.requireNonNullElse;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.rest.RestConstants.SEMAPHORE_MAX_ACTIVE_THREADS;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_IS_INACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.rest.core.exceptions.ErrorCodes.ERROR_REMOVING_INVOICE_LINE_ENCUMBRANCES;
import static org.folio.rest.core.exceptions.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.rest.core.exceptions.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletionException;

import javax.money.MonetaryAmount;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Tags;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.summary.OrderTransactionSummariesService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Future;
import io.vertxconcurrent.Semaphore;

public class EncumbranceService {

  private static final String ENCUMBRANCE_CRITERIA = "transactionType==Encumbrance";
  public static final String AND = " and ";
  public static final String FUND_CODE = "fundCode";
  public static final String EXPENSE_CLASS_NAME = "expenseClassName";
  private static final Logger logger = LogManager.getLogger();

  private final TransactionService transactionService;
  private final OrderTransactionSummariesService orderTransactionSummariesService;
  private final InvoiceLineService invoiceLineService;
  private final OrderInvoiceRelationService orderInvoiceRelationService;
  private final FiscalYearService fiscalYearService;


  public EncumbranceService(TransactionService transactionService,
                            OrderTransactionSummariesService orderTransactionSummariesService,
                            InvoiceLineService invoiceLineService,
                            OrderInvoiceRelationService orderInvoiceRelationService,
                            FiscalYearService fiscalYearService) {
    this.transactionService = transactionService;
    this.orderTransactionSummariesService = orderTransactionSummariesService;
    this.invoiceLineService = invoiceLineService;
    this.orderInvoiceRelationService = orderInvoiceRelationService;
    this.fiscalYearService = fiscalYearService;
  }


  public Future<Void> createOrUpdateEncumbrances(EncumbrancesProcessingHolder holder, RequestContext requestContext) {

    if (holder.getAllEncumbrancesQuantity() == 0)
      return Future.succeededFuture();
    return unreleaseEncumbrancesFirst(holder, requestContext)
        .compose(v -> orderTransactionSummariesService.createOrUpdateOrderTransactionSummary(holder, requestContext))
        .compose(v -> createEncumbrances(holder.getEncumbrancesForCreate(), requestContext))
        .compose(v -> releaseEncumbrances(holder.getEncumbrancesForRelease(), requestContext))
        .compose(v -> unreleaseEncumbrances(holder.getEncumbrancesForUnrelease(), requestContext))
        .compose(v -> updateEncumbrances(holder, requestContext))
        .compose(v -> deleteEncumbrances(holder.getEncumbrancesForDelete(), requestContext))
        .compose(v -> releaseEncumbrancesAfter(holder, requestContext));
  }

  public Future<Void> createEncumbrances(List<EncumbranceRelationsHolder> relationsHolders, RequestContext requestContext) {
    if (relationsHolders.isEmpty())
      return Future.succeededFuture();

    Semaphore semaphore = new Semaphore(SEMAPHORE_MAX_ACTIVE_THREADS, requestContext.getContext().owner());

    return requestContext.getContext().owner()
      .<List<Future<Transaction>>>executeBlocking(event -> {

        List<Future<Transaction>> futures = new ArrayList<>();

        for (EncumbranceRelationsHolder holder : relationsHolders) {
          Future<Transaction> createTransactionFuture = transactionService.createTransaction(holder.getNewEncumbrance(), requestContext)
            .map(transaction -> {
              holder.getFundDistribution().setEncumbrance(transaction.getId());
              return null;
            });
          var future = createTransactionFuture.otherwise(fail -> {
            checkCustomTransactionError(fail);
            return null;
          });

          futures.add(future);
          semaphore.acquire(() -> future
            .onComplete(asyncResult -> semaphore.release()));
        }
        event.complete(futures);
      })
      .compose(futures -> GenericCompositeFuture.all(new ArrayList<>(futures))
        .mapEmpty());
  }


  public Future<Void> updateEncumbrancesOrderStatus(CompositePurchaseOrder compPo, RequestContext requestContext) {
    return getOrderUnreleasedEncumbrances(compPo.getId(), requestContext).compose(encumbrs -> {
      if (isEncumbrancesOrderStatusUpdateNeeded(compPo.getWorkflowStatus(), encumbrs)) {
        return orderTransactionSummariesService.updateTransactionSummary(compPo.getId(), encumbrs.size(), requestContext)
          .map(v -> {
            syncEncumbrancesOrderStatus(compPo.getWorkflowStatus(), encumbrs);
            return encumbrs;
          })
          .compose(transactions -> transactionService.updateTransactions(transactions, requestContext));
      }
      return Future.succeededFuture();
    });
  }

  public Future<Void> updateOrderTransactionSummary(CompositePoLine compOrderLine, List<Transaction> encumbrs, RequestContext requestContext) {
    return orderTransactionSummariesService.updateTransactionSummary(compOrderLine.getPurchaseOrderId(), encumbrs.size(), requestContext);
  }

  private void syncEncumbrancesOrderStatus(CompositePurchaseOrder.WorkflowStatus workflowStatus,
                                           List<Transaction> encumbrances) {
    Encumbrance.OrderStatus orderStatus = Encumbrance.OrderStatus.fromValue(workflowStatus.value());
    encumbrances.forEach(encumbrance -> encumbrance.getEncumbrance().setOrderStatus(orderStatus));
  }


  private boolean isEncumbrancesOrderStatusUpdateNeeded(CompositePurchaseOrder.WorkflowStatus orderStatus, List<Transaction> encumbrs) {
    return !CollectionUtils.isEmpty(encumbrs) && !orderStatus.value().equals(encumbrs.get(0).getEncumbrance().getOrderStatus().value());
  }


  private Future<Void> updateEncumbrances(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
    if (holder.getEncumbrancesForUpdate().isEmpty())
      return Future.succeededFuture();

    List<Transaction> encumbrances = holder.getEncumbrancesForUpdate().stream()
            .map(EncumbranceRelationsHolder::getNewEncumbrance)
            .collect(toList());
    return updateEncumbrances(encumbrances, requestContext);
  }

  public Future<List<Transaction>> getOrderEncumbrances(String orderId, RequestContext requestContext) {
    return transactionService.getTransactions(buildEncumbrancesByOrderQuery(orderId), requestContext);
  }

  public Future<List<Transaction>> getOrderUnreleasedEncumbrances(String orderId, RequestContext requestContext) {
    return transactionService.getTransactions(buildUnreleasedEncumbrancesByOrderQuery(orderId), requestContext);
  }

  public Future<List<Transaction>> getOrderEncumbrancesToUnrelease(CompositePurchaseOrder compPO,
                                                                              Map<String, List<CompositePoLine>> mapFiscalYearWithCompPOLines,
                                                                              RequestContext requestContext) {
    // Check invoice line's releaseEncumbrance value for each order line
    List<Future<List<Transaction>>> futures =
      compPO.getCompositePoLines()
          .stream()
          .filter(poLines -> CollectionUtils.isNotEmpty(poLines.getFundDistribution()))
        .map(poLine -> getPoLineEncumbrancesToUnrelease(compPO.getOrderType(), poLine, mapFiscalYearWithCompPOLines, requestContext))
        .collect(toList());
    return collectResultsOnSuccess(futures)
      .map(listOfLists -> listOfLists.stream().flatMap(List::stream).collect(toList()));
  }

  public Future<List<Transaction>> getPoLineEncumbrances(String poLineId, RequestContext requestContext) {
    return transactionService.getTransactions(buildEncumbrancesByPoLineQuery(poLineId), requestContext);
  }

  public Future<List<Transaction>> getPoLineUnreleasedEncumbrances(String poLineId, RequestContext requestContext) {
    return transactionService.getTransactions(buildUnreleasedEncumbrancesByPoLineQuery(poLineId), requestContext);
  }

  public Future<List<Transaction>> getPoLineReleasedEncumbrances(CompositePoLine poLine, RequestContext requestContext) {
    String currentFiscalYear = poLine.getFundDistribution().stream()
      .findFirst()
      .map(FundDistribution::getFundId)
      .orElse("");

    return fiscalYearService.getCurrentFiscalYearByFundId(currentFiscalYear, requestContext)
      .compose(fiscalYear -> transactionService.getTransactions(buildReleasedEncumbranceByPoLineQuery(poLine.getId(), fiscalYear.getId()), requestContext));
  }

  public Future<List<Transaction>> getEncumbrancesByIds(List<String> transactionIds, RequestContext requestContext) {
    return transactionService.getTransactionsByIds(transactionIds, requestContext);
  }

  public Future<List<Transaction>> getCurrentPoLinesEncumbrances(List<CompositePoLine> poLines, String fiscalYearId, RequestContext requestContext) {
    String searchCriteria = "fiscalYearId==" + fiscalYearId;
    List<String> poLineIds = poLines.stream().map(CompositePoLine::getId).collect(toList());
    return transactionService.getTransactionsByPoLinesIds(poLineIds, searchCriteria, requestContext);
  }

  public Future<List<Transaction>> getEncumbrancesByPoLinesFromCurrentFy(Map<String, List<CompositePoLine>> poLinesByFy, RequestContext requestContext) {
    return collectResultsOnSuccess(poLinesByFy.entrySet().stream()
        .map(entry -> getCurrentPoLinesEncumbrances(entry.getValue(), entry.getKey(), requestContext))
        .collect(toList()))
      .map(encumbrances -> encumbrances.stream().flatMap(Collection::stream).collect(toList()));
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

  public Future<Void> releaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    if (encumbrances.isEmpty())
      return Future.succeededFuture();

    encumbrances.forEach(transaction -> transaction.getEncumbrance().setStatus(Encumbrance.Status.RELEASED));
    return transactionService.updateTransactions(encumbrances,requestContext );
  }

  public Future<Void> unreleaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    if (encumbrances.isEmpty())
      return Future.succeededFuture();

    encumbrances.forEach(transaction -> transaction.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED));
    return transactionService.updateTransactions(encumbrances, requestContext);
  }

  public Future<Void> updateEncumbrances(List<Transaction> transactions, RequestContext requestContext) {
    return transactionService.updateTransactions(transactions, requestContext);
  }

  public Future<Void> deleteEncumbrances(List<EncumbranceRelationsHolder> holders, RequestContext requestContext) {
    if (holders.isEmpty())
      return Future.succeededFuture();

    // before deleting encumbrances, set the fund distribution encumbrance to null if a new encumbrance has not been created
    // (as with pending->pending)
    holders.stream()
      .filter(erh -> erh.getFundDistribution() != null && erh.getOldEncumbrance().getId().equals(erh.getFundDistribution().getEncumbrance()))
      .forEach(erh -> erh.getFundDistribution().setEncumbrance(null));
    List<Transaction> transactions = holders.stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .collect(toList());
    if (transactions.isEmpty()) {
      return Future.succeededFuture();
    } else {
      return transactionService.deleteTransactions(transactions, requestContext)
        .compose(v -> deleteEncumbranceLinksInInvoiceLines(transactions, requestContext));
    }
  }

  public Future<Void> deletePoLineEncumbrances(PoLine poline, RequestContext requestContext) {
    return getPoLineEncumbrances(poline.getId(), requestContext)
      .compose(encumbrances -> orderTransactionSummariesService.updateTransactionSummary(poline.getPurchaseOrderId(), encumbrances.size(), requestContext)
        .compose(v -> releaseEncumbrances(encumbrances, requestContext))
        .compose(ok -> transactionService.deleteTransactions(encumbrances, requestContext)));
  }

  public Future<Void> deleteOrderEncumbrances(String orderId, RequestContext requestContext) {
    return getOrderEncumbrances(orderId, requestContext)
      .compose(encumbrances -> orderTransactionSummariesService.updateTransactionSummary(orderId, encumbrances.size(), requestContext)
        .compose(v -> releaseEncumbrances(encumbrances, requestContext))
        .compose(v -> transactionService.deleteTransactions(encumbrances, requestContext)));
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

  public Future<Void> deleteEncumbranceLinksInInvoiceLines(List<Transaction> transactions, RequestContext requestContext) {
    String orderId = transactions.get(0).getEncumbrance().getSourcePurchaseOrderId();
    return orderInvoiceRelationService.isOrderLinkedToAnInvoice(orderId, requestContext)
      .compose(linked -> {
        if (!linked)
          return Future.succeededFuture();
        List<String> poLineIds = transactions.stream()
          .map(tr -> tr.getEncumbrance().getSourcePoLineId())
          .distinct()
          .collect(toList());
        List<String> transactionIds = transactions.stream().map(Transaction::getId).collect(toList());
        return invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext)
          .compose(invoiceLines -> invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds, requestContext));
      })
       .recover(t -> {
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

  private Future<List<Transaction>> getPoLineEncumbrancesToUnrelease(CompositePurchaseOrder.OrderType orderType,
      CompositePoLine poLine, Map<String, List<CompositePoLine>> mapFiscalYearWithCompPOLines, RequestContext requestContext) {

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
    if (orderType == CompositePurchaseOrder.OrderType.ONE_TIME) {
      return hasNotInvoiceLineWithReleaseEncumbrance(poLine, requestContext).compose(hasNotInvoiceLineWithReleaseEncumbrance -> {
        if (hasNotInvoiceLineWithReleaseEncumbrance) {
          return Future.succeededFuture(Collections.emptyList());
        }
        return transactionService.getTransactions(buildReleasedEncumbranceByPoLineQuery(poLine.getId(), currentFiscalYearId[0]), requestContext);
      });
    }
    return transactionService.getTransactions(buildReleasedEncumbranceByPoLineQuery(poLine.getId(), currentFiscalYearId[0]), requestContext);
  }

  private Future<Boolean> hasNotInvoiceLineWithReleaseEncumbrance(CompositePoLine poLine, RequestContext requestContext) {
    return invoiceLineService.retrieveInvoiceLines("poLineId == " + poLine.getId() + AND + "releaseEncumbrance == true", requestContext)
      .map(invoiceLines -> !invoiceLines.isEmpty())
       .otherwise(t -> {
        Throwable cause = t.getCause();
        // ignore 404 errors as mod-invoice may be unavailable
        if (cause instanceof HttpException && ((HttpException)cause).getCode() == HttpStatus.HTTP_NOT_FOUND.toInt())
          return false;
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
  }

  protected void checkCustomTransactionError(Throwable fail) throws RuntimeException {
    if (fail.getMessage().contains(BUDGET_NOT_FOUND_FOR_TRANSACTION.getDescription())) {
      throw new HttpException(422, BUDGET_NOT_FOUND_FOR_TRANSACTION);
    } else if (fail.getMessage().contains(LEDGER_NOT_FOUND_FOR_TRANSACTION.getDescription())) {
      throw new HttpException(422, LEDGER_NOT_FOUND_FOR_TRANSACTION);
    } else if (fail.getMessage().contains(BUDGET_IS_INACTIVE.getDescription())) {
      throw new HttpException(422, BUDGET_IS_INACTIVE);
    } else if (fail.getMessage().contains(FUND_CANNOT_BE_PAID.getDescription())) {
      throw new HttpException(422, FUND_CANNOT_BE_PAID);
    } else {
      throw new CompletionException(fail);
    }
  }

  private Future<Void> unreleaseEncumbrancesFirst(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
    // Update the encumbrances to be updated after unreleasing them
    List<Transaction> encumbrances = holder.getEncumbrancesToUnreleaseBefore();
    if (encumbrances.isEmpty())
      return Future.succeededFuture();
    String orderId = encumbrances.get(0).getEncumbrance().getSourcePurchaseOrderId();
    List<String> encumbranceIds = encumbrances.stream().map(Transaction::getId).collect(toList());
    return orderTransactionSummariesService.updateOrCreateTransactionSummary(orderId, encumbrances.size(), requestContext)
      .compose(v -> unreleaseEncumbrances(encumbrances, requestContext))
      .compose(v -> transactionService.getTransactionsByIds(encumbranceIds, requestContext))
      .map(newEncumbrances -> {
        holder.getEncumbrancesForUpdate().forEach(h -> {
          String id = h.getOldEncumbrance().getId();
          Transaction newT = h.getNewEncumbrance();
          newEncumbrances.stream().filter(tr -> tr.getId().equals(id)).findFirst().ifPresent(matching -> {
            h.withOldEncumbrance(matching);
            newT.setVersion(matching.getVersion());
            newT.getEncumbrance().setStatus(matching.getEncumbrance().getStatus());
          });
        });
        return null;
      });
  }

  private Future<Void> releaseEncumbrancesAfter(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
    // Get the updated version of the encumbrances before releasing them.
    List<Transaction> encumbrances = holder.getEncumbrancesToUnreleaseAfter();
    if (encumbrances.isEmpty())
      return Future.succeededFuture();
    String orderId = encumbrances.get(0).getEncumbrance().getSourcePurchaseOrderId();
    List<String> encumbranceIds = encumbrances.stream().map(Transaction::getId).collect(toList());
    return transactionService.getTransactionsByIds(encumbranceIds, requestContext)
      .compose(updatedEncumbrances ->
        orderTransactionSummariesService.updateOrCreateTransactionSummary(orderId, updatedEncumbrances.size(),
            requestContext)
          .compose(v -> releaseEncumbrances(updatedEncumbrances, requestContext))
      );
  }
}
