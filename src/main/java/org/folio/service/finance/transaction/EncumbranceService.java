package org.folio.service.finance.transaction;

import static java.util.Objects.requireNonNullElse;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
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
import java.util.UUID;
import java.util.concurrent.CompletionException;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionPatch;
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
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.orders.OrderInvoiceRelationService;

import io.vertx.core.Future;

public class EncumbranceService {

  private static final String ENCUMBRANCE_CRITERIA = "transactionType==Encumbrance";
  public static final String AND = " and ";
  public static final String FUND_CODE = "fundCode";
  public static final String EXPENSE_CLASS_NAME = "expenseClassName";
  private static final Logger logger = LogManager.getLogger();

  private final TransactionService transactionService;
  private final InvoiceLineService invoiceLineService;
  private final OrderInvoiceRelationService orderInvoiceRelationService;
  private final FiscalYearService fiscalYearService;


  public EncumbranceService(TransactionService transactionService,
                            InvoiceLineService invoiceLineService,
                            OrderInvoiceRelationService orderInvoiceRelationService,
                            FiscalYearService fiscalYearService) {
    this.transactionService = transactionService;
    this.invoiceLineService = invoiceLineService;
    this.orderInvoiceRelationService = orderInvoiceRelationService;
    this.fiscalYearService = fiscalYearService;
  }


  public Future<Void> createOrUpdateEncumbrances(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
    if (holder.isEmpty()) {
      return Future.succeededFuture();
    }
    List<Transaction> transactionsToCreate = prepareTransactionsToCreate(holder.getEncumbrancesForCreate());
    List<Transaction> transactionsToUpdate = prepareTransactionsToUpdate(holder);
    transactionsToUpdate.addAll(holder.getPendingPaymentsToUpdate());
    List<Transaction> transactionsToDelete = prepareTransactionsToDelete(holder.getEncumbrancesForDelete());
    List<String> idsOfTransactionsToDelete =  transactionsToDelete.stream().map(Transaction::getId).toList();
    List<TransactionPatch> transactionPatches = Collections.emptyList();
    return transactionService.batchAllOrNothing(transactionsToCreate, transactionsToUpdate, idsOfTransactionsToDelete,
        transactionPatches, requestContext)
      .recover(t -> {
        try {
          checkCustomTransactionError(t);
        } catch(Exception ex) {
          return Future.failedFuture(ex);
        }
        return Future.failedFuture(t);
      })
      .compose(v -> {
        if (transactionsToDelete.isEmpty()) {
          return Future.succeededFuture();
        }
        return deleteEncumbranceLinksInInvoiceLines(transactionsToDelete, requestContext);
      });
  }

  private List<Transaction> prepareTransactionsToCreate(List<EncumbranceRelationsHolder> relationsHolders) {
    if (CollectionUtils.isEmpty(relationsHolders)) {
      return Collections.emptyList();
    }
    relationsHolders.forEach(holder -> {
      Transaction encumbrance = holder.getNewEncumbrance();
      if (encumbrance.getId() == null) {
        encumbrance.setId(UUID.randomUUID().toString());
      }
      holder.getFundDistribution().setEncumbrance(encumbrance.getId());
    });
    return relationsHolders.stream().map(EncumbranceRelationsHolder::getNewEncumbrance).toList();
  }

  private List<Transaction> prepareTransactionsToUpdate(EncumbrancesProcessingHolder holder) {
    ArrayList<Transaction> encumbrances = new ArrayList<>(
      holder.getEncumbrancesForUpdate().stream()
        .map(EncumbranceRelationsHolder::getNewEncumbrance)
        .toList());
    holder.getEncumbrancesForRelease().forEach(encToRelease -> {
      if (encumbrances.stream().noneMatch(enc -> enc.getId().equals(encToRelease.getId()))) {
        encToRelease.getEncumbrance().setStatus(Encumbrance.Status.RELEASED);
        encumbrances.add(encToRelease);
      } else {
        encumbrances.stream()
          .filter(enc -> enc.getId().equals(encToRelease.getId()))
          .forEach(enc -> enc.getEncumbrance().setStatus(Encumbrance.Status.RELEASED));
      }
    });
    holder.getEncumbrancesForUnrelease().forEach(encToUnrelease -> {
      if (encumbrances.stream().noneMatch(enc -> enc.getId().equals(encToUnrelease.getId()))) {
        encToUnrelease.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED);
        encumbrances.add(encToUnrelease);
      } else {
        encumbrances.stream()
          .filter(enc -> enc.getId().equals(encToUnrelease.getId()))
          .forEach(enc -> enc.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED));
      }
    });
    return encumbrances;
  }

  private List<Transaction> prepareTransactionsToDelete(List<EncumbranceRelationsHolder> holders) {
    // before deleting encumbrances, set the fund distribution encumbrance to null if a new encumbrance has not been created
    // (as with pending->pending)
    holders.stream()
      .filter(erh -> erh.getFundDistribution() != null && erh.getOldEncumbrance().getId().equals(erh.getFundDistribution().getEncumbrance()))
      .forEach(erh -> erh.getFundDistribution().setEncumbrance(null));
    return holders.stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .toList();
  }

  public Future<Void> updateEncumbrancesOrderStatusAndReleaseIfClosed(CompositePurchaseOrder compPo, RequestContext requestContext) {
    logger.info("updateEncumbrancesOrderStatusAndReleaseIfClosed:: orderId {}  ", compPo.getId());
    return getOrderUnreleasedEncumbrances(compPo.getId(), requestContext).compose(encumbrs -> {
      if (isEncumbrancesOrderStatusUpdateNeeded(compPo.getWorkflowStatus(), encumbrs)) {
        syncEncumbrancesOrderStatusAndReleaseIfClosed(compPo.getWorkflowStatus(), encumbrs);
        // NOTE: we will have to use transactionPatches when it is available (see MODORDERS-1008)
        return transactionService.batchUpdate(encumbrs, requestContext);
      }
      return Future.succeededFuture();
    });
  }

  private void syncEncumbrancesOrderStatusAndReleaseIfClosed(CompositePurchaseOrder.WorkflowStatus workflowStatus,
      List<Transaction> encumbrances) {
    Encumbrance.OrderStatus orderStatus = Encumbrance.OrderStatus.fromValue(workflowStatus.value());
    encumbrances.forEach(encumbrance -> {
      encumbrance.getEncumbrance().setOrderStatus(orderStatus);
      if (orderStatus == Encumbrance.OrderStatus.CLOSED)
        encumbrance.getEncumbrance().setStatus(Encumbrance.Status.RELEASED);
    });
  }


  private boolean isEncumbrancesOrderStatusUpdateNeeded(CompositePurchaseOrder.WorkflowStatus orderStatus, List<Transaction> encumbrs) {
    return !CollectionUtils.isEmpty(encumbrs) && !orderStatus.value().equals(encumbrs.get(0).getEncumbrance().getOrderStatus().value());
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
      .map(listOfLists -> listOfLists.stream()
        .flatMap(List::stream)
        .collect(toList()));
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

  public Future<Void> releaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    if (encumbrances.isEmpty()) {
      return Future.succeededFuture();
    }
    return transactionService.batchRelease(encumbrances, requestContext);
  }

  public Future<Void> unreleaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    if (encumbrances.isEmpty()) {
      return Future.succeededFuture();
    }
    return transactionService.batchUnrelease(encumbrances, requestContext);
  }

  public Future<Void> updateEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    if (encumbrances.isEmpty()) {
      return Future.succeededFuture();
    }
    return transactionService.batchUpdate(encumbrances, requestContext);
  }

  public Future<Void> deletePoLineEncumbrances(PoLine poline, RequestContext requestContext) {
    return getPoLineEncumbrances(poline.getId(), requestContext)
      .compose(encumbrances -> {
        if (encumbrances.isEmpty()) {
          return Future.succeededFuture();
        }
        return transactionService.batchReleaseAndDelete(encumbrances, requestContext);
      });
  }

  public Future<Void> deleteOrderEncumbrances(String orderId, RequestContext requestContext) {
    return getOrderEncumbrances(orderId, requestContext)
      .compose(encumbrances -> {
        if (encumbrances.isEmpty()) {
          return Future.succeededFuture();
        }
        return transactionService.batchReleaseAndDelete(encumbrances, requestContext);
      });
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
      .recover(cause -> {
        // ignore 404 errors as mod-invoice may be unavailable
        if (cause instanceof HttpException && ((HttpException)cause).getCode() == HttpStatus.HTTP_NOT_FOUND.toInt())
          return Future.succeededFuture(false);

        return Future.failedFuture(cause);
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

}
