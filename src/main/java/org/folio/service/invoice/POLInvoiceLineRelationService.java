package org.folio.service.invoice;

import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.orders.utils.validators.TransactionValidator;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;

import io.vertx.core.Future;
import org.folio.service.finance.transaction.PendingPaymentService;

import javax.money.CurrencyUnit;
import javax.money.Monetary;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static java.util.Objects.nonNull;
import static org.folio.orders.utils.InvoiceUtil.filterInvoiceLinesByStatuses;
import static org.folio.rest.acq.model.finance.Encumbrance.Status.RELEASED;
import static org.folio.rest.acq.model.finance.Encumbrance.Status.UNRELEASED;
import static org.folio.service.finance.transaction.FinanceUtils.calculateEncumbranceEffectiveAmount;
import static org.folio.service.finance.transaction.FinanceUtils.sumAmounts;

@Log4j2
public class POLInvoiceLineRelationService {

  private final InvoiceLineService invoiceLineService;
  private final PendingPaymentService pendingPaymentService;

  public POLInvoiceLineRelationService(InvoiceLineService invoiceLineService, PendingPaymentService pendingPaymentService) {
    this.invoiceLineService = invoiceLineService;
    this.pendingPaymentService = pendingPaymentService;
  }

  public Future<EncumbrancesProcessingHolder> manageInvoiceRelation(EncumbrancesProcessingHolder processingHolder,
                                                                    RequestContext requestContext) {
    List<Transaction> forDelete = processingHolder.getEncumbrancesForDelete().stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .toList();
    List<Transaction> forCreate = processingHolder.getEncumbrancesForCreate().stream()
      .map(EncumbranceRelationsHolder::getNewEncumbrance)
      .toList();
    List<String> poLineIds = forDelete.stream()
      .map(transaction -> transaction.getEncumbrance().getSourcePoLineId())
      .distinct()
      .toList();
    return invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext)
      .compose(invoiceLines -> {
        validateInvoiceLineStatuses(invoiceLines);
        if (forCreate.isEmpty() && forDelete.isEmpty()) {
          return Future.succeededFuture(processingHolder);
        }
        if (!forCreate.isEmpty() && !forDelete.isEmpty()) {
          CurrencyUnit currency = getCurrency(processingHolder);
          copyAmountsAndRecalculateNewEncumbrance(forCreate, forDelete, invoiceLines, currency);
        }
        if (forDelete.isEmpty()) {
          return Future.succeededFuture(processingHolder);
        }
        if (forCreate.isEmpty()) {
          forDelete.forEach(TransactionValidator::validateEncumbranceForDeletion);
        }
        List<String> encumbranceForDeleteIds = forDelete.stream()
          .map(Transaction::getId)
          .distinct()
          .toList();
        return removeEncumbranceReferenceFromTransactions(encumbranceForDeleteIds, processingHolder, requestContext);
      });
  }

  private void validateInvoiceLineStatuses(List<InvoiceLine> invoiceLines) {
    List<InvoiceLine> approvedInvoices = filterInvoiceLinesByStatuses(invoiceLines, List.of(InvoiceLine.InvoiceLineStatus.APPROVED));
    if (CollectionUtils.isNotEmpty(approvedInvoices)) {
      String invoiceLineIds = approvedInvoices.stream()
        .map(InvoiceLine::getId)
        .collect(Collectors.joining(", "));
      Error error = new Error()
        .withCode(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getCode())
        .withMessage(String.format(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getDescription(), invoiceLineIds));
      throw new HttpException(HttpStatus.SC_FORBIDDEN, error);
    }
  }

  private CurrencyUnit getCurrency(EncumbrancesProcessingHolder processingHolder) {
    return processingHolder.getEncumbrancesForCreate().stream()
      .map(EncumbranceRelationsHolder::getCurrency)
      .filter(Objects::nonNull)
      .map(Monetary::getCurrency)
      .findFirst()
      .orElseThrow();
  }

  private void copyAmountsAndRecalculateNewEncumbrance(List<Transaction> forCreate, List<Transaction> forDelete,
                                                       List<InvoiceLine> invoiceLines, CurrencyUnit currency) {
    // Update encumbrances when an order line is linked to a paid invoice
    var awaitingPayment = sumAmounts(forDelete, Encumbrance::getAmountAwaitingPayment);
    var expended = sumAmounts(forDelete, Encumbrance::getAmountExpended);
    var credited = sumAmounts(forDelete, Encumbrance::getAmountCredited);
    var isReleaseEncumbrance = extractReleaseEncumbrance(invoiceLines);
    forCreate.stream().findFirst()
      .ifPresent(transaction -> {
        var oldTransaction = getOldTransaction(forDelete, transaction);
        var data = getNewEncumbranceData(isReleaseEncumbrance, oldTransaction, transaction, expended, credited, awaitingPayment, currency);
        log.info("copyAmountsAndRecalculateNewEncumbrance:: New transaction amount={} status={}, invoice release encumbrance={}",
          data.amount(), data.status(), isReleaseEncumbrance);
        var newEncumbrance = transaction.getEncumbrance()
          .withAmountAwaitingPayment(awaitingPayment)
          .withAmountExpended(expended)
          .withAmountCredited(credited)
          .withStatus(data.status());
        transaction.withAmount(data.amount())
          .withEncumbrance(newEncumbrance);
    });
  }

  private boolean extractReleaseEncumbrance(List<InvoiceLine> invoiceLines) {
    return filterInvoiceLinesByStatuses(invoiceLines, List.of(InvoiceLine.InvoiceLineStatus.PAID))
      .stream()
      .anyMatch(InvoiceLine::getReleaseEncumbrance);
  }

  private Transaction getOldTransaction(List<Transaction> forDelete, Transaction transaction) {
    return forDelete.stream()
      .filter(oldTransaction -> StringUtils.equals(oldTransaction.getEncumbrance().getSourcePoLineId(), transaction.getEncumbrance().getSourcePoLineId()))
      .peek(oldTransaction -> log.info("getOldTransaction:: Old transaction amount={} status={}",
        oldTransaction.getAmount(), oldTransaction.getEncumbrance().getStatus()))
      .findFirst().orElse(null);
  }

  private NewEncumbranceData getNewEncumbranceData(boolean isReleaseEncumbrance, Transaction oldTransaction, Transaction transaction,
                                                   double expended, double credited, double awaitingPayment, CurrencyUnit currency) {
    if (isReleaseEncumbrance || (nonNull(oldTransaction) && oldTransaction.getEncumbrance().getStatus() == RELEASED)) {
      return new NewEncumbranceData(0d, RELEASED);
    }
    var initialAmount = transaction.getEncumbrance().getInitialAmountEncumbered();
    var newAmount = calculateEncumbranceEffectiveAmount(initialAmount, expended, credited, awaitingPayment, currency);
    return new NewEncumbranceData(newAmount, UNRELEASED);
  }

  private record NewEncumbranceData(double amount, Encumbrance.Status status) {
  }

  private Future<EncumbrancesProcessingHolder> removeEncumbranceReferenceFromTransactions(List<String> encumbranceIds,
                                                                                          EncumbrancesProcessingHolder processingHolder,
                                                                                          RequestContext requestContext) {
    // this is useful for cancelled invoices (approved won't pass the previous check, open and paid won't have pending payments)
    return pendingPaymentService.getTransactionsByEncumbranceIds(encumbranceIds, requestContext)
      .map(pendingPayments -> {
        if (pendingPayments.isEmpty()) {
          return processingHolder;
        }
        pendingPayments.forEach(pp -> pp.getAwaitingPayment().setEncumbranceId(null));
        processingHolder.withPendingPaymentsToUpdate(pendingPayments);
        return processingHolder;
      });
  }
}
