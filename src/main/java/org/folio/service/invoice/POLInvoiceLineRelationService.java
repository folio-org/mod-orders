package org.folio.service.invoice;

import org.apache.commons.collections4.CollectionUtils;
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
import java.util.stream.Collectors;

import static org.folio.orders.utils.InvoiceUtil.filterInvoiceLinesByStatuses;
import static org.folio.service.finance.transaction.FinanceUtils.calculateEncumbranceEffectiveAmount;
import static org.folio.service.finance.transaction.FinanceUtils.sumAmounts;

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
      .collect(Collectors.toList());
    List<Transaction> forCreate = processingHolder.getEncumbrancesForCreate().stream()
      .map(EncumbranceRelationsHolder::getNewEncumbrance)
      .collect(Collectors.toList());
    List<String> poLineIds = forDelete.stream()
      .map(transaction -> transaction.getEncumbrance().getSourcePoLineId())
      .distinct()
      .collect(Collectors.toList());
    return invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext)
      .compose(invoiceLines -> {
        validateInvoiceLineStatuses(invoiceLines);
        if (forCreate.isEmpty() && forDelete.isEmpty()) {
          return Future.succeededFuture(processingHolder);
        }
        if (!forCreate.isEmpty() && !forDelete.isEmpty()) {
          String currency = processingHolder.getEncumbrancesForCreate().stream()
            .map(EncumbranceRelationsHolder::getCurrency)
            .findFirst()
            .orElseThrow();
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

  private void copyAmountsAndRecalculateNewEncumbrance(List<Transaction> forCreate, List<Transaction> forDelete,
                                                       List<InvoiceLine> invoiceLines, String currencyValue) {
    // Update encumbrances when an order line is linked to a paid invoice
    double expended = sumAmounts(forDelete, Encumbrance::getAmountExpended);
    double credited = sumAmounts(forDelete, Encumbrance::getAmountCredited);
    double awaitingPayment = sumAmounts(forDelete, Encumbrance::getAmountAwaitingPayment);
    CurrencyUnit currency = Monetary.getCurrency(currencyValue);
    boolean isReleaseEncumbranceEnabled = filterInvoiceLinesByStatuses(invoiceLines, List.of(InvoiceLine.InvoiceLineStatus.PAID))
      .stream()
      .anyMatch(InvoiceLine::getReleaseEncumbrance);
    forCreate.stream().findFirst()
      .ifPresent(transaction -> {
        Encumbrance oldEncumbrance = transaction.getEncumbrance();
        double initialAmount = oldEncumbrance.getInitialAmountEncumbered();
        double newAmount;
        Encumbrance.Status newStatus;
        if (isReleaseEncumbranceEnabled) {
          newAmount = 0d;
          newStatus = Encumbrance.Status.RELEASED;
        } else {
          newAmount = calculateEncumbranceEffectiveAmount(initialAmount, expended, credited, awaitingPayment, currency);
          newStatus = Encumbrance.Status.UNRELEASED;
        }
        Encumbrance newEncumbrance = oldEncumbrance
          .withAmountExpended(expended)
          .withAmountCredited(credited)
          .withAmountAwaitingPayment(awaitingPayment)
          .withStatus(newStatus);
        transaction
          .withAmount(newAmount)
          .withEncumbrance(newEncumbrance);
    });
  }

  private void validateInvoiceLineStatuses(List<InvoiceLine> invoiceLines) {
    List<InvoiceLine> approvedInvoices = filterInvoiceLinesByStatuses(invoiceLines, List.of(InvoiceLine.InvoiceLineStatus.APPROVED));
    if (CollectionUtils.isNotEmpty(approvedInvoices)) {
      String invoiceLineIds = approvedInvoices.stream().map(InvoiceLine::getId).collect(Collectors.joining(", "));
      String message = String.format(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getDescription(), invoiceLineIds);
      Error error = new Error().withCode(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getCode()).withMessage(message);
      throw new HttpException(HttpStatus.SC_FORBIDDEN, error);
    }
  }
}
