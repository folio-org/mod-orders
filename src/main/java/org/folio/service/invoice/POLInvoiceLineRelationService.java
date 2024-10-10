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
import org.folio.service.finance.transaction.FinanceUtils;
import org.folio.service.finance.transaction.PendingPaymentService;

import javax.money.Monetary;
import java.util.List;
import java.util.stream.Collectors;

import static org.folio.orders.utils.InvoiceUtil.filterInvoiceLinesByStatuses;

public class POLInvoiceLineRelationService {
  private final InvoiceLineService invoiceLineService;
  private final PendingPaymentService pendingPaymentService;

  public POLInvoiceLineRelationService(InvoiceLineService invoiceLineService, PendingPaymentService pendingPaymentService) {
    this.invoiceLineService = invoiceLineService;
    this.pendingPaymentService = pendingPaymentService;
  }

  public Future<EncumbrancesProcessingHolder> manageInvoiceRelation(EncumbrancesProcessingHolder encumbrancesProcessingHolder, RequestContext requestContext) {
    List<Transaction> forDelete = encumbrancesProcessingHolder.getEncumbrancesForDelete().stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance).collect(Collectors.toList());
    List<Transaction> forCreate = encumbrancesProcessingHolder.getEncumbrancesForCreate().stream()
      .map(EncumbranceRelationsHolder::getNewEncumbrance).collect(Collectors.toList());
    List<String> poLineIds = forDelete.stream().map(transaction -> transaction.getEncumbrance().getSourcePoLineId()).distinct().collect(Collectors.toList());

    return invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext)
      .compose(invoiceLines -> {
        validateInvoiceLineStatuses(invoiceLines);
        if (forCreate.isEmpty() && forDelete.isEmpty()) {
          return Future.succeededFuture(encumbrancesProcessingHolder);
        }
        if (!forCreate.isEmpty() && !forDelete.isEmpty()) {
          String currency = encumbrancesProcessingHolder.getEncumbrancesForCreate().stream()
            .map(EncumbranceRelationsHolder::getCurrency).findFirst().orElseThrow();
          copyAmountsAndRecalculateNewEncumbrance(forCreate, forDelete, invoiceLines, currency);
        }
        if (forDelete.isEmpty()) {
          return Future.succeededFuture(encumbrancesProcessingHolder);
        }
        if (forCreate.isEmpty()) {
          forDelete.forEach(TransactionValidator::validateEncumbranceForDeletion);
        }
        List<String> encumbranceForDeleteIds = forDelete.stream().map(Transaction::getId).distinct().toList();
        return removeEncumbranceReferenceFromTransactions(encumbranceForDeleteIds, encumbrancesProcessingHolder, requestContext);
      });
  }

  private Future<EncumbrancesProcessingHolder> removeEncumbranceReferenceFromTransactions(List<String> encumbranceIds,
      EncumbrancesProcessingHolder encumbrancesProcessingHolder, RequestContext requestContext) {
    // this is useful for cancelled invoices (approved won't pass the previous check, open and paid won't have pending payments)
    return pendingPaymentService.getTransactionsByEncumbranceIds(encumbranceIds, requestContext)
      .map(pendingPayments -> {
        if (pendingPayments.isEmpty()) {
          return encumbrancesProcessingHolder;
        }
        pendingPayments.forEach(pp -> pp.getAwaitingPayment().setEncumbranceId(null));
        encumbrancesProcessingHolder.withPendingPaymentsToUpdate(pendingPayments);
        return encumbrancesProcessingHolder;
      });
  }

  private void copyAmountsAndRecalculateNewEncumbrance(List<Transaction> forCreate, List<Transaction> forDelete, List<InvoiceLine> invoiceLines, String currency) {
    // Update encumbrances when an order line is linked to a paid invoice
    double amountExpended = FinanceUtils.sumAmounts(forDelete, Encumbrance::getAmountExpended);
    double amountCredited = FinanceUtils.sumAmounts(forDelete, Encumbrance::getAmountCredited);
    double amountAwaitingPayment = FinanceUtils.sumAmounts(forDelete, Encumbrance::getAmountAwaitingPayment);

    boolean isReleaseEncumbranceEnabled = filterInvoiceLinesByStatuses(invoiceLines, List.of(InvoiceLine.InvoiceLineStatus.PAID))
      .stream()
      .anyMatch(InvoiceLine::getReleaseEncumbrance);

    forCreate.stream().findFirst().ifPresent(transaction -> {
      Encumbrance encumbrance = transaction.getEncumbrance();
      double encumbranceAmount = FinanceUtils.calculateEncumbranceEffectiveAmount(
        encumbrance.getInitialAmountEncumbered(),
        amountExpended,
        amountCredited,
        amountAwaitingPayment,
        Monetary.getCurrency(currency)
      );
      var encumbranceStatus = isReleaseEncumbranceEnabled && encumbranceAmount == 0d ?
        Encumbrance.Status.RELEASED : Encumbrance.Status.UNRELEASED;
      transaction
        .withAmount(encumbranceAmount)
        .withEncumbrance(
          encumbrance
            .withAmountExpended(amountExpended)
            .withAmountCredited(amountCredited)
            .withAmountAwaitingPayment(amountAwaitingPayment)
            .withStatus(encumbranceStatus)
        );
    });
  }

  private void validateInvoiceLineStatuses(List<InvoiceLine> invoiceLines) {
    List<InvoiceLine> approvedInvoices = filterInvoiceLinesByStatuses(invoiceLines, List.of(InvoiceLine.InvoiceLineStatus.APPROVED));
    if (CollectionUtils.isNotEmpty(approvedInvoices)) {
      String invoiceLineIds = approvedInvoices.stream().map(InvoiceLine::getId).collect(Collectors.joining(", "));
      String message = String.format(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getDescription(), invoiceLineIds);
      throw new HttpException(HttpStatus.SC_FORBIDDEN, new Error().withCode(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getCode()).withMessage(message));
    }
  }

}
