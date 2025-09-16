package org.folio.service.finance;

import lombok.experimental.UtilityClass;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;

import java.util.List;
import java.util.Objects;

import static org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus.PAID;

@UtilityClass
public class EncumbranceUtils {

  public static List<Transaction> collectAllowedTransactionsForUnrelease(List<Transaction> encumbrances,
                                                                         List<Transaction> payments,
                                                                         List<InvoiceLine> invoiceLines) {
    // Unreleased if invoiceLine has releaseEncumbrance=false and status=Paid
    // OR unrelease of amountExpended + amountCredited + amountAwaitingPayment = 0
    return encumbrances.stream()
      .filter(encumbrance -> {
        var allowTransactionToUnrelease = payments.stream()
          .filter(payment -> Objects.nonNull(payment.getPaymentEncumbranceId()))
          .filter(payment -> payment.getPaymentEncumbranceId().equals(encumbrance.getId()))
          .filter(payment -> Objects.nonNull(payment.getSourceInvoiceLineId()))
          .map(payment -> isInvoiceLineReleaseEncumbranceFalseAndPaid(payment, invoiceLines))
          .toList();
        return (allowTransactionToUnrelease.getFirst()) || allowTransactionToUnrelease(encumbrance);
      })
      .toList();
  }

  private static Boolean isInvoiceLineReleaseEncumbranceFalseAndPaid(Transaction payment, List<InvoiceLine> invoiceLines) {
    var optional = invoiceLines.stream()
      .filter(invoiceLine -> invoiceLine.getId().equals(payment.getSourceInvoiceLineId()))
      .findFirst();
    return optional.isEmpty() ||
      (!optional.get().getReleaseEncumbrance() && optional.get().getInvoiceLineStatus() == PAID);
  }

  public static boolean allowTransactionToUnrelease(Transaction encumbranceTransaction) {
    var encumbrance = encumbranceTransaction.getEncumbrance();
    return encumbrance.getAmountExpended() == 0
      && encumbrance.getAmountCredited() == 0
      && encumbrance.getAmountAwaitingPayment() == 0;
  }
}
