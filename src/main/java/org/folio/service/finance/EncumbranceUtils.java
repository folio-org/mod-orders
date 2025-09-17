package org.folio.service.finance;

import lombok.experimental.UtilityClass;
import org.folio.models.EncumbranceUnreleaseHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus.PAID;

@UtilityClass
public class EncumbranceUtils {

  public static List<Transaction> collectAllowedTransactionsForUnrelease(EncumbranceUnreleaseHolder holder) {
    // Unreleased if invoiceLine has releaseEncumbrance=false and status=Paid
    // OR unrelease if encumbrance amountExpended + amountCredited + amountAwaitingPayment = 0
    return holder.getEncumbrances().stream()
      .filter(encumbrance -> {
        var allowTransactionToUnrelease = Optional.<Boolean>empty();
        if (!CollectionUtils.isEmpty(holder.getPayments())) {
           allowTransactionToUnrelease = holder.getPayments().stream()
            .filter(payment -> Objects.nonNull(payment.getPaymentEncumbranceId()))
            .filter(payment -> payment.getPaymentEncumbranceId().equals(encumbrance.getId()))
            .filter(payment -> Objects.nonNull(payment.getSourceInvoiceLineId()))
            .map(payment -> isInvoiceLineReleaseEncumbranceFalseAndPaid(payment, holder.getInvoiceLines()))
            .findFirst();
        }
        return (allowTransactionToUnrelease.isPresent() && allowTransactionToUnrelease.get())
          || allowTransactionToUnrelease(encumbrance);
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
    return encumbrance.getStatus() == Encumbrance.Status.RELEASED &&
      (encumbrance.getAmountExpended() == 0
      && encumbrance.getAmountCredited() == 0
      && encumbrance.getAmountAwaitingPayment() == 0);
  }

  public static boolean allowTransactionToReleaseOnReopen(Encumbrance encumbrance) {
    return encumbrance.getStatus() == Encumbrance.Status.PENDING
      && (encumbrance.getAmountExpended() > 0
      || encumbrance.getAmountCredited() > 0
      || encumbrance.getAmountAwaitingPayment() > 0);
  }
}
