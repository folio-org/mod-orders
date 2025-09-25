package org.folio.service.finance;

import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;
import org.folio.models.EncumbranceUnreleaseHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.springframework.util.CollectionUtils;

import java.util.EnumSet;
import java.util.List;

import static java.util.Objects.nonNull;
import static org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus.APPROVED;
import static org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus.PAID;

@Log4j2
@UtilityClass
public class EncumbranceUtils {

  public static List<Transaction> collectAllowedEncumbrancesForUnrelease(EncumbranceUnreleaseHolder holder) {
    // Unreleased if invoiceLine has releaseEncumbrance=false and status=Approved OR Paid
    // OR unrelease if encumbrance amountExpended + amountCredited + amountAwaitingPayment = 0
    return holder.getEncumbrances().stream()
      .filter(encumbrance -> {
        var hasPendingPayments = hasPendingPayments(holder, encumbrance);
        var hasPayments = hasPayments(holder, encumbrance);
        var hasZeroAmounts = allowEncumbranceToUnrelease(encumbrance);
        log.info("collectAllowedEncumbrancesForUnrelease:: Encumbrance status: {}, has pending payments: {}, has payments: {}, has zero amounts: {}",
          encumbrance.getEncumbrance().getStatus(), hasPendingPayments, hasPayments, hasZeroAmounts);
        return hasPendingPayments || hasPayments || hasZeroAmounts;
      })
      .toList();
  }

  private static Boolean hasPendingPayments(EncumbranceUnreleaseHolder holder, Transaction encumbrance) {
    if (CollectionUtils.isEmpty(holder.getPendingPayments())) {
      return false;
    }
    return holder.getPendingPayments().stream()
      .filter(pendingPayment -> nonNull(pendingPayment.getAwaitingPayment()) && nonNull(pendingPayment.getAwaitingPayment().getEncumbranceId()))
      .filter(pendingPayment -> pendingPayment.getAwaitingPayment().getEncumbranceId().equals(encumbrance.getId()))
      .filter(pendingPayment -> nonNull(pendingPayment.getSourceInvoiceLineId()))
      .map(pendingPayment -> hasReleaseEncumbranceFalseAndIsApprovedOrPaid(pendingPayment, holder.getInvoiceLines()))
      .findFirst().orElse(false);
  }

  private static Boolean hasPayments(EncumbranceUnreleaseHolder holder, Transaction encumbrance) {
    if (CollectionUtils.isEmpty(holder.getPayments())) {
      return false;
    }
    return holder.getPayments().stream()
      .filter(payment -> nonNull(payment.getPaymentEncumbranceId()))
      .filter(payment -> payment.getPaymentEncumbranceId().equals(encumbrance.getId()))
      .filter(payment -> nonNull(payment.getSourceInvoiceLineId()))
      .map(payment -> hasReleaseEncumbranceFalseAndIsApprovedOrPaid(payment, holder.getInvoiceLines()))
      .findFirst().orElse(false);
  }

  private static Boolean hasReleaseEncumbranceFalseAndIsApprovedOrPaid(Transaction payment, List<InvoiceLine> invoiceLines) {
    var optional = invoiceLines.stream()
      .filter(invoiceLine -> invoiceLine.getId().equals(payment.getSourceInvoiceLineId()))
      .findFirst();
    return optional.isEmpty() ||
      (!optional.get().getReleaseEncumbrance() && EnumSet.of(APPROVED, PAID).contains(optional.get().getInvoiceLineStatus()));
  }

  public static boolean allowEncumbranceToUnrelease(Transaction encumbranceTransaction) {
    var encumbrance = encumbranceTransaction.getEncumbrance();
    return encumbrance.getStatus() == Encumbrance.Status.RELEASED &&
      (encumbrance.getAmountExpended() == 0
      && encumbrance.getAmountCredited() == 0
      && encumbrance.getAmountAwaitingPayment() == 0);
  }

  public static boolean allowEncumbranceToReleaseOnReopen(Encumbrance encumbrance) {
    return encumbrance.getStatus() == Encumbrance.Status.PENDING
      && (encumbrance.getAmountExpended() > 0
      || encumbrance.getAmountCredited() > 0
      || encumbrance.getAmountAwaitingPayment() > 0);
  }
}
