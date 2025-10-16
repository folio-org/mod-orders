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
import java.util.Objects;

import static java.util.Objects.nonNull;
import static org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus.APPROVED;
import static org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus.PAID;

@Log4j2
@UtilityClass
public class EncumbranceUtils {

  /**
   * Collects encumbrances that are allowed to be unreleased based on invoice lines and secondary encumbrance amounts.
   * @param holder The holder containing encumbrances and related data
   * @return List of transactions that can be unreleased
   */
  public static List<Transaction> collectAllowedEncumbrancesForUnrelease(EncumbranceUnreleaseHolder holder) {
    // Unreleased if invoiceLine has releaseEncumbrance=false and status=Approved OR Paid
    // OR unrelease if encumbrance amountExpended + amountCredited + amountAwaitingPayment = 0
    return holder.getEncumbrances().stream()
      .filter(encumbrance -> {
        var hasPendingPayments = hasPendingPayments(holder, encumbrance);
        var hasPayments = hasPayments(holder, encumbrance);
        var hasZeroAmounts = allowEncumbranceToUnrelease(encumbrance);
        log.info("collectAllowedEncumbrancesForUnrelease:: Current encumbrance status: {}, has pending payments: {}, has payments: {}, has zero amounts: {}",
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
      .peek(pendingPayment -> log.info("hasPendingPayments:: Encumbrance id={}, pending payment encumbranceId={} amount={}",
        encumbrance.getId(), nonNull(pendingPayment.getAwaitingPayment()) ? pendingPayment.getAwaitingPayment().getEncumbranceId() : null, pendingPayment.getAmount()))
      .filter(pendingPayment -> nonNull(pendingPayment.getAwaitingPayment())
        && nonNull(pendingPayment.getAwaitingPayment().getEncumbranceId())
        && pendingPayment.getAwaitingPayment().getEncumbranceId().equals(encumbrance.getId())
        && nonNull(pendingPayment.getSourceInvoiceLineId()))
      .anyMatch(pendingPayment -> hasReleaseEncumbranceFalseAndIsApprovedOrPaid(pendingPayment, holder.getInvoiceLines()));
  }

  private static Boolean hasPayments(EncumbranceUnreleaseHolder holder, Transaction encumbrance) {
    if (CollectionUtils.isEmpty(holder.getPayments())) {
      return false;
    }
    return holder.getPayments().stream()
      .peek(payment -> log.info("hasPayments:: Encumbrance id={}, payment encumbranceId={} amount={}",
        encumbrance.getId(), payment.getPaymentEncumbranceId(), payment.getAmount()))
      .filter(payment -> nonNull(payment.getPaymentEncumbranceId())
        && payment.getPaymentEncumbranceId().equals(encumbrance.getId())
        && nonNull(payment.getSourceInvoiceLineId()))
      .anyMatch(payment -> hasReleaseEncumbranceFalseAndIsApprovedOrPaid(payment, holder.getInvoiceLines()));
  }

  private static Boolean hasReleaseEncumbranceFalseAndIsApprovedOrPaid(Transaction transaction, List<InvoiceLine> invoiceLines) {
    return invoiceLines.stream()
      .peek(invoiceLine -> log.info("hasReleaseEncumbranceFalseAndIsApprovedOrPaid:: transactionType={} sourceInvoiceLineId={} amount={}, invoice line id={} releaseEncumbrance={} total={} status={}",
        transaction.getTransactionType().name(), transaction.getSourceInvoiceLineId(), transaction.getAmount(),
        invoiceLine.getId(), invoiceLine.getReleaseEncumbrance(), invoiceLine.getTotal(), invoiceLine.getInvoiceLineStatus()))
      .filter(invoiceLine -> invoiceLine.getId().equals(transaction.getSourceInvoiceLineId()))
      .anyMatch(invoiceLine -> Boolean.FALSE.equals(invoiceLine.getReleaseEncumbrance()) && EnumSet.of(APPROVED, PAID).contains(invoiceLine.getInvoiceLineStatus()));
  }

  public static boolean allowEncumbranceToUnrelease(Transaction encumbranceTransaction) {
    if (Objects.isNull(encumbranceTransaction) || Objects.isNull(encumbranceTransaction.getEncumbrance())) {
      log.warn("allowEncumbranceToUnrelease:: Invalid transaction or encumbrance");
      return false;
    }
    var encumbrance = encumbranceTransaction.getEncumbrance();
    return encumbrance.getStatus() == Encumbrance.Status.RELEASED &&
      (encumbrance.getAmountExpended() == 0
      && encumbrance.getAmountCredited() == 0
      && encumbrance.getAmountAwaitingPayment() == 0);
  }

  public static boolean allowEncumbranceToReleaseOnReopen(Encumbrance encumbrance) {
    if (Objects.isNull(encumbrance)) {
      log.warn("allowEncumbranceToReleaseOnReopen:: Invalid transaction or encumbrance");
      return false;
    }
    return encumbrance.getStatus() == Encumbrance.Status.PENDING
      && (encumbrance.getAmountExpended() > 0
      || encumbrance.getAmountCredited() > 0
      || encumbrance.getAmountAwaitingPayment() > 0);
  }
}
