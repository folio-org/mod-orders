package org.folio.service.finance;

import org.folio.CopilotGenerated;
import org.folio.models.EncumbranceUnreleaseHolder;
import org.folio.rest.acq.model.finance.AwaitingPayment;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@CopilotGenerated(partiallyGenerated = true, model = "GPT-4.1")
public class EncumbranceUtilsTest {

  // 1. collectAllowedEncumbrancesForUnrelease (public)
  @Test
  void testCollectAllowedEncumbrancesForUnrelease_hasPendingPayments() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("enc1");

    var encumbranceObj = mock(Encumbrance.class);
    when(encumbrance.getEncumbrance()).thenReturn(encumbranceObj);
    when(encumbranceObj.getStatus()).thenReturn(Encumbrance.Status.PENDING);

    var pendingPayment = mock(Transaction.class);
    var awaitingPayment = mock(AwaitingPayment.class);
    when(pendingPayment.getAwaitingPayment()).thenReturn(awaitingPayment);
    when(pendingPayment.getAwaitingPayment().getEncumbranceId()).thenReturn("enc1");
    when(pendingPayment.getSourceInvoiceLineId()).thenReturn("inv1");
    when(pendingPayment.getTransactionType()).thenReturn(Transaction.TransactionType.PENDING_PAYMENT);

    var invoiceLine = mock(InvoiceLine.class);
    when(invoiceLine.getId()).thenReturn("inv1");
    when(invoiceLine.getReleaseEncumbrance()).thenReturn(false);
    when(invoiceLine.getInvoiceLineStatus()).thenReturn(InvoiceLine.InvoiceLineStatus.APPROVED);

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getEncumbrances()).thenReturn(List.of(encumbrance));
    when(holder.getPendingPayments()).thenReturn(List.of(pendingPayment));
    when(holder.getPayments()).thenReturn(List.of());
    when(holder.getInvoiceLines()).thenReturn(List.of(invoiceLine));

    var result = EncumbranceUtils.collectAllowedEncumbrancesForUnrelease(holder);
    assertEquals(1, result.size());
    assertEquals(encumbrance, result.getFirst());
  }

  @Test
  void testCollectAllowedEncumbrancesForUnrelease_hasPayments() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("enc2");

    var encumbranceObj = mock(Encumbrance.class);
    when(encumbrance.getEncumbrance()).thenReturn(encumbranceObj);
    when(encumbranceObj.getStatus()).thenReturn(Encumbrance.Status.PENDING);

    var payment = mock(Transaction.class);
    when(payment.getPaymentEncumbranceId()).thenReturn("enc2");
    when(payment.getSourceInvoiceLineId()).thenReturn("inv2");
    when(payment.getTransactionType()).thenReturn(Transaction.TransactionType.PAYMENT);

    var invoiceLine = mock(InvoiceLine.class);
    when(invoiceLine.getId()).thenReturn("inv2");
    when(invoiceLine.getReleaseEncumbrance()).thenReturn(false);
    when(invoiceLine.getInvoiceLineStatus()).thenReturn(InvoiceLine.InvoiceLineStatus.PAID);

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getEncumbrances()).thenReturn(List.of(encumbrance));
    when(holder.getPendingPayments()).thenReturn(List.of());
    when(holder.getPayments()).thenReturn(List.of(payment));
    when(holder.getInvoiceLines()).thenReturn(List.of(invoiceLine));

    var result = EncumbranceUtils.collectAllowedEncumbrancesForUnrelease(holder);
    assertEquals(1, result.size());
    assertEquals(encumbrance, result.getFirst());
  }

  @Test
  void testCollectAllowedEncumbrancesForUnrelease_allowEncumbranceToUnrelease() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("enc3");

    var encumbranceObj = mock(Encumbrance.class);
    when(encumbrance.getEncumbrance()).thenReturn(encumbranceObj);
    when(encumbranceObj.getStatus()).thenReturn(Encumbrance.Status.RELEASED);
    when(encumbranceObj.getAmountExpended()).thenReturn(0.0);
    when(encumbranceObj.getAmountCredited()).thenReturn(0.0);
    when(encumbranceObj.getAmountAwaitingPayment()).thenReturn(0.0);

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getEncumbrances()).thenReturn(List.of(encumbrance));
    when(holder.getPendingPayments()).thenReturn(List.of());
    when(holder.getPayments()).thenReturn(List.of());
    when(holder.getInvoiceLines()).thenReturn(List.of());

    var result = EncumbranceUtils.collectAllowedEncumbrancesForUnrelease(holder);
    assertEquals(1, result.size());
    assertEquals(encumbrance, result.getFirst());
  }

  @Test
  void testCollectAllowedEncumbrancesForUnrelease_noneAllowed() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("enc4");

    var encumbranceObj = mock(Encumbrance.class);
    when(encumbrance.getEncumbrance()).thenReturn(encumbranceObj);
    when(encumbranceObj.getStatus()).thenReturn(Encumbrance.Status.PENDING);
    when(encumbranceObj.getAmountExpended()).thenReturn(0.0);
    when(encumbranceObj.getAmountCredited()).thenReturn(0.0);
    when(encumbranceObj.getAmountAwaitingPayment()).thenReturn(0.0);

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getEncumbrances()).thenReturn(List.of(encumbrance));
    when(holder.getPendingPayments()).thenReturn(List.of());
    when(holder.getPayments()).thenReturn(List.of());
    when(holder.getInvoiceLines()).thenReturn(List.of());

    var result = EncumbranceUtils.collectAllowedEncumbrancesForUnrelease(holder);
    assertEquals(0, result.size());
  }

  // 2. hasPendingPayments (private)
  @Test
  void testHasPendingPayments_emptyAndNull() {
    var encumbrance = mock(Transaction.class);

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getPendingPayments()).thenReturn(null);
    assertFalse(invokeHasPendingPayments(holder, encumbrance));
    when(holder.getPendingPayments()).thenReturn(List.of());
    assertFalse(invokeHasPendingPayments(holder, encumbrance));
  }

  @Test
  void testHasPendingPayments_noMatch() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("encX");

    var pendingPayment = mock(Transaction.class);

    var awaitingPayment = mock(org.folio.rest.acq.model.finance.AwaitingPayment.class);
    when(pendingPayment.getAwaitingPayment()).thenReturn(awaitingPayment);
    when(awaitingPayment.getEncumbranceId()).thenReturn("other");

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getPendingPayments()).thenReturn(List.of(pendingPayment));
    when(holder.getInvoiceLines()).thenReturn(List.of());
    assertFalse(invokeHasPendingPayments(holder, encumbrance));
  }

  @Test
  void testHasPendingPayments_nullAwaitingPayment() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("encZ");

    var pendingPayment = mock(Transaction.class);
    when(pendingPayment.getAwaitingPayment()).thenReturn(null); // null awaitingPayment

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getPendingPayments()).thenReturn(List.of(pendingPayment));
    when(holder.getInvoiceLines()).thenReturn(List.of());
    // Should return false because awaitingPayment is null
    assertFalse(invokeHasPendingPayments(holder, encumbrance));
  }

  @Test
  void testHasPendingPayments_nullEncumbranceId() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("encZ");

    var pendingPayment = mock(Transaction.class);

    var awaitingPayment = mock(AwaitingPayment.class);
    when(pendingPayment.getAwaitingPayment()).thenReturn(awaitingPayment);
    when(awaitingPayment.getEncumbranceId()).thenReturn(null); // null encumbranceId

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getPendingPayments()).thenReturn(List.of(pendingPayment));
    when(holder.getInvoiceLines()).thenReturn(List.of());
    // Should return false because encumbranceId is null
    assertFalse(invokeHasPendingPayments(holder, encumbrance));
  }

  @Test
  void testHasPendingPayments_nullSourceInvoiceLineId() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("encZ");

    var pendingPayment = mock(Transaction.class);

    var awaitingPayment = mock(AwaitingPayment.class);
    when(pendingPayment.getAwaitingPayment()).thenReturn(awaitingPayment);
    when(awaitingPayment.getEncumbranceId()).thenReturn("encZ");
    when(pendingPayment.getSourceInvoiceLineId()).thenReturn(null); // null sourceInvoiceLineId

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getPendingPayments()).thenReturn(List.of(pendingPayment));
    when(holder.getInvoiceLines()).thenReturn(List.of());
    // Should return false because sourceInvoiceLineId is null
    assertFalse(invokeHasPendingPayments(holder, encumbrance));
  }

  // 3. hasPayments (private)
  @Test
  void testHasPayments_emptyAndNull() {
    var encumbrance = mock(Transaction.class);

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getPayments()).thenReturn(null);
    assertFalse(invokeHasPayments(holder, encumbrance));
    when(holder.getPayments()).thenReturn(List.of());
    assertFalse(invokeHasPayments(holder, encumbrance));
  }

  @Test
  void testHasPayments_noMatch() {
    var encumbrance = mock(Transaction.class);
    when(encumbrance.getId()).thenReturn("encY");

    var payment = mock(Transaction.class);
    when(payment.getPaymentEncumbranceId()).thenReturn("other");

    var holder = mock(EncumbranceUnreleaseHolder.class);
    when(holder.getPayments()).thenReturn(List.of(payment));
    when(holder.getInvoiceLines()).thenReturn(List.of());
    assertFalse(invokeHasPayments(holder, encumbrance));
  }

  // 4. hasReleaseEncumbranceFalseAndIsApprovedOrPaid (private)
  @ParameterizedTest
  @CsvSource({
    "true,PENDING_PAYMENT,APPROVED,false,true",
    "true,PAYMENT,PAID,false,true",
    "true,PAYMENT,CANCELLED,false,false",
    "true,PENDING_PAYMENT,APPROVED,true,false",
    "false,PENDING_PAYMENT,APPROVED,false,false"
  })
  void testHasReleaseEncumbranceFalseAndIsApprovedOrPaid(boolean match, String transactionType, String status, boolean release, boolean expected) {
    var payment = mock(Transaction.class);
    when(payment.getSourceInvoiceLineId()).thenReturn("inv1");
    when(payment.getTransactionType()).thenReturn(Transaction.TransactionType.valueOf(transactionType));

    var invoiceLine = mock(InvoiceLine.class);
    when(invoiceLine.getId()).thenReturn(match ? "inv1" : "other");
    when(invoiceLine.getReleaseEncumbrance()).thenReturn(release);
    when(invoiceLine.getInvoiceLineStatus()).thenReturn(InvoiceLine.InvoiceLineStatus.valueOf(status));
    assertEquals(expected, invokeHasReleaseEncumbranceFalseAndIsApprovedOrPaid(payment, List.of(invoiceLine)));
  }

  // 5. allowEncumbranceToUnrelease & allowEncumbranceToReleaseOnReopen (public)
  @ParameterizedTest
  @CsvSource({
    // allowEncumbranceToUnrelease
    "RELEASED,0,0,0,true",
    "RELEASED,1,0,0,false",
    "RELEASED,0,1,0,false",
    "RELEASED,0,0,1,false",
    "PENDING,0,0,0,false",
    // allowEncumbranceToReleaseOnReopen
    "PENDING,1,0,0,true",
    "PENDING,0,1,0,true",
    "PENDING,0,0,1,true",
    "PENDING,0,0,0,false",
    "RELEASED,1,0,0,false"
  })
  void testAllowEncumbranceToUnrelease_and_ReleaseOnReopen(
    Encumbrance.Status status,
    double amountExpended,
    double amountCredited,
    double amountAwaitingPayment,
    boolean expected
  ) {
    var encumbrance = mock(Encumbrance.class);
    when(encumbrance.getStatus()).thenReturn(status);
    when(encumbrance.getAmountExpended()).thenReturn(amountExpended);
    when(encumbrance.getAmountCredited()).thenReturn(amountCredited);
    when(encumbrance.getAmountAwaitingPayment()).thenReturn(amountAwaitingPayment);

    var transaction = mock(Transaction.class);
    when(transaction.getEncumbrance()).thenReturn(encumbrance);
    // Test allowEncumbranceToUnrelease
    var allowUnrelease = EncumbranceUtils.allowEncumbranceToUnrelease(transaction);
    if (status == Encumbrance.Status.RELEASED) {
      assertEquals(expected, allowUnrelease);
    }

    // Test allowEncumbranceToReleaseOnReopen
    var allowRelease = EncumbranceUtils.allowEncumbranceToReleaseOnReopen(encumbrance);
    if (status == Encumbrance.Status.PENDING) {
      assertEquals(expected, allowRelease);
    } else {
      assertFalse(allowRelease);
    }
  }

  // Reflection helpers for private methods
  private boolean invokeHasPendingPayments(EncumbranceUnreleaseHolder holder, Transaction encumbrance) {
    try {
      var method = EncumbranceUtils.class.getDeclaredMethod("hasPendingPayments", EncumbranceUnreleaseHolder.class, Transaction.class);
      method.setAccessible(true);
      return (Boolean) method.invoke(null, holder, encumbrance);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private boolean invokeHasPayments(EncumbranceUnreleaseHolder holder, Transaction encumbrance) {
    try {
      var method = EncumbranceUtils.class.getDeclaredMethod("hasPayments", EncumbranceUnreleaseHolder.class, Transaction.class);
      method.setAccessible(true);
      return (Boolean) method.invoke(null, holder, encumbrance);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private boolean invokeHasReleaseEncumbranceFalseAndIsApprovedOrPaid(Transaction payment, List<InvoiceLine> invoiceLines) {
    try {
      var method = EncumbranceUtils.class.getDeclaredMethod("hasReleaseEncumbranceFalseAndIsApprovedOrPaid", Transaction.class, List.class);
      method.setAccessible(true);
      return (boolean) method.invoke(null, payment, invoiceLines);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
