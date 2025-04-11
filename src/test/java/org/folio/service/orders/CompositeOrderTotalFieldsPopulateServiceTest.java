package org.folio.service.orders;

import static org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus.PAID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.folio.CopilotGenerated;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;

@CopilotGenerated(partiallyGenerated = true)
public class CompositeOrderTotalFieldsPopulateServiceTest {

  @InjectMocks
  private CompositeOrderTotalFieldsPopulateService populateService;

  @Mock
  private TransactionService transactionService;
  @Mock
  private InvoiceService invoiceService;
  @Mock
  private InvoiceLineService invoiceLineService;
  @Mock
  private FiscalYearService fiscalYearService;

  @Mock
  private RequestContext requestContext;
  private AutoCloseable mockitoMocks;

  @BeforeEach
  public void initMocks() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void shouldPopulateTotalFieldsWhenInvoicesInvoiceLinesAndTransactionsExist() {
    String fiscalYearId = UUID.randomUUID().toString();
    PoLine poLine1 = new PoLine().withId(UUID.randomUUID().toString());
    PoLine poLine2 = new PoLine().withId(UUID.randomUUID().toString());
    PoLine poLine3 = new PoLine().withId(UUID.randomUUID().toString());
    Invoice invoice1 = new Invoice()
      .withId(UUID.randomUUID().toString())
      .withCurrency("USD")
      .withFiscalYearId(fiscalYearId);
    Invoice invoice2 = new Invoice()
      .withId(UUID.randomUUID().toString())
      .withCurrency("USD")
      .withFiscalYearId(fiscalYearId);
    InvoiceLine invoiceLine1 = new InvoiceLine()
      .withInvoiceId(invoice1.getId())
      .withTotal(100.0)
      .withInvoiceLineStatus(PAID)
      .withPoLineId(poLine1.getId());
    InvoiceLine invoiceLine2 = new InvoiceLine()
      .withInvoiceId(invoice2.getId())
      .withTotal(200.0)
      .withInvoiceLineStatus(PAID)
      .withPoLineId(poLine2.getId());
    InvoiceLine invoiceLine3 = new InvoiceLine()
      .withInvoiceId(invoice2.getId())
      .withTotal(-500.0)
      .withInvoiceLineStatus(PAID)
      .withPoLineId(poLine3.getId());
    List<Invoice> invoices = List.of(invoice1, invoice2);
    Transaction transaction1 = new Transaction().withAmount(100.0).withCurrency("USD");
    Transaction transaction2 = new Transaction().withAmount(700.0).withCurrency("USD");
    List<Transaction> transactions = List.of(transaction1, transaction2);
    CompositePurchaseOrder order = new CompositePurchaseOrder()
      .withId(UUID.randomUUID().toString())
      .withPoLines(List.of(poLine1, poLine2, poLine3));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
      .withFiscalYear(new FiscalYear().withId(fiscalYearId));

    when(invoiceService.getInvoicesByOrderId(anyString(), any()))
      .thenReturn(Future.succeededFuture(invoices));
    when(invoiceLineService.getInvoiceLinesByInvoiceIdAndStatus(eq(invoice1.getId()), eq(PAID), any()))
      .thenReturn(Future.succeededFuture(List.of(invoiceLine1)));
    when(invoiceLineService.getInvoiceLinesByInvoiceIdAndStatus(eq(invoice2.getId()), eq(PAID), any()))
      .thenReturn(Future.succeededFuture(List.of(invoiceLine2, invoiceLine3)));
    when(transactionService.getTransactions(anyString(), any()))
      .thenReturn(Future.succeededFuture(transactions));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(800.0, resultHolder.getOrder().getTotalEncumbered());
    assertEquals(300.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(500.0, resultHolder.getOrder().getTotalCredited());
  }

  @Test
  void shouldReturnZeroWhenNoInvoicesAndTransactionsExist() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
      .withFiscalYear(new FiscalYear().withId(UUID.randomUUID().toString()));

    when(invoiceService.getInvoicesByOrderId(anyString(), any())).thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(transactionService.getTransactions(anyString(), any())).thenReturn(Future.succeededFuture(Collections.emptyList()));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(0.0, resultHolder.getOrder().getTotalEncumbered());
    assertEquals(0.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(0.0, resultHolder.getOrder().getTotalCredited());
  }

  @Test
  void shouldReturnZeroWhenNoInvoiceLinesAndTransactionsExist() {
    String fiscalYearId = UUID.randomUUID().toString();
    Invoice invoice = new Invoice().withId(UUID.randomUUID().toString()).withCurrency("USD").withFiscalYearId(fiscalYearId);
    List<Invoice> invoices = List.of(invoice);
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
      .withFiscalYear(new FiscalYear().withId(fiscalYearId));

    when(invoiceService.getInvoicesByOrderId(anyString(), any())).thenReturn(Future.succeededFuture(invoices));
    when(invoiceLineService.getInvoiceLinesByInvoiceIdAndStatus(anyString(), eq(PAID), any())).thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(transactionService.getTransactions(anyString(), any())).thenReturn(Future.succeededFuture(Collections.emptyList()));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(0.0, resultHolder.getOrder().getTotalEncumbered());
    assertEquals(0.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(0.0, resultHolder.getOrder().getTotalCredited());
  }

  @Test
  void shouldHandleNullFiscalYearWithInvoicesInvoiceLinesAndNoTransactions() {
    String fiscalYearId = UUID.randomUUID().toString();
    PoLine poLine1 = new PoLine().withId(UUID.randomUUID().toString());
    PoLine poLine2 = new PoLine().withId(UUID.randomUUID().toString());
    Invoice invoice1 = new Invoice()
      .withId(UUID.randomUUID().toString())
      .withCurrency("USD")
      .withFiscalYearId(fiscalYearId);
    Invoice invoice2 = new Invoice()
      .withId(UUID.randomUUID().toString())
      .withCurrency("USD")
      .withFiscalYearId(fiscalYearId);
    InvoiceLine invoiceLine1 = new InvoiceLine()
      .withInvoiceId(invoice1.getId())
      .withTotal(100.0)
      .withInvoiceLineStatus(PAID)
      .withPoLineId(poLine1.getId());
    InvoiceLine invoiceLine2 = new InvoiceLine()
      .withInvoiceId(invoice2.getId())
      .withTotal(-500.0)
      .withInvoiceLineStatus(PAID)
      .withPoLineId(poLine2.getId());
    List<Invoice> invoices = List.of(invoice1, invoice2);
    CompositePurchaseOrder order = new CompositePurchaseOrder()
      .withId(UUID.randomUUID().toString())
      .withPoLines(List.of(poLine1, poLine2));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);

    when(invoiceService.getInvoicesByOrderId(anyString(), any()))
      .thenReturn(Future.succeededFuture(invoices));
    when(invoiceLineService.getInvoiceLinesByInvoiceIdAndStatus(eq(invoice1.getId()), eq(PAID), any()))
      .thenReturn(Future.succeededFuture(List.of(invoiceLine1)));
    when(invoiceLineService.getInvoiceLinesByInvoiceIdAndStatus(eq(invoice2.getId()), eq(PAID), any()))
      .thenReturn(Future.succeededFuture(List.of(invoiceLine2)));
    when(transactionService.getTransactions(anyString(), any()))
      .thenReturn(Future.succeededFuture(List.of()));
    when(fiscalYearService.getCurrentFYForSeriesByFYId(anyString(), any()))
      .thenReturn(Future.succeededFuture(fiscalYearId));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(0.0, resultHolder.getOrder().getTotalEncumbered());
    assertEquals(100.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(500.0, resultHolder.getOrder().getTotalCredited());
  }

  @Test
  void shouldNotUseUnrelatedInvoiceLinesToCalculateOrderExpendedAmount() {
    String fiscalYearId = UUID.randomUUID().toString();
    PoLine poLine1 = new PoLine().withId(UUID.randomUUID().toString());
    PoLine poLine2 = new PoLine().withId(UUID.randomUUID().toString());
    Invoice invoice1 = new Invoice()
      .withId(UUID.randomUUID().toString())
      .withCurrency("USD")
      .withFiscalYearId(fiscalYearId);
    Invoice invoice2 = new Invoice()
      .withId(UUID.randomUUID().toString())
      .withCurrency("USD")
      .withFiscalYearId(fiscalYearId);
    InvoiceLine invoiceLine1 = new InvoiceLine()
      .withInvoiceId(invoice1.getId())
      .withTotal(100.0)
      .withInvoiceLineStatus(PAID)
      .withPoLineId(poLine1.getId());
    InvoiceLine invoiceLine2 = new InvoiceLine()
      .withInvoiceId(invoice2.getId())
      .withTotal(-500.0)
      .withInvoiceLineStatus(PAID)
      .withPoLineId(poLine2.getId());
    InvoiceLine invoiceLine3 = new InvoiceLine()
      .withInvoiceId(invoice1.getId())
      .withTotal(200.0)
      .withInvoiceLineStatus(PAID)
      .withPoLineId(UUID.randomUUID().toString());
    InvoiceLine invoiceLine4 = new InvoiceLine()
      .withInvoiceId(invoice2.getId())
      .withTotal(-100.0)
      .withInvoiceLineStatus(PAID);
    List<Invoice> invoices = List.of(invoice1, invoice2);
    CompositePurchaseOrder order = new CompositePurchaseOrder()
      .withId(UUID.randomUUID().toString())
      .withPoLines(List.of(poLine1, poLine2));
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);

    when(invoiceService.getInvoicesByOrderId(anyString(), any()))
      .thenReturn(Future.succeededFuture(invoices));
    when(invoiceLineService.getInvoiceLinesByInvoiceIdAndStatus(eq(invoice1.getId()), eq(PAID), any()))
      .thenReturn(Future.succeededFuture(List.of(invoiceLine1, invoiceLine3)));
    when(invoiceLineService.getInvoiceLinesByInvoiceIdAndStatus(eq(invoice2.getId()), eq(PAID), any()))
      .thenReturn(Future.succeededFuture(List.of(invoiceLine2, invoiceLine4)));
    when(transactionService.getTransactions(anyString(), any()))
      .thenReturn(Future.succeededFuture(List.of()));
    when(fiscalYearService.getCurrentFYForSeriesByFYId(anyString(), any()))
      .thenReturn(Future.succeededFuture(fiscalYearId));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(100.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(500.0, resultHolder.getOrder().getTotalCredited());
  }
}
