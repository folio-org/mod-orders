package org.folio.service.orders;

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
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;

@CopilotGenerated
public class InvoicesTotalFieldsPopulateServiceTest {

  @InjectMocks
  private InvoicesTotalFieldsPopulateService populateService;

  @Mock
  private InvoiceService invoiceService;

  @Mock
  private InvoiceLineService invoiceLineService;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void shouldPopulateTotalFieldsWhenInvoicesAndInvoiceLinesExist() {
    Invoice invoice1 = new Invoice().withId(UUID.randomUUID().toString()).withCurrency("USD");
    Invoice invoice2 = new Invoice().withId(UUID.randomUUID().toString()).withCurrency("USD");
    InvoiceLine invoiceLine1 = new InvoiceLine().withInvoiceId(invoice1.getId()).withTotal(100.0);
    InvoiceLine invoiceLine2 = new InvoiceLine().withInvoiceId(invoice2.getId()).withTotal(200.0);
    InvoiceLine invoiceLine3 = new InvoiceLine().withInvoiceId(invoice2.getId()).withTotal(-500.0);
    List<Invoice> invoices = List.of(invoice1, invoice2);
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
      .withFiscalYear(new FiscalYear().withId(UUID.randomUUID().toString()));

    when(invoiceService.getInvoicesByOrderId(anyString(), any())).thenReturn(Future.succeededFuture(invoices));
    when(invoiceLineService.getInvoiceLinesByInvoiceId(eq(invoice1.getId()), any())).thenReturn(Future.succeededFuture(List.of(invoiceLine1)));
    when(invoiceLineService.getInvoiceLinesByInvoiceId(eq(invoice2.getId()), any())).thenReturn(Future.succeededFuture(List.of(invoiceLine2, invoiceLine3)));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(800.0, resultHolder.getOrder().getTotalEncumbered());
    assertEquals(300.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(500.0, resultHolder.getOrder().getTotalCredited());
  }

  @Test
  void shouldReturnZeroWhenNoInvoicesExist() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
      .withFiscalYear(new FiscalYear().withId(UUID.randomUUID().toString()));

    when(invoiceService.getInvoicesByOrderId(anyString(), any())).thenReturn(Future.succeededFuture(Collections.emptyList()));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(0.0, resultHolder.getOrder().getTotalEncumbered());
    assertEquals(0.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(0.0, resultHolder.getOrder().getTotalCredited());
  }

  @Test
  void shouldReturnZeroWhenNoInvoiceLinesExist() {
    Invoice invoice = new Invoice().withId(UUID.randomUUID().toString()).withCurrency("USD");
    List<Invoice> invoices = List.of(invoice);
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order)
      .withFiscalYear(new FiscalYear().withId(UUID.randomUUID().toString()));

    when(invoiceService.getInvoicesByOrderId(anyString(), any())).thenReturn(Future.succeededFuture(invoices));
    when(invoiceLineService.getInvoiceLinesByInvoiceId(anyString(), any())).thenReturn(Future.succeededFuture(Collections.emptyList()));

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(0.0, resultHolder.getOrder().getTotalEncumbered());
    assertEquals(0.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(0.0, resultHolder.getOrder().getTotalCredited());
  }

  @Test
  void shouldHandleNullFiscalYear() {
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositeOrderRetrieveHolder holder = new CompositeOrderRetrieveHolder(order);

    CompositeOrderRetrieveHolder resultHolder = populateService.populate(holder, requestContext).result();

    assertEquals(0.0, resultHolder.getOrder().getTotalEncumbered());
    assertEquals(0.0, resultHolder.getOrder().getTotalExpended());
    assertEquals(0.0, resultHolder.getOrder().getTotalCredited());
  }

}
