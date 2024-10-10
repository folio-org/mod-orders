package org.folio.service.invoice;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.invoice.Adjustment;
import org.folio.rest.acq.model.invoice.FundDistribution;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
@CopilotGenerated(partiallyGenerated = true)
public class InvoiceLineServiceTest {

  @InjectMocks
  private InvoiceLineService invoiceLineService;

  @Mock
  private RequestContext requestContextMock;
  @Mock
  private RestClient restClient;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void shouldRemoveEncumbranceLinks(VertxTestContext vertxTestContext) {
    //Given
    String poLineId1 = UUID.randomUUID().toString();
    String poLineId2 = UUID.randomUUID().toString();
    String encumbrance1Id = UUID.randomUUID().toString();
    String encumbrance2Id = UUID.randomUUID().toString();
    String encumbrance3Id = UUID.randomUUID().toString();
    List<String> transactionIds = List.of(encumbrance1Id, encumbrance2Id, encumbrance3Id);

    String invoiceLineId1 = UUID.randomUUID().toString();
    InvoiceLine invoiceLine1 = new InvoiceLine()
      .withId(invoiceLineId1)
      .withPoLineId(poLineId1)
      .withInvoiceLineStatus(InvoiceLineStatus.OPEN)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance1Id)))
      .withAdjustments(List.of(new Adjustment().withFundDistributions(List.of(
        new org.folio.rest.acq.model.invoice.FundDistribution().withEncumbrance(encumbrance2Id)))));
    String invoiceLineId2 = UUID.randomUUID().toString();
    InvoiceLine invoiceLine2 = new InvoiceLine()
      .withId(invoiceLineId2)
      .withPoLineId(poLineId2)
      .withInvoiceLineStatus(InvoiceLineStatus.REVIEWED)
      .withAdjustments(List.of(new Adjustment().withFundDistributions(List.of(
        new org.folio.rest.acq.model.invoice.FundDistribution().withEncumbrance(encumbrance3Id)))));
    List<InvoiceLine> invoiceLines = List.of(invoiceLine1, invoiceLine2);
    InvoiceLine expectedInvoiceLine1 = JsonObject.mapFrom(invoiceLine1).mapTo(InvoiceLine.class);
    expectedInvoiceLine1.getFundDistributions().get(0).setEncumbrance(null);
    expectedInvoiceLine1.getAdjustments().get(0).getFundDistributions().get(0).setEncumbrance(null);
    InvoiceLine expectedInvoiceLine2 = JsonObject.mapFrom(invoiceLine2).mapTo(InvoiceLine.class);
    expectedInvoiceLine2.getAdjustments().get(0).getFundDistributions().get(0).setEncumbrance(null);

    when(restClient.put(any(RequestEntry.class), any(InvoiceLine.class), eq(requestContextMock)))
      .thenReturn(Future.succeededFuture(null));
    when(requestContextMock.getContext()).thenReturn(Vertx.vertx().getOrCreateContext());

    //When
    Future<Void> future = invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds, requestContextMock);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        //Then
        verify(restClient, times(1)).put(
          any(RequestEntry.class),
          eq(expectedInvoiceLine1),
          eq(requestContextMock));
        verify(restClient, times(1)).put(
          any(RequestEntry.class),
          eq(expectedInvoiceLine2),
          eq(requestContextMock));
        vertxTestContext.completeNow();
      });

  }

  @Test
  void shouldGetInvoiceLinesByOrderLineIds(VertxTestContext vertxTestContext) {
    //Given
    List<String> poLineIds = List.of("1", "2");
    when(restClient.get(any(RequestEntry.class), eq(InvoiceLineCollection.class), eq(requestContextMock)))
      .thenReturn(Future.succeededFuture(new InvoiceLineCollection()));
    //When
    Future<List<InvoiceLine>> future = invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContextMock);
    //Then
    vertxTestContext.assertComplete(future)
      .onComplete(res-> vertxTestContext.completeNow());

  }

  @Test
  void removeEncumbranceLinks_shouldUpdateEncumbranceLinks_whenEncumbrancesMatch() {
    List<InvoiceLine> invoiceLines = new ArrayList<>();
    InvoiceLine invoiceLine = new InvoiceLine().withId("1").withInvoiceLineStatus(InvoiceLineStatus.OPEN)
      .withFundDistributions(List.of(new FundDistribution().withEncumbrance("enc1")));
    invoiceLines.add(invoiceLine);
    List<String> transactionIds = List.of("enc1");
    RequestContext requestContext = mock(RequestContext.class);
    when(restClient.put(any(RequestEntry.class), any(InvoiceLine.class), eq(requestContext))).thenReturn(Future.succeededFuture());

    Future<Void> result = invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds, requestContext);

    assertTrue(result.succeeded());
    assertNull(invoiceLine.getFundDistributions().get(0).getEncumbrance());
  }

  @Test
  void removeEncumbranceLinks_shouldHandleEmptyInvoiceLines() {
    List<InvoiceLine> invoiceLines = new ArrayList<>();
    List<String> transactionIds = List.of("enc1");
    RequestContext requestContext = mock(RequestContext.class);

    Future<Void> result = invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds, requestContext);

    assertTrue(result.succeeded());
    verifyNoInteractions(restClient);
  }

  @Test
  void removeEncumbranceLinks_shouldNotUpdateEncumbranceLinks_whenNoEncumbrancesMatch() {
    List<InvoiceLine> invoiceLines = new ArrayList<>();
    InvoiceLine invoiceLine = new InvoiceLine().withId("1").withInvoiceLineStatus(InvoiceLineStatus.OPEN)
      .withFundDistributions(List.of(new FundDistribution().withEncumbrance("enc2")));
    invoiceLines.add(invoiceLine);
    List<String> transactionIds = List.of("enc1");
    RequestContext requestContext = mock(RequestContext.class);

    Future<Void> result = invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds, requestContext);

    assertTrue(result.succeeded());
    assertEquals("enc2", invoiceLine.getFundDistributions().get(0).getEncumbrance());
    verifyNoInteractions(restClient);
  }

  @Test
  void removeEncumbranceLinks_shouldHandleEmptyTransactionIds() {
    List<InvoiceLine> invoiceLines = new ArrayList<>();
    InvoiceLine invoiceLine = new InvoiceLine().withId("1").withInvoiceLineStatus(InvoiceLineStatus.OPEN)
      .withFundDistributions(List.of(new FundDistribution().withEncumbrance("enc1")));
    invoiceLines.add(invoiceLine);
    List<String> transactionIds = new ArrayList<>();
    RequestContext requestContext = mock(RequestContext.class);

    Future<Void> result = invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds, requestContext);

    assertTrue(result.succeeded());
    assertEquals("enc1", invoiceLine.getFundDistributions().get(0).getEncumbrance());
    verifyNoInteractions(restClient);
  }

  @Test
  void removeEncumbranceLinks_shouldNotUpdateEncumbranceLinks_whenInvoiceLineStatusNotEditable() {
    List<InvoiceLine> invoiceLines = new ArrayList<>();
    InvoiceLine invoiceLine = new InvoiceLine().withId("1").withInvoiceLineStatus(InvoiceLineStatus.PAID)
      .withFundDistributions(List.of(new FundDistribution().withEncumbrance("enc1")));
    invoiceLines.add(invoiceLine);
    List<String> transactionIds = List.of("enc1");
    RequestContext requestContext = mock(RequestContext.class);

    Future<Void> result = invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds, requestContext);

    assertTrue(result.succeeded());
    assertEquals("enc1", invoiceLine.getFundDistributions().get(0).getEncumbrance());
  }
}
