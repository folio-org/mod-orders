package org.folio.service.invoice;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.acq.model.invoice.Adjustment;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class InvoiceLineServiceTest {

  private static final Logger logger = LogManager.getLogger();

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
  void shouldRemoveEncumbranceLinks() {
    //Given
    String poLineId = UUID.randomUUID().toString();
    String encumbrance1Id = UUID.randomUUID().toString();
    String encumbrance2Id = UUID.randomUUID().toString();
    List<String> transactionIds = List.of(encumbrance1Id, encumbrance2Id);

    String invoiceLineId = UUID.randomUUID().toString();
    InvoiceLine invoiceLine = new InvoiceLine()
      .withId(invoiceLineId)
      .withPoLineId(poLineId)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance1Id)))
      .withAdjustments(List.of(new Adjustment().withFundDistributions(List.of(
        new org.folio.rest.acq.model.invoice.FundDistribution().withEncumbrance(encumbrance2Id)))));
    List<InvoiceLine> invoiceLines = List.of(invoiceLine);
    InvoiceLine expectedInvoiceLine = JsonObject.mapFrom(invoiceLine).mapTo(InvoiceLine.class);
    expectedInvoiceLine.getFundDistributions().get(0).setEncumbrance(null);
    expectedInvoiceLine.getAdjustments().get(0).getFundDistributions().get(0).setEncumbrance(null);

    when(restClient.put(any(RequestEntry.class), any(InvoiceLine.class), eq(requestContextMock)))
      .thenReturn(CompletableFuture.completedFuture(null));
    when(requestContextMock.getContext()).thenReturn(Vertx.vertx().getOrCreateContext());

    //When
    CompletableFuture<Void> result = invoiceLineService.removeEncumbranceLinks(invoiceLines, transactionIds, requestContextMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    //Then
    verify(restClient, times(1)).put(
      argThat(requestEntry -> invoiceLineId.equals(requestEntry.getPathParams().get("id"))),
      eq(expectedInvoiceLine),
      eq(requestContextMock));
  }

  @Test
  void shouldGetInvoiceLinesByOrderLineIds() {
    //Given
    List<String> poLineIds = List.of("1", "2");
    when(restClient.get(any(RequestEntry.class), eq(requestContextMock), eq(InvoiceLineCollection.class)))
      .thenReturn(CompletableFuture.completedFuture(new InvoiceLineCollection()));
    //When
    CompletableFuture<List<InvoiceLine>> result = invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContextMock);
    assertFalse(result.isCompletedExceptionally());
    result.join();
    //Then
    verify(restClient, times(1)).get(
      argThat(requestEntry -> encodeQuery("poLineId == (\"1\" OR \"2\")", logger)
        .equals(requestEntry.getQueryParams().get("query"))),
      eq(requestContextMock),
      eq(InvoiceLineCollection.class));
  }
}
