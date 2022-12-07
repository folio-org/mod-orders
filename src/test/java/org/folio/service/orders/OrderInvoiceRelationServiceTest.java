package org.folio.service.orders;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.folio.rest.acq.model.OrderInvoiceRelationship;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.invoice.InvoiceLineService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class OrderInvoiceRelationServiceTest {

  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  private static final String poLineIdNotConnectedToInvoice = "2d4405bb-f79d-4f70-b640-ab381e966b47";
  private static final String poLineIdConnectedToInvoice = "0b94dd8e-afa8-4366-a947-c080d5147828";
  private static final String invoiceId = "f7d8f496-a5f1-4c12-8481-f633ad594663";

  @InjectMocks
  public OrderInvoiceRelationService orderInvoiceRelationService;
  @Mock
  public RestClient restClient;
  @Mock
  public InvoiceLineService invoiceLineService;
  private RequestContext requestContext;
  @Mock
  private Map<String, String> okapiHeadersMock;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
    requestContext = new RequestContext(Vertx.vertx().getOrCreateContext(), okapiHeadersMock) ;
  }

  @Test
  void testShouldThrowExceptionWhenOrderLineLinkedToInvoice(VertxTestContext vertxTestContext) {
    // GIVEN
    OrderInvoiceRelationshipCollection oirCollection = new OrderInvoiceRelationshipCollection()
      .withOrderInvoiceRelationships(Collections.singletonList(new OrderInvoiceRelationship()))
      .withTotalRecords(1);

    doReturn(succeededFuture(oirCollection)).when(restClient).get(any(RequestEntry.class), any(), any());

    PoLine poLineLinkedToInvoice = new PoLine().withId(poLineIdConnectedToInvoice);
    InvoiceLine invoiceLine1 = new InvoiceLine().withInvoiceId(invoiceId).withPoLineId(poLineIdConnectedToInvoice);
    InvoiceLine invoiceLine2 = new InvoiceLine().withInvoiceId(invoiceId).withPoLineId(UUID.randomUUID().toString());
    List<InvoiceLine> invoiceLines = Arrays.asList(invoiceLine1, invoiceLine2);
    InvoiceLineCollection invoiceLineCollection = new InvoiceLineCollection();
    invoiceLineCollection.setInvoiceLines(invoiceLines);

    // WHEN
    when(invoiceLineService.getInvoiceLinesByOrderLineId(eq(poLineIdConnectedToInvoice), any())).thenReturn(succeededFuture(invoiceLines));
    Future<Void> future = orderInvoiceRelationService.checkOrderPOLineLinkedToInvoiceLine(poLineLinkedToInvoice, requestContext);

    // THEN
    vertxTestContext.assertFailure(future)
      .onComplete(exception -> {
        HttpException httpException = (HttpException) exception.cause();
        assertEquals(ErrorCodes.ORDER_RELATES_TO_INVOICE.getDescription(), httpException.getMessage());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testShouldDeletePoLIneWhenOrderLineIsNotLinkedToInvoice(VertxTestContext vertxTestContext) {
    OrderInvoiceRelationshipCollection oirCollection = new OrderInvoiceRelationshipCollection()
      .withOrderInvoiceRelationships(Collections.singletonList(new OrderInvoiceRelationship()))
      .withTotalRecords(0);

    doReturn(succeededFuture(oirCollection)).when(restClient).get(any(RequestEntry.class), any(), any());

    PoLine poLineNotLinkedToInvoice = new PoLine().withId(poLineIdNotConnectedToInvoice);
    // WHEN
    when(invoiceLineService.getInvoiceLinesByOrderLineId(eq(poLineIdNotConnectedToInvoice), any())).thenReturn(succeededFuture(
      Collections.emptyList()));
    var future = orderInvoiceRelationService.checkOrderPOLineLinkedToInvoiceLine(poLineNotLinkedToInvoice, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }
}
