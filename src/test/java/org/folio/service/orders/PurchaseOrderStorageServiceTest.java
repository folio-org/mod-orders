package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;


@ExtendWith(VertxExtension.class)
public class PurchaseOrderStorageServiceTest {
  @InjectMocks
  private PurchaseOrderStorageService purchaseOrderStorageService;

  @Mock
  private RestClient restClientMock;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void successRetrievePurchaseOrdersByQuery() {
    String orderId = UUID.randomUUID().toString();
    List<PurchaseOrder> purchaseOrders = Collections.singletonList(new PurchaseOrder()
      .withId(orderId));

    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection()
      .withPurchaseOrders(purchaseOrders)
      .withTotalRecords(1);

    when(restClientMock.get(any(RequestEntry.class), any(), any()))
      .thenReturn(Future.succeededFuture(purchaseOrderCollection));

    String expectedQuery =  String.format("id==%s", orderId);
    PurchaseOrderCollection actOrders = purchaseOrderStorageService.getPurchaseOrders(expectedQuery, 0, Integer.MAX_VALUE, requestContext).result();

    verify(restClientMock).get(any(RequestEntry.class), eq(PurchaseOrderCollection.class), eq(requestContext));
    assertEquals(purchaseOrderCollection, actOrders);
  }

  @Test
  void successRetrievePurchaseOrdersByIds(VertxTestContext vertxTestContext) {
    String orderId = UUID.randomUUID().toString();
    List<PurchaseOrder> purchaseOrders = Collections.singletonList(new PurchaseOrder()
      .withId(orderId));

    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection()
      .withPurchaseOrders(purchaseOrders)
      .withTotalRecords(1);

    when(restClientMock.get(any(RequestEntry.class), any(), any()))
      .thenReturn(Future.succeededFuture(purchaseOrderCollection));

    var future = purchaseOrderStorageService.getPurchaseOrdersByIds(List.of(orderId), requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        verify(restClientMock).get(any(RequestEntry.class), eq(PurchaseOrderCollection.class), eq(requestContext));
        assertEquals(purchaseOrderCollection.getPurchaseOrders(), result.result());
        vertxTestContext.completeNow();
      });

  }

}
