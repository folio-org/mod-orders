package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class PurchaseOrderServiceTest {
  @InjectMocks
  private PurchaseOrderService purchaseOrderService;

  @Mock
  private RestClient restClientMock;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  void successRetrievePurchaseOrdersByQuery() {
    String orderId = UUID.randomUUID().toString();
    List<PurchaseOrder> purchaseOrders = Collections.singletonList(new PurchaseOrder()
      .withId(orderId));

    PurchaseOrderCollection purchaseOrderCollection = new PurchaseOrderCollection()
      .withPurchaseOrders(purchaseOrders)
      .withTotalRecords(1);

    when(restClientMock.get(any(), any(), any()))
      .thenReturn(CompletableFuture.completedFuture(purchaseOrderCollection));

    String expectedQuery =  String.format("id==%s", orderId);
    PurchaseOrderCollection actOrders = purchaseOrderService.getPurchaseOrders(expectedQuery, Integer.MAX_VALUE, 0, requestContext).join();

    verify(restClientMock).get(any(), eq(requestContext), eq(PurchaseOrderCollection.class));
    assertEquals(purchaseOrderCollection, actOrders);
  }

}
