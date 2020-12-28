package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class PurchaseOrderLineServiceTest {
  @InjectMocks
  private PurchaseOrderLineService purchaseOrderLineService;

  @Mock
  private RestClient restClientMock;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  void successRetrievePurchaseOrderLinesByQuery() {
    String orderLineId = UUID.randomUUID().toString();
    List<PoLine> purchaseOrderLines = Collections.singletonList(new PoLine()
      .withId(orderLineId));

    PoLineCollection expLines = new PoLineCollection()
      .withPoLines(purchaseOrderLines)
      .withTotalRecords(1);

    when(restClientMock.get(anyString(), anyInt(), anyInt(), any(), any())).thenReturn(CompletableFuture.completedFuture(expLines));

    String expectedQuery =  String.format("id==%s", orderLineId);
    List<PoLine> actLines = purchaseOrderLineService.getOrderLines(expectedQuery,  0, Integer.MAX_VALUE, requestContext).join();

    verify(restClientMock).get(eq(expectedQuery), eq(0), eq(Integer.MAX_VALUE), eq(requestContext), eq(PoLineCollection.class));
    assertEquals(purchaseOrderLines, actLines);
  }


  @Test
  void successUpdateSinglePurchaseOrderLine() {
    String orderLineId = UUID.randomUUID().toString();
    PoLine purchaseOrderLine = new PoLine().withId(orderLineId);

    when(restClientMock.put(anyString(), any(), any())).thenReturn(CompletableFuture.completedFuture(null));

    purchaseOrderLineService.updateOrderLine(purchaseOrderLine, requestContext).join();

    verify(restClientMock).put(eq(orderLineId), eq(purchaseOrderLine), eq(requestContext));
  }

  @Test
  void successUpdatePurchaseOrderLines() {
    String orderLineId1 = UUID.randomUUID().toString();
    String orderLineId2 = UUID.randomUUID().toString();
    List<PoLine> purchaseOrderLines = List.of(new PoLine().withId(orderLineId1), new PoLine().withId(orderLineId2));

    when(restClientMock.put(anyString(), any(), any())).thenReturn(CompletableFuture.completedFuture(null));

    purchaseOrderLineService.updateOrderLines(purchaseOrderLines, requestContext).join();

    verify(restClientMock).put(eq(orderLineId1), eq(purchaseOrderLines.get(0)), eq(requestContext));
    verify(restClientMock).put(eq(orderLineId2), eq(purchaseOrderLines.get(1)), eq(requestContext));
  }
}
