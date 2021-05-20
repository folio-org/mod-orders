package org.folio.service.inventory;

import static org.folio.TestUtils.getMockData;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import io.vertx.core.json.JsonObject;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.orders.HoldingsSummaryService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class HoldingsSummaryServiceTest {

  @InjectMocks
  private HoldingsSummaryService holdingsSummaryService;

  @Mock
  private PurchaseOrderService purchaseOrderService;

  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testGetHoldingSummaryById() throws IOException {
    List<PurchaseOrder> purchaseOrders = new ArrayList<>();
    List<PoLine> polines = new ArrayList<>();

    var order = new JsonObject(getMockData(BASE_MOCK_DATA_PATH + "purchase-orders/81_ongoing_pending.json")).mapTo(PurchaseOrder.class);
    var line = new JsonObject(getMockData(BASE_MOCK_DATA_PATH + "lines/81-1_pending_fomat-other.json")).mapTo(PoLine.class);
    purchaseOrders.add(order);
    polines.add(line);

    when(purchaseOrderService.getPurchaseOrdersByIds(any(), any()))
      .thenReturn(CompletableFuture.completedFuture(purchaseOrders));

    when(purchaseOrderLineService.getOrderLines(anyString(), anyInt(), anyInt(), any()))
      .thenReturn(CompletableFuture.completedFuture(polines));

    var hs = holdingsSummaryService.getHoldingsSummary(UUID.randomUUID().toString(), requestContext)
      .join();

    assertThat(hs.getHoldingSummaries().size(), greaterThan(0));

  }
}
