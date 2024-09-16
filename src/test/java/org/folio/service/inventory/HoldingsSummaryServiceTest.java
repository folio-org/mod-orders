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

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.orders.HoldingsSummaryService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceStorageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class HoldingsSummaryServiceTest {

  @InjectMocks
  private HoldingsSummaryService holdingsSummaryService;

  @Mock
  private PurchaseOrderStorageService purchaseOrderStorageService;

  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;

  @Mock PieceStorageService pieceStorageService;

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
    var pieces = new JsonObject(getMockData(BASE_MOCK_DATA_PATH + "pieces/pieceRecords-d471d766-8dbb-4609-999a-02681dea6c22.json")).mapTo(PieceCollection.class);
    purchaseOrders.add(order);
    polines.add(line);

    when(purchaseOrderStorageService.getPurchaseOrdersByIds(any(), any()))
      .thenReturn(Future.succeededFuture(purchaseOrders));

    when(purchaseOrderLineService.getOrderLines(anyString(), anyInt(), anyInt(), any()))
      .thenReturn(Future.succeededFuture(polines));

    when(pieceStorageService.getAllPieces(anyString(), any()))
      .thenReturn(Future.succeededFuture(pieces));

    var hs = holdingsSummaryService.getHoldingsSummary(UUID.randomUUID().toString(), requestContext)
      .result();

    assertThat(hs.getHoldingSummaries().size(), greaterThan(0));

  }
}
