package org.folio.service.orders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.folio.models.PoLineLocationsPair;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.service.inventory.InventoryHoldingManager;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;

public class PurchaseOrderLineServiceTest {
  @InjectMocks
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private InventoryHoldingManager inventoryHoldingManager;
  @Mock
  private RestClient restClientMock;
  @Mock
  private RequestContext requestContext;
  private AutoCloseable mockitoMocks;

  @BeforeEach
  public void initMocks() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void successRetrievePurchaseOrderLinesByQuery() {
    String orderLineId = UUID.randomUUID().toString();
    List<PoLine> purchaseOrderLines = Collections.singletonList(new PoLine()
      .withId(orderLineId));

    PoLineCollection expLines = new PoLineCollection()
      .withPoLines(purchaseOrderLines)
      .withTotalRecords(1);

    when(restClientMock.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(expLines));

    String expectedQuery =  String.format("id==%s", orderLineId);
    purchaseOrderLineService.getOrderLines(expectedQuery,  0, Integer.MAX_VALUE, requestContext);

    verify(restClientMock).get(any(RequestEntry.class), eq(PoLineCollection.class), eq(requestContext));
  }

  @Test
  void successUpdateSinglePurchaseOrderLine() {
    String orderLineId = UUID.randomUUID().toString();
    PoLine purchaseOrderLine = new PoLine().withId(orderLineId);

    when(restClientMock.put(any(RequestEntry.class), any(PoLine.class), eq(requestContext))).thenReturn(Future.succeededFuture(null));

    purchaseOrderLineService.saveOrderLine(purchaseOrderLine, requestContext);

    verify(restClientMock).put(any(RequestEntry.class), eq(purchaseOrderLine), eq(requestContext));
  }

  @Test
  void successUpdateOrderLinesWithUpdatingSpecificLocations() {
    String locationId = UUID.randomUUID().toString();
    String locationIdResolvedFromHolding = UUID.randomUUID().toString();
    PoLineLocationsPair pair = PoLineLocationsPair.of(new PoLine().withId(UUID.randomUUID().toString()), List.of(
      new Location().withLocationId(locationId),
      new Location().withHoldingId(UUID.randomUUID().toString())));
    when(inventoryHoldingManager.getLocationIdsFromHoldings(anyList(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of(locationIdResolvedFromHolding)));
    when(restClientMock.put(any(RequestEntry.class), any(PoLineCollection.class), eq(requestContext))).thenReturn(Future.succeededFuture(null));

    purchaseOrderLineService.saveOrderLinesWithLocations(List.of(pair), requestContext);

    verify(restClientMock).put(any(RequestEntry.class), any(PoLineCollection.class), eq(requestContext));
    verify(inventoryHoldingManager).getLocationIdsFromHoldings(anyList(), eq(requestContext));
    List<String> searchLocationIds = pair.getPoLine().getSearchLocationIds();
    assertEquals(2, searchLocationIds.size());
    assertTrue(searchLocationIds.contains(locationId));
    assertTrue(searchLocationIds.contains(locationIdResolvedFromHolding));
  }

  @Test
  void successUpdateOrderLinesWithoutUpdatingSearchLocations() {
    List<PoLine> purchaseOrderLines = List.of(
      new PoLine().withId(UUID.randomUUID().toString()),
      new PoLine().withId(UUID.randomUUID().toString())
        .withLocations(List.of(new Location().withHoldingId(UUID.randomUUID().toString()))));
    PoLineCollection poLineCollection = new PoLineCollection().withPoLines(purchaseOrderLines).withTotalRecords(purchaseOrderLines.size());
    when(restClientMock.put(any(RequestEntry.class), any(PoLineCollection.class), eq(requestContext))).thenReturn(Future.succeededFuture(null));

    purchaseOrderLineService.saveOrderLinesWithoutSearchLocationsUpdate(purchaseOrderLines, requestContext);

    verify(restClientMock).put(any(RequestEntry.class), eq(poLineCollection), eq(requestContext));
    verifyNoInteractions(inventoryHoldingManager);
  }

  @Test
  void successUpdatePurchaseOrderLinesWhenLocationHasHoldingIdPopulated() {
    String locationId = UUID.randomUUID().toString();
    List<PoLine> purchaseOrderLines = List.of(
      new PoLine().withId(UUID.randomUUID().toString()),
      new PoLine().withId(UUID.randomUUID().toString())
        .withLocations(List.of(new Location().withHoldingId(UUID.randomUUID().toString()))));
    PoLineCollection poLineCollection = new PoLineCollection().withPoLines(purchaseOrderLines).withTotalRecords(purchaseOrderLines.size());
    when(restClientMock.put(any(RequestEntry.class), any(PoLineCollection.class), eq(requestContext))).thenReturn(Future.succeededFuture(null));
    when(inventoryHoldingManager.getLocationIdsFromHoldings(anyList(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of(locationId)));

    purchaseOrderLineService.saveOrderLines(purchaseOrderLines, requestContext);

    verify(restClientMock).put(any(RequestEntry.class), eq(poLineCollection), eq(requestContext));
    verify(inventoryHoldingManager).getLocationIdsFromHoldings(anyList(), eq(requestContext));
    assertEquals(locationId, purchaseOrderLines.get(1).getSearchLocationIds().get(0));
  }

  @Test
  void successUpdatePurchaseOrderLinesWhenLocationHasLocationIdPopulated() {
    String locationId = UUID.randomUUID().toString();
    List<PoLine> purchaseOrderLines = List.of(
      new PoLine().withId(UUID.randomUUID().toString()),
      new PoLine().withId(UUID.randomUUID().toString())
        .withLocations(List.of(new Location().withLocationId(locationId))));
    PoLineCollection poLineCollection = new PoLineCollection().withPoLines(purchaseOrderLines).withTotalRecords(purchaseOrderLines.size());
    when(restClientMock.put(any(RequestEntry.class), any(PoLineCollection.class), eq(requestContext))).thenReturn(Future.succeededFuture(null));
    when(inventoryHoldingManager.getLocationIdsFromHoldings(anyList(), eq(requestContext)))
      .thenReturn(Future.succeededFuture(List.of(locationId)));

    purchaseOrderLineService.saveOrderLines(purchaseOrderLines, requestContext);

    verify(restClientMock).put(any(RequestEntry.class), eq(poLineCollection), eq(requestContext));
    verify(inventoryHoldingManager).getLocationIdsFromHoldings(anyList(), eq(requestContext));
    assertEquals(locationId, purchaseOrderLines.get(1).getSearchLocationIds().get(0));
  }
}
