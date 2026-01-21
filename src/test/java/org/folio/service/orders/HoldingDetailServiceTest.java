package org.folio.service.orders;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.folio.CopilotGenerated;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PiecesDetail;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceStorageService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(VertxExtension.class)
@CopilotGenerated(model = "Claude Sonnet 4.5")
public class HoldingDetailServiceTest {

  @InjectMocks private HoldingDetailService holdingDetailService;
  @Mock private PurchaseOrderLineService purchaseOrderLineService;
  @Mock private PieceStorageService pieceStorageService;
  @Mock private InventoryItemManager inventoryItemManager;
  @Mock private RequestContext requestContext;
  @Mock private Context vertxContext;
  private AutoCloseable mocks;

  @BeforeEach
  void setUp() {
    mocks = MockitoAnnotations.openMocks(this);
    var okapiHeaders = new CaseInsensitiveMap<String, String>();
    okapiHeaders.put(XOkapiHeaders.TENANT, "test-tenant");
    okapiHeaders.put(XOkapiHeaders.USER_ID, "test-user");
    when(requestContext.getHeaders()).thenReturn(okapiHeaders);
    when(requestContext.getContext()).thenReturn(vertxContext);
  }

  @AfterEach
  void tearDown() throws Exception {
    if (mocks != null) {
      mocks.close();
    }
  }

  @Test
  void testPostOrdersHoldingDetailWithNullHoldingIds(VertxTestContext vertxTestContext) {
    var future = holdingDetailService.postOrdersHoldingDetail(null, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(0, holdingDetailResults.getAdditionalProperties().size());
        verify(purchaseOrderLineService, never()).getPoLinesByHoldingIds(any(), any());
        verify(pieceStorageService, never()).getPiecesByLineIdsByChunks(any(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailWithEmptyHoldingIds(VertxTestContext vertxTestContext) {
    var future = holdingDetailService.postOrdersHoldingDetail(Collections.emptyList(), requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(0, holdingDetailResults.getAdditionalProperties().size());
        verify(purchaseOrderLineService, never()).getPoLinesByHoldingIds(any(), any());
        verify(pieceStorageService, never()).getPiecesByLineIdsByChunks(any(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailWithNoPieces(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var poLine = createPoLine(poLineId, holdingId);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(1, holdingDetailResults.getAdditionalProperties().size());
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertEquals(1, property.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(0, property.getPiecesDetailCollection().getPiecesDetail().size());
        verify(purchaseOrderLineService).getPoLinesByHoldingIds(holdingIds, requestContext);
        verify(pieceStorageService).getPiecesByLineIdsByChunks(List.of(poLineId), requestContext);
        verify(inventoryItemManager, never()).getItemsByHoldingId(anyString(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailSuccess(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var pieceId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();
    var poLineId = UUID.randomUUID().toString();
    var tenantId = "tenant-1";

    var poLine = createPoLine(poLineId, holdingId);
    var piece = new Piece()
      .withId(pieceId)
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withItemId(itemId)
      .withReceivingTenantId(tenantId);

    var item = new JsonObject()
      .put("id", itemId)
      .put("holdingsRecordId", holdingId);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(1, holdingDetailResults.getAdditionalProperties().size());
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));

        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertNotNull(property.getPoLinesDetailCollection());
        assertNotNull(property.getPiecesDetailCollection());
        assertNotNull(property.getItemsDetailCollection());
        assertEquals(1, property.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(1, property.getPoLinesDetailCollection().getTotalRecords());
        assertEquals(poLineId, property.getPoLinesDetailCollection().getPoLinesDetail().getFirst().getId());
        assertEquals(1, property.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(1, property.getItemsDetailCollection().getItemsDetail().size());

        verify(purchaseOrderLineService).getPoLinesByHoldingIds(holdingIds, requestContext);
        verify(pieceStorageService).getPiecesByLineIdsByChunks(List.of(poLineId), requestContext);
        verify(inventoryItemManager).getItemsByHoldingId(eq(holdingId), any(RequestContext.class));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailMultipleHoldingsAndTenants(VertxTestContext vertxTestContext) {
    var holdingId1 = UUID.randomUUID().toString();
    var holdingId2 = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId1, holdingId2);
    var tenant1 = "tenant-1";
    var tenant2 = "tenant-2";
    var poLineId1 = UUID.randomUUID().toString();
    var poLineId2 = UUID.randomUUID().toString();

    var poLine1 = createPoLine(poLineId1, holdingId1);
    var poLine2 = createPoLine(poLineId2, holdingId2);

    var piece1 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId1)
      .withHoldingId(holdingId1)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(tenant1);

    var piece2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId2)
      .withHoldingId(holdingId2)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(tenant2);

    var item1 = new JsonObject().put("id", UUID.randomUUID().toString());
    var item2 = new JsonObject().put("id", UUID.randomUUID().toString());

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine1, poLine2)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId1, poLineId2), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece1, piece2)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item1)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId2), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item2)));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(2, holdingDetailResults.getAdditionalProperties().size());
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId1));
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId2));

        verify(purchaseOrderLineService).getPoLinesByHoldingIds(holdingIds, requestContext);
        verify(pieceStorageService).getPiecesByLineIdsByChunks(List.of(poLineId1, poLineId2), requestContext);
        verify(inventoryItemManager).getItemsByHoldingId(eq(holdingId1), any(RequestContext.class));
        verify(inventoryItemManager).getItemsByHoldingId(eq(holdingId2), any(RequestContext.class));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailWithNullTenantId(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(null);

    var item = new JsonObject().put("id", UUID.randomUUID().toString());

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(1, holdingDetailResults.getAdditionalProperties().size());
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailWithEmptyItems(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(1, holdingDetailResults.getAdditionalProperties().size());
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));

        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertEquals(1, property.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(0, property.getItemsDetailCollection().getItemsDetail().size());
        vertxTestContext.completeNow();
      });
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("provideGroupingTestCases")
  void testGroupPiecesByTenantIdAndHoldingId(String testName, List<Piece> pieces, int expectedTenantGroups,
                                             int expectedHoldingGroups) {
    var result = holdingDetailService.groupPiecesByTenantIdAndHoldingId(pieces);

    assertNotNull(result);
    assertEquals(expectedTenantGroups, result.size());

    var totalHoldingGroups = result.values().stream()
      .mapToInt(Map::size)
      .sum();
    assertEquals(expectedHoldingGroups, totalHoldingGroups);
  }

  static Stream<Arguments> provideGroupingTestCases() {
    var holdingId1 = UUID.randomUUID().toString();
    var holdingId2 = UUID.randomUUID().toString();
    var tenant1 = "tenant-1";
    var tenant2 = "tenant-2";

    return Stream.of(
      Arguments.of(
        "Empty list",
        Collections.emptyList(),
        0, 0
      ),
      Arguments.of(
        "Single piece with tenant and holding",
        List.of(createPiece(holdingId1, tenant1)),
        1, 1
      ),
      Arguments.of(
        "Multiple pieces same tenant and holding",
        List.of(
          createPiece(holdingId1, tenant1),
          createPiece(holdingId1, tenant1)
        ),
        1, 1
      ),
      Arguments.of(
        "Multiple pieces same tenant different holdings",
        List.of(
          createPiece(holdingId1, tenant1),
          createPiece(holdingId2, tenant1)
        ),
        1, 2
      ),
      Arguments.of(
        "Multiple pieces different tenants same holding",
        List.of(
          createPiece(holdingId1, tenant1),
          createPiece(holdingId1, tenant2)
        ),
        2, 2
      ),
      Arguments.of(
        "Multiple pieces different tenants different holdings",
        List.of(
          createPiece(holdingId1, tenant1),
          createPiece(holdingId2, tenant1),
          createPiece(holdingId1, tenant2),
          createPiece(holdingId2, tenant2)
        ),
        2, 4
      ),
      Arguments.of(
        "Pieces with null tenant - converted to empty string",
        List.of(
          createPiece(holdingId1, null),
          createPiece(holdingId1, null)
        ),
        1, 1
      ),
      Arguments.of(
        "Mixed null and non-null tenants",
        List.of(
          createPiece(holdingId1, tenant1),
          createPiece(holdingId1, null)
        ),
        2, 2
      )
    );
  }

  @Test
  void testGroupPiecesByTenantIdAndHoldingIdWithNullList() {
    var result = holdingDetailService.groupPiecesByTenantIdAndHoldingId(null);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testGroupPiecesByTenantIdAndHoldingIdFilterNullPieces() {
    var pieces = new ArrayList<Piece>();
    pieces.add(null);
    pieces.add(createPiece(UUID.randomUUID().toString(), "tenant-1"));
    pieces.add(null);

    var result = holdingDetailService.groupPiecesByTenantIdAndHoldingId(pieces);

    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  void testGroupPiecesByTenantIdAndHoldingIdFilterNullHoldingId() {
    var pieces = List.of(
      createPiece(null, "tenant-1"),
      createPiece(UUID.randomUUID().toString(), "tenant-1")
    );

    var result = holdingDetailService.groupPiecesByTenantIdAndHoldingId(pieces);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(1, result.get("tenant-1").size());
  }

  @Test
  void testGroupPiecesByTenantIdAndHoldingIdNullTenantIdConversion() {
    var holdingId = UUID.randomUUID().toString();
    var pieces = List.of(createPiece(holdingId, null));

    var result = holdingDetailService.groupPiecesByTenantIdAndHoldingId(pieces);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertTrue(result.containsKey(""));
    assertEquals(1, result.get("").size());
    assertTrue(result.get("").containsKey(holdingId));
  }

  @Test
  void testGetHolderFuture(VertxTestContext vertxTestContext) {
    var tenantId = "tenant-1";
    var holdingId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();
    var poLineId = UUID.randomUUID().toString();

    var poLinesDetails = List.of(
      new org.folio.rest.jaxrs.model.PoLinesDetail()
        .withId(poLineId)
    );

    var piecesDetail = List.of(
      new org.folio.rest.jaxrs.model.PiecesDetail()
        .withId(UUID.randomUUID().toString())
        .withItemId(itemId)
        .withTenantId(tenantId)
    );

    var item = new JsonObject()
      .put("id", itemId)
      .put("holdingsRecordId", holdingId);

    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, poLinesDetails, piecesDetail, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holder = result.result();
        assertNotNull(holder);
        assertEquals(holdingId, holder.holdingId());
        assertEquals(1, holder.poLines().size());
        assertEquals(poLineId, holder.poLines().getFirst().getId());
        assertEquals(1, holder.pieces().size());
        assertEquals(1, holder.items().size());
        assertEquals(itemId, holder.items().getFirst().getId());
        assertEquals(tenantId, holder.items().getFirst().getTenantId());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testGetHolderFutureWithEmptyTenantId(VertxTestContext vertxTestContext) {
    var tenantId = "";
    var holdingId = UUID.randomUUID().toString();
    var poLinesDetails = Collections.<org.folio.rest.jaxrs.model.PoLinesDetail>emptyList();
    var piecesDetail = Collections.<PiecesDetail>emptyList();

    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, poLinesDetails, piecesDetail, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        verify(inventoryItemManager).getItemsByHoldingId(eq(holdingId), any(RequestContext.class));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testGetHolderFutureWithNullItemsList(VertxTestContext vertxTestContext) {
    var tenantId = "tenant-1";
    var holdingId = UUID.randomUUID().toString();
    var poLinesDetails = List.of(
      new org.folio.rest.jaxrs.model.PoLinesDetail()
        .withId(UUID.randomUUID().toString())
    );
    var piecesDetail = List.of(
      new org.folio.rest.jaxrs.model.PiecesDetail()
        .withId(UUID.randomUUID().toString())
        .withTenantId(tenantId)
    );

    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(null));

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, poLinesDetails, piecesDetail, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holder = result.result();
        assertNotNull(holder);
        assertEquals(holdingId, holder.holdingId());
        assertEquals(1, holder.poLines().size());
        assertEquals(1, holder.pieces().size());
        assertEquals(0, holder.items().size()); // null items should return empty list
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testGetHolderFutureWithNullPieces(VertxTestContext vertxTestContext) {
    var tenantId = "tenant-1";
    var holdingId = UUID.randomUUID().toString();

    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, null, null, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holder = result.result();
        assertNotNull(holder);
        assertEquals(holdingId, holder.holdingId());
        assertEquals(0, holder.poLines().size()); // null poLinesDetails should be empty list
        assertEquals(0, holder.pieces().size()); // null piecesDetail should be empty list
        assertEquals(0, holder.items().size());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testGetHolderFutureFilterNullItems(VertxTestContext vertxTestContext) {
    var tenantId = "tenant-1";
    var holdingId = UUID.randomUUID().toString();
    var poLinesDetails = Collections.<org.folio.rest.jaxrs.model.PoLinesDetail>emptyList();
    var piecesDetail = Collections.<PiecesDetail>emptyList();

    var items = new ArrayList<JsonObject>();
    items.add(null);
    items.add(new JsonObject().put("id", UUID.randomUUID().toString()));
    items.add(null);

    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(items));

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, poLinesDetails, piecesDetail, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holder = result.result();
        assertEquals(1, holder.items().size());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailPieceStorageFailure(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var errorMessage = "Storage service error";

    var poLine = createPoLine(poLineId, holdingId);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.failedFuture(new RuntimeException(errorMessage)));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        assertTrue(result.cause().getMessage().contains(errorMessage));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreateItemDetailWithNullItem(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var tenantId = "tenant-1";
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withReceivingTenantId(tenantId);

    // Create a list with a null item
    var items = new ArrayList<JsonObject>();
    items.add(null);
    items.add(new JsonObject().put("id", UUID.randomUUID().toString()));

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(items));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        // Null items should be filtered out, only 1 valid item should remain
        assertEquals(1, property.getItemsDetailCollection().getItemsDetail().size());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailWithNullGroupedPieces(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    // Create a piece with null holdingId which will be filtered out
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(null)  // This will be filtered
      .withReceivingTenantId("tenant-1");

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        // Since piece has null holdingId, it gets filtered during grouping
        // No holders are created, so we get empty results (because pieces exist, just all filtered)
        assertEquals(0, holdingDetailResults.getAdditionalProperties().size());
        verify(inventoryItemManager, never()).getItemsByHoldingId(anyString(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailInventoryFailure(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var errorMessage = "Inventory service error";
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withReceivingTenantId("tenant-1");

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException(errorMessage)));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        assertNotNull(result.cause());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePieceDetailWithMissingItemId(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var piece1 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withItemId(null)  // null itemId
      .withReceivingTenantId("tenant-1");

    var piece2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece1, piece2)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertEquals(2, property.getPiecesDetailCollection().getPiecesDetail().size());
        // Verify both pieces are included even with null itemId
        var piecesDetail = property.getPiecesDetailCollection().getPiecesDetail();
        assertTrue(piecesDetail.stream().anyMatch(pd -> pd.getItemId() == null));
        assertTrue(piecesDetail.stream().anyMatch(pd -> pd.getItemId() != null));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePieceDetailWithMissingPieceId(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var piece1 = new Piece()
      .withId(null)  // null piece id
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    var piece2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece1, piece2)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertEquals(2, property.getPiecesDetailCollection().getPiecesDetail().size());
        // Verify both pieces are included even with null id
        var piecesDetail = property.getPiecesDetailCollection().getPiecesDetail();
        assertTrue(piecesDetail.stream().anyMatch(pd -> pd.getId() == null));
        assertTrue(piecesDetail.stream().anyMatch(pd -> pd.getId() != null));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePieceDetailWithMixedNullPieces(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var validPiece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    var pieces = new ArrayList<Piece>();
    pieces.add(null);  // null piece
    pieces.add(validPiece);
    pieces.add(null);  // another null piece

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        // Only the valid piece should be included
        assertEquals(1, property.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(validPiece.getId(), property.getPiecesDetailCollection().getPiecesDetail().getFirst().getId());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePieceDetailWithAllNullPieces(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);

    var poLine = createPoLine(UUID.randomUUID().toString(), holdingId);
    // Create a list with only null pieces
    var pieces = new ArrayList<Piece>();
    pieces.add(null);
    pieces.add(null);
    pieces.add(null);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLine.getId()), requestContext))
      .thenReturn(Future.succeededFuture(pieces));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        // All pieces are null, after filtering we get empty grouping, so no holders created
        assertEquals(0, holdingDetailResults.getAdditionalProperties().size());
        verify(inventoryItemManager, never()).getItemsByHoldingId(anyString(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePoLineDetailWithSinglePiece(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var tenantId = "tenant-1";

    var poLine = createPoLine(poLineId, holdingId);
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withReceivingTenantId(tenantId);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var property = result.result().getAdditionalProperties().get(holdingId);
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        assertEquals(1, poLinesDetail.size());
        assertEquals(poLineId, poLinesDetail.getFirst().getId());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePoLineDetailWithDuplicatePoLineIdSameTenant(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var tenantId = "tenant-1";

    var poLine = createPoLine(poLineId, holdingId);
    var pieces = List.of(
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId)
    );

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var property = result.result().getAdditionalProperties().get(holdingId);
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        // Should be deduplicated to 1 entry
        assertEquals(1, poLinesDetail.size());
        assertEquals(poLineId, poLinesDetail.getFirst().getId());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePoLineDetailWithSamePoLineIdDifferentTenants(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var tenant1 = "tenant-1";
    var tenant2 = "tenant-2";

    var poLine = createPoLine(poLineId, holdingId);
    var pieces = List.of(
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenant1),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenant2)
    );

    // Need to setup expectations for both tenants
    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var additionalProps = result.result().getAdditionalProperties();
        // Since pieces have different tenants, they're grouped separately
        // But both use same holdingId as key, so the later one overwrites
        // Actually, each creates its own holder, so we get 1 entry with the last processed
        assertEquals(1, additionalProps.size());
        assertTrue(additionalProps.containsKey(holdingId));

        // The holder will have 1 poLine entry (since within each tenant group, same poLineId is deduplicated)
        var poLinesDetail = additionalProps.get(holdingId).getPoLinesDetailCollection().getPoLinesDetail();
        assertEquals(1, poLinesDetail.size());
        assertEquals(poLineId, poLinesDetail.getFirst().getId());

        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePoLineDetailWithMultipleDifferentPoLines(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId1 = UUID.randomUUID().toString();
    var poLineId2 = UUID.randomUUID().toString();
    var poLineId3 = UUID.randomUUID().toString();
    var tenantId = "tenant-1";

    var poLine1 = createPoLine(poLineId1, holdingId);
    var poLine2 = createPoLine(poLineId2, holdingId);
    var poLine3 = createPoLine(poLineId3, holdingId);
    var pieces = List.of(
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId1)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId2)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId3)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId)
    );

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine1, poLine2, poLine3)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId1, poLineId2, poLineId3), requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var property = result.result().getAdditionalProperties().get(holdingId);
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        assertEquals(3, poLinesDetail.size());
        assertTrue(poLinesDetail.stream().anyMatch(pl -> pl.getId().equals(poLineId1)));
        assertTrue(poLinesDetail.stream().anyMatch(pl -> pl.getId().equals(poLineId2)));
        assertTrue(poLinesDetail.stream().anyMatch(pl -> pl.getId().equals(poLineId3)));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePoLineDetailWithNullPoLineId(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var tenantId = "tenant-1";
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var pieces = List.of(
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(null)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId)
    );

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var additionalProps = result.result().getAdditionalProperties();
        assertTrue(additionalProps.containsKey(holdingId));
        var property = additionalProps.get(holdingId);
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        // PoLines are created from pieces using createPoLineDetailFromPieces
        // 1 piece with null poLineId + 1 piece with poLineId = 2 PoLinesDetail (one null, one with id)
        assertEquals(2, poLinesDetail.size());
        assertTrue(poLinesDetail.stream().anyMatch(pl -> pl.getId() == null));
        assertTrue(poLinesDetail.stream().anyMatch(pl -> poLineId.equals(pl.getId())));
        // Both pieces should be included
        assertEquals(2, property.getPiecesDetailCollection().getPiecesDetail().size());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePoLineDetailWithNullTenantId(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();

    var poLine = createPoLine(poLineId, holdingId);
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withReceivingTenantId(null);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var additionalProps = result.result().getAdditionalProperties();
        assertEquals(1, additionalProps.size());
        var property = additionalProps.values().iterator().next();
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        assertEquals(1, poLinesDetail.size());
        assertEquals(poLineId, poLinesDetail.getFirst().getId());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePoLineDetailComplexScenario(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId1 = UUID.randomUUID().toString();
    var poLineId2 = UUID.randomUUID().toString();
    var tenant1 = "tenant-1";
    var tenant2 = "tenant-2";

    var poLine1 = createPoLine(poLineId1, holdingId);
    var poLine2 = createPoLine(poLineId2, holdingId);
    var pieces = List.of(
      // poLineId1 with tenant1 - should appear once
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId1)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenant1),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId1)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenant1),
      // poLineId1 with tenant2 - should appear once
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId1)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenant2),
      // poLineId2 with tenant1 - should appear once
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId2)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenant1),
      // poLineId2 with tenant2 - should appear once
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId2)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenant2),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId2)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenant2)
    );

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine1, poLine2)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId1, poLineId2), requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var additionalProps = result.result().getAdditionalProperties();
        // Different tenants get processed separately, but use same holdingId key
        // So we get 1 entry (last one processed overwrites)
        assertEquals(1, additionalProps.size());
        assertTrue(additionalProps.containsKey(holdingId));

        // Within the final holder, should have deduplicated poLines for that tenant
        var poLinesDetail = additionalProps.get(holdingId).getPoLinesDetailCollection().getPoLinesDetail();
        // Should have 2 distinct poLineIds for whichever tenant was processed last
        assertEquals(2, poLinesDetail.size());

        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePoLineDetailWithMixedNullPoLineIds(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var tenantId = "tenant-1";

    var poLine = createPoLine(poLineId, holdingId);
    var pieces = List.of(
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(null)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(null)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId)
    );

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId), requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var additionalProps = result.result().getAdditionalProperties();
        assertTrue(additionalProps.containsKey(holdingId));
        var property = additionalProps.get(holdingId);
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        // PoLines created from pieces: 2 null poLineIds (distinct = 1) + 1 poLineId = 2 total
        assertEquals(2, poLinesDetail.size());
        assertEquals(1, poLinesDetail.stream().filter(pl -> pl.getId() == null).count());
        assertEquals(1, poLinesDetail.stream().filter(pl -> poLineId.equals(pl.getId())).count());
        // All 3 pieces should be included
        assertEquals(3, property.getPiecesDetailCollection().getPiecesDetail().size());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPoLinesDetailCollectionTotalRecordsAccuracy(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId1 = UUID.randomUUID().toString();
    var poLineId2 = UUID.randomUUID().toString();
    var tenantId = "tenant-1";

    var poLine1 = createPoLine(poLineId1, holdingId);
    var poLine2 = createPoLine(poLineId2, holdingId);
    var pieces = List.of(
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId1)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId),
      new Piece()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId2)
        .withHoldingId(holdingId)
        .withReceivingTenantId(tenantId)
    );

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine1, poLine2)));
    when(pieceStorageService.getPiecesByLineIdsByChunks(List.of(poLineId1, poLineId2), requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var property = result.result().getAdditionalProperties().get(holdingId);
        var poLinesDetailCollection = property.getPoLinesDetailCollection();
        assertEquals(2, poLinesDetailCollection.getTotalRecords());
        assertEquals(2, poLinesDetailCollection.getPoLinesDetail().size());
        vertxTestContext.completeNow();
      });
  }

  private static Piece createPiece(String holdingId, String tenantId) {
    return new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(tenantId);
  }

  private static PoLine createPoLine(String poLineId, String holdingId) {
    return new PoLine()
      .withId(poLineId)
      .withLocations(List.of(new Location().withHoldingId(holdingId)));
  }
}

