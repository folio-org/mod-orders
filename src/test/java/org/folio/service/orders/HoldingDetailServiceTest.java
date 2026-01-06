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
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PiecesDetail;
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
        verify(pieceStorageService, never()).getPiecesByHoldingIds(any(), any());
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
        verify(pieceStorageService, never()).getPiecesByHoldingIds(any(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailWithNoPieces(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        verify(pieceStorageService).getPiecesByHoldingIds(holdingIds, requestContext);
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
    var tenantId = "tenant-1";

    var piece = new Piece()
      .withId(pieceId)
      .withHoldingId(holdingId)
      .withItemId(itemId)
      .withReceivingTenantId(tenantId);

    var item = new JsonObject()
      .put("id", itemId)
      .put("holdingsRecordId", holdingId);

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
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
        assertNotNull(property.getPiecesDetailCollection());
        assertNotNull(property.getItemsDetailCollection());
        assertEquals(1, property.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(1, property.getItemsDetailCollection().getItemsDetail().size());

        verify(pieceStorageService).getPiecesByHoldingIds(holdingIds, requestContext);
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

    var piece1 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId1)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(tenant1);

    var piece2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId2)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(tenant2);

    var item1 = new JsonObject().put("id", UUID.randomUUID().toString());
    var item2 = new JsonObject().put("id", UUID.randomUUID().toString());

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
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

        verify(pieceStorageService).getPiecesByHoldingIds(holdingIds, requestContext);
        verify(inventoryItemManager).getItemsByHoldingId(eq(holdingId1), any(RequestContext.class));
        verify(inventoryItemManager).getItemsByHoldingId(eq(holdingId2), any(RequestContext.class));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailWithNullTenantId(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);

    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(null);

    var item = new JsonObject().put("id", UUID.randomUUID().toString());

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
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
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testPostOrdersHoldingDetailWithEmptyItems(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);

    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
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

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, piecesDetail, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holder = result.result();
        assertNotNull(holder);
        assertEquals(holdingId, holder.holdingId());
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
    var piecesDetail = Collections.<PiecesDetail>emptyList();

    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, piecesDetail, requestContext);

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
    var piecesDetail = List.of(
      new org.folio.rest.jaxrs.model.PiecesDetail()
        .withId(UUID.randomUUID().toString())
        .withTenantId(tenantId)
    );

    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(null));

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, piecesDetail, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holder = result.result();
        assertNotNull(holder);
        assertEquals(holdingId, holder.holdingId());
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

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, null, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holder = result.result();
        assertNotNull(holder);
        assertEquals(holdingId, holder.holdingId());
        assertEquals(0, holder.pieces().size()); // null piecesDetail should be empty list
        assertEquals(0, holder.items().size());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testGetHolderFutureFilterNullItems(VertxTestContext vertxTestContext) {
    var tenantId = "tenant-1";
    var holdingId = UUID.randomUUID().toString();
    var piecesDetail = Collections.<PiecesDetail>emptyList();

    var items = new ArrayList<JsonObject>();
    items.add(null);
    items.add(new JsonObject().put("id", UUID.randomUUID().toString()));
    items.add(null);

    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(items));

    var future = holdingDetailService.getHolderFuture(tenantId, holdingId, piecesDetail, requestContext);

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
    var holdingIds = List.of(UUID.randomUUID().toString());
    var errorMessage = "Storage service error";

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
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

    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withReceivingTenantId(tenantId);

    // Create a list with a null item
    var items = new ArrayList<JsonObject>();
    items.add(null);
    items.add(new JsonObject().put("id", UUID.randomUUID().toString()));

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(items));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
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

    // Create a piece with null holdingId which will be filtered out
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(null)  // This will be filtered
      .withReceivingTenantId("tenant-1");

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        // Since all pieces were filtered out (null holdingId), no holdings should be present
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

    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withReceivingTenantId("tenant-1");

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
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
  void testCreatePieceDetailWithMissingItemId(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);

    var piece1 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(null)  // null itemId
      .withReceivingTenantId("tenant-1");

    var piece2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece1, piece2)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
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

    var piece1 = new Piece()
      .withId(null)  // null piece id
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    var piece2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece1, piece2)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
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

    var validPiece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId("tenant-1");

    var pieces = new ArrayList<Piece>();
    pieces.add(null);  // null piece
    pieces.add(validPiece);
    pieces.add(null);  // another null piece

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
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

    // Create a piece with holdingId but it will be filtered out by null check
    var pieces = new ArrayList<Piece>();
    pieces.add(null);
    pieces.add(null);
    pieces.add(null);

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        // No valid pieces, so no holdings should be processed
        assertEquals(0, holdingDetailResults.getAdditionalProperties().size());
        verify(inventoryItemManager, never()).getItemsByHoldingId(anyString(), any());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePieceDetailPreservesAllFields(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var pieceId = UUID.randomUUID().toString();
    var itemId = UUID.randomUUID().toString();
    var tenantId = "tenant-1";

    var piece = new Piece()
      .withId(pieceId)
      .withHoldingId(holdingId)
      .withItemId(itemId)
      .withReceivingTenantId(tenantId)
      .withPoLineId(UUID.randomUUID().toString())  // Additional fields that should not affect createPieceDetail
      .withTitleId(UUID.randomUUID().toString());

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        var pieceDetail = property.getPiecesDetailCollection().getPiecesDetail().getFirst();

        // Verify the correct fields are mapped
        assertEquals(pieceId, pieceDetail.getId());
        assertEquals(itemId, pieceDetail.getItemId());
        assertEquals(tenantId, pieceDetail.getTenantId());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCreatePieceDetailWithManyPieces(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var tenantId = "tenant-1";

    // Create 100 pieces for the same holding
    var pieces = new ArrayList<Piece>();
    for (int i = 0; i < 100; i++) {
      pieces.add(new Piece()
        .withId(UUID.randomUUID().toString())
        .withHoldingId(holdingId)
        .withItemId(UUID.randomUUID().toString())
        .withReceivingTenantId(tenantId));
    }

    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(holdingId), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        // All 100 pieces should be included
        assertEquals(100, property.getPiecesDetailCollection().getPiecesDetail().size());
        vertxTestContext.completeNow();
      });
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("provideCreatePieceDetailTestCases")
  void testCreatePieceDetailEdgeCases(String testName, List<Piece> pieces, int expectedPieceDetailCount) {
    var holdingIds = pieces.isEmpty() ? Collections.<String>emptyList()
      : List.of(pieces.getFirst() != null && pieces.getFirst().getHoldingId() != null
        ? pieces.getFirst().getHoldingId()
        : UUID.randomUUID().toString());

    if (holdingIds.isEmpty()) {
      // For empty test cases
      var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);
      assertTrue(future.result().getAdditionalProperties().isEmpty());
      return;
    }

    when(pieceStorageService.getPiecesByHoldingIds(any(), any()))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(any(), any()))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);
    var result = future.result();

    if (expectedPieceDetailCount == 0) {
      assertTrue(result.getAdditionalProperties().isEmpty());
    } else {
      var holdingId = holdingIds.getFirst();
      var property = result.getAdditionalProperties().get(holdingId);
      assertEquals(expectedPieceDetailCount, property.getPiecesDetailCollection().getPiecesDetail().size());
    }
  }

  static Stream<Arguments> provideCreatePieceDetailTestCases() {
    var holdingId = UUID.randomUUID().toString();

    return Stream.of(
      Arguments.of(
        "Piece with all null fields except holdingId",
        List.of(new Piece().withHoldingId(holdingId).withReceivingTenantId("t1")),
        1
      ),
      Arguments.of(
        "Piece with empty string values",
        List.of(new Piece()
          .withId("")
          .withHoldingId(holdingId)
          .withItemId("")
          .withReceivingTenantId("t1")),
        1
      ),
      Arguments.of(
        "Multiple pieces with same data",
        List.of(
          new Piece().withId("id1").withHoldingId(holdingId).withItemId("item1").withReceivingTenantId("t1"),
          new Piece().withId("id1").withHoldingId(holdingId).withItemId("item1").withReceivingTenantId("t1")
        ),
        2
      ),
      Arguments.of(
        "Pieces with varying completeness",
        List.of(
          new Piece().withId("id1").withHoldingId(holdingId).withReceivingTenantId("t1"),
          new Piece().withId("id2").withHoldingId(holdingId).withItemId("item2").withReceivingTenantId("t1"),
          new Piece().withHoldingId(holdingId).withReceivingTenantId("t1")
        ),
        3
      )
    );
  }

  private static Piece createPiece(String holdingId, String tenantId) {
    return new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(tenantId);
  }
}

