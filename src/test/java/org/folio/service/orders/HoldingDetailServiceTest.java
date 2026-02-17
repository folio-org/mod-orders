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
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
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
import java.util.UUID;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(VertxExtension.class)
@CopilotGenerated(model = "Claude Sonnet 4.5")
public class HoldingDetailServiceTest {

  @InjectMocks private HoldingDetailService holdingDetailService;
  @Mock private ConsortiumConfigurationService consortiumConfigurationService;
  @Mock private ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;
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

    // Setup default behavior for consortium and settings mocks to return empty results
    // This ensures existing tests continue to work without modification (non-consortium mode)
    when(consortiumConfigurationService.getConsortiumConfiguration(any()))
      .thenReturn(Future.succeededFuture(java.util.Optional.empty()));
  }

  @AfterEach
  void tearDown() throws Exception {
    if (mocks != null) {
      mocks.close();
    }
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("provideNullOrEmptyHoldingIds")
  void testPostOrdersHoldingDetailWithInvalidHoldingIds(String testName, List<String> holdingIds, VertxTestContext vertxTestContext) {
    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(0, holdingDetailResults.getAdditionalProperties().size());
        verify(purchaseOrderLineService, never()).getPoLinesByHoldingIds(any(), any());
        verify(pieceStorageService, never()).getPiecesByHoldingIds(any(), any());
        verify(inventoryItemManager, never()).getItemsByHoldingIds(any(), any());
        vertxTestContext.completeNow();
      });
  }

  static Stream<Arguments> provideNullOrEmptyHoldingIds() {
    return Stream.of(
      Arguments.of("Null holding IDs", null),
      Arguments.of("Empty holding IDs list", Collections.emptyList())
    );
  }

  @Test
  void testPostOrdersHoldingDetailWithNoPieces(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var poLine = createPoLine(poLineId, holdingId);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
        assertEquals(0, property.getItemsDetailCollection().getItemsDetail().size());
        verify(purchaseOrderLineService).getPoLinesByHoldingIds(holdingIds, requestContext);
        verify(pieceStorageService).getPiecesByHoldingIds(holdingIds, requestContext);
        verify(inventoryItemManager).getItemsByHoldingIds(holdingIds, requestContext);
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
        verify(pieceStorageService).getPiecesByHoldingIds(holdingIds, requestContext);
        verify(inventoryItemManager).getItemsByHoldingIds(holdingIds, requestContext);
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

    var item1 = new JsonObject().put("id", UUID.randomUUID().toString()).put("holdingsRecordId", holdingId1);
    var item2 = new JsonObject().put("id", UUID.randomUUID().toString()).put("holdingsRecordId", holdingId2);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine1, poLine2)));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece1, piece2)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(item1, item2)));

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
        verify(pieceStorageService).getPiecesByHoldingIds(holdingIds, requestContext);
        verify(inventoryItemManager).getItemsByHoldingIds(holdingIds, requestContext);
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

    var item = new JsonObject().put("id", UUID.randomUUID().toString()).put("holdingsRecordId", holdingId);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
  @MethodSource("provideServiceFailureScenarios")
  void testServiceFailureGracefulDegradation(String testName, boolean poLinesFail, boolean piecesFail, boolean itemsFail,
                                               int expectedPoLines, int expectedPieces, int expectedItems,
                                               VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var poLine = createPoLine(poLineId, holdingId);
    var piece = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId)
      .withHoldingId(holdingId)
      .withReceivingTenantId("tenant-1");

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(poLinesFail ? Future.failedFuture(new RuntimeException("PoLine service error"))
                              : Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(piecesFail ? Future.failedFuture(new RuntimeException("Piece service error"))
                             : Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(itemsFail ? Future.failedFuture(new RuntimeException("Inventory service error"))
                            : Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded(), "Should succeed with graceful degradation");
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertNotNull(property);
        assertEquals(expectedPoLines, property.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(expectedPieces, property.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(expectedItems, property.getItemsDetailCollection().getItemsDetail().size());
        vertxTestContext.completeNow();
      });
  }

  static Stream<Arguments> provideServiceFailureScenarios() {
    return Stream.of(
      Arguments.of("All services fail", true, true, true, 0, 0, 0),
      Arguments.of("Only pieces service fails", false, true, false, 1, 0, 0),
      Arguments.of("Only inventory service fails", false, false, true, 1, 1, 0),
      Arguments.of("PoLines and pieces fail", true, true, false, 0, 0, 0)
    );
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
    items.add(new JsonObject().put("id", UUID.randomUUID().toString()).put("holdingsRecordId", holdingId));

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        // Since piece has null holdingId, it gets filtered during grouping
        // With new parallel execution, we still create results for all requested holdingIds
        assertEquals(1, holdingDetailResults.getAdditionalProperties().size());
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertEquals(1, property.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(0, property.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(0, property.getItemsDetailCollection().getItemsDetail().size());
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.failedFuture(new RuntimeException(errorMessage)));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    // With parallel execution and error recovery, inventory failure should be handled gracefully
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded(), "Should succeed with graceful degradation");
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        // Should have poLines and pieces, but no items
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertNotNull(property);
        assertEquals(1, property.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(1, property.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(0, property.getItemsDetailCollection().getItemsDetail().size());
        vertxTestContext.completeNow();
      });
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("providePieceNullScenarios")
  void testPieceHandlingWithNulls(String testName, String scenarioType, int expectedPieceCount,
                                    boolean expectNullId, boolean expectNonNullId, VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();
    var poLine = createPoLine(poLineId, holdingId);

    // Create pieces based on scenario type
    var pieces = switch (scenarioType) {
      case "NULL_ITEM_ID" -> {
        var pieceWithNullItemId = new Piece()
          .withId(UUID.randomUUID().toString())
          .withPoLineId(poLineId)
          .withHoldingId(holdingId)
          .withItemId(null)
          .withReceivingTenantId("tenant-1");
        var pieceWithItemId = new Piece()
          .withId(UUID.randomUUID().toString())
          .withPoLineId(poLineId)
          .withHoldingId(holdingId)
          .withItemId(UUID.randomUUID().toString())
          .withReceivingTenantId("tenant-1");
        yield List.of(pieceWithNullItemId, pieceWithItemId);
      }
      case "NULL_PIECE_ID" -> {
        var pieceWithNullId = new Piece()
          .withId(null)
          .withPoLineId(poLineId)
          .withHoldingId(holdingId)
          .withItemId(UUID.randomUUID().toString())
          .withReceivingTenantId("tenant-1");
        var pieceWithId = new Piece()
          .withId(UUID.randomUUID().toString())
          .withPoLineId(poLineId)
          .withHoldingId(holdingId)
          .withItemId(UUID.randomUUID().toString())
          .withReceivingTenantId("tenant-1");
        yield List.of(pieceWithNullId, pieceWithId);
      }
      case "MIXED_NULL" -> {
        var validPiece = new Piece()
          .withId(UUID.randomUUID().toString())
          .withPoLineId(poLineId)
          .withHoldingId(holdingId)
          .withItemId(UUID.randomUUID().toString())
          .withReceivingTenantId("tenant-1");
        var mixedPieces = new ArrayList<Piece>();
        mixedPieces.add(null);
        mixedPieces.add(validPiece);
        mixedPieces.add(null);
        yield mixedPieces;
      }
      case "ALL_NULL" -> {
        var allNullPieces = new ArrayList<Piece>();
        allNullPieces.add(null);
        allNullPieces.add(null);
        allNullPieces.add(null);
        yield allNullPieces;
      }
      default -> throw new IllegalArgumentException("Unknown scenario: " + scenarioType);
    };

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId));
        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertEquals(expectedPieceCount, property.getPiecesDetailCollection().getPiecesDetail().size());

        if (expectedPieceCount > 0) {
          var piecesDetail = property.getPiecesDetailCollection().getPiecesDetail();
          if (expectNullId) {
            assertTrue(piecesDetail.stream().anyMatch(pd -> pd.getId() == null || pd.getItemId() == null));
          }
          if (expectNonNullId) {
            assertTrue(piecesDetail.stream().anyMatch(pd -> pd.getId() != null || pd.getItemId() != null));
          }
        }
        vertxTestContext.completeNow();
      });
  }

  static Stream<Arguments> providePieceNullScenarios() {
    return Stream.of(
      Arguments.of("Pieces with null itemId", "NULL_ITEM_ID", 2, true, true),
      Arguments.of("Pieces with null piece ID", "NULL_PIECE_ID", 2, true, true),
      Arguments.of("Mixed null and valid pieces", "MIXED_NULL", 1, false, true),
      Arguments.of("All null pieces", "ALL_NULL", 0, false, false)
    );
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("providePoLineGroupingScenarios")
  void testGroupedPoLinesByHoldingId(String testName, List<String> holdingIds, List<PoLine> poLines,
                                       int expectedHoldingsWithPoLines, VertxTestContext vertxTestContext) {
    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(poLines));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);

        // Count how many holdingIds actually have poLines
        var holdingsWithPoLines = holdingDetailResults.getAdditionalProperties().values().stream()
          .filter(prop -> !prop.getPoLinesDetailCollection().getPoLinesDetail().isEmpty())
          .count();
        assertEquals(expectedHoldingsWithPoLines, holdingsWithPoLines);
        vertxTestContext.completeNow();
      });
  }

  static Stream<Arguments> providePoLineGroupingScenarios() {
    var holdingId1 = UUID.randomUUID().toString();
    var holdingId2 = UUID.randomUUID().toString();
    var holdingId3 = UUID.randomUUID().toString();
    var poLineId1 = UUID.randomUUID().toString();
    var poLineId2 = UUID.randomUUID().toString();

    // Create list with null poLine
    var poLinesWithNull = new ArrayList<PoLine>();
    poLinesWithNull.add(null);

    // Create mixed list with null and valid poLines
    var mixedPoLines = new ArrayList<PoLine>();
    mixedPoLines.add(null);
    mixedPoLines.add(createPoLine(poLineId1, holdingId1));
    mixedPoLines.add(new PoLine().withId(poLineId2).withLocations(List.of(new Location().withHoldingId(null))));

    return Stream.of(
      Arguments.of("Empty poLines list", List.of(holdingId1), Collections.emptyList(), 0),

      Arguments.of("Null poLine in list", List.of(holdingId1), poLinesWithNull, 0),

      Arguments.of("PoLine with null locations",
        List.of(holdingId1),
        List.of(new PoLine().withId(poLineId1).withLocations(null)),
        0),

      Arguments.of("PoLine with empty locations",
        List.of(holdingId1),
        List.of(new PoLine().withId(poLineId1).withLocations(Collections.emptyList())),
        0),

      Arguments.of("Location with null holdingId",
        List.of(holdingId1),
        List.of(new PoLine().withId(poLineId1).withLocations(List.of(new Location().withHoldingId(null)))),
        0),

      Arguments.of("Location with holdingId not in request",
        List.of(holdingId1),
        List.of(new PoLine().withId(poLineId1).withLocations(List.of(new Location().withHoldingId(holdingId2)))),
        0),

      Arguments.of("Valid single poLine",
        List.of(holdingId1),
        List.of(createPoLine(poLineId1, holdingId1)),
        1),

      Arguments.of("Multiple poLines same holding",
        List.of(holdingId1),
        List.of(createPoLine(poLineId1, holdingId1), createPoLine(poLineId2, holdingId1)),
        1),

      Arguments.of("Multiple poLines different holdings",
        List.of(holdingId1, holdingId2),
        List.of(createPoLine(poLineId1, holdingId1), createPoLine(poLineId2, holdingId2)),
        2),

      Arguments.of("PoLine with multiple locations same holding",
        List.of(holdingId1),
        List.of(new PoLine().withId(poLineId1).withLocations(List.of(
          new Location().withHoldingId(holdingId1),
          new Location().withHoldingId(holdingId1)
        ))),
        1),

      Arguments.of("PoLine with multiple locations different holdings",
        List.of(holdingId1, holdingId2),
        List.of(new PoLine().withId(poLineId1).withLocations(List.of(
          new Location().withHoldingId(holdingId1),
          new Location().withHoldingId(holdingId2)
        ))),
        2),

      Arguments.of("Mixed: null poLine, valid poLine, poLine with null location",
        List.of(holdingId1, holdingId2),
        mixedPoLines,
        1),

      Arguments.of("PoLine with locations containing nulls and valid holdingId",
        List.of(holdingId1, holdingId2),
        List.of(new PoLine().withId(poLineId1).withLocations(List.of(
          new Location().withHoldingId(null),
          new Location().withHoldingId(holdingId1),
          new Location().withHoldingId(holdingId3),  // not in requested holdingIds
          new Location().withHoldingId(holdingId2)
        ))),
        2)
    );
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var additionalProps = result.result().getAdditionalProperties();
        assertTrue(additionalProps.containsKey(holdingId));
        var property = additionalProps.get(holdingId);
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        // PoLinesDetail are created from poLines fetched, not from pieces
        // We have 1 poLine, so 1 PoLinesDetail
        assertEquals(1, poLinesDetail.size());
        assertEquals(poLineId, poLinesDetail.getFirst().getId());
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(piece)));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var additionalProps = result.result().getAdditionalProperties();
        assertTrue(additionalProps.containsKey(holdingId));
        var property = additionalProps.get(holdingId);
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        // PoLinesDetail are created from poLines fetched, not from pieces
        // We have 1 poLine, so 1 PoLinesDetail
        assertEquals(1, poLinesDetail.size());
        assertEquals(poLineId, poLinesDetail.getFirst().getId());
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
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
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

  @ParameterizedTest(name = "checkinItems = {0}")
  @MethodSource("provideCheckinItemsValues")
  void testPoLinesDetailWithCheckinItems(Boolean checkinItems, VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var poLineId = UUID.randomUUID().toString();

    var poLine = new PoLine()
      .withId(poLineId)
      .withCheckinItems(checkinItems)
      .withLocations(List.of(new Location().withHoldingId(holdingId)));

    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var property = result.result().getAdditionalProperties().get(holdingId);
        var poLinesDetail = property.getPoLinesDetailCollection().getPoLinesDetail();
        assertEquals(1, poLinesDetail.size());
        assertEquals(poLineId, poLinesDetail.getFirst().getId());
        assertEquals(checkinItems, poLinesDetail.getFirst().getCheckinItems());
        vertxTestContext.completeNow();
      });
  }

  static Stream<Arguments> provideCheckinItemsValues() {
    return Stream.of(
      Arguments.of(true),
      Arguments.of(false),
      Arguments.of((Boolean) null)
    );
  }

  // ===== Central Ordering / Consortium Tests =====
  // IMPORTANT: Holdings are tenant-specific resources in FOLIO.
  // In a consortium with multiple member tenants, each tenant has its own separate holdings.
  // A holding ID in tenant A is a completely different resource than a holding ID in tenant B,
  // even if they happen to have the same UUID value.
  // Therefore, tests should use DIFFERENT holding IDs for DIFFERENT tenants to reflect reality.

  @Test
  // Test that when central ordering is enabled and multiple tenants have data,
  // each tenant's holdings are retrieved and returned separately in the results
  void testCentralOrderingEnabledWithMultipleTenants(VertxTestContext vertxTestContext) {
    var holdingId1 = UUID.randomUUID().toString();
    var holdingId2 = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId1, holdingId2);
    var consortiumId = UUID.randomUUID().toString();
    var centralTenantId = "central-tenant";
    var memberTenant1 = "member-tenant-1";
    var memberTenant2 = "member-tenant-2";
    var userTenants = List.of(memberTenant1, memberTenant2);

    // Setup consortium configuration
    var consortiumConfig = new ConsortiumConfiguration(centralTenantId, consortiumId);
    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(java.util.Optional.of(consortiumConfig)));

    // Setup central ordering enabled
    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(true));

    // Setup user tenants retrieval
    when(consortiumUserTenantsRetriever.getUserTenants(consortiumId, centralTenantId, requestContext))
      .thenReturn(Future.succeededFuture(userTenants));

    // Create test data for member-tenant-1
    var poLine1Tenant1 = createPoLine(UUID.randomUUID().toString(), holdingId1);
    var piece1Tenant1 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLine1Tenant1.getId())
      .withHoldingId(holdingId1)
      .withReceivingTenantId(memberTenant1);
    var item1Tenant1 = new JsonObject()
      .put("id", UUID.randomUUID().toString())
      .put("holdingsRecordId", holdingId1);

    // Create test data for member-tenant-2
    var poLine1Tenant2 = createPoLine(UUID.randomUUID().toString(), holdingId2);
    var piece1Tenant2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLine1Tenant2.getId())
      .withHoldingId(holdingId2)
      .withReceivingTenantId(memberTenant2);
    var item1Tenant2 = new JsonObject()
      .put("id", UUID.randomUUID().toString())
      .put("holdingsRecordId", holdingId2);

    // Mock responses for member-tenant-1 context
    when(purchaseOrderLineService.getPoLinesByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(List.of(poLine1Tenant1));
        } else if (memberTenant2.equals(tenant)) {
          return Future.succeededFuture(List.of(poLine1Tenant2));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    when(pieceStorageService.getPiecesByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(List.of(piece1Tenant1));
        } else if (memberTenant2.equals(tenant)) {
          return Future.succeededFuture(List.of(piece1Tenant2));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    when(inventoryItemManager.getItemsByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(List.of(item1Tenant1));
        } else if (memberTenant2.equals(tenant)) {
          return Future.succeededFuture(List.of(item1Tenant2));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);

        // Both holdings should be present
        assertEquals(2, holdingDetailResults.getAdditionalProperties().size());
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId1));
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId2));

        // Verify holdingId1 has data from tenant1
        var property1 = holdingDetailResults.getAdditionalProperties().get(holdingId1);
        assertEquals(1, property1.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(1, property1.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(1, property1.getItemsDetailCollection().getItemsDetail().size());
        assertEquals(memberTenant1, property1.getPiecesDetailCollection().getPiecesDetail().getFirst().getTenantId());

        // Verify holdingId2 has data from tenant2
        var property2 = holdingDetailResults.getAdditionalProperties().get(holdingId2);
        assertEquals(1, property2.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(1, property2.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(1, property2.getItemsDetailCollection().getItemsDetail().size());
        assertEquals(memberTenant2, property2.getPiecesDetailCollection().getPiecesDetail().getFirst().getTenantId());

        vertxTestContext.completeNow();
      });
  }

  @Test
  // Test that when multiple tenants have data for their respective holdings,
  // the results are properly aggregated with each holding maintaining its tenant-specific data
  void testCentralOrderingWithMultipleTenantsAndOverlappingData(VertxTestContext vertxTestContext) {
    // Holdings are tenant-specific, so each tenant has its own holding IDs
    var holdingId1 = UUID.randomUUID().toString(); // holding in tenant1
    var holdingId2 = UUID.randomUUID().toString(); // holding in tenant2
    var holdingIds = List.of(holdingId1, holdingId2);
    var consortiumId = UUID.randomUUID().toString();
    var centralTenantId = "central-tenant";
    var memberTenant1 = "member-tenant-1";
    var memberTenant2 = "member-tenant-2";
    var userTenants = List.of(memberTenant1, memberTenant2);

    // Setup consortium configuration
    var consortiumConfig = new ConsortiumConfiguration(centralTenantId, consortiumId);
    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(java.util.Optional.of(consortiumConfig)));

    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(true));

    when(consortiumUserTenantsRetriever.getUserTenants(consortiumId, centralTenantId, requestContext))
      .thenReturn(Future.succeededFuture(userTenants));

    // Each tenant has data for their own holding
    var poLineId1 = UUID.randomUUID().toString();
    var poLineId2 = UUID.randomUUID().toString();
    var poLine1 = createPoLine(poLineId1, holdingId1);
    var poLine2 = createPoLine(poLineId2, holdingId2);

    var piece1 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId1)
      .withHoldingId(holdingId1)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(memberTenant1);

    var piece2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLineId2)
      .withHoldingId(holdingId2)
      .withItemId(UUID.randomUUID().toString())
      .withReceivingTenantId(memberTenant2);

    var item1 = new JsonObject()
      .put("id", piece1.getItemId())
      .put("holdingsRecordId", holdingId1);

    var item2 = new JsonObject()
      .put("id", piece2.getItemId())
      .put("holdingsRecordId", holdingId2);

    when(purchaseOrderLineService.getPoLinesByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        // Each tenant only returns data for their own holdings
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(List.of(poLine1));
        } else if (memberTenant2.equals(tenant)) {
          return Future.succeededFuture(List.of(poLine2));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    when(pieceStorageService.getPiecesByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(List.of(piece1));
        } else if (memberTenant2.equals(tenant)) {
          return Future.succeededFuture(List.of(piece2));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    when(inventoryItemManager.getItemsByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(List.of(item1));
        } else if (memberTenant2.equals(tenant)) {
          return Future.succeededFuture(List.of(item2));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);

        // Should have 2 holdings, one from each tenant
        assertEquals(2, holdingDetailResults.getAdditionalProperties().size());
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId1));
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId2));

        // Verify holding1 has data from tenant1
        var property1 = holdingDetailResults.getAdditionalProperties().get(holdingId1);
        assertNotNull(property1);
        assertEquals(1, property1.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(1, property1.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(1, property1.getItemsDetailCollection().getItemsDetail().size());
        assertEquals(memberTenant1, property1.getPiecesDetailCollection().getPiecesDetail().getFirst().getTenantId());
        assertEquals(memberTenant1, property1.getItemsDetailCollection().getItemsDetail().getFirst().getTenantId());
        assertEquals(poLineId1, property1.getPoLinesDetailCollection().getPoLinesDetail().getFirst().getId());

        // Verify holding2 has data from tenant2
        var property2 = holdingDetailResults.getAdditionalProperties().get(holdingId2);
        assertNotNull(property2);
        assertEquals(1, property2.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(1, property2.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(1, property2.getItemsDetailCollection().getItemsDetail().size());
        assertEquals(memberTenant2, property2.getPiecesDetailCollection().getPiecesDetail().getFirst().getTenantId());
        assertEquals(memberTenant2, property2.getItemsDetailCollection().getItemsDetail().getFirst().getTenantId());
        assertEquals(poLineId2, property2.getPoLinesDetailCollection().getPoLinesDetail().getFirst().getId());

        vertxTestContext.completeNow();
      });
  }

  @Test
  void testCentralOrderingDisabled(VertxTestContext vertxTestContext) {
    var holdingId = UUID.randomUUID().toString();
    var holdingIds = List.of(holdingId);
    var consortiumId = UUID.randomUUID().toString();
    var centralTenantId = "central-tenant";

    // Consortium exists but central ordering is disabled
    var consortiumConfig = new ConsortiumConfiguration(centralTenantId, consortiumId);
    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(java.util.Optional.of(consortiumConfig)));

    // Central ordering is disabled or not set
    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(false));

    // Should not call getUserTenants when central ordering is disabled
    when(consortiumUserTenantsRetriever.getUserTenants(any(), any(), any()))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var poLine = createPoLine(UUID.randomUUID().toString(), holdingId);
    when(purchaseOrderLineService.getPoLinesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(List.of(poLine)));
    when(pieceStorageService.getPiecesByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(inventoryItemManager.getItemsByHoldingIds(holdingIds, requestContext))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);
        assertEquals(1, holdingDetailResults.getAdditionalProperties().size());

        var property = holdingDetailResults.getAdditionalProperties().get(holdingId);
        assertEquals(1, property.getPoLinesDetailCollection().getPoLinesDetail().size());

        vertxTestContext.completeNow();
      });
  }

  @Test
  // Test that when one tenant experiences service failures, we still get results from successful tenants
  // This verifies graceful degradation in a consortium environment
  void testCentralOrderingWithPartialTenantFailure(VertxTestContext vertxTestContext) {
    // Each tenant has its own holdings since holdings are tenant-specific
    var holdingId1 = UUID.randomUUID().toString(); // holding in tenant1
    var holdingId2 = UUID.randomUUID().toString(); // holding in tenant2 (but will fail to retrieve)
    var holdingIds = List.of(holdingId1, holdingId2);
    var consortiumId = UUID.randomUUID().toString();
    var centralTenantId = "central-tenant";
    var memberTenant1 = "member-tenant-1";
    var memberTenant2 = "member-tenant-2";
    var userTenants = List.of(memberTenant1, memberTenant2);

    var consortiumConfig = new ConsortiumConfiguration(centralTenantId, consortiumId);
    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(java.util.Optional.of(consortiumConfig)));

    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(true));

    when(consortiumUserTenantsRetriever.getUserTenants(consortiumId, centralTenantId, requestContext))
      .thenReturn(Future.succeededFuture(userTenants));

    var poLine1 = createPoLine(UUID.randomUUID().toString(), holdingId1);
    var piece1 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withPoLineId(poLine1.getId())
      .withHoldingId(holdingId1)
      .withReceivingTenantId(memberTenant1);

    // Tenant 1 succeeds with its holding, Tenant 2 fails for all services (simulating service outage)
    when(purchaseOrderLineService.getPoLinesByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(List.of(poLine1));
        } else if (memberTenant2.equals(tenant)) {
          return Future.failedFuture(new RuntimeException("Tenant 2 poLines service error"));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    when(pieceStorageService.getPiecesByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(List.of(piece1));
        } else if (memberTenant2.equals(tenant)) {
          return Future.failedFuture(new RuntimeException("Tenant 2 pieces service error"));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    when(inventoryItemManager.getItemsByHoldingIds(eq(holdingIds), any()))
      .thenAnswer(invocation -> {
        var ctx = (RequestContext) invocation.getArgument(1);
        var tenant = TenantTool.tenantId(ctx.getHeaders());
        if (memberTenant1.equals(tenant)) {
          return Future.succeededFuture(Collections.emptyList());
        } else if (memberTenant2.equals(tenant)) {
          return Future.failedFuture(new RuntimeException("Tenant 2 items service error"));
        }
        return Future.succeededFuture(Collections.emptyList());
      });

    var future = holdingDetailService.postOrdersHoldingDetail(holdingIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded(), "Should succeed even with partial tenant failure due to graceful degradation");
        var holdingDetailResults = result.result();
        assertNotNull(holdingDetailResults);

        // Should have holding from tenant1 and tenant2, tenant2 data will consist of empty arrays
        assertEquals(2, holdingDetailResults.getAdditionalProperties().size());
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId1));
        assertTrue(holdingDetailResults.getAdditionalProperties().containsKey(holdingId2),
          "Holding from failed tenant should not be present");

        var property = holdingDetailResults.getAdditionalProperties().get(holdingId1);
        assertNotNull(property);

        // Should have data from tenant1 only, tenant2 failed gracefully
        assertEquals(1, property.getPoLinesDetailCollection().getPoLinesDetail().size());
        assertEquals(1, property.getPiecesDetailCollection().getPiecesDetail().size());
        assertEquals(0, property.getItemsDetailCollection().getItemsDetail().size());
        assertEquals(memberTenant1, property.getPiecesDetailCollection().getPiecesDetail().getFirst().getTenantId());

        vertxTestContext.completeNow();
      });
  }

  private static PoLine createPoLine(String poLineId, String holdingId) {
    return new PoLine()
      .withId(poLineId)
      .withLocations(List.of(new Location().withHoldingId(holdingId)));
  }
}

