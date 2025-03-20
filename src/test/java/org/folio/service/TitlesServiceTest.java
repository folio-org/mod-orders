package org.folio.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitleInstanceService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class TitlesServiceTest {

  private static final String TITLE_ID = "test-title-id";
  private static final String POLINE_ID = "test-poline-id";
  private static final String HOLDING_ID_1 = "holding-1";
  private static final String HOLDING_ID_2 = "holding-2";
  private static final String PIECE_ID_1 = "piece-1";
  private static final String ITEM_ID_1 = "item-1";
  private static final String TENANT_ID = "tenant-id";

  @Mock
  private RestClient restClient;

  @Mock
  private ProtectionService protectionService;

  @Mock
  private TitleInstanceService titleInstanceService;

  @Mock
  private InventoryHoldingManager inventoryHoldingManager;

  @Mock
  private InventoryItemManager inventoryItemManager;

  @Mock
  private PurchaseOrderLineService purchaseOrderLineService;

  @Mock
  private PieceStorageService pieceStorageService;

  @Spy
  @InjectMocks
  private TitlesService titlesService;

  private RequestContext requestContext;
  private Title title;
  private PoLine poLine;

  @BeforeEach
  void setUp() {
    requestContext = mock(RequestContext.class);

    title = new Title()
      .withId(TITLE_ID)
      .withPoLineId(POLINE_ID);

    poLine = new PoLine()
      .withId(POLINE_ID);
  }

  @Test
  void positive_testUnlinkTitleFromPackage_unlinkTitleWhenNoHoldings() {
    // Setup
    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    doReturn(Future.succeededFuture()).when(titlesService).deleteTitle(eq(TITLE_ID), any(RequestContext.class));

    // Execute
    Future<List<String>> result = titlesService.unlinkTitleFromPackage(TITLE_ID, null, requestContext);

    // Verify
    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(titlesService).deleteTitle(eq(TITLE_ID), any(RequestContext.class));
      verify(inventoryHoldingManager, never()).deleteHoldingById(anyString(), anyBoolean(), any(RequestContext.class));
      verify(inventoryItemManager, never()).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
      verify(pieceStorageService, never()).deletePiecesByIds(anyList(), any(RequestContext.class));
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_returnHoldingsWhenMultipleTitlesExist() {
    // Setup
    List<String> holdingIds = Arrays.asList(HOLDING_ID_1, HOLDING_ID_2);
    List<Piece> pieces = Arrays.asList(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId(TENANT_ID).withTitleId(TITLE_ID),
      new Piece().withHoldingId(HOLDING_ID_2).withReceivingTenantId(TENANT_ID).withTitleId(TITLE_ID)
    );
    Map<String, List<String>> holdingsByTenant = new HashMap<>();
    holdingsByTenant.put(TENANT_ID, holdingIds);

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));

    // Execute
    Future<List<String>> result = titlesService.unlinkTitleFromPackage(TITLE_ID, null, requestContext);

    // Verify
    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(titlesService, never()).deleteTitle(anyString(), any(RequestContext.class));
      assertEquals(holdingIds, ar.result());
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_returnOnlyDeletableHoldings() {
    // Setup
    List<Piece> pieces = Arrays.asList(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId(TENANT_ID).withTitleId(TITLE_ID),
      new Piece().withHoldingId(HOLDING_ID_2).withReceivingTenantId(TENANT_ID).withTitleId(TITLE_ID)
    );

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));

    // Execute
    Future<List<String>> result = titlesService.unlinkTitleFromPackage(TITLE_ID, null, requestContext);

    // Verify
    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(titlesService, never()).deleteTitle(anyString(), any(RequestContext.class));
      assertEquals(List.of(HOLDING_ID_1, HOLDING_ID_2), ar.result());
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_deleteHoldingsItemsAndPiecesWhenDeleteHoldingIsTrue() {
    List<Piece> pieces = Collections.singletonList(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId(TENANT_ID).withTitleId(TITLE_ID)
    );
    JsonObject item = new JsonObject().put("id", ITEM_ID_1);
    List<JsonObject> items = Collections.singletonList(item);
    Map<String, List<String>> holdingsByTenant = new HashMap<>();
    holdingsByTenant.put(TENANT_ID, Collections.singletonList(HOLDING_ID_1));

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));
    // For checking deletability
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(items))
      .thenReturn(Future.succeededFuture(items)); // Called twice
    when(inventoryItemManager.deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(pieceStorageService.deletePiecesByIds(eq(Collections.singletonList(PIECE_ID_1)), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    when(inventoryHoldingManager.deleteHoldingById(eq(HOLDING_ID_1), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    doReturn(Future.succeededFuture()).when(titlesService).deleteTitle(eq(TITLE_ID), any(RequestContext.class));

    // Execute
    Future<List<String>> result = titlesService.unlinkTitleFromPackage(TITLE_ID, "true", requestContext);

    // Verify
    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(inventoryItemManager).deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class));
      verify(pieceStorageService).deletePiecesByIds(eq(Collections.singletonList(PIECE_ID_1)), any(RequestContext.class));
      verify(inventoryHoldingManager).deleteHoldingById(eq(HOLDING_ID_1), eq(true), any(RequestContext.class));
      verify(titlesService).deleteTitle(eq(TITLE_ID), any(RequestContext.class));
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_notDeleteHoldingsWhenDeleteHoldingIsFalse() {
    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    doReturn(Future.succeededFuture()).when(titlesService).deleteTitle(eq(TITLE_ID), any(RequestContext.class));

    Future<List<String>> result = titlesService.unlinkTitleFromPackage(TITLE_ID, "false", requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(inventoryHoldingManager, never()).deleteHoldingById(anyString(), anyBoolean(), any(RequestContext.class));
      verify(inventoryItemManager, never()).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
      verify(pieceStorageService, never()).deletePiecesByIds(anyList(), any(RequestContext.class));
      verify(titlesService).deleteTitle(eq(TITLE_ID), any(RequestContext.class));
    });
  }

  @Test
  void negative_testUnlinkTitleFromPackage_throwExceptionWhenDeleteHoldingIsInvalid() {
    // Setup
    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));

    // Execute
    Future<List<String>> result = titlesService.unlinkTitleFromPackage(TITLE_ID, "invalid", requestContext);

    // Verify
    result.onComplete(ar -> {
      assertTrue(ar.failed());
      assertInstanceOf(IllegalArgumentException.class, ar.cause());
      assertEquals("deleteHolding must be either 'true' or 'false'", ar.cause().getMessage());
    });
  }

  @Test
  void negative_testUnlinkTitleFromPackage_handleFailureInGettingTitle() {
    when(titlesService.getTitleById(anyString(), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Title not found")));

    Future<List<String>> result = titlesService.unlinkTitleFromPackage("test-title-id", "true", requestContext);

    result.onComplete(ar -> {
      assert ar.failed();
      assertEquals("Title not found", ar.cause().getMessage());
    });
  }

  @Test
  void negative_testUnlinkTitleFromPackage_handleFailureInGettingPoLine() {
    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("PoLine not found")));

    Future<List<String>> result = titlesService.unlinkTitleFromPackage(TITLE_ID, "true", requestContext);

    result.onComplete(ar -> {
      assert ar.failed();
      assertEquals("PoLine not found", ar.cause().getMessage());
    });
  }
}
