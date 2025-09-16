package org.folio.service.titles;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getItem;
import static org.folio.rest.core.exceptions.ErrorCodes.EXISTING_HOLDINGS_FOR_DELETE_CONFIRMATION;
import static org.folio.rest.core.exceptions.ErrorCodes.EXISTING_RECEIVED_PIECES_TITLE_REMOVAL;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import io.vertx.core.Future;
import org.folio.CopilotGenerated;
import org.folio.models.ItemStatus;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.acq.model.SequenceNumbers;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.ProtectionService;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

@CopilotGenerated(model = "Claude Sonnet 4")
@ExtendWith(MockitoExtension.class)
public class TitlesServiceTest {
  private static final String TITLE_ID = "test-title-id";
  private static final String DELETE_TITLE_ENDPOINT = "/orders-storage/titles/" + TITLE_ID;
  private static final String POLINE_ID = "test-poline-id";
  private static final String HOLDING_ID_1 = "holding-1";
  private static final String HOLDING_ID_2 = "holding-2";
  private static final String HOLDING_ID_3 = "holding-3";
  private static final String PIECE_ID_1 = "piece-1";
  private static final String PIECE_ID_2 = "piece-2";
  private static final String PIECE_ID_3 = "piece-3";
  private static final String PIECE_ID_4 = "piece-4";
  private static final String ITEM_ID_1 = "item-1";
  private static final String ITEM_ID_2 = "item-2";
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

  @Mock
  private ConsortiumConfigurationService consortiumConfigurationService;

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
  void positive_testDeleteTest_deleteTitleWhenNoHoldingsToDelete() {

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    doReturn(Future.succeededFuture()).when(restClient).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(true));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));

    var result = titlesService.deleteTitle(TITLE_ID, null, requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(restClient).delete(anyString(), any());
      verify(inventoryHoldingManager, never()).deleteHoldingById(anyString(), anyBoolean(), any(RequestContext.class));
      verify(inventoryItemManager, never()).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
      verify(pieceStorageService, never()).deletePiecesByIds(anyList(), any(RequestContext.class));
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_throwErrorWhenDeletableHoldingsExistAndWithoutParamUsage() {
    var deletableHoldings = List.of(HOLDING_ID_1);
    var pieces = Arrays.asList(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId(TENANT_ID).withTitleId(TITLE_ID),
      new Piece().withHoldingId(HOLDING_ID_2).withReceivingTenantId(TENANT_ID).withTitleId(TITLE_ID)
    );
    var item1 = getItem(ITEM_ID_1, POLINE_ID);
    var item2 = getItem(UUID.randomUUID().toString(), UUID.randomUUID().toString());

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(true));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item1)));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_2), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item2)));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));

    var result = titlesService.deleteTitle(TITLE_ID, null, requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.failed());

      var error = ((HttpException) ar.cause()).getError();
      assertEquals(EXISTING_HOLDINGS_FOR_DELETE_CONFIRMATION.getCode(), error.getCode());
      assertEquals(deletableHoldings.toString(), error.getParameters().getFirst().getValue());

      verify(restClient, never()).delete(anyString(), any());
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_deleteHoldingsItemsAndPiecesWhenDeleteHoldingIsTrue() {
    List<Piece> pieces = Collections.singletonList(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId(TENANT_ID).withTitleId(TITLE_ID)
    );
    var item = getItem(ITEM_ID_1, POLINE_ID);

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));
    when(inventoryItemManager.deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(pieceStorageService.deletePiecesByIds(eq(Collections.singletonList(PIECE_ID_1)), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    when(inventoryHoldingManager.deleteHoldingById(eq(HOLDING_ID_1), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    doReturn(Future.succeededFuture()).when(restClient).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(false));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));

    var result = titlesService.deleteTitle(TITLE_ID, "true", requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(inventoryItemManager).deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class));
      verify(pieceStorageService).deletePiecesByIds(eq(Collections.singletonList(PIECE_ID_1)), any(RequestContext.class));
      verify(inventoryHoldingManager).deleteHoldingById(eq(HOLDING_ID_1), eq(true), any(RequestContext.class));
      verify(restClient).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_deleteHoldingsItemsAndPiecesWhenDeleteHoldingIsTrue_withEnabledCentralOrdering() {
    List<Piece> pieces = Collections.singletonList(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId("college").withTitleId(TITLE_ID)
    );
    var item = getItem(ITEM_ID_1, POLINE_ID);

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));
    when(inventoryItemManager.deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(pieceStorageService.deletePiecesByIds(eq(Collections.singletonList(PIECE_ID_1)), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    when(inventoryHoldingManager.deleteHoldingById(eq(HOLDING_ID_1), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    doReturn(Future.succeededFuture()).when(restClient).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(true));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));

    var result = titlesService.deleteTitle(TITLE_ID, "true", requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(consortiumConfigurationService).isCentralOrderingEnabled(any(RequestContext.class));
      verify(inventoryItemManager).deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class));
      verify(pieceStorageService).deletePiecesByIds(eq(Collections.singletonList(PIECE_ID_1)), any(RequestContext.class));
      verify(inventoryHoldingManager).deleteHoldingById(eq(HOLDING_ID_1), eq(true), any(RequestContext.class));
      verify(restClient).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_notDeleteHoldingsWhenDeleteHoldingIsFalse() {
    List<Piece> pieces = Collections.singletonList(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId("college").withTitleId(TITLE_ID)
    );
    var item = getItem(ITEM_ID_1, POLINE_ID);

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));
    when(inventoryItemManager.deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(pieceStorageService.deletePiecesByIds(eq(Collections.singletonList(PIECE_ID_1)), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    doReturn(Future.succeededFuture()).when(restClient).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(false));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));

    var result = titlesService.deleteTitle(TITLE_ID, "false", requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(inventoryHoldingManager, never()).deleteHoldingById(anyString(), anyBoolean(), any(RequestContext.class));
      verify(inventoryItemManager).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
      verify(pieceStorageService).deletePiecesByIds(anyList(), any(RequestContext.class));
      verify(restClient).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    });
  }

  @Test
  void positive_testUnlinkTitleFromPackage_holdingWithItemsWithDifferentStatusOrDifferentPoLine() {
    List<Piece> pieces = List.of(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId("college").withTitleId(TITLE_ID),
      new Piece().withId(PIECE_ID_2).withHoldingId(HOLDING_ID_2).withReceivingTenantId("college").withTitleId(TITLE_ID),
      new Piece().withId(PIECE_ID_3).withHoldingId(HOLDING_ID_3).withReceivingTenantId("college").withTitleId(TITLE_ID)
    );
    var item1 = getItem(ITEM_ID_1, POLINE_ID, ItemStatus.ORDER_CLOSED);
    var item2 = getItem(ITEM_ID_2, POLINE_ID, ItemStatus.AVAILABLE); // Will not be deleted
    var item3 = getItem(UUID.randomUUID().toString(), UUID.randomUUID().toString()); // Will not be deleted

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item1, item2)));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_2), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item3)));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_3), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of()));
    when(inventoryItemManager.deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Collections.emptyList()));
    when(pieceStorageService.deletePiecesByIds(eq(List.of(PIECE_ID_1, PIECE_ID_2, PIECE_ID_3)), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    when(inventoryHoldingManager.deleteHoldingById(eq(HOLDING_ID_3), eq(true), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    when(restClient.delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture());
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(true));
    when(protectionService.isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext)))
      .thenReturn(succeededFuture(null));

    var result = titlesService.deleteTitle(TITLE_ID, "true", requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      verify(consortiumConfigurationService).isCentralOrderingEnabled(any(RequestContext.class));
      verify(inventoryItemManager).deleteItems(eq(Collections.singletonList(ITEM_ID_1)), eq(true), any(RequestContext.class));
      verify(pieceStorageService).deletePiecesByIds(eq(List.of(PIECE_ID_1, PIECE_ID_2, PIECE_ID_3)), any(RequestContext.class));
      verify(inventoryHoldingManager, times(0)).deleteHoldingById(eq(HOLDING_ID_1), eq(true), any(RequestContext.class));
      verify(inventoryHoldingManager, times(0)).deleteHoldingById(eq(HOLDING_ID_2), eq(true), any(RequestContext.class));
      verify(inventoryHoldingManager, times(1)).deleteHoldingById(eq(HOLDING_ID_3), eq(true), any(RequestContext.class));
      verify(restClient).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    });
  }

  @Test
  void negative_testUnlinkTitleFromPackage_throwExceptionWhenDeleteHoldingIsInvalid() {
    List<Piece> pieces = Collections.singletonList(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId("college").withTitleId(TITLE_ID)
    );
    var item = getItem(ITEM_ID_1, POLINE_ID);

    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(false));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of(item)));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));

    var result = titlesService.deleteTitle(TITLE_ID, "invalid", requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.failed());
      assertInstanceOf(IllegalArgumentException.class, ar.cause());
      assertEquals("deleteHolding must be either 'true' or 'false'", ar.cause().getMessage());
      verify(restClient, never()).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    });
  }

  @Test
  void negative_testUnlinkTitleFromPackage_handleFailureInGettingTitle() {
    when(titlesService.getTitleById(anyString(), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Title not found")));

    var result = titlesService.deleteTitle("test-title-id", "true", requestContext);

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
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));

    var result = titlesService.deleteTitle(TITLE_ID, "true", requestContext);

    result.onComplete(ar -> {
      assert ar.failed();
      assertEquals("PoLine not found", ar.cause().getMessage());
    });
  }

  @Test
  void negative_testUnlinkTitleFromPackage_handleFailureInConsortiumConfiguration() {
    doReturn(Future.succeededFuture(title)).when(titlesService).getTitleById(eq(TITLE_ID), any(RequestContext.class));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of()));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Consortium configuration error")));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));

    var result = titlesService.deleteTitle(TITLE_ID, "true", requestContext);

    result.onComplete(ar -> {
      assert ar.failed();
      assertEquals("Consortium configuration error", ar.cause().getMessage());
    });
  }

  @Test
  void negative_testUnlinkTitleFromPackage_receivedPieceExists() {
    List<Piece> pieces = List.of(
      new Piece().withId(PIECE_ID_1).withHoldingId(HOLDING_ID_1).withReceivingTenantId("college").withTitleId(TITLE_ID).withReceivingStatus(Piece.ReceivingStatus.EXPECTED),
      new Piece().withId(PIECE_ID_2).withHoldingId(HOLDING_ID_1).withReceivingTenantId("college").withTitleId(TITLE_ID).withReceivingStatus(Piece.ReceivingStatus.RECEIVED)
    );

    when(titlesService.getTitleById(eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(title));
    when(protectionService.isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext)))
      .thenReturn(succeededFuture(null));
    when(purchaseOrderLineService.getOrderLineById(eq(POLINE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(poLine));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(eq(POLINE_ID), eq(TITLE_ID), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(pieces));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(true));
    when(inventoryItemManager.getItemsByHoldingId(eq(HOLDING_ID_1), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(List.of()));

    var result = titlesService.deleteTitle(TITLE_ID, "true", requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.failed());
      assertInstanceOf(HttpException.class, ar.cause());

      var error = ((HttpException) ar.cause()).getError();
      assertEquals(EXISTING_RECEIVED_PIECES_TITLE_REMOVAL.getCode(), error.getCode());

      verify(restClient, never()).delete(eq(DELETE_TITLE_ENDPOINT), any(RequestContext.class));
    });
  }

  // Tests for generateNextSequenceNumbers method

  @Test
  void testGenerateNextSequenceNumbers_emptyPiecesList() {
    List<Piece> pieces = Collections.emptyList();

    var result = titlesService.generateNextSequenceNumbers(pieces, title, requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      assertNotNull(ar.result());
      assertTrue(ar.result().isEmpty());
      verify(restClient, never()).postBatch(any(RequestEntry.class), any(), eq(SequenceNumbers.class), any(RequestContext.class));
    });
  }

  @Test
  void testGenerateNextSequenceNumbers_nullPiecesList() {
    List<Piece> pieces = null;

    var result = titlesService.generateNextSequenceNumbers(pieces, title, requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      assertNotNull(ar.result());
      assertTrue(ar.result().isEmpty());
      verify(restClient, never()).postBatch(any(RequestEntry.class), any(), eq(SequenceNumbers.class), any(RequestContext.class));
    });
  }

  @Test
  void testGenerateNextSequenceNumbers_piecesWithNoSequenceNumbers() {
    var pieces = List.of(
      new Piece().withId(PIECE_ID_1).withSequenceNumber(null),
      new Piece().withId(PIECE_ID_2).withSequenceNumber(null),
      new Piece().withId(PIECE_ID_3).withSequenceNumber(null)
    );

    var sequenceNumbers = new SequenceNumbers().withSequenceNumbers(List.of("1", "2", "3"));

    when(restClient.postBatch(any(RequestEntry.class), any(), eq(SequenceNumbers.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(sequenceNumbers));

    var result = titlesService.generateNextSequenceNumbers(pieces, title, requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      var resultPieces = ar.result();
      assertEquals(3, resultPieces.size());
      assertEquals(Integer.valueOf(1), resultPieces.get(0).getSequenceNumber());
      assertEquals(Integer.valueOf(2), resultPieces.get(1).getSequenceNumber());
      assertEquals(Integer.valueOf(3), resultPieces.get(2).getSequenceNumber());

      ArgumentCaptor<RequestEntry> requestCaptor = ArgumentCaptor.forClass(RequestEntry.class);
      verify(restClient).postBatch(requestCaptor.capture(), any(), eq(SequenceNumbers.class), eq(requestContext));

      var capturedRequest = requestCaptor.getValue();
      assertTrue(capturedRequest.buildEndpoint().contains(title.getId()));
      assertTrue(capturedRequest.buildEndpoint().contains("sequenceNumbers=3"));
    });
  }

  @Test
  void testGenerateNextSequenceNumbers_piecesWithExistingSequenceNumbers() {
    var pieces = List.of(
      new Piece().withId(PIECE_ID_1).withSequenceNumber(1),
      new Piece().withId(PIECE_ID_2).withSequenceNumber(2),
      new Piece().withId(PIECE_ID_3).withSequenceNumber(3)
    );

    var sequenceNumbers = new SequenceNumbers().withSequenceNumbers(List.of("1", "2", "3"));

    when(restClient.postBatch(any(RequestEntry.class), any(), eq(SequenceNumbers.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(sequenceNumbers));

    var result = titlesService.generateNextSequenceNumbers(pieces, title, requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      var resultPieces = ar.result();
      assertEquals(3, resultPieces.size());
      // Existing sequence numbers should remain unchanged
      assertEquals(Integer.valueOf(1), resultPieces.get(0).getSequenceNumber());
      assertEquals(Integer.valueOf(2), resultPieces.get(1).getSequenceNumber());
      assertEquals(Integer.valueOf(3), resultPieces.get(2).getSequenceNumber());
    });
  }

  @Test
  void testGenerateNextSequenceNumbers_mixedPiecesWithAndWithoutSequenceNumbers() {
    var pieces = List.of(
      new Piece().withId(PIECE_ID_1).withSequenceNumber(null),
      new Piece().withId(PIECE_ID_2).withSequenceNumber(2),
      new Piece().withId(PIECE_ID_3).withSequenceNumber(null),
      new Piece().withId(PIECE_ID_4).withSequenceNumber(4)
    );

    var sequenceNumbers = new SequenceNumbers().withSequenceNumbers(List.of("1", "2", "3", "4"));

    when(restClient.postBatch(any(RequestEntry.class), any(), eq(SequenceNumbers.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(sequenceNumbers));

    var result = titlesService.generateNextSequenceNumbers(pieces, title, requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.succeeded());
      var resultPieces = ar.result();
      assertEquals(4, resultPieces.size());

      // First piece (null) should get first available number (1)
      assertEquals(Integer.valueOf(1), resultPieces.get(0).getSequenceNumber());
      // Second piece already has number 2, should remain unchanged
      assertEquals(Integer.valueOf(2), resultPieces.get(1).getSequenceNumber());
      // Third piece (null) should get next available number (3)
      assertEquals(Integer.valueOf(3), resultPieces.get(2).getSequenceNumber());
      // Fourth piece already has number 4, should remain unchanged
      assertEquals(Integer.valueOf(4), resultPieces.get(3).getSequenceNumber());
    });
  }

  @Test
  void testGenerateNextSequenceNumbers_restClientFailure() {
    var pieces = List.of(
      new Piece().withId(PIECE_ID_1).withSequenceNumber(null)
    );

    when(restClient.postBatch(any(RequestEntry.class), any(), eq(SequenceNumbers.class), eq(requestContext)))
      .thenReturn(Future.failedFuture(new RuntimeException("REST call failed")));

    var result = titlesService.generateNextSequenceNumbers(pieces, title, requestContext);

    result.onComplete(ar -> {
      assertTrue(ar.failed());
      assertEquals("REST call failed", ar.cause().getMessage());
    });
  }

}
