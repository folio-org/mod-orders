package org.folio.service.orders.flows.update.unopen;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.folio.ApiTestSuite;
import org.folio.models.ItemStatus;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategy;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.utils.RequestContextMatcher;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

@ExtendWith(VertxExtension.class)
public class UnOpenCompositeOrderManagerTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  private static final String PIECE_ID = "57cbee32-c6f8-4467-a4c7-97366792b0ce";
  private static final String ITEM_ID = "50bd38d8-2560-4084-b987-a939bb198865";
  private static final String HOLDING_ID = "41620fe3-9ab5-4a35-a8cf-6f3611ec50c8";
  private static final String EFFECTIVE_LOCATION_ID = "41620fe3-9ab5-4a35-a8cf-6f3611ec50c8";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Autowired
  private UnOpenCompositeOrderManager unOpenCompositeOrderManager;
  @Autowired
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;
  @Autowired
  private InventoryItemManager inventoryItemManager;
  @Autowired
  private InventoryHoldingManager inventoryHoldingManager;
  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired
  private ProtectionService protectionService;
  @Mock
  private OpenToPendingEncumbranceStrategy openToPendingEncumbranceStrategy;
  @Mock
  private Map<String, String> okapiHeadersMock;
  private Context ctxMock;
  private RequestContext requestContext;

  private static boolean runningOnOwn;

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(UnOpenCompositeOrderManagerTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @BeforeEach
  void beforeEach() {
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
    reset(encumbranceWorkflowStrategyFactory, pieceStorageService, inventoryItemManager, inventoryHoldingManager);
  }

  @Test
  void testDeleteItemsAndPiecesForSynchronizedWorkflowWhenDeleteHoldingsFalse() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, false, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    // pieces should be deleted
    verify(pieceStorageService).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    // items should be deleted
    verify(inventoryItemManager).deleteItems(List.of(ITEM_ID), false, requestContext);
    // holdings should not be deleted because deleteHodlings = false
    verify(inventoryHoldingManager, never()).deleteHoldingById(anyString(), anyBoolean(), any());
  }

  @Test
  void testDeleteItemsAndPiecesAndHoldingsForSynchronizedWorkflowWhenDeleteHoldingsTrue() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, true, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    // piece should be deleted
    verify(pieceStorageService).deletePiece(PIECE_ID, requestContext);
    // item should be deleted
    verify(inventoryItemManager).deleteItem(ITEM_ID, true, requestContext);
    // holdings should be deleted because deleteHodlings = true
    verify(inventoryHoldingManager).deleteHoldingById(HOLDING_ID, true, requestContext);
  }

  @Test
  void testDeleteItemsAndPiecesNotHoldingsThatHaveOnorderItemsForSynchronizedWorkflowWhenDeleteHoldingsTrue() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    prepareInitialSetup(order, orderFromStorage, poLine);
    doReturn(succeededFuture(List.of(getItem()))).when(inventoryItemManager).getItemsByHoldingId(HOLDING_ID, requestContext);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, true, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    // piece should be deleted
    verify(pieceStorageService).deletePiece(PIECE_ID, requestContext);
    // item should be deleted
    verify(inventoryItemManager).deleteItem(ITEM_ID, true, requestContext);
    // holding should not be deleted, because have links to on-order items
    verify(inventoryHoldingManager, never()).deleteHoldingById(anyString(), anyBoolean(), any());
  }

  @Test
  void testDeletePiecesItemsAndHoldingsForSynchronizedWorkflowWhenDeleteHoldingsFalse() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, false, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    // piece should be deleted
    verify(pieceStorageService).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    // item should be deleted
    verify(inventoryItemManager).deleteItems(List.of(ITEM_ID), false, requestContext);
    // holding should not be deleted when delete holdings = false
    verify(inventoryHoldingManager, never()).deleteHoldingById(anyString(), anyBoolean(), any(RequestContext.class));
  }

  @Test
  void testDoNotDoAnyActionsForPackageOrderLine() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine packagePoLine = getPoLine(order);
    // make package order line
    packagePoLine.setIsPackage(true);
    prepareInitialSetup(order, orderFromStorage, packagePoLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, false, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    verifyNoInteractions(inventoryItemManager);
  }

  @Test
  void testDeletePiecesWhenCreateItemWasNoneAndSynchronizedFlow() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    poLine.getPhysical().setCreateInventory(Physical.CreateInventory.NONE);
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, false, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    // only pieces should be deleted because of Synchronized flow, items not because CreateInventory is None
    verify(pieceStorageService).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    verifyNoInteractions(inventoryItemManager);
  }

  @Test
  void testRemainPiecesWhenCreateItemWasNoneAndSynchronizedFlow() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    // make Independent flow with Create Inventory is None and receipt not required status
    poLine.setCheckinItems(true);
    poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED);
    poLine.setPaymentStatus(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED);
    poLine.getPhysical().setCreateInventory(Physical.CreateInventory.NONE);
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, false, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecksForReceiptNotRequired(order, orderFromStorage);
    // pieces and inventory should not be deleted
    verifyNoInteractions(pieceStorageService);
    verifyNoInteractions(inventoryItemManager);
  }

  @Test
  void testDeleteHoldingsWhenOnlyHoldingWasCreatedAndDeleteHoldingsIsTrue() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    poLine.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    poLine.getLocations().forEach(location -> location.setHoldingId(HOLDING_ID));
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, true, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    verify(pieceStorageService).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    verify(inventoryHoldingManager).deleteHoldingById(eq(HOLDING_ID), eq(true), RequestContextMatcher.matchCentralTenant());
    verify(inventoryItemManager, never()).deleteItem(anyString(), anyBoolean(), any(RequestContext.class));
    verify(inventoryItemManager, never()).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
  }

  @Test
  void testDeleteHoldingsForSynchronizedWorkflowWhenHoldingHasRelatedItems() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    poLine.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    poLine.getLocations().forEach(location -> location.setHoldingId(HOLDING_ID));
    prepareInitialSetup(order, orderFromStorage, poLine);
    doReturn(succeededFuture(List.of(getItem()))).when(inventoryItemManager).getItemsByHoldingId(HOLDING_ID, requestContext);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, true, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    verify(pieceStorageService).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    // holding is not deleted because it has associated items
    verify(inventoryHoldingManager, never()).deleteHoldingById(HOLDING_ID, true, requestContext);
    verify(inventoryItemManager, never()).deleteItem(anyString(), anyBoolean(), any(RequestContext.class));
    verify(inventoryItemManager, never()).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
  }

  @Test
  void testDeleteHoldingForSynchronizedWorkflowWhenDeleteHoldingIsFalse() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    poLine.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    poLine.getLocations().forEach(location -> location.setHoldingId(HOLDING_ID));
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, false, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    verify(pieceStorageService).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    verify(inventoryHoldingManager, never()).deleteHoldingById(HOLDING_ID, true, requestContext);
    verify(inventoryItemManager, never()).deleteItem(anyString(), anyBoolean(), any(RequestContext.class));
    verify(inventoryItemManager, never()).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
  }

  @Test
  void testDeleteHoldingForIndependentWorkflowWhenDeleteHoldingIsTrue() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    poLine.setCheckinItems(true);
    poLine.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    poLine.getLocations().forEach(location -> location.setHoldingId(HOLDING_ID));
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, true, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    verify(pieceStorageService, never()).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    verify(inventoryHoldingManager).deleteHoldingById(eq(HOLDING_ID), eq(true), RequestContextMatcher.matchCentralTenant());
    verify(inventoryItemManager, never()).deleteItem(anyString(), anyBoolean(), any(RequestContext.class));
    verify(inventoryItemManager, never()).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
  }

  @Test
  void testDeleteHoldingForIndependentWorkflowWhenDeleteHoldingIsFalse() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePurchaseOrder orderFromStorage = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = getPoLine(order);
    poLine.setCheckinItems(true);
    poLine.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    poLine.getLocations().forEach(location -> location.setHoldingId(HOLDING_ID));
    prepareInitialSetup(order, orderFromStorage, poLine);
    //When
    unOpenCompositeOrderManager.process(order, orderFromStorage, false, requestContext).result();
    //Then
    makeBasicUnOpenWorkflowChecks(order, orderFromStorage);
    verify(pieceStorageService, never()).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    verify(inventoryHoldingManager, never()).deleteHoldingById(HOLDING_ID, true, requestContext);
    verify(inventoryItemManager, never()).deleteItem(anyString(), anyBoolean(), any(RequestContext.class));
    verify(inventoryItemManager, never()).deleteItems(anyList(), anyBoolean(), any(RequestContext.class));
  }

  private void prepareInitialSetup(CompositePurchaseOrder order, CompositePurchaseOrder orderFromStorage, CompositePoLine poLine) {
    doReturn(openToPendingEncumbranceStrategy).when(encumbranceWorkflowStrategyFactory).getStrategy(eq(OrderWorkflowType.OPEN_TO_PENDING));
    doReturn(succeededFuture(null)).when(openToPendingEncumbranceStrategy).processEncumbrances(eq(order), eq(orderFromStorage), any());
    JsonObject item = getItem();
    List<JsonObject> onOrderItems = List.of(item);
    doReturn(succeededFuture(onOrderItems)).when(inventoryItemManager).getItemsByPoLineIdsAndStatus(List.of(poLine.getId()), ItemStatus.ON_ORDER.value(), requestContext);
    doReturn(succeededFuture()).when(inventoryItemManager).deleteItems(List.of(ITEM_ID), false, requestContext);
    doReturn(succeededFuture(onOrderItems)).when(inventoryItemManager).getItemRecordsByIds(List.of(ITEM_ID), requestContext);
    Piece piece = new Piece().withId(PIECE_ID).withItemId(ITEM_ID).withPoLineId(poLine.getId());
    PieceCollection pieceCollection = new PieceCollection().withPieces(List.of(piece));
    doReturn(succeededFuture(pieceCollection)).when(pieceStorageService).getExpectedPiecesByLineId(poLine.getId(), requestContext);
    doReturn(succeededFuture()).when(pieceStorageService).deletePiecesByIds(List.of(PIECE_ID), requestContext);
    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(PIECE_ID, requestContext);
    PoLine simpleLine = new PoLine().withId(poLine.getId()).withPurchaseOrderId(order.getId()).withIsPackage(poLine.getIsPackage());
    doReturn(succeededFuture(simpleLine)).when(purchaseOrderLineService).getOrderLineById(poLine.getId(), requestContext);
    PurchaseOrder simpleOrder = new PurchaseOrder().withId(order.getId());
    doReturn(succeededFuture(simpleOrder)).when(purchaseOrderStorageService).getPurchaseOrderById(order.getId(), requestContext);
    doReturn(succeededFuture()).when(protectionService).isOperationRestricted(anyList(), any(ProtectedOperationType.class), any());
    doReturn(succeededFuture(0)).when(inventoryItemManager).getNumberOfRequestsByItemId(ITEM_ID, requestContext);
    doReturn(succeededFuture()).when(pieceStorageService).deletePiece(PIECE_ID, requestContext);
    doReturn(succeededFuture()).when(inventoryItemManager).deleteItem(piece.getItemId(), true, requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(inventoryItemManager).getItemsByHoldingId(eq(HOLDING_ID), RequestContextMatcher.matchCentralTenant());
    doReturn(succeededFuture()).when(inventoryHoldingManager).deleteHoldingById(eq(HOLDING_ID), eq(true), RequestContextMatcher.matchCentralTenant());
    JsonObject holding = new JsonObject().put("id", HOLDING_ID).put("permanentLocationId", poLine.getLocations().get(0).getLocationId());
    doReturn(Map.of("folio_shared", succeededFuture(List.of(holding)))).when(inventoryHoldingManager).getHoldingsByLocationTenants(poLine, requestContext);
  }

  private JsonObject getItem() {
    return new JsonObject()
      .put("id", ITEM_ID)
      .put("status", new JsonObject().put("name", ItemStatus.ON_ORDER.value()))
      .put("holdingsRecordId", HOLDING_ID)
      .put("effectiveLocation", new JsonObject().put("id", EFFECTIVE_LOCATION_ID));
  }

  private CompositePoLine getPoLine(CompositePurchaseOrder order) {
    CompositePoLine poLine = order.getCompositePoLines().get(0);
    poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    poLine.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
    return poLine;
  }

  private void makeBasicUnOpenWorkflowChecks(CompositePurchaseOrder order, CompositePurchaseOrder orderFromStorage) {
    assertEquals(CompositePoLine.ReceiptStatus.PENDING, order.getCompositePoLines().get(0).getReceiptStatus());
    assertEquals(CompositePoLine.PaymentStatus.PENDING, order.getCompositePoLines().get(0).getPaymentStatus());
    verify(openToPendingEncumbranceStrategy).processEncumbrances(order, orderFromStorage, requestContext);
    verify(purchaseOrderLineService).saveOrderLine(any(PoLine.class), eq(requestContext));
  }

  private void makeBasicUnOpenWorkflowChecksForReceiptNotRequired(CompositePurchaseOrder order, CompositePurchaseOrder orderFromStorage) {
    assertEquals(CompositePoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED, order.getCompositePoLines().get(0).getReceiptStatus());
    assertEquals(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED, order.getCompositePoLines().get(0).getPaymentStatus());
    verify(openToPendingEncumbranceStrategy).processEncumbrances(order, orderFromStorage, requestContext);
    verify(purchaseOrderLineService).saveOrderLine(any(PoLine.class), eq(requestContext));
  }

  /**
   * Define unit test specific beans to override actual ones
   */
  static class ContextConfiguration {

    @Bean
    public PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean
    public EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory() {
      return mock(EncumbranceWorkflowStrategyFactory.class);
    }

    @Bean
    public InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean
    public InventoryHoldingManager inventoryHoldingManager() {
      return mock(InventoryHoldingManager.class);
    }

    @Bean
    public PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    public PurchaseOrderStorageService purchaseOrderStorageService() {
      return mock(PurchaseOrderStorageService.class);
    }

    @Bean
    public ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean
    UnOpenCompositeOrderManager unOpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
                                                            EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                                                            InventoryItemManager inventoryItemManager,
                                                            InventoryHoldingManager inventoryHoldingManager,
                                                            PieceStorageService pieceStorageService,
                                                            PurchaseOrderStorageService purchaseOrderStorageService,
                                                            ProtectionService protectionService) {
      return spy(new UnOpenCompositeOrderManager(purchaseOrderLineService, encumbranceWorkflowStrategyFactory, inventoryItemManager,
        inventoryHoldingManager, pieceStorageService, purchaseOrderStorageService, protectionService));
    }
  }
}
