package org.folio.service.pieces.flows.update;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.ID;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategyResolver;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

public class PieceUpdateFlowManagerTest {
  @Autowired PieceUpdateFlowManager pieceUpdateFlowManager;
  @Autowired PieceStorageService pieceStorageService;
  @Autowired ProtectionService protectionService;
  @Autowired PurchaseOrderService purchaseOrderService;
  @Autowired PurchaseOrderLineService purchaseOrderLineService;
  @Autowired InventoryManager inventoryManager;
  @Autowired ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  @Autowired PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver;
  @Autowired PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  @Autowired PieceService pieceService;

  @Spy
  private Context ctxMock = getFirstContextFromVertx(getVertx());
  @Mock
  private Map<String, String> okapiHeadersMock;

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(PieceUpdateFlowManagerTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
    Mockito.reset(pieceStorageService, protectionService,
      purchaseOrderService, purchaseOrderLineService, inventoryManager,
      receivingEncumbranceStrategy, pieceFlowUpdatePoLineStrategyResolver);
  }

  @Test
  void shouldNotUpdateLineQuantityIfPoLineIsPackageAndShouldRunProcessInventory() throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String holdingIdTpUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    JsonObject item = new JsonObject().put(ID, itemId);
    String pieceId = UUID.randomUUID().toString();
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingIdTpUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder(purchaseOrder, poLine,true);

    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(pieceToUpdate.getId(), requestContext);
    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(pieceToUpdate.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    doNothing().when(pieceService).receiptConsistencyPiecePoLine(any(JsonObject.class), eq(requestContext));
    //When
    pieceUpdateFlowManager.updatePiece(pieceToUpdate, true, requestContext).get();
    //Then
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(pieceStorageService).updatePiece(eq(pieceToUpdate), eq(requestContext));
    verify(receivingEncumbranceStrategy, times(0)).processEncumbrances(holder.getPurchaseOrderToSave(),
      holder.getOriginPurchaseOrder(), requestContext);
    verify(purchaseOrderLineService, times(0)).updateOrderLine(eq(holder.getPoLineToSave()), eq(requestContext));
    verify(pieceFlowUpdatePoLineStrategyResolver, times(0)).resolve(any(PieceFlowUpdatePoLineKey.class));
    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndHoldingReferenceChangedAndShouldRunProcessInventory() throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String holdingIdTpUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingIdTpUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder(purchaseOrder, poLine,true);

    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(pieceToUpdate.getId(), requestContext);
    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
    doNothing().when(pieceService).receiptConsistencyPiecePoLine(any(JsonObject.class), eq(requestContext));
    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(pieceToUpdate.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(holder.getPurchaseOrderToSave(),
                                                                        holder.getOriginPurchaseOrder(), requestContext);

    //When
    pieceUpdateFlowManager.updatePiece(pieceToUpdate, true, requestContext).get();
    //Then
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(pieceStorageService).updatePiece(eq(pieceToUpdate), eq(requestContext));
    verify(receivingEncumbranceStrategy, times(1)).processEncumbrances(holder.getPurchaseOrderToSave(),
      holder.getOriginPurchaseOrder(), requestContext);
    verify(purchaseOrderLineService, times(1)).updateOrderLine(eq(holder.getPoLineToSave()), eq(requestContext));

    verify(pieceFlowUpdatePoLineStrategyResolver, times(2)).resolve(any(PieceFlowUpdatePoLineKey.class));
    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndLocationReferenceChangedAndShouldRunProcessInventory() throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String locationToUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withLocationId(locationToUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder(purchaseOrder, poLine,true);

    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(pieceToUpdate.getId(), requestContext);
    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
    doNothing().when(pieceService).receiptConsistencyPiecePoLine(any(JsonObject.class), eq(requestContext));
    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(pieceToUpdate.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(holder.getOriginPurchaseOrder(),
      holder.getPurchaseOrderToSave(), requestContext);

    //When
    pieceUpdateFlowManager.updatePiece(pieceToUpdate, true, requestContext).get();
    //Then
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(pieceStorageService).updatePiece(eq(pieceToUpdate), eq(requestContext));
    verify(receivingEncumbranceStrategy, times(1)).processEncumbrances(holder.getOriginPurchaseOrder(),
      holder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService, times(1)).updateOrderLine(eq(holder.getPoLineToSave()), eq(requestContext));

    verify(pieceFlowUpdatePoLineStrategyResolver, times(2)).resolve(any(PieceFlowUpdatePoLineKey.class));
    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
  }

  private static class ContextConfiguration {
    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }
    @Bean ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }
    @Bean PurchaseOrderService purchaseOrderService() {
      return mock(PurchaseOrderService.class);
    }
    @Bean PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }
    @Bean ReceivingEncumbranceStrategy receivingEncumbranceStrategy() {
      return mock(ReceivingEncumbranceStrategy.class);
    }
    @Bean PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver() {
      return mock(PieceFlowUpdatePoLineStrategyResolver.class);
    }
    @Bean InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }
    @Bean PieceService pieceService() {
      return mock(PieceService.class);
    }
    @Bean PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager() {
      return mock(PieceUpdateFlowInventoryManager.class);
    }

    @Bean PieceUpdateFlowManager pieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService,
                        ProtectionService protectionService, PurchaseOrderService purchaseOrderService,
                        PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy,
                        PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver,
                        PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager) {
      return new PieceUpdateFlowManager(pieceStorageService, pieceService, protectionService,
                        purchaseOrderService, purchaseOrderLineService, receivingEncumbranceStrategy,
                        pieceFlowUpdatePoLineStrategyResolver, pieceUpdateFlowInventoryManager);
    }
  }
}
