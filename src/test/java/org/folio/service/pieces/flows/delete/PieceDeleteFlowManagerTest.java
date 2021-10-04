package org.folio.service.pieces.flows.delete;

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
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.vertx.core.json.JsonObject;
import org.folio.ApiTestSuite;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.ProtectionService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineKey;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategies;
import org.folio.service.pieces.flows.PieceFlowUpdatePoLineStrategyResolver;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

public class PieceDeleteFlowManagerTest {
  @Autowired
  PieceDeleteFlowManager pieceDeleteFlowManager;
  @Autowired
  PieceStorageService pieceStorageService;
  @Autowired
  ProtectionService protectionService;
  @Autowired
  PurchaseOrderService purchaseOrderService;
  @Autowired
  PurchaseOrderLineService purchaseOrderLineService;
  @Autowired
  InventoryManager inventoryManager;
  @Autowired
  ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  @Autowired
  PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver;

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
    initSpringContext(PieceDeleteFlowManagerTest.ContextConfiguration.class);
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
  void shouldNotUpdateLineQuantityIfPoLineIsPackage() throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
                                          .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
                                .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceDeletionHolder holder = new PieceDeletionHolder(purchaseOrder, poLine,true);

    doReturn(completedFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));

    //When
    pieceDeleteFlowManager.deletePieceWithItem(piece.getId(), true, requestContext).get();
    //Then
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(receivingEncumbranceStrategy, times(0)).processEncumbrances(holder.getPurchaseOrderToSave(),
                                    holder.getOriginPurchaseOrder(), requestContext);
    verify(purchaseOrderLineService, times(0)).updateOrderLine(eq(holder.getPoLineToSave()), eq(requestContext));
    verify(pieceFlowUpdatePoLineStrategyResolver, times(0)).resolve(any(PieceFlowUpdatePoLineKey.class));
  }

  @Test
  void shouldNotUpdateLineQuantityIfPoLineIsNotPackageAndManualPieceCreateTrue() throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withLocations(List.of(loc)).withCost(cost).withCheckinItems(true);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceDeletionHolder holder = new PieceDeletionHolder(purchaseOrder, poLine,true);

    doReturn(completedFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));

    //When
    pieceDeleteFlowManager.deletePieceWithItem(piece.getId(), true, requestContext).get();
    //Then
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(receivingEncumbranceStrategy, times(0)).processEncumbrances(holder.getPurchaseOrderToSave(),
      holder.getOriginPurchaseOrder(), requestContext);
    verify(purchaseOrderLineService, times(0)).updateOrderLine(eq(holder.getPoLineToSave()), eq(requestContext));
    verify(pieceFlowUpdatePoLineStrategyResolver, times(0)).resolve(any());
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndManualPieceCreateFalse() throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    PoLine poLine = new PoLine().withIsPackage(false).withCheckinItems(false).withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.NONE))
                  .withPurchaseOrderId(orderId).withId(lineId).withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    doReturn(completedFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));
    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(null)).when(purchaseOrderLineService).updateOrderLine(any(CompositePoLine.class), eq(requestContext));
    doReturn(Optional.of(PieceFlowUpdatePoLineStrategies.DELETE)).when(pieceFlowUpdatePoLineStrategyResolver)
                                                                              .resolve(any(PieceFlowUpdatePoLineKey.class));
    //When
    pieceDeleteFlowManager.deletePieceWithItem(piece.getId(), true, requestContext).get();
    //Then
    verify(receivingEncumbranceStrategy, times(1)).processEncumbrances(any(CompositePurchaseOrder.class),
                                                            any(CompositePurchaseOrder.class), eq(requestContext));
    verify(purchaseOrderLineService, times(1)).getOrderLineById(eq(piece.getPoLineId()), eq(requestContext));
    verify(purchaseOrderLineService, times(1)).updateOrderLine(any(CompositePoLine.class), eq(requestContext));
    verify(pieceFlowUpdatePoLineStrategyResolver, times(1)).resolve(any());
    verify(pieceStorageService, times(1)).deletePiece(eq(piece.getId()), eq(requestContext));
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndManualPieceCreateFalseAndInventoryInstanceVsHolding() throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    PoLine poLine = new PoLine().withIsPackage(false).withCheckinItems(false).withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING))
      .withPurchaseOrderId(orderId).withId(lineId).withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceDeletionHolder holder = spy(new PieceDeletionHolder(purchaseOrder, poLine,true));

    doReturn(completedFuture(piece)).when(pieceStorageService).getPieceById(piece.getId(), requestContext);
    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceStorageService).deletePiece(eq(piece.getId()), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).getNumberOfRequestsByItemId(eq(piece.getItemId()), eq(requestContext));
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holdingId, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).getItemsByHoldingId(holdingId, requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).deleteHolding(piece.getHoldingId(), true, requestContext);
    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(null)).when(purchaseOrderLineService).updateOrderLine(any(CompositePoLine.class), eq(requestContext));
    doReturn(Optional.of(PieceFlowUpdatePoLineStrategies.DELETE)).when(pieceFlowUpdatePoLineStrategyResolver)
      .resolve(any(PieceFlowUpdatePoLineKey.class));
    doNothing().when(holder).shallowCopy(holder);
    //When
    pieceDeleteFlowManager.deletePieceWithItem(piece.getId(), true, requestContext).get();
    //Then
    Assertions.assertEquals(holdingId, holder.getPoLineToSave().getLocations().get(0).getHoldingId());
    verify(receivingEncumbranceStrategy, times(1)).processEncumbrances(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    verify(purchaseOrderLineService, times(1)).getOrderLineById(eq(piece.getPoLineId()), eq(requestContext));
    verify(purchaseOrderLineService, times(1)).updateOrderLine(any(CompositePoLine.class), eq(requestContext));
    verify(pieceFlowUpdatePoLineStrategyResolver, times(1)).resolve(any());
    verify(pieceStorageService, times(1)).deletePiece(eq(piece.getId()), eq(requestContext));
    verify(inventoryManager, times(1)).deleteHolding(piece.getHoldingId(), true, requestContext);
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


    @Bean PieceDeleteFlowManager pieceDeleteFlowManager(PieceStorageService pieceStorageService, ProtectionService protectionService,
      PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService, InventoryManager inventoryManager,
      ReceivingEncumbranceStrategy receivingEncumbranceStrategy, PieceFlowUpdatePoLineStrategyResolver pieceFlowUpdatePoLineStrategyResolver) {
      return new PieceDeleteFlowManager(pieceStorageService, protectionService, purchaseOrderService, purchaseOrderLineService,
                            inventoryManager, receivingEncumbranceStrategy, pieceFlowUpdatePoLineStrategyResolver);
    }
  }
}
