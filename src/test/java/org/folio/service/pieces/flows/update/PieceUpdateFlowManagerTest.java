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
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING_ITEM;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.ProtectionService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.BasePieceFlowHolderBuilder;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

public class PieceUpdateFlowManagerTest {
  @Autowired PieceUpdateFlowManager pieceUpdateFlowManager;
  @Autowired PieceStorageService pieceStorageService;
  @Autowired ProtectionService protectionService;
  @Autowired PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  @Autowired PieceService pieceService;
  @Autowired BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  @Autowired PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService;

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
    Mockito.reset(pieceStorageService, pieceService, protectionService,
                  pieceUpdateFlowPoLineService, pieceUpdateFlowInventoryManager, basePieceFlowHolderBuilder);
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

    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(pieceToUpdate.getId(), requestContext);
    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
    final ArgumentCaptor<PieceUpdateHolder> pieceUpdateHolderCapture = ArgumentCaptor.forClass(PieceUpdateHolder.class);
    doAnswer((Answer<CompletableFuture<Void>>) invocation -> {
      PieceUpdateHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return completedFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));
    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    doNothing().when(pieceService).receiptConsistencyPiecePoLine(any(JsonObject.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceUpdateFlowPoLineService).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));
    //When
    pieceUpdateFlowManager.updatePiece(pieceToUpdate, true, true, requestContext).get();
    //Then
    PieceUpdateHolder holder = pieceUpdateHolderCapture.getValue();
    assertNull(poLine.getLocations().get(0).getLocationId());
    assertEquals(holdingId, poLine.getLocations().get(0).getHoldingId());
    verify(pieceStorageService).updatePiece(eq(pieceToUpdate), eq(requestContext));
    verify(basePieceFlowHolderBuilder, times(0)).updateHolderWithOrderInformation(holder, requestContext);
    verify(pieceUpdateFlowPoLineService).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));
    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndHoldingReferenceChangedAndShouldRunProcessInventory() throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String oldHoldingId = UUID.randomUUID().toString();
    String holdingIdToUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    JsonObject holding = new JsonObject().put(ID, oldHoldingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(oldHoldingId).withFormat(Piece.Format.ELECTRONIC);
    Piece incomingPieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingIdToUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1);
    Location loc = new Location().withHoldingId(oldHoldingId).withQuantityElectronic(1).withQuantity(1);
    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
                                .withEresource(new Eresource().withCreateInventory(INSTANCE_HOLDING_ITEM))
                                .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
                                .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(incomingPieceToUpdate.getId(), requestContext);
    final ArgumentCaptor<Piece> pieceToUpdateCapture = ArgumentCaptor.forClass(Piece.class);
    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdateCapture.capture(), eq(requestContext));
    doNothing().when(pieceService).receiptConsistencyPiecePoLine(any(JsonObject.class), eq(requestContext));
    final ArgumentCaptor<PieceUpdateHolder> pieceUpdateHolderCapture = ArgumentCaptor.forClass(PieceUpdateHolder.class);
    doAnswer((Answer<CompletableFuture<Void>>) invocation -> {
      PieceUpdateHolder answerHolder = invocation.getArgument(0);
      answerHolder.withOrderInformation(purchaseOrder, poLine);
      return completedFuture(null);
    }).when(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));

    final ArgumentCaptor<CompositePoLine> poLineToUpdateCapture = ArgumentCaptor.forClass(CompositePoLine.class);
        doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceUpdateFlowPoLineService).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));

    //When
    pieceUpdateFlowManager.updatePiece(incomingPieceToUpdate, true, true, requestContext).get();
    //Then
//    CompositePoLine poLineToUpdate = poLineToUpdateCapture.getValue();
      Piece pieceToUpdate = pieceToUpdateCapture.getValue();
//    assertNull(pieceToUpdate.getLocationId());
//    assertEquals(holdingIdToUpdate, pieceToUpdate.getHoldingId());
//    Location locationToSave = poLineToUpdate.getLocations().get(0);
//    Cost costToSave = poLineToUpdate.getCost();
//    assertNull(locationToSave.getLocationId());
//    assertEquals(1, locationToSave.getQuantityElectronic());
//    assertEquals(1, locationToSave.getQuantity());
//    assertEquals(pieceToUpdate.getHoldingId(), locationToSave.getHoldingId());
//    assertEquals(1, costToSave.getQuantityElectronic());
//    assertNull(costToSave.getQuantityPhysical());
//
//    assertNull(poLine.getLocations().get(0).getLocationId());
//    assertEquals(oldHoldingId, poLine.getLocations().get(0).getHoldingId());
    verify(pieceStorageService).updatePiece(eq(pieceToUpdate), eq(requestContext));
    verify(basePieceFlowHolderBuilder).updateHolderWithOrderInformation(pieceUpdateHolderCapture.capture(), eq(requestContext));
    verify(pieceUpdateFlowPoLineService).updatePoLine(pieceUpdateHolderCapture.capture(), eq(requestContext));
    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
  }
//
//  @Test
//  void shouldUpdateLineQuantityAndAddNewLocationWithHoldingIfPoLineIsNotPackageAndHoldingReferenceChangedAndShouldRunProcessInventory() throws ExecutionException, InterruptedException {
//    String orderId = UUID.randomUUID().toString();
//    String oldHoldingId = UUID.randomUUID().toString();
//    String holdingIdToUpdate = UUID.randomUUID().toString();
//    String lineId = UUID.randomUUID().toString();
//    String titleId = UUID.randomUUID().toString();
//    String itemId = UUID.randomUUID().toString();
//    String locationId = UUID.randomUUID().toString();
//    String pieceId = UUID.randomUUID().toString();
//    JsonObject item = new JsonObject().put(ID, itemId);
//    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
//    JsonObject holding = new JsonObject().put(ID, oldHoldingId);
//    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
//    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
//      .withHoldingId(oldHoldingId).withFormat(Piece.Format.PHYSICAL);
//    Piece incomingPieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
//      .withHoldingId(holdingIdToUpdate).withFormat(Piece.Format.PHYSICAL);
//    Cost cost = new Cost().withQuantityPhysical(2);
//    Location loc = new Location().withHoldingId(oldHoldingId).withQuantityPhysical(2).withQuantity(2);
//    PoLine poLine = new PoLine().withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
//                                .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
//                                .withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
//                                .withLocations(List.of(loc)).withCost(cost);
//    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
//
//    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(incomingPieceToUpdate.getId(), requestContext);
//    final ArgumentCaptor<Piece> pieceToUpdateCapture = ArgumentCaptor.forClass(Piece.class);
//    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdateCapture.capture(), eq(requestContext));
//    doNothing().when(pieceService).receiptConsistencyPiecePoLine(any(JsonObject.class), eq(requestContext));
//    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(incomingPieceToUpdate.getPoLineId()), eq(requestContext));
//
//    final ArgumentCaptor<CompositePoLine> poLineToUpdateCapture = ArgumentCaptor.forClass(CompositePoLine.class);
//    doReturn(completedFuture(null)).when(purchaseOrderLineService).updateOrderLine(poLineToUpdateCapture.capture(), eq(requestContext));
//
//    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
//    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
//    doReturn(completedFuture(null)).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
//    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(any(CompositePurchaseOrder.class),
//      any(CompositePurchaseOrder.class), eq(requestContext));
//
//    //When
//    pieceUpdateFlowManager.updatePiece(incomingPieceToUpdate, true, true, requestContext).get();
//    //Then
//    CompositePoLine poLineToUpdate = poLineToUpdateCapture.getValue();
//    Piece pieceToUpdate = pieceToUpdateCapture.getValue();
//    assertNull(pieceToUpdate.getLocationId());
//    assertEquals(holdingIdToUpdate, pieceToUpdate.getHoldingId());
//    Location oldLocationToSave = poLineToUpdate.getLocations().stream()
//                                  .filter(loca -> loca.getHoldingId().equals(oldHoldingId)).findAny().get();
//    Location newLocationToSave = poLineToUpdate.getLocations().stream()
//                                  .filter(loca -> loca.getHoldingId().equals(holdingIdToUpdate)).findAny().get();
//    Cost costToSave = poLineToUpdate.getCost();
//    assertNull(oldLocationToSave.getLocationId());
//    assertEquals(1, oldLocationToSave.getQuantityPhysical());
//    assertNull(oldLocationToSave.getQuantityElectronic());
//    assertEquals(1, oldLocationToSave.getQuantity());
//    assertEquals(oldHoldingId, oldLocationToSave.getHoldingId());
//    assertNull(newLocationToSave.getQuantityElectronic());
//    assertEquals(1, newLocationToSave.getQuantityPhysical());
//    assertEquals(1, newLocationToSave.getQuantity());
//    assertEquals(holdingIdToUpdate, newLocationToSave.getHoldingId());
//    assertNull(newLocationToSave.getQuantityElectronic());
//    assertEquals(2, costToSave.getQuantityPhysical());
//    assertNull(costToSave.getQuantityElectronic());
//
//    verify(pieceStorageService).updatePiece(eq(pieceToUpdate), eq(requestContext));
//    verify(receivingEncumbranceStrategy, times(1)).processEncumbrances(any(CompositePurchaseOrder.class),
//      any(CompositePurchaseOrder.class), eq(requestContext));
//    verify(purchaseOrderLineService, times(1)).updateOrderLine(eq(poLineToUpdate), eq(requestContext));
//    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
//    verify(pieceStorageService).updatePiece(pieceToUpdate, requestContext);
//  }
//
//  @Test
//  void shouldUpdateLineQuantityIfPoLineIsNotPackageAndLocationReferenceChangedAndShouldRunProcessInventory() throws ExecutionException, InterruptedException {
//    String orderId = UUID.randomUUID().toString();
//    String oldHoldingId = UUID.randomUUID().toString();
//    String newCreatedHoldingId = UUID.randomUUID().toString();
//    String locationToUpdate = UUID.randomUUID().toString();
//    String lineId = UUID.randomUUID().toString();
//    String titleId = UUID.randomUUID().toString();
//    String itemId = UUID.randomUUID().toString();
//    String locationId = UUID.randomUUID().toString();
//    String pieceId = UUID.randomUUID().toString();
//    JsonObject item = new JsonObject().put(ID, itemId);
//    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
//    JsonObject holding = new JsonObject().put(ID, oldHoldingId);
//    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
//    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
//                                        .withHoldingId(oldHoldingId).withFormat(Piece.Format.ELECTRONIC);
//    Piece incomingPieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
//                                    .withLocationId(locationToUpdate).withFormat(Piece.Format.ELECTRONIC);
//    Cost cost = new Cost().withQuantityElectronic(1);
//    Location loc = new Location().withHoldingId(oldHoldingId).withQuantityElectronic(1).withQuantity(1);
//    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
//      .withLocations(List.of(loc)).withCost(cost);
//    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
//
//    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(incomingPieceToUpdate.getId(), requestContext);
//
//    final ArgumentCaptor<Piece> pieceToUpdateCapture = ArgumentCaptor.forClass(Piece.class);
//    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(pieceToUpdateCapture.capture(), eq(requestContext));
//    doNothing().when(pieceService).receiptConsistencyPiecePoLine(any(JsonObject.class), eq(requestContext));
//    doReturn(completedFuture(poLine)).when(purchaseOrderLineService).getOrderLineById(eq(incomingPieceToUpdate.getPoLineId()), eq(requestContext));
//
//    final ArgumentCaptor<CompositePoLine> poLineToUpdateCapture = ArgumentCaptor.forClass(CompositePoLine.class);
//    doReturn(completedFuture(null)).when(purchaseOrderLineService).updateOrderLine(poLineToUpdateCapture.capture(), eq(requestContext));
//    doReturn(completedFuture(purchaseOrder)).when(purchaseOrderService).getPurchaseOrderById(poLine.getPurchaseOrderId(), requestContext);
//    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), any(ProtectedOperationType.class), eq(requestContext));
//    doAnswer((Answer<CompletableFuture<Void>>) invocation -> {
//      PieceUpdateHolder answerHolder = invocation.getArgument(0);
//      answerHolder.getPieceToUpdate().setLocationId(null);
//      answerHolder.getPieceToUpdate().setHoldingId(newCreatedHoldingId);
//      return completedFuture(null);
//    }).when(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
//    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(any(CompositePurchaseOrder.class),
//                                            any(CompositePurchaseOrder.class), eq(requestContext));
//    //When
//    pieceUpdateFlowManager.updatePiece(incomingPieceToUpdate, true, true, requestContext).get();
//    //Then
//    CompositePoLine poLineToUpdate = poLineToUpdateCapture.getValue();
//    Piece pieceToUpdate = pieceToUpdateCapture.getValue();
//    assertNull(pieceToUpdate.getLocationId());
//    assertEquals(newCreatedHoldingId, pieceToUpdate.getHoldingId());
//    Location locationToSave = poLineToUpdate.getLocations().get(0);
//    Cost costToSave = poLineToUpdate.getCost();
//    assertNull(locationToSave.getLocationId());
//    assertEquals(1, locationToSave.getQuantityElectronic());
//    assertEquals(1, locationToSave.getQuantity());
//    assertEquals(pieceToUpdate.getHoldingId(), locationToSave.getHoldingId());
//    assertEquals(1, costToSave.getQuantityElectronic());
//    assertNull(costToSave.getQuantityPhysical());
//
//    verify(pieceStorageService).updatePiece(eq(pieceToUpdate), eq(requestContext));
//    verify(receivingEncumbranceStrategy, times(1)).processEncumbrances(any(CompositePurchaseOrder.class),
//                any(CompositePurchaseOrder.class), eq(requestContext));
//    verify(purchaseOrderLineService, times(1)).updateOrderLine(eq(poLineToUpdate), eq(requestContext));
//    verify(pieceUpdateFlowInventoryManager).processInventory(any(PieceUpdateHolder.class), eq(requestContext));
//    verify(pieceStorageService).updatePiece(incomingPieceToUpdate, requestContext);
//  }

  private static class ContextConfiguration {
    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }
    @Bean ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }
    @Bean PieceService pieceService() {
      return mock(PieceService.class);
    }
    @Bean PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager() {
      return mock(PieceUpdateFlowInventoryManager.class);
    }

    @Bean BasePieceFlowHolderBuilder basePieceFlowHolderBuilder() {
      return mock(BasePieceFlowHolderBuilder.class);
    }
    @Bean PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService() {
      return  mock(PieceUpdateFlowPoLineService.class);
    }

    @Bean PieceUpdateFlowManager pieceUpdateFlowManager(PieceStorageService pieceStorageService, PieceService pieceService,
                      ProtectionService protectionService, PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService,
              PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager, BasePieceFlowHolderBuilder basePieceFlowHolderBuilder) {
      return new PieceUpdateFlowManager(pieceStorageService, pieceService, protectionService,
                      pieceUpdateFlowPoLineService, pieceUpdateFlowInventoryManager, basePieceFlowHolderBuilder);
    }
  }
}
