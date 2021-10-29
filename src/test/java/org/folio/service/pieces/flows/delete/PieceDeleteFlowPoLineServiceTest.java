package org.folio.service.pieces.flows.delete;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus.OPEN;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.pieces.PieceDeletionHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

public class PieceDeleteFlowPoLineServiceTest {
  @Autowired PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired PurchaseOrderLineService purchaseOrderLineService;
  @Autowired ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  @Autowired PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService;

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
    initSpringContext(PieceDeleteFlowPoLineServiceTest.ContextConfiguration.class);
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
    Mockito.reset(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
  }


  @Test
  @DisplayName("Delete 1 physical piece with holding to physical pol with 1 location and same holding id as in piece")
  void physDeleteStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame()
                 throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityPhysical(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(2);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertEquals(1, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantity());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Delete 1 physical piece with location to physical pol with 1 location and same location id as in piece")
  void physDeleteStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                  throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(2);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertEquals(1, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantity());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Delete 1 electronic piece with location to electronic pol with 1 location and same location id as in piece")
  void elecDeleteStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                  throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(2);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
        incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
          incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
        pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(1, poLineToSave.getCost().getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantity());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Should delete POL location if 1 physical piece with location to physical pol with 1 location and same location id as in piece")
  void physDeleteLocationStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(1).withQuantity(1);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(1);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(Collections.emptyList(), poLineToSave.getLocations());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Should delete POL location if 1 electronic piece with location to electronic pol with 1 location and same location id as in piece")
  void elecDeleteLocationStrategyShouldDecreaseIncreaseQuantityForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(1);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertEquals(Collections.emptyList(), poLineToSave.getLocations());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Delete 1 electronic piece with holding to electronic pol with 1 location and same holding id as in piece")
  void electDeleteStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame()
                throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(2);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(1, poLineToSave.getCost().getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantity());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  private static class ContextConfiguration {
    @Bean PurchaseOrderStorageService purchaseOrderService() {
      return mock(PurchaseOrderStorageService.class);
    }

    @Bean PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean ReceivingEncumbranceStrategy receivingEncumbranceStrategy() {
      return mock(ReceivingEncumbranceStrategy.class);
    }

    @Bean  PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
                      PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
      return new PieceDeleteFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
    }
  }
}
