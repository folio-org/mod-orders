package org.folio.service.pieces.flows.create;

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
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.pieces.PieceCreationHolder;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

public class PieceCreateFlowPoLineServiceTest {
  @Autowired PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired PurchaseOrderLineService purchaseOrderLineService;
  @Autowired ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  @Autowired PieceCreateFlowPoLineService pieceCreateFlowPoLineService;

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
    initSpringContext(PieceCreateFlowPoLineServiceTest.ContextConfiguration.class);
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
  @DisplayName("Add 1 electronic piece with holding to electronic pol with 1 location and same holding id as in piece")
  void electAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame()
                                        throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
                                      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(2, poLineToSave.getCost().getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantity());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
                                        incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }


  @Test
  @DisplayName("Add 1 physical piece with holding to physical pol with 1 location and same holding id as in piece")
  void physAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame()
                throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityPhysical(1).withQuantity(1);
    Cost cost = new Cost().withQuantityPhysical(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
                      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
                      .withPhysical(physical)
                      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();

    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertEquals(2, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantity());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Add 1 physical piece with location to physical pol with 1 location and same location id as in piece")
  void physAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                      throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(1).withQuantity(1);
    Cost cost = new Cost().withQuantityPhysical(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertEquals(2, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantity());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Add 1 electronic piece with location to electronic pol with 1 location and same location id as in piece")
  void elecAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(2, poLineToSave.getCost().getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantity());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Add 1 electronic piece with location to electronic pol with 1 location with holding id")
  void elecAddStrategyShouldIncreaseQuantityTo2ForCostAndLocationIfInitiallyWas1AndLocationIdInPieceAndPOLWithHoldingId()
                        throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(2, poLineToSave.getCost().getQuantityElectronic());
    assertEquals(2, poLineToSave.getLocations().size());
    assertNull(poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertNull(poLineToSave.getLocations().get(1).getQuantityPhysical());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantity());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
                                        incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Add 1 electronic piece with holding id to mixed pol with 1 location with holding id and electronic and physical qty")
  void shouldIncreaseQuantityTo3ForCostAndLocationIfInitiallyWas2AndHoldingIdInPieceAndMixedPOLWithHoldingId()
    throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece pieceToCreate = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantityPhysical(1).withQuantity(2);
    Cost cost = new Cost().withQuantityElectronic(1).withQuantityPhysical(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.P_E_MIX).withId(lineId)
      .withEresource(eresource)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertEquals(1, poLineToSave.getLocations().size());
    assertEquals(3, poLineToSave.getLocations().get(0).getQuantity());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantityElectronic());

    assertEquals(1, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(2, poLineToSave.getCost().getQuantityElectronic());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Add 1 electronic piece with location id to mixed pol with 1 location with location id and electronic and physical qty")
  void shouldIncreaseQuantityTo3ForCostAndLocationIfInitiallyWas2AndLocationIdInPieceAndMixedPOLWithLocationId()
    throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece pieceToCreate = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantityPhysical(1).withQuantity(2);
    Cost cost = new Cost().withQuantityElectronic(1).withQuantityPhysical(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.P_E_MIX).withId(lineId)
      .withEresource(eresource)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertEquals(1, poLineToSave.getLocations().size());
    assertEquals(3, poLineToSave.getLocations().get(0).getQuantity());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertEquals(2, poLineToSave.getLocations().get(0).getQuantityElectronic());

    assertEquals(1, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(2, poLineToSave.getCost().getQuantityElectronic());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Add 1 electronic piece without location id to mixed pol with 1 location with location id and physical qty and create inventory NONE")
  void shouldNotUpdateLocationIfCreateInventoryNoneForElectronicButUpdateCostQty()
    throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece pieceToCreate = new Piece().withPoLineId(lineId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1).withQuantityPhysical(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.NONE);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.P_E_MIX).withId(lineId)
      .withEresource(eresource)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertEquals(1, poLineToSave.getLocations().size());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantity());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityPhysical());
    assertNull(poLineToSave.getLocations().get(0).getQuantityElectronic());

    assertEquals(1, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(2, poLineToSave.getCost().getQuantityElectronic());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Add 1 physical piece without location id to mixed pol with 1 location with location id and physical qty and create inventory NONE")
  void shouldNotUpdateLocationIfCreateInventoryNoneForPhysicalButUpdateCostQty()
    throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece pieceToCreate = new Piece().withPoLineId(lineId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1).withQuantityPhysical(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.NONE);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.P_E_MIX).withId(lineId)
      .withEresource(eresource)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertEquals(1, poLineToSave.getLocations().size());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantity());
    assertEquals(1, poLineToSave.getLocations().get(0).getQuantityElectronic());
    assertNull(poLineToSave.getLocations().get(0).getQuantityPhysical());

    assertEquals(2, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(1, poLineToSave.getCost().getQuantityElectronic());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Add 1 physical piece without location id to physical pol without location and physical qty and create inventory NONE")
  void shouldNotUpdateLocationIfCreateInventoryNoneForPhysicalAndNoLocationInTheLineButUpdateCostQty()
    throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece pieceToCreate = new Piece().withPoLineId(lineId).withFormat(Piece.Format.PHYSICAL);
    Cost cost = new Cost().withQuantityElectronic(1).withQuantityPhysical(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.NONE);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withPhysical(physical)
      .withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(pieceToCreate).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertEquals(0, poLineToSave.getLocations().size());

    assertEquals(2, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(1, poLineToSave.getCost().getQuantityElectronic());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @ParameterizedTest
  @DisplayName("Add 1 electronic piece with location id to electronic pol with 2 location and another location id as in piece")
  @CsvSource(value = {"Electronic Resource:Instance:2:3:Electronic:6:6.0",
                      "Electronic Resource:None:2:3:Electronic:6:6.0"}, delimiter = ':')
  void electAddStrategyShouldIncreaseQuantityTo3ForCostAndLocationIfInitiallyWas2AndLocationIdInPOLAndPieceTheSameInventoryNone(
          String lineType, String createInventory, int qty1, int qty2, String pieceFormat, int expQty, double expEstimatedPrice)
          throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String locationId1 = UUID.randomUUID().toString();
    String locationId2 = UUID.randomUUID().toString();
    String locationId3 = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId3).withFormat(Piece.Format.fromValue(pieceFormat));
    Location loc1 = new Location().withLocationId(locationId1).withQuantityElectronic(qty1).withQuantity(qty1);
    Location loc2 = new Location().withLocationId(locationId2).withQuantityElectronic(qty2).withQuantity(qty2);
    Cost cost = new Cost().withQuantityElectronic(qty1 + qty2).withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
                                  .withPoLineEstimatedPrice((double) (qty1 + qty2));
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(createInventory));
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.fromValue(lineType)).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc1, loc2)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(expQty, poLineToSave.getCost().getQuantityElectronic());
    assertEquals(expEstimatedPrice, poLineToSave.getCost().getPoLineEstimatedPrice());
    assertEquals(3, poLineToSave.getLocations().size());
    Location actLoc1 = poLineToSave.getLocations().stream().filter(loc -> locationId1.equals(loc.getLocationId())).findFirst().get();
    Location actLoc2 = poLineToSave.getLocations().stream().filter(loc -> locationId2.equals(loc.getLocationId())).findFirst().get();
    Location actLoc3 = poLineToSave.getLocations().stream().filter(loc -> locationId3.equals(loc.getLocationId())).findFirst().get();

    assertEquals(qty1, actLoc1.getQuantityElectronic());
    assertEquals(qty1, actLoc1.getQuantity());
    assertNull(actLoc1.getQuantityPhysical());
    assertEquals(qty2, actLoc2.getQuantityElectronic());
    assertEquals(qty2, actLoc2.getQuantity());
    assertNull(actLoc2.getQuantityPhysical());
    assertEquals(1, actLoc3.getQuantityElectronic());
    assertEquals(1, actLoc3.getQuantity());
    assertNull(actLoc3.getQuantityPhysical());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @ParameterizedTest
  @DisplayName("Add 1 physical piece with location id to physical pol with 2 location and another location id as in piece")
  @CsvSource(value = {"Physical Resource:None:2:4:Physical:7:7.0",
                      "Physical Resource:Instance:2:4:Physical:7:7.0",
                      "Other:None:2:4:Other:7:7.0",
                      "Other:None:2:4:Other:7:7.0"}, delimiter = ':')
  void physOrNonetAddStrategyShouldIncreaseQuantityTo3ForCostAndLocationIfInitiallyWas2AndLocationIdInPOLAndPieceTheSameInventoryNone(
    String lineType, String createInventory, int qty1, int qty2, String pieceFormat, int expQty, double expEstimatedPrice)
    throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String locationId1 = UUID.randomUUID().toString();
    String locationId2 = UUID.randomUUID().toString();
    String locationId3 = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId3).withFormat(Piece.Format.fromValue(pieceFormat));
    Location loc1 = new Location().withLocationId(locationId1).withQuantityPhysical(qty1).withQuantity(qty1);
    Location loc2 = new Location().withLocationId(locationId2).withQuantityPhysical(qty2).withQuantity(qty2);
    Cost cost = new Cost().withQuantityPhysical(qty1 + qty2).withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice((double) (qty1 + qty2));
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(createInventory));
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.fromValue(lineType)).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc1, loc2)).withCost(cost);

    PieceCreationHolder incomingUpdateHolder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(completedFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(completedFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);

    //When
    pieceCreateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).get();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertEquals(expQty, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(expEstimatedPrice, poLineToSave.getCost().getPoLineEstimatedPrice());
    assertEquals(3, poLineToSave.getLocations().size());
    Location actLoc1 = poLineToSave.getLocations().stream().filter(loc -> locationId1.equals(loc.getLocationId())).findFirst().get();
    Location actLoc2 = poLineToSave.getLocations().stream().filter(loc -> locationId2.equals(loc.getLocationId())).findFirst().get();
    Location actLoc3 = poLineToSave.getLocations().stream().filter(loc -> locationId3.equals(loc.getLocationId())).findFirst().get();

    assertEquals(qty1, actLoc1.getQuantityPhysical());
    assertEquals(qty1, actLoc1.getQuantity());
    assertNull(actLoc1.getQuantityElectronic());
    assertEquals(qty2, actLoc2.getQuantityPhysical());
    assertEquals(qty2, actLoc2.getQuantity());
    assertNull(actLoc2.getQuantityElectronic());
    assertEquals(1, actLoc3.getQuantityPhysical());
    assertEquals(1, actLoc3.getQuantity());
    assertNull(actLoc3.getQuantityElectronic());

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

    @Bean PieceCreateFlowPoLineService pieceCreateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
      PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
      return new PieceCreateFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy);
    }
  }
}
