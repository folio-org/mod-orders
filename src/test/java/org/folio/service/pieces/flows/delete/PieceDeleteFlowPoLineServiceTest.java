package org.folio.service.pieces.flows.delete;

import static io.vertx.core.Future.succeededFuture;
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
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.junit5.VertxExtension;

@ExtendWith(VertxExtension.class)
public class PieceDeleteFlowPoLineServiceTest {
  @Autowired PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired PurchaseOrderLineService purchaseOrderLineService;
  @Autowired ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  @Autowired PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService;

  private final Context ctx = getFirstContextFromVertx(getVertx());
  @Mock
  private Map<String, String> okapiHeadersMock;

  private RequestContext requestContext;
  private static boolean runningOnOwn;
  private AutoCloseable mockitoMocks;

  @BeforeEach
  void initMocks(){
    mockitoMocks = MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
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
  void resetMocks() throws Exception {
    clearServiceInteractions();
    mockitoMocks.close();
  }


  @Test
  @DisplayName("Delete 1 physical piece with holding to physical pol with 1 location and same holding id as in piece")
  void physDeleteStrategyShouldDecreaseQuantityTo1ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame()
                 throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityPhysical(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(2)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(2d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(succeededFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
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
  void physDeleteStrategyShouldDecreaseQuantityTo1ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                  throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(2)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(2d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(succeededFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
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
  void elecDeleteStrategyShouldDecreaseQuantityTo1ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                  throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(2)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(locations).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
        incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(succeededFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
          incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
        pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
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
  void physDeleteLocationStrategyShouldDecreaseQuantityTo1ForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withLocationId(locationId).withQuantityPhysical(1).withQuantity(1);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityPhysical(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(physical)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(succeededFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertEquals(0, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(Collections.emptyList(), poLineToSave.getLocations());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Should delete POL location if 1 electronic piece with location to electronic pol with 1 location and same location id as in piece")
  void elecDeleteLocationStrategyShouldDecreaseQuantityForCostAndLocationIfInitiallyWas1AndLocationIdInPOLAndPieceTheSame()
                throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(succeededFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(0, poLineToSave.getCost().getQuantityElectronic());
    assertEquals(Collections.emptyList(), poLineToSave.getLocations());
    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @Test
  @DisplayName("Delete 1 electronic piece with holding to electronic pol with 1 location and same holding id as in piece")
  void electDeleteStrategyShouldDecreaseQuantityTo1ForCostAndLocationIfInitiallyWas1AndHoldingIdInPOLAndPieceTheSame()
                throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece piece = new Piece().withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(2).withQuantity(2);
    List<Location> locations = new ArrayList<>();
    locations.add(loc);
    Cost cost = new Cost().withQuantityElectronic(2)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(2d);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(eresource)
      .withLocations(List.of(loc)).withCost(cost);

    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(succeededFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
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

  @ParameterizedTest
  @DisplayName("Should decrease quantity for cost if initially POL without location")
  @CsvSource(value = {
                   "Electronic Resource:Instance:2:Electronic:1",
                   "Electronic Resource:None:2:Electronic:1",
                   "Electronic Resource:None:1:Electronic:0"}, delimiter = ':')
  void shouldDecreaseElectronicQuantityTo1ForCostIfInitiallyWas2AndPOLWithoutLocation(
              String lineType, String createInventory, int qty, String pieceFormat, int expQty)
    throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece.Format pieceFormatE = Piece.Format.fromValue(pieceFormat);
    Piece piece = new Piece().withPoLineId(lineId).withFormat(pieceFormatE);
    Cost cost = new Cost();
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.fromValue(lineType)).withId(lineId)
      .withCost(cost);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(createInventory));
    originPoLine.withEresource(eresource);
    cost.withQuantityElectronic(qty)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice((double) qty);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(succeededFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityPhysical());
    assertEquals(expQty, poLineToSave.getCost().getQuantityElectronic());
    assertEquals(0, poLineToSave.getLocations().size());

    verify(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
  }

  @ParameterizedTest
  @DisplayName("Should decrease quantity for cost if initially POL without location")
  @CsvSource(value = {
    "Physical Resource:None:2:Physical:1",
    "Physical Resource:Instance:2:Physical:1",
    "Physical Resource:None:1:Physical:0",
    "Other:None:2:Other:1",
    "Other:None:1:Other:0"}, delimiter = ':')
  void shouldDecreasePhysicalQuantityTo1ForCostIfInitiallyWas2AndPOLWithoutLocation(
    String lineType, String createInventory, int qty, String pieceFormat, int expQty)
    throws ExecutionException, InterruptedException {
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    Piece.Format pieceFormatE = Piece.Format.fromValue(pieceFormat);
    Piece piece = new Piece().withPoLineId(lineId).withFormat(pieceFormatE);
    Cost cost = new Cost();
    PoLine originPoLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(PoLine.OrderFormat.fromValue(lineType)).withId(lineId)
      .withCost(cost);
    Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.fromValue(createInventory));
    originPoLine.withPhysical(physical);
    cost.withQuantityPhysical(qty)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice((double) qty);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(OPEN);
    PieceDeletionHolder incomingUpdateHolder = new PieceDeletionHolder().withPieceToDelete(piece).withDeleteHolding(true);
    incomingUpdateHolder.withOrderInformation(purchaseOrder, originPoLine);

    doReturn(succeededFuture(null)).when(receivingEncumbranceStrategy).processEncumbrances(incomingUpdateHolder.getPurchaseOrderToSave(),
      incomingUpdateHolder.getPurchaseOrderToSave(), requestContext);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), requestContext);
    //When
    pieceDeleteFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToSave = incomingUpdateHolder.getPoLineToSave();
    assertNull(poLineToSave.getCost().getQuantityElectronic());
    assertEquals(expQty, poLineToSave.getCost().getQuantityPhysical());
    assertEquals(0, poLineToSave.getLocations().size());

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
