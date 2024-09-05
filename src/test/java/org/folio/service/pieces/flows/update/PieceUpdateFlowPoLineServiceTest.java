package org.folio.service.pieces.flows.update;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.pieces.PieceUpdateHolder;
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
import org.folio.service.pieces.PieceUtil;
import org.folio.service.pieces.flows.create.PieceCreateFlowPoLineService;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowPoLineService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.junit5.VertxExtension;

@ExtendWith(VertxExtension.class)
public class PieceUpdateFlowPoLineServiceTest {
  @Autowired PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired PurchaseOrderLineService purchaseOrderLineService;
  @Autowired ReceivingEncumbranceStrategy receivingEncumbranceStrategy;
  @Autowired PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService;

  private final Context ctx = getFirstContextFromVertx(getVertx());
  @Mock
  private Map<String, String> okapiHeadersMock;

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(PieceUpdateFlowPoLineServiceTest.ContextConfiguration.class);
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
  void shouldUpdateLineQuantityIfElectronicPoLineIsNotPackageAndHoldingReferenceChanged() {
    String orderId = UUID.randomUUID().toString();
    String oldHoldingId = UUID.randomUUID().toString();
    String newHoldingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(oldHoldingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(newHoldingId).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    Location loc = new Location().withHoldingId(oldHoldingId).withQuantityElectronic(1).withQuantity(1);
    PoLine poLineFromStorage = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrderFromStorage = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    PieceUpdateHolder incomingUpdateHolder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate)
                                                                    .withPieceFromStorage(pieceFromStorage);
    incomingUpdateHolder.withOrderInformation(purchaseOrderFromStorage, poLineFromStorage);

    ArgumentCaptor<CompositePoLine> poLineToSaveCapture = ArgumentCaptor.forClass(CompositePoLine.class);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(poLineToSaveCapture.capture(), anyList(), eq(requestContext));

    //When
    pieceUpdateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToUpdate = poLineToSaveCapture.getValue();
    assertNull(pieceToUpdate.getLocationId());
    assertEquals(newHoldingId, pieceToUpdate.getHoldingId());
    Location locationToSave = poLineToUpdate.getLocations().get(0);
    Cost costToSave = poLineToUpdate.getCost();
    assertNull(locationToSave.getLocationId());
    assertEquals(1, locationToSave.getQuantityElectronic());
    assertEquals(1, locationToSave.getQuantity());
    assertEquals(pieceToUpdate.getHoldingId(), locationToSave.getHoldingId());
    assertEquals(1, costToSave.getQuantityElectronic());
    assertNull(costToSave.getQuantityPhysical());

    assertNull(poLineFromStorage.getLocations().get(0).getLocationId());
    assertEquals(oldHoldingId, poLineFromStorage.getLocations().get(0).getHoldingId());
    List<Location> locations = PieceUtil.findOrderPieceLineLocation(pieceToUpdate, poLineToUpdate);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), locations, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityAndAddNewLocationWithHoldingIfPhysicalPoLineIsNotPackageAndHoldingReferenceChanged() {
    String orderId = UUID.randomUUID().toString();
    String oldHoldingId = UUID.randomUUID().toString();
    String holdingIdToUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(oldHoldingId).withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingIdToUpdate).withFormat(Piece.Format.PHYSICAL);
    Cost cost = new Cost().withQuantityPhysical(2)
      .withListUnitPrice(2d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(2d);
    Location loc = new Location().withHoldingId(oldHoldingId).withQuantityPhysical(2).withQuantity(2);
    PoLine poLineFromStorage = new PoLine().withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
                                .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
                                .withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
                                .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrderFromStorage = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    PieceUpdateHolder incomingUpdateHolder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate)
      .withPieceFromStorage(pieceFromStorage);
    incomingUpdateHolder.withOrderInformation(purchaseOrderFromStorage, poLineFromStorage);

    ArgumentCaptor<CompositePoLine> poLineToSaveCapture = ArgumentCaptor.forClass(CompositePoLine.class);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(poLineToSaveCapture.capture(), anyList(), eq(requestContext));

    //When
    pieceUpdateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToUpdate = poLineToSaveCapture.getValue();
    assertNull(pieceToUpdate.getLocationId());
    assertEquals(holdingIdToUpdate, pieceToUpdate.getHoldingId());
    Location oldLocationToSave = poLineToUpdate.getLocations().stream()
                                  .filter(loca -> loca.getHoldingId().equals(oldHoldingId)).findAny().get();
    Location newLocationToSave = poLineToUpdate.getLocations().stream()
                                  .filter(loca -> loca.getHoldingId().equals(holdingIdToUpdate)).findAny().get();
    Cost costToSave = poLineToUpdate.getCost();
    assertNull(oldLocationToSave.getLocationId());
    assertEquals(1, oldLocationToSave.getQuantityPhysical());
    assertNull(oldLocationToSave.getQuantityElectronic());
    assertEquals(1, oldLocationToSave.getQuantity());
    assertEquals(oldHoldingId, oldLocationToSave.getHoldingId());
    assertNull(newLocationToSave.getQuantityElectronic());
    assertEquals(1, newLocationToSave.getQuantityPhysical());
    assertEquals(1, newLocationToSave.getQuantity());
    assertEquals(holdingIdToUpdate, newLocationToSave.getHoldingId());
    assertNull(newLocationToSave.getQuantityElectronic());
    assertEquals(2, costToSave.getQuantityPhysical());
    assertNull(costToSave.getQuantityElectronic());

    List<Location> locations = PieceUtil.findOrderPieceLineLocation(pieceToUpdate, poLineToUpdate);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), locations, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityWithAddNeLocationIfMixedPoLineIsNotPackageAndLocationReferenceChanged() {
    String orderId = UUID.randomUUID().toString();
    String oldHoldingId = UUID.randomUUID().toString();
    String locationToUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
                                        .withHoldingId(oldHoldingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
                                    .withLocationId(locationToUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1).withQuantityPhysical(1)
      .withListUnitPrice(1d).withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    Location loc = new Location().withHoldingId(oldHoldingId).withQuantityPhysical(1).withQuantityElectronic(1).withQuantity(2);
    PoLine poLineFromStorage = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
                    .withOrderFormat(PoLine.OrderFormat.P_E_MIX)
                    .withEresource(new Eresource().withCreateInventory(INSTANCE))
                    .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
                    .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrderFromStorage = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    PieceUpdateHolder incomingUpdateHolder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate)
      .withPieceFromStorage(pieceFromStorage);
    incomingUpdateHolder.withOrderInformation(purchaseOrderFromStorage, poLineFromStorage);

    ArgumentCaptor<CompositePoLine> poLineToSaveCapture = ArgumentCaptor.forClass(CompositePoLine.class);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(poLineToSaveCapture.capture(), anyList(), eq(requestContext));

    //When
    pieceUpdateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToUpdate = poLineToSaveCapture.getValue();
    assertEquals(locationToUpdate, pieceToUpdate.getLocationId());
    assertNull(pieceToUpdate.getHoldingId());
    assertEquals(2, poLineToUpdate.getLocations().size());
    Location physicalLocationToSave =  poLineToUpdate.getLocations().stream()
      .filter(location -> location.getQuantityPhysical() != null).findAny().get();
    assertNull(physicalLocationToSave.getQuantityElectronic());
    assertNull(physicalLocationToSave.getLocationId());
    assertEquals(1, physicalLocationToSave.getQuantity());
    assertEquals(oldHoldingId, physicalLocationToSave.getHoldingId());

    Location elecLocationToSave =  poLineToUpdate.getLocations().stream()
      .filter(location -> location.getQuantityElectronic() != null).findAny().get();
    assertNull(elecLocationToSave.getQuantityPhysical());
    assertNull(elecLocationToSave.getHoldingId());
    assertEquals(1, elecLocationToSave.getQuantity());
    assertEquals(locationToUpdate, elecLocationToSave.getLocationId());

    Cost costToSave = poLineToUpdate.getCost();
    assertEquals(1, costToSave.getQuantityElectronic());
    assertEquals(1, costToSave.getQuantityPhysical());

    List<Location> locations = PieceUtil.findOrderPieceLineLocation(pieceToUpdate, poLineToUpdate);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), locations, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityWithAddNeHoldingIfMixedPoLineIsNotPackageAndHoldingReferenceChanged() {
    String orderId = UUID.randomUUID().toString();
    String oldHoldingId = UUID.randomUUID().toString();
    String holdingToUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(oldHoldingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingToUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1).withQuantityPhysical(1)
      .withListUnitPrice(1d).withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);
    Location loc = new Location().withHoldingId(oldHoldingId).withQuantityPhysical(1).withQuantityElectronic(1).withQuantity(2);
    PoLine poLineFromStorage = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.P_E_MIX)
      .withEresource(new Eresource().withCreateInventory(INSTANCE_HOLDING))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrderFromStorage = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    PieceUpdateHolder incomingUpdateHolder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate)
      .withPieceFromStorage(pieceFromStorage);
    incomingUpdateHolder.withOrderInformation(purchaseOrderFromStorage, poLineFromStorage);

    ArgumentCaptor<CompositePoLine> poLineToSaveCapture = ArgumentCaptor.forClass(CompositePoLine.class);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(poLineToSaveCapture.capture(), anyList(), eq(requestContext));

    //When
    pieceUpdateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToUpdate = poLineToSaveCapture.getValue();
    assertEquals(holdingToUpdate, pieceToUpdate.getHoldingId());
    assertNull(pieceToUpdate.getLocationId());
    assertEquals(2, poLineToUpdate.getLocations().size());
    Location physicalLocationToSave =  poLineToUpdate.getLocations().stream()
      .filter(location -> location.getQuantityPhysical() != null).findAny().get();
    assertNull(physicalLocationToSave.getQuantityElectronic());
    assertNull(physicalLocationToSave.getLocationId());
    assertEquals(1, physicalLocationToSave.getQuantity());
    assertEquals(oldHoldingId, physicalLocationToSave.getHoldingId());

    Location elecLocationToSave =  poLineToUpdate.getLocations().stream()
      .filter(location -> location.getQuantityElectronic() != null).findAny().get();
    assertNull(elecLocationToSave.getQuantityPhysical());
    assertNull(elecLocationToSave.getLocationId());
    assertEquals(1, elecLocationToSave.getQuantity());
    assertEquals(holdingToUpdate, elecLocationToSave.getHoldingId());

    Cost costToSave = poLineToUpdate.getCost();
    assertEquals(1, costToSave.getQuantityElectronic());
    assertEquals(1, costToSave.getQuantityPhysical());

    List<Location> locations = PieceUtil.findOrderPieceLineLocation(pieceToUpdate, poLineToUpdate);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), locations, requestContext);
  }

  @Test
  void shouldUpdateLineQuantityWithAddNeHoldingIfMixedPoLineWith2LocationsIsNotPackageAndHoldingReferenceChanged() {
    String orderId = UUID.randomUUID().toString();
    String oldHoldingId = UUID.randomUUID().toString();
    String holdingToUpdate = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    Piece pieceFromStorage = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(oldHoldingId).withFormat(Piece.Format.ELECTRONIC);
    Piece pieceToUpdate = new Piece().withId(pieceId).withPoLineId(lineId).withItemId(itemId).withTitleId(titleId)
      .withHoldingId(holdingToUpdate).withFormat(Piece.Format.ELECTRONIC);
    Cost cost = new Cost().withQuantityElectronic(1).withQuantityPhysical(1)
      .withListUnitPrice(1d).withListUnitPriceElectronic(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(2d);
    Location loc1 = new Location().withHoldingId(oldHoldingId).withQuantityElectronic(1).withQuantity(1);
    Location loc2 = new Location().withHoldingId(oldHoldingId).withQuantityPhysical(1).withQuantity(1);
    PoLine poLineFromStorage = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.P_E_MIX)
      .withEresource(new Eresource().withCreateInventory(INSTANCE_HOLDING))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc1, loc2)).withCost(cost);
    PurchaseOrder purchaseOrderFromStorage = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);

    PieceUpdateHolder incomingUpdateHolder = new PieceUpdateHolder().withPieceToUpdate(pieceToUpdate)
      .withPieceFromStorage(pieceFromStorage);
    incomingUpdateHolder.withOrderInformation(purchaseOrderFromStorage, poLineFromStorage);

    ArgumentCaptor<CompositePoLine> poLineToSaveCapture = ArgumentCaptor.forClass(CompositePoLine.class);
    doReturn(succeededFuture(null)).when(purchaseOrderLineService).saveOrderLine(poLineToSaveCapture.capture(), anyList(), eq(requestContext));

    //When
    pieceUpdateFlowPoLineService.updatePoLine(incomingUpdateHolder, requestContext).result();
    //Then
    CompositePoLine poLineToUpdate = poLineToSaveCapture.getValue();
    assertEquals(holdingToUpdate, pieceToUpdate.getHoldingId());
    assertNull(pieceToUpdate.getLocationId());
    assertEquals(2, poLineToUpdate.getLocations().size());
    Location physicalLocationToSave =  poLineToUpdate.getLocations().stream()
      .filter(location -> location.getQuantityPhysical() != null).findAny().get();
    assertNull(physicalLocationToSave.getQuantityElectronic());
    assertNull(physicalLocationToSave.getLocationId());
    assertEquals(1, physicalLocationToSave.getQuantity());
    assertEquals(oldHoldingId, physicalLocationToSave.getHoldingId());

    Location elecLocationToSave =  poLineToUpdate.getLocations().stream()
      .filter(location -> location.getQuantityElectronic() != null).findAny().get();
    assertNull(elecLocationToSave.getQuantityPhysical());
    assertNull(elecLocationToSave.getLocationId());
    assertEquals(1, elecLocationToSave.getQuantity());
    assertEquals(holdingToUpdate, elecLocationToSave.getHoldingId());

    Cost costToSave = poLineToUpdate.getCost();
    assertEquals(1, costToSave.getQuantityElectronic());
    assertEquals(1, costToSave.getQuantityPhysical());

    List<Location> locations = PieceUtil.findOrderPieceLineLocation(pieceToUpdate, poLineToUpdate);
    verify(purchaseOrderLineService).saveOrderLine(incomingUpdateHolder.getPoLineToSave(), locations, requestContext);
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
    @Bean PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
      PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
      return spy(new PieceDeleteFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy));
    }

    @Bean PieceCreateFlowPoLineService pieceCreateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
      PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy) {
      return spy(new PieceCreateFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy));
    }
    @Bean PieceUpdateFlowPoLineService pieceUpdateFlowPoLineService(PurchaseOrderStorageService purchaseOrderStorageService,
                    PurchaseOrderLineService purchaseOrderLineService, ReceivingEncumbranceStrategy receivingEncumbranceStrategy,
              PieceDeleteFlowPoLineService pieceDeleteFlowPoLineService, PieceCreateFlowPoLineService pieceCreateFlowPoLineService) {
      return new PieceUpdateFlowPoLineService(purchaseOrderStorageService, purchaseOrderLineService, receivingEncumbranceStrategy,
                                              pieceCreateFlowPoLineService, pieceDeleteFlowPoLineService);
    }
  }
}
