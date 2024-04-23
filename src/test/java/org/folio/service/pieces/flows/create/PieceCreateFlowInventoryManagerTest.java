package org.folio.service.pieces.flows.create;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.consortium.SharingInstance;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.titles.TitlesInstanceService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.junit5.VertxExtension;


@ExtendWith(VertxExtension.class)
public class PieceCreateFlowInventoryManagerTest {
  @Autowired
  PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;
  @Autowired
  TitlesService titlesService;
  @Autowired
  PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  InventoryInstanceManager inventoryInstanceManager;
  @Autowired
  PieceStorageService pieceStorageService;

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
    initSpringContext(PieceCreateFlowInventoryManagerTest.ContextConfiguration.class);
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
    Mockito.reset(titlesService, inventoryInstanceManager, pieceUpdateInventoryService);
  }

  @Test
  void testPieceCreateForPackagePoLineWithCreateInventoryInstanceHoldingItem() {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    Piece piece = new Piece().withId(pieceId).withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);

    CompositePoLine compPOL = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
                                    .withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
                                    .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM))
                                    .withLocations(List.of(loc)).withCost(cost);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(orderId).withCompositePoLines(List.of(compPOL));
    SharingInstance sharingInstance = new SharingInstance(UUID.randomUUID(), UUID.randomUUID().toString(), UUID.randomUUID().toString());

    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(pieceId, requestContext);
    doReturn(succeededFuture(List.of(piece))).when(pieceStorageService).getPiecesByHoldingId(piece.getId(), requestContext);
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).saveTitleWithInstance(title, anyBoolean(), requestContext);
    doReturn(succeededFuture(sharingInstance)).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(String.class), eq(requestContext));
    doReturn(succeededFuture(itemId)).when(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(piece, compPOL, requestContext);
    doReturn(succeededFuture(holdingId)).when(pieceUpdateInventoryService).handleHoldingsRecord(eq(compPOL), any(Location.class), eq(title.getInstanceId()), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceUpdateInventoryService).deleteHoldingConnectedToPiece(piece, requestContext);

    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    holder.withOrderInformation(compositePurchaseOrder);
    holder.withTitleInformation(title);

    pieceCreateFlowInventoryManager.processInventory(holder.getOriginPoLine(), holder.getPieceToCreate(),
      holder.isCreateItem(), requestContext).result();

    assertEquals(itemId, piece.getItemId());
    assertEquals(holdingId, piece.getHoldingId());
    verify(titlesService).getTitleById(piece.getTitleId(), requestContext);

    verify(pieceUpdateInventoryService, times(0)).handleHoldingsRecord(eq(compPOL), any(Location.class), eq(title.getInstanceId()), eq(requestContext));
    verify(pieceUpdateInventoryService).manualPieceFlowCreateItemRecord(piece, compPOL, requestContext);
  }

  @Test
  void testShouldNotCreateHoldingPieceCreateForPackagePoLineWithCreateInventoryInstance() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    Piece piece = new Piece().withId(pieceId).withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);

    CompositePoLine compPOL = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
      .withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
      .withCheckinItems(false)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE))
      .withLocations(List.of(loc)).withCost(cost);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(orderId).withCompositePoLines(List.of(compPOL));
    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(pieceId, requestContext);
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).saveTitleWithInstance(title, anyBoolean(), requestContext);
    doReturn(succeededFuture(instanceId)).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(String.class), eq(requestContext));

    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    holder.withOrderInformation(compositePurchaseOrder);
    holder.withTitleInformation(title);

    pieceCreateFlowInventoryManager.processInventory(holder.getOriginPoLine(), holder.getPieceToCreate(),
      holder.isCreateItem(), requestContext).result();

    assertNull(piece.getItemId());
    assertNull(piece.getHoldingId());
    assertEquals(locationId, piece.getLocationId());
    verify(titlesService).getTitleById(piece.getTitleId(), requestContext);

    verify(pieceUpdateInventoryService, times(0)).handleHoldingsRecord(eq(compPOL), any(Location.class), eq(title.getInstanceId()), eq(requestContext));
    verify(pieceUpdateInventoryService, times(0)).manualPieceFlowCreateItemRecord(piece, compPOL, requestContext);
  }


  @Test
  void testShouldNotCreateHoldingPieceCreateForNonPackagePoLineWithCreateInventoryInstance() {
    String orderId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(instanceId);
    Piece piece = new Piece().withId(pieceId).withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1)
      .withListUnitPrice(1d).withExchangeRate(1d).withCurrency("USD")
      .withPoLineEstimatedPrice(1d);

    CompositePoLine compPOL = new CompositePoLine().withIsPackage(false).withPurchaseOrderId(orderId)
      .withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE))
      .withLocations(List.of(loc)).withCost(cost);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(orderId).withCompositePoLines(List.of(compPOL));
    doReturn(succeededFuture(piece)).when(pieceStorageService).getPieceById(pieceId, requestContext);
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).saveTitleWithInstance(title, anyBoolean(), requestContext);
    doReturn(succeededFuture(instanceId)).when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), any(String.class), eq(requestContext));

    PieceCreationHolder holder = new PieceCreationHolder().withPieceToCreate(piece).withCreateItem(true);
    holder.withOrderInformation(compositePurchaseOrder);
    holder.withTitleInformation(title);

    pieceCreateFlowInventoryManager.processInventory(holder.getOriginPoLine(), holder.getPieceToCreate(),
      holder.isCreateItem(), requestContext).result();

    assertNull(piece.getItemId());
    assertNull(piece.getHoldingId());
    assertEquals(locationId, piece.getLocationId());
    verify(titlesService).getTitleById(piece.getTitleId(), requestContext);

    verify(pieceUpdateInventoryService, times(0)).handleHoldingsRecord(eq(compPOL), any(Location.class), eq(title.getInstanceId()), eq(requestContext));
    verify(pieceUpdateInventoryService, times(0)).manualPieceFlowCreateItemRecord(piece, compPOL, requestContext);
  }

  private static class ContextConfiguration {
    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    TitlesInstanceService titlesInstanceService () {
      return mock(TitlesInstanceService.class);
    }

    @Bean
    InventoryHoldingManager inventoryHoldingManager() {
      return mock(InventoryHoldingManager.class);
    }

    @Bean
    InventoryInstanceManager inventoryInstanceManager() {
      return mock(InventoryInstanceManager.class);
    }

    @Bean
    PieceUpdateInventoryService pieceUpdateInventoryService() {
      return mock(PieceUpdateInventoryService.class);
    }

    @Bean
    PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager(TitlesService titlesService,
                                                                    TitlesInstanceService titlesInstanceService,
                                                                    PieceUpdateInventoryService pieceUpdateInventoryService,
                                                                    InventoryHoldingManager inventoryHoldingManager) {
      return new PieceCreateFlowInventoryManager(titlesService, titlesInstanceService, pieceUpdateInventoryService, inventoryHoldingManager);
    }
  }
}
