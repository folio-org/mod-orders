package org.folio.service.pieces.flows.update;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.PieceUpdateInventoryService;
import org.folio.service.titles.TitlesService;
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

public class PieceUpdateFlowInventoryManagerTest {
  @Autowired
  PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager;
  @Autowired
  TitlesService titlesService;
  @Autowired
  PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  InventoryManager inventoryManager;
  @Autowired
  PieceStorageService pieceStorageService;

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
    initSpringContext(PieceUpdateFlowInventoryManagerTest.ContextConfiguration.class);
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
    Mockito.reset(titlesService, inventoryManager, pieceUpdateInventoryService);
  }

  @Test
  void testPieceUpdateForPackagePoLineWithCreateInventoryInstanceHoldingItem() {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(UUID.randomUUID().toString());
    Piece piece = new Piece().withId(pieceId).withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(true).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder(piece, true);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(),
                                holder.getPieceToUpdate().getPoLineId(), requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();
  }

  @Test
  void testPieceUpdateForNonPackagePoLineWithCreateInventoryInstanceHoldingItem() {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(UUID.randomUUID().toString());
    Piece piece = new Piece().withId(pieceId).withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);

    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE).withId(lineId)
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder(piece, true);
    holder.shallowCopy(new PieceUpdateHolder(purchaseOrder, poLine));

    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(holder.getPieceToUpdate().getItemId(),
      holder.getPieceToUpdate().getPoLineId(), requestContext);

    pieceUpdateFlowInventoryManager.processInventory(holder, requestContext).join();
  }

  private static class ContextConfiguration {
    @Bean TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean PieceUpdateInventoryService pieceUpdateInventoryService() {
      return mock(PieceUpdateInventoryService.class);
    }

    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }
    @Bean PieceUpdateFlowInventoryManager pieceUpdateFlowInventoryManager(TitlesService titlesService,
      PieceUpdateInventoryService pieceUpdateInventoryService, InventoryManager inventoryManager) {
      return new PieceUpdateFlowInventoryManager(titlesService, pieceUpdateInventoryService, inventoryManager);
    }
  }
}
