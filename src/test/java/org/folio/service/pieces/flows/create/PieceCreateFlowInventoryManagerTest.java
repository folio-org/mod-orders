package org.folio.service.pieces.flows.create;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import io.vertx.core.Context;
import org.folio.ApiTestSuite;
import org.folio.models.pieces.PieceCreationHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
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

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

public class PieceCreateFlowInventoryManagerTest {
  @Autowired
  PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;
  @Autowired
  TitlesService titlesService;
  @Autowired
  PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  InventoryManager inventoryManager;

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
    Mockito.reset(titlesService, inventoryManager, pieceUpdateInventoryService);
  }

  @Test
  void testPieceCreateForPackagePoLineWithCreateInventoryInstanceHoldingItem() {
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    Title title = new Title().withId(titleId).withPoLineId(lineId).withInstanceId(UUID.randomUUID().toString());
    Piece piece = new Piece().withPoLineId(lineId).withLocationId(locationId).withFormat(Piece.Format.ELECTRONIC);
    Location loc = new Location().withLocationId(locationId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    CompositePoLine compPOL = new CompositePoLine().withIsPackage(true).withPurchaseOrderId(orderId)
                                    .withOrderFormat(ELECTRONIC_RESOURCE).withId(lineId)
                                    .withLocations(List.of(loc)).withCost(cost);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(orderId).withCompositePoLines(List.of(compPOL));

    doReturn(completedFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(completedFuture(null)).when(titlesService).saveTitle(title, requestContext);
    doReturn(completedFuture(title)).when(pieceUpdateInventoryService).handleInstanceRecord(title, requestContext);
    doReturn(completedFuture(holdingId)).when(pieceUpdateInventoryService).handleHoldingsRecord(eq(compPOL), any(Location.class), eq(title.getInstanceId()), eq(requestContext));
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(compPOL, holdingId, requestContext);

    PieceCreationHolder holder = new PieceCreationHolder(piece, true);
    holder.shallowCopy(new PieceCreationHolder(compositePurchaseOrder));

    pieceCreateFlowInventoryManager.processInventory(holder, requestContext).join();

    assertEquals(itemId, piece.getItemId());
    assertEquals(holdingId, piece.getHoldingId());
    verify(titlesService).getTitleById(piece.getTitleId(), requestContext);

    verify(pieceUpdateInventoryService).handleHoldingsRecord(eq(compPOL), any(Location.class), eq(title.getInstanceId()), eq(requestContext));
    verify(pieceUpdateInventoryService).createItemRecord(compPOL, holdingId, requestContext);
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

    @Bean PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager(TitlesService titlesService,
                  PieceUpdateInventoryService pieceUpdateInventoryService) {
      return new PieceCreateFlowInventoryManager(titlesService, pieceUpdateInventoryService);
    }
  }
}
