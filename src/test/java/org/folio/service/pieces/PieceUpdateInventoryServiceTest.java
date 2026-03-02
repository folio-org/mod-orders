package org.folio.service.pieces;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryHoldingManager.ID;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

@ExtendWith(MockitoExtension.class)
public class PieceUpdateInventoryServiceTest {

  @Autowired
  private PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  private InventoryItemManager inventoryItemManager;
  @Autowired
  private InventoryHoldingManager inventoryHoldingManager;
  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private Map<String, String> okapiHeadersMock;

  private final Context ctx = getFirstContextFromVertx(getVertx());
  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    autowireDependencies(this);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(PieceUpdateInventoryServiceTest.ContextConfiguration.class);
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
  }

  @Test
  void shouldNotDeleteHoldingIfHoldingIdIsNull() {
    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(null, requestContext);

    verify(inventoryHoldingManager, times(0)).getHoldingById(null , true, requestContext);
    verify(inventoryHoldingManager, times(0)).deleteHoldingById(null , true, requestContext);
  }

  @Test
  void shouldNotDeleteHoldingIfHoldingIdIsNotNullButNotFoundInTheDB() {
    String holdingId = UUID.randomUUID().toString();
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);
    doReturn(succeededFuture(null)).when(inventoryHoldingManager).getHoldingById(piece.getHoldingId() , true, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryHoldingManager, times(0)).deleteHoldingById(piece.getHoldingId() , true, requestContext);
  }


  @Test
  void shouldDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndNoPiecesAndItems() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId).withPoLineId(UUID.randomUUID().toString());
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(purchaseOrderLineService).getPoLinesByHoldingIds(List.of(holdingId), requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(null)).when(inventoryHoldingManager).deleteHoldingById(holdingId, true, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryHoldingManager, times(1)).deleteHoldingById(holdingId, true, requestContext);
  }

  @Test
  void shouldNoDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndPiecesExistAndNoItems() {
    String holdingId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId).withPoLineId(UUID.randomUUID().toString());
    Piece piece2 = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(purchaseOrderLineService).getPoLinesByHoldingIds(List.of(holdingId), requestContext);
    doReturn(succeededFuture(List.of(piece, piece2))).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryHoldingManager, times(0)).deleteHoldingById(holdingId, true, requestContext);
  }

  @Test
  void shouldNoDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndNoPiecesAndItemsExist() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    JsonObject item = new JsonObject().put(ID, UUID.randomUUID().toString());
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId).withPoLineId(UUID.randomUUID().toString());
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(purchaseOrderLineService).getPoLinesByHoldingIds(List.of(holdingId), requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryHoldingManager, times(0)).deleteHoldingById(holdingId, true, requestContext);
  }


  private static class ContextConfiguration {
    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean
    InventoryHoldingManager inventoryHoldingManager() {
      return mock(InventoryHoldingManager.class);
    }

    @Bean
    PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean
    PieceUpdateInventoryService pieceUpdateInventoryService(InventoryItemManager inventoryItemManager,
                                                            InventoryHoldingManager inventoryHoldingManager,
                                                            PieceStorageService pieceStorageService,
                                                            PurchaseOrderLineService purchaseOrderLineService) {
      return spy(new PieceUpdateInventoryService(inventoryItemManager, inventoryHoldingManager, pieceStorageService, purchaseOrderLineService));
    }
  }
}
