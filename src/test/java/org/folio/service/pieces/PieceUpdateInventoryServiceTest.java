package org.folio.service.pieces;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

public class PieceUpdateInventoryServiceTest {
  @Autowired
  private PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private  PieceStorageService pieceStorageService;
  @Mock
  private Map<String, String> okapiHeadersMock;
  private final Context ctx = getFirstContextFromVertx(getVertx());

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

    verify(inventoryManager, times(0)).getHoldingById(null , true, requestContext);
    verify(inventoryManager, times(0)).deleteHoldingById(null , true, requestContext);
  }

  @Test
  void shouldNotDeleteHoldingIfHoldingIdIsNotNullButNotFoundInTheDB() throws ExecutionException, InterruptedException {
    String holdingId = UUID.randomUUID().toString();
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);
    doReturn(succeededFuture(null)).when(inventoryManager).getHoldingById(piece.getHoldingId() , true, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryManager, times(0)).deleteHoldingById(piece.getHoldingId() , true, requestContext);
  }


  @Test
  void shouldDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndNoPiecesAndItems() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holding);
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(holding)).when(inventoryManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(new ArrayList<>())).when(inventoryManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext);

    verify(inventoryManager, times(1)).deleteHoldingById(holdingId , true, requestContext);
  }

  @Test
  void shouldNoDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndPiecesExistAndNoItems() throws ExecutionException, InterruptedException {
    String holdingId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holding);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);
    Piece piece2 = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);
    doReturn(succeededFuture(List.of(piece, piece2))).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(holding)).when(inventoryManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(new ArrayList<>())).when(inventoryManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryManager, times(0)).deleteHoldingById(holdingId , true, requestContext);
  }

  @Test
  void shouldNoDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndNoPiecesAndItemsExist()
    throws ExecutionException, InterruptedException {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holding);
    JsonObject item = new JsonObject().put(ID, UUID.randomUUID().toString());
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(holding)).when(inventoryManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(List.of(item))).when(inventoryManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryManager, times(0)).deleteHoldingById(holdingId , true, requestContext);
  }


  private static class ContextConfiguration {
    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean
    PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    PieceUpdateInventoryService pieceUpdateInventoryService(InventoryManager inventoryManager,
                                PieceStorageService pieceStorageService) {
      return spy(new PieceUpdateInventoryService(inventoryManager, pieceStorageService));
    }
  }
}
