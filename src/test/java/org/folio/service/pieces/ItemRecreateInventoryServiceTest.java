package org.folio.service.pieces;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.impl.MockServer.CONSISTENT_ECS_PURCHASE_ORDER_ID_ELECTRONIC;
import static org.folio.rest.impl.MockServer.CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PIECES_JSON;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PO_LINE_JSON;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PURCHASE_ORDER_JSON;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryHoldingManager.ID;
import static org.folio.service.pieces.ItemRecreateInventoryService.ITEM_QUANTITY;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import org.folio.ApiTestSuite;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
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

public class ItemRecreateInventoryServiceTest {

  @Autowired
  private ItemRecreateInventoryService itemRecreateInventoryService;
  @Autowired
  private InventoryItemManager inventoryItemManager;
  @Autowired
  private InventoryHoldingManager inventoryHoldingManager;
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

    initSpringContext(ItemRecreateInventoryServiceTest.ContextConfiguration.class);
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
    itemRecreateInventoryService.deleteHoldingConnectedToPiece(null, requestContext);

    verify(inventoryHoldingManager, times(0)).getHoldingById(null , true, requestContext);
    verify(inventoryHoldingManager, times(0)).deleteHoldingById(null , true, requestContext);
  }

  @Test
  void shouldRecreatePhysicalItem() throws IOException {
    var purchaseOrderMock = getMockData(String.format(ECS_CONSORTIUM_PURCHASE_ORDER_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL));

    var piece = new JsonObject(piecesMock).mapTo(Piece.class);
    var purchaseOrder = new JsonObject(purchaseOrderMock).mapTo(PurchaseOrder.class);
    var poLine = new JsonObject(poLineMock).mapTo(PoLine.class);

    var pieceHolder = (PieceUpdateHolder) new PieceUpdateHolder()
      .withPieceToUpdate(piece)
      .withOrderInformation(purchaseOrder, poLine);

    var srcLocCtx = RequestContextUtil.createContextWithNewTenantId(requestContext, "tenant1");
    var dstLocCtx = RequestContextUtil.createContextWithNewTenantId(requestContext, "tenant2");

    itemRecreateInventoryService.recreateItemInDestinationTenant(pieceHolder,srcLocCtx,dstLocCtx).result();

    verify(inventoryItemManager, times(1))
      .recreateMissingPhysicalItems(pieceHolder.getPoLineToSave(), pieceHolder.getPieceToUpdate(), ITEM_QUANTITY, dstLocCtx);
  }

  @Test
  void shouldRecreateElectronicItem() throws IOException {
    var purchaseOrderMock = getMockData(String.format(ECS_CONSORTIUM_PURCHASE_ORDER_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_ELECTRONIC));
    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_ELECTRONIC));
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, CONSISTENT_ECS_PURCHASE_ORDER_ID_ELECTRONIC));

    var piece = new JsonObject(piecesMock).mapTo(Piece.class);
    var purchaseOrder = new JsonObject(purchaseOrderMock).mapTo(PurchaseOrder.class);
    var poLine = new JsonObject(poLineMock).mapTo(PoLine.class);

    var pieceHolder = (PieceUpdateHolder) new PieceUpdateHolder()
      .withPieceToUpdate(piece)
      .withOrderInformation(purchaseOrder, poLine);

    var srcLocCtx = RequestContextUtil.createContextWithNewTenantId(requestContext, "tenant1");
    var dstLocCtx = RequestContextUtil.createContextWithNewTenantId(requestContext, "tenant2");

    itemRecreateInventoryService.recreateItemInDestinationTenant(pieceHolder,srcLocCtx,dstLocCtx).result();

    verify(inventoryItemManager, times(1))
      .recreateMissingElectronicItems(pieceHolder.getPoLineToSave(), pieceHolder.getPieceToUpdate(), ITEM_QUANTITY, dstLocCtx);
  }

  @Test
  void shouldNotDeleteHoldingIfHoldingIdIsNotNullButNotFoundInTheDB() {
    String holdingId = UUID.randomUUID().toString();
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);

    doReturn(succeededFuture(null))
      .when(inventoryHoldingManager).getHoldingById(piece.getHoldingId() , true, requestContext);

    itemRecreateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryHoldingManager, times(0)).deleteHoldingById(piece.getHoldingId() , true, requestContext);
  }


  @Test
  void shouldDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndNoPiecesAndItems() {
    String holdingId = UUID.randomUUID().toString();

    JsonObject holding = new JsonObject();
    holding.put(ID, holding);

    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);

    doReturn(succeededFuture(Collections.emptyList()))
      .when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(holding))
      .when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(new ArrayList<>()))
      .when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);

    itemRecreateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext);

    verify(inventoryHoldingManager, times(1)).deleteHoldingById(holdingId , true, requestContext);
  }

  @Test
  void shouldNoDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndPiecesExistAndNoItems() {
    String holdingId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();

    JsonObject holding = new JsonObject();
    holding.put(ID, holding);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);

    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);
    Piece piece2 = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);

    doReturn(succeededFuture(List.of(piece, piece2)))
      .when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(holding))
      .when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(new ArrayList<>()))
      .when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);

    itemRecreateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryHoldingManager, times(0)).deleteHoldingById(holdingId , true, requestContext);
  }

  @Test
  void shouldNoDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndNoPiecesAndItemsExist() {
    String holdingId = UUID.randomUUID().toString();

    JsonObject holding = new JsonObject();
    holding.put(ID, holding);

    JsonObject item = new JsonObject().put(ID, UUID.randomUUID().toString());

    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withHoldingId(holdingId);

    doReturn(succeededFuture(Collections.emptyList()))
      .when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(holding))
      .when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(List.of(item)))
      .when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);

    itemRecreateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryHoldingManager, times(0)).deleteHoldingById(holdingId , true, requestContext);
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
    ItemRecreateInventoryService pieceUpdateInventoryService(InventoryItemManager inventoryItemManager,
                                                             InventoryHoldingManager inventoryHoldingManager,
                                                             PieceStorageService pieceStorageService) {
      return spy(new ItemRecreateInventoryService(inventoryItemManager, inventoryHoldingManager, pieceStorageService));
    }
  }
}
