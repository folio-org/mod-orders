package org.folio.service.pieces;

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
import static org.folio.service.pieces.ItemRecreateInventoryService.ITEM_QUANTITY;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import org.folio.ApiTestSuite;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.orders.utils.RequestContextUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.inventory.InventoryItemManager;
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

    itemRecreateInventoryService.recreateItemInDestinationTenant(pieceHolder, srcLocCtx ,dstLocCtx).result();

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

    itemRecreateInventoryService.recreateItemInDestinationTenant(pieceHolder, srcLocCtx, dstLocCtx).result();

    verify(inventoryItemManager, times(1))
      .recreateMissingElectronicItems(pieceHolder.getPoLineToSave(), pieceHolder.getPieceToUpdate(), ITEM_QUANTITY, dstLocCtx);
  }

  private static class ContextConfiguration {

    @Bean
    InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean
    ItemRecreateInventoryService pieceUpdateInventoryService(InventoryItemManager inventoryItemManager) {
      return spy(new ItemRecreateInventoryService(inventoryItemManager));
    }
  }
}
