package org.folio.service.pieces.flows;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.ID;
import static org.folio.service.inventory.InventoryHoldingManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.folio.ApiTestSuite;
import org.folio.models.ItemStatus;
import org.folio.models.pieces.PieceUpdateHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

@ExtendWith(VertxExtension.class)
public class BasePieceFlowHolderBuilderTest {
  @Autowired
  private BasePieceFlowHolderBuilder basePieceFlowHolderBuilder;
  @Autowired
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;
  @Autowired
  private TitlesService titlesService;
  private final Context ctx = getFirstContextFromVertx(getVertx());
  @Mock
  private Map<String, String> okapiHeadersMock;
  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks() {
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
    initSpringContext(BasePieceFlowHolderBuilderTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  void resetMocks() {
    clearServiceInteractions();
    Mockito.reset(titlesService, purchaseOrderLineService, purchaseOrderStorageService);
  }

  @Test
  void shouldUpdateHolderWithOrderInformation() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    List<String> acqUnitIds = List.of(UUID.randomUUID().toString());
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    item.put(ITEM_HOLDINGS_RECORD_ID, holdingFromStorageId);
    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingFromStorageId)
      .withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId)
      .withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withInstanceId(instanceId).withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder()
      .withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);
    Title expectedTitle = new Title();
    expectedTitle.setId(titleId);
    expectedTitle.setAcqUnitIds(acqUnitIds);

    // Given
    when(purchaseOrderStorageService.getPurchaseOrderById(orderId, requestContext)).thenReturn(succeededFuture(purchaseOrder));
    when(purchaseOrderLineService.getOrderLineById(lineId, requestContext)).thenReturn(succeededFuture(poLine));
    // When
    basePieceFlowHolderBuilder.updateHolderWithOrderInformation(holder, requestContext);

    // Then
    assertEquals(purchaseOrder.getId(), holder.getPurchaseOrderToSave().getId());
    assertEquals(lineId, holder.getOrderLineId());
  }

  @Test
  public void testUpdateHolderWithTitleInformation() {
    String orderId = UUID.randomUUID().toString();
    String holdingFromStorageId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();
    List<String> acqUnitIds = List.of(UUID.randomUUID().toString());
    JsonObject item = new JsonObject().put(ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    item.put(ITEM_HOLDINGS_RECORD_ID, holdingFromStorageId);
    JsonObject holding = new JsonObject().put(ID, holdingFromStorageId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    Piece pieceFromStorage = new Piece().withId(pieceId).withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingFromStorageId)
      .withFormat(Piece.Format.PHYSICAL);
    Piece pieceToUpdate = new Piece().withId(pieceId)
      .withTitleId(titleId).withItemId(itemId)
      .withPoLineId(lineId).withHoldingId(holdingId).withFormat(Piece.Format.PHYSICAL);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(1);
    PoLine poLine = new PoLine().withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withInstanceId(instanceId).withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE).withId(lineId)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
      .withLocations(List.of(loc)).withCost(cost);
    PurchaseOrder purchaseOrder = new PurchaseOrder().withId(orderId).withWorkflowStatus(PurchaseOrder.WorkflowStatus.OPEN);
    PieceUpdateHolder holder = new PieceUpdateHolder()
      .withPieceToUpdate(pieceToUpdate).withPieceFromStorage(pieceFromStorage)
      .withCreateItem(true).withDeleteHolding(true);
    holder.withOrderInformation(purchaseOrder, poLine);
    Title expectedTitle = new Title();
    expectedTitle.setId(titleId);
    expectedTitle.setAcqUnitIds(acqUnitIds);

    // Given
    when(titlesService.getTitleById(titleId, requestContext)).thenReturn(succeededFuture(expectedTitle));

    // When
    basePieceFlowHolderBuilder.updateHolderWithTitleInformation(holder, requestContext)
      .onComplete(ar -> {
        assertEquals(titleId, holder.getTitleId());
        assertEquals(expectedTitle.getAcqUnitIds(), holder.getTitle().getAcqUnitIds());
      });
  }

  private static class ContextConfiguration {
    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    PurchaseOrderStorageService purchaseOrderStorageService() {
      return mock(PurchaseOrderStorageService.class);
    }

    @Bean
    PurchaseOrderLineService purchaseOrderLineStorageService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean
    BasePieceFlowHolderBuilder basePieceFlowHolderBuilder(PurchaseOrderStorageService purchaseOrderStorageService,
                                                          PurchaseOrderLineService purchaseOrderLineStorageService,
                                                          TitlesService titlesService) {
      return new BasePieceFlowHolderBuilder(purchaseOrderStorageService, purchaseOrderLineStorageService, titlesService);
    }
  }
}
