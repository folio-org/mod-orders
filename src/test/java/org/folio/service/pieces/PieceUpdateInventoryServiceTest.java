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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.consortium.ConsortiumUserTenantService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
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
  private ConsortiumUserTenantService consortiumUserTenantService;
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
    doReturn(succeededFuture(Collections.emptyList())).when(consortiumUserTenantService).getUserTenantsIfNeeded(any());
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
    doReturn(succeededFuture(Collections.emptyList())).when(consortiumUserTenantService).getUserTenantsIfNeeded(any());
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
    doReturn(succeededFuture(Collections.emptyList())).when(consortiumUserTenantService).getUserTenantsIfNeeded(any());
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(purchaseOrderLineService).getPoLinesByHoldingIds(List.of(holdingId), requestContext);
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(succeededFuture(List.of(item))).when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(piece, requestContext).result();

    verify(inventoryHoldingManager, times(0)).deleteHoldingById(holdingId, true, requestContext);
  }

  @Test
  void shouldNotDeleteHoldingWhenMultipleTenantsHaveLinkedPoLinesAndPieces() {
    // Setup test data
    String holdingId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);

    String tenant1 = "tenant-1";
    String tenant2 = "tenant-2";
    String tenant3 = "tenant-3";
    List<String> userTenants = List.of(tenant1, tenant2, tenant3);

    // Piece being deleted
    Piece pieceToDelete = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withPoLineId(UUID.randomUUID().toString());

    // Tenant 1: has 2 poLines and 1 piece
    String poLineId1Tenant1 = UUID.randomUUID().toString();
    String poLineId2Tenant1 = UUID.randomUUID().toString();
    PoLine poLine1Tenant1 = new PoLine()
      .withId(poLineId1Tenant1)
      .withLocations(List.of(new Location().withHoldingId(holdingId)));
    PoLine poLine2Tenant1 = new PoLine()
      .withId(poLineId2Tenant1)
      .withLocations(List.of(new Location().withHoldingId(holdingId)));
    Piece piece1Tenant1 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withPoLineId(poLineId1Tenant1);

    // Tenant 2: has 1 poLine and 2 pieces
    String poLineId1Tenant2 = UUID.randomUUID().toString();
    PoLine poLine1Tenant2 = new PoLine()
      .withId(poLineId1Tenant2)
      .withLocations(List.of(new Location().withHoldingId(holdingId)));
    Piece piece1Tenant2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withPoLineId(poLineId1Tenant2);
    Piece piece2Tenant2 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withPoLineId(poLineId1Tenant2);

    // Tenant 3: has 1 poLine and 1 piece
    String poLineId1Tenant3 = UUID.randomUUID().toString();
    PoLine poLine1Tenant3 = new PoLine()
      .withId(poLineId1Tenant3)
      .withLocations(List.of(new Location().withHoldingId(holdingId)));
    Piece piece1Tenant3 = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withPoLineId(poLineId1Tenant3);

    // Mock consortium service to return multiple tenants (triggers lines 119-125)
    doReturn(succeededFuture(userTenants)).when(consortiumUserTenantService).getUserTenantsIfNeeded(any());

    // Mock holding retrieval
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);

    // Mock getPoLinesByHoldingIds for each tenant - use any() for RequestContext as it's recreated per tenant
    doReturn(succeededFuture(List.of(poLine1Tenant1, poLine2Tenant1)))
      .when(purchaseOrderLineService).getPoLinesByHoldingIds(eq(List.of(holdingId)), any());

    // Mock getPiecesByHoldingId for each tenant
    doReturn(succeededFuture(List.of(piece1Tenant1)))
      .doReturn(succeededFuture(List.of(piece1Tenant2, piece2Tenant2)))
      .doReturn(succeededFuture(List.of(piece1Tenant3)))
      .when(pieceStorageService).getPiecesByHoldingId(eq(holdingId), any());

    // Mock items (empty for all tenants)
    doReturn(succeededFuture(Collections.emptyList())).when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);

    // Execute
    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(pieceToDelete, requestContext).result();

    // Verify deletion did NOT happen because linked poLines and pieces exist across tenants
    verify(inventoryHoldingManager, times(0)).deleteHoldingById(holdingId, true, requestContext);

    // Verify that the service queried all 3 tenants (lines 119-125 executed for each tenant)
    verify(purchaseOrderLineService, times(3)).getPoLinesByHoldingIds(eq(List.of(holdingId)), any());
    verify(pieceStorageService, times(3)).getPiecesByHoldingId(eq(holdingId), any());
  }

  @Test
  void shouldDeleteHoldingWhenMultipleTenantsHaveNoLinkedData() {
    // Setup test data
    String holdingId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, locationId);

    String tenant1 = "tenant-1";
    String tenant2 = "tenant-2";
    List<String> userTenants = List.of(tenant1, tenant2);

    Piece pieceToDelete = new Piece()
      .withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withPoLineId(UUID.randomUUID().toString());

    // Mock consortium service to return multiple tenants (triggers lines 119-125)
    doReturn(succeededFuture(userTenants)).when(consortiumUserTenantService).getUserTenantsIfNeeded(any());

    // Mock holding retrieval
    doReturn(succeededFuture(holding)).when(inventoryHoldingManager).getHoldingById(holdingId, true, requestContext);

    // All tenants return empty results
    doReturn(succeededFuture(Collections.emptyList()))
      .when(purchaseOrderLineService).getPoLinesByHoldingIds(eq(List.of(holdingId)), any());
    doReturn(succeededFuture(Collections.emptyList()))
      .when(pieceStorageService).getPiecesByHoldingId(eq(holdingId), any());
    doReturn(succeededFuture(Collections.emptyList()))
      .when(inventoryItemManager).getItemsByHoldingId(holdingId, requestContext);

    // Mock successful deletion
    doReturn(succeededFuture(null)).when(inventoryHoldingManager).deleteHoldingById(holdingId, true, requestContext);

    // Execute
    pieceUpdateInventoryService.deleteHoldingConnectedToPiece(pieceToDelete, requestContext).result();

    // Verify deletion happened because no linked data exists across all tenants
    verify(inventoryHoldingManager, times(1)).deleteHoldingById(holdingId, true, requestContext);

    // Verify that the service queried both tenants (lines 119-125 executed for each tenant)
    verify(purchaseOrderLineService, times(2)).getPoLinesByHoldingIds(eq(List.of(holdingId)), any());
    verify(pieceStorageService, times(2)).getPiecesByHoldingId(eq(holdingId), any());
  }


  private static class ContextConfiguration {
    @Bean
    ConsortiumUserTenantService consortiumUserTenantService() {
      return mock(ConsortiumUserTenantService.class);
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
    PieceUpdateInventoryService pieceUpdateInventoryService(ConsortiumUserTenantService consortiumUserTenantService,
                                                            InventoryItemManager inventoryItemManager,
                                                            InventoryHoldingManager inventoryHoldingManager,
                                                            PieceStorageService pieceStorageService,
                                                            PurchaseOrderLineService purchaseOrderLineService) {
      return spy(new PieceUpdateInventoryService(consortiumUserTenantService,
                                                 inventoryItemManager, inventoryHoldingManager,
                                                 pieceStorageService, purchaseOrderLineService));
    }
  }
}
