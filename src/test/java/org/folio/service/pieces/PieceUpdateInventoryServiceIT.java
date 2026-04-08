package org.folio.service.pieces;

import static io.vertx.core.Future.succeededFuture;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import org.folio.ApiTestSuiteIT;
import org.folio.models.HoldingDataExclusionConfig;
import org.folio.models.HoldingDataExclusionMode;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.acq.Location;
import org.folio.service.HoldingDeletionService;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

@ExtendWith(MockitoExtension.class)
public class PieceUpdateInventoryServiceIT {

  @Autowired
  private ConsortiumUserTenantService consortiumUserTenantService;
  @Autowired
  private PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  private HoldingDeletionService holdingDeletionService;
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
      ApiTestSuiteIT.before();
      runningOnOwn = true;
    }
    initSpringContext(PieceUpdateInventoryServiceIT.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuiteIT.after();
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

  @ParameterizedTest(name = "{0}")
  @MethodSource("excludePoLinesTestCases")
  @SuppressWarnings("unchecked")
  void testExcludePoLines(String testName, List<PoLine> input, Set<String> excludeIds, List<String> expectedIds) {
    try {
      var method = HoldingDeletionService.class.getDeclaredMethod("excludePoLines", List.class, HoldingDataExclusionConfig.class);
      method.setAccessible(true);
      var exclusionConfig = new HoldingDataExclusionConfig(HoldingDataExclusionMode.PIECE_RECEIVING, excludeIds, Set.of());
      List<PoLine> result = (List<PoLine>) method.invoke(holdingDeletionService, input, exclusionConfig);
      assertEquals(expectedIds, result.stream().map(PoLine::getId).toList());
    } catch (Exception e) {
      throw new RuntimeException("Failed to invoke excludePoLines", e);
    }
  }

  static Stream<Arguments> excludePoLinesTestCases() {
    return Stream.of(
      Arguments.of("checkinItems=true kept even in excludeIds",
        List.of(new PoLine().withId("id1").withCheckinItems(true)), Set.of("id1"), List.of("id1")),
      Arguments.of("checkinItems=false kept when NOT in excludeIds",
        List.of(new PoLine().withId("id1").withCheckinItems(false)), Set.of("id2"), List.of("id1")),
      Arguments.of("checkinItems=false filtered when in excludeIds",
        List.of(new PoLine().withId("id1").withCheckinItems(false)), Set.of("id1"), List.of()),
      Arguments.of("Mixed scenario: checkinItems logic + excludeIds",
        List.of(
          new PoLine().withId("id1").withCheckinItems(true),
          new PoLine().withId("id2").withCheckinItems(false),
          new PoLine().withId("id3").withCheckinItems(false),
          new PoLine().withId("id4").withCheckinItems(true)
        ), Set.of("id3", "id4"), List.of("id1", "id2", "id4")),
      Arguments.of("All checkinItems=false in excludeIds filtered",
        List.of(new PoLine().withId("id1").withCheckinItems(false), new PoLine().withId("id2").withCheckinItems(false)),
        Set.of("id1", "id2"), List.of())
    );
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
    HoldingDeletionService holdingDeletionService(ConsortiumUserTenantService consortiumUserTenantService,
                                                  InventoryHoldingManager inventoryHoldingManager,
                                                  InventoryItemManager inventoryItemManager,
                                                  PieceStorageService pieceStorageService,
                                                  PurchaseOrderLineService purchaseOrderLineService) {
      return spy(new HoldingDeletionService(consortiumUserTenantService, inventoryHoldingManager,
        inventoryItemManager, pieceStorageService, purchaseOrderLineService));
    }

    @Bean
    PieceUpdateInventoryService pieceUpdateInventoryService(InventoryHoldingManager inventoryHoldingManager,
                                                            InventoryItemManager inventoryItemManager,
                                                            HoldingDeletionService holdingDeletionService) {
      return spy(new PieceUpdateInventoryService(inventoryHoldingManager, inventoryItemManager, holdingDeletionService));
    }
  }
}
