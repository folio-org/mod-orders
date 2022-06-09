package org.folio.service.orders.lines.update.instance;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import org.folio.ApiTestSuite;
import org.folio.TestConstants;
import org.folio.models.ItemStatus;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.inventory.InventoryManager;

import org.folio.service.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.service.orders.lines.update.OrderLineUpdateInstanceStrategy;
import org.folio.service.pieces.PieceStorageService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.impl.MockServer.HOLDINGS_OLD_NEW_PATH;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryManager.ITEM_STATUS_NAME;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

public class WithHoldingOrderLineUpdateInstanceStrategyTest {
  @Autowired
  OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy;
  @Autowired
  InventoryManager inventoryManager;
  @Autowired
  RestClient restClient;

  @Mock
  private Map<String, String> okapiHeadersMock;
  @Spy
  private Context ctxMock;

  private RequestContext requestContext;
  private HttpClientInterface httpClient;
  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(WithHoldingOrderLineUpdateInstanceStrategyTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @BeforeEach
  public void beforeEach() {
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  @Test
  public void updateInstanceForMoveHoldingOperation() throws IOException {
    String orderLineId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
        .map(o -> ((JsonObject) o))
        .collect(toList());

    String holdingId = holdings.get(0).getString("ID");

    Location location = new Location()
        .withHoldingId(holdingId)
        .withQuantity(1)
        .withQuantityPhysical(1);

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(location);

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING)).withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.MOVE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext);
  }

  @Test
  public void updateInstanceForFindOrCreateHoldingOperation() {
    String orderLineId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();

    Location location = new Location()
        .withHoldingId(holdingId)
        .withQuantity(1)
        .withQuantityPhysical(1);

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(location);

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING)).withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.FIND_OR_CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine)
        .withPathOrderLineRequest(patchOrderLineRequest);

    JsonObject holding = new JsonObject();
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    holding.put(ID, holdingId);
    doReturn(completedFuture(holding)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext);
  }

  @Test
  public void updateInstanceForFindOrCreateHoldingOperationAndDeleteAbandonedHoldings() {
    String orderLineId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();

    Location location = new Location()
        .withHoldingId(holdingId)
        .withQuantity(1)
        .withQuantityPhysical(1);

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(location);

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING)).withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.FIND_OR_CREATE)
            .withDeleteAbandonedHoldings(true));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine)
        .withPathOrderLineRequest(patchOrderLineRequest);

    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    doReturn(completedFuture(holding)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext);
  }

  @Test
  public void updateInstanceForCreateHoldingOperation() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();

    Location location = new Location()
        .withHoldingId(holdingId)
        .withQuantity(1)
        .withQuantityPhysical(1);

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(location);

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
        .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    doReturn(completedFuture(holding)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext);
  }

  @Test
  public void updateInstanceAndItemsForCreateHoldingOperation() throws IOException {
    String orderLineId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();

    JsonObject item = new JsonObject().put(TestConstants.ID, itemId);
    item.put(ITEM_STATUS, new JsonObject().put(ITEM_STATUS_NAME, ItemStatus.ON_ORDER.value()));
    item.put(ITEM_HOLDINGS_RECORD_ID, holdingId);

    Location location = new Location()
        .withHoldingId(holdingId)
        .withQuantity(1)
        .withQuantityPhysical(1);

    ArrayList<Location> locations = new ArrayList<>();
    locations.add(location);

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM))
        .withLocations(locations);

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    holding.put(HOLDING_PERMANENT_LOCATION_ID, UUID.randomUUID().toString());
    doReturn(completedFuture(holding)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    doReturn(completedFuture(item)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    withHoldingOrderLineUpdateInstanceStrategy.updateInstance(orderLineUpdateInstanceHolder, requestContext);
  }

  static class ContextConfiguration {
    @Bean
    RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean
    public ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }

    @Bean
    public PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    public InventoryManager inventoryManager(RestClient restClient, ConfigurationEntriesService configurationEntriesService,
        PieceStorageService pieceStorageService) {
      return spy(new InventoryManager(restClient, configurationEntriesService, pieceStorageService));
    }


    @Bean OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy(InventoryManager inventoryManager) {
      return new WithHoldingOrderLineUpdateInstanceStrategy(inventoryManager);
    }
  }
}