package org.folio.service.orders.lines.update;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.RestClientTest.X_OKAPI_TENANT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.orders.lines.update.OrderLineUpdateInstanceHolder;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CreateInventoryType;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.consortium.SharingInstanceService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.orders.lines.update.instance.WithHoldingOrderLineUpdateInstanceStrategy;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.settings.SettingsRetriever;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import com.google.common.collect.Lists;

import io.vertx.core.Context;
import io.vertx.core.Vertx;

public class OrderLineUpdateInstanceHandlerTest {

  private static boolean runningOnOwn;

  @Autowired
  private OrderLinePatchOperationService orderLinePatchOperationService;

  @Mock
  private Map<String, String> okapiHeadersMock;
  @Mock
  private PieceStorageService pieceStorageService;

  private RequestContext requestContext;
  private AutoCloseable openMocks;

  @BeforeEach
  public void initMocks() {
    openMocks = MockitoAnnotations.openMocks(this);
    Context ctxMock = Vertx.vertx().getOrCreateContext();
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put("x-okapi-tenant", X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
    autowireDependencies(this);
    doReturn(succeededFuture(Lists.newArrayList())).when(pieceStorageService).getPiecesByPoLineId(any(), any());
  }

  @AfterEach
  public void resetMocks() throws Exception {
    if (openMocks != null) {
      openMocks.close();
    }
  }

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(OrderLineUpdateInstanceHandlerTest.ContextConfiguration.class);
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  public void updateInstanceHoldingForMIXOrderFormat() {
    String orderLineId = UUID.randomUUID().toString();
    PoLine poLine = new PoLine().
        withId(orderLineId).
        withOrderFormat(PoLine.OrderFormat.P_E_MIX)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING))
        .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM));

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.MOVE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    orderLinePatchOperationService.handleUpdateInstance(orderLineUpdateInstanceHolder, requestContext);
  }

  @Test
  public void updateInstanceHoldingForPhysicalOrderFormat() {
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
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    orderLinePatchOperationService.handleUpdateInstance(orderLineUpdateInstanceHolder, requestContext);
  }

  @Test
  public void updateInstanceHoldingForEresourceOrderFormat() {
    String orderLineId = UUID.randomUUID().toString();

    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
        .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM));

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.FIND_OR_CREATE)
            .withDeleteAbandonedHoldings(true));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    orderLinePatchOperationService.handleUpdateInstance(orderLineUpdateInstanceHolder, requestContext);

  }

  @Test
  public void updateInstanceHoldingForOtherOrderFormat() {
    String orderLineId = UUID.randomUUID().toString();
    PoLine poLine = new PoLine().
            withId(orderLineId).
            withOrderFormat(PoLine.OrderFormat.OTHER)
        .withPhysical(new Physical()
            .withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING));

    PatchOrderLineRequest patchOrderLineRequest = new PatchOrderLineRequest();
    patchOrderLineRequest.withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    OrderLineUpdateInstanceHolder orderLineUpdateInstanceHolder = new OrderLineUpdateInstanceHolder()
        .withStoragePoLine(poLine).withPathOrderLineRequest(patchOrderLineRequest);

    orderLinePatchOperationService.handleUpdateInstance(orderLineUpdateInstanceHolder, requestContext);

  }

  static class ContextConfiguration {
    @Bean RestClient restClient() {
      return new RestClient();
    }

    @Bean AcquisitionsUnitsService acquisitionsUnitsService(RestClient restClient) {
      return new AcquisitionsUnitsService(restClient);
    }

    @Bean ProtectionService protectionService(AcquisitionsUnitsService acquisitionsUnitsService) {
      return new ProtectionService(acquisitionsUnitsService);
    }

    @Bean PieceStorageService pieceStorageService(ConsortiumConfigurationService consortiumConfigurationService,
                                                  ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever,
                                                  SettingsRetriever settingsRetriever,
                                                  RestClient restClient) {
      return new PieceStorageService(consortiumConfigurationService, consortiumUserTenantsRetriever, settingsRetriever, restClient);
    }

    @Bean InventoryService inventoryService (RestClient restClient) {
      return new InventoryService(restClient);
    }

    @Bean SharingInstanceService sharingInstanceService (RestClient restClient) {
      return new SharingInstanceService(restClient);
    }

    @Bean
    ConsortiumConfigurationService consortiumConfigurationService(RestClient restClient, SettingsRetriever settingsRetriever) {
      return new ConsortiumConfigurationService(restClient, settingsRetriever, 1L);
    }

    @Bean
    ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever(RestClient restClient) {
      return new ConsortiumUserTenantsRetriever(restClient, 1L);
    }

    @Bean PurchaseOrderStorageService purchaseOrderStorageService () {
      return mock(PurchaseOrderStorageService.class);
    }

    @Bean
    ConfigurationEntriesService configurationEntriesService(RestClient restClient) {
      return new ConfigurationEntriesService(restClient);
    }

    @Bean
    public InventoryItemManager inventoryItemManager(RestClient restClient,
                                                     ConfigurationEntriesCache configurationEntriesCache,
                                                     InventoryCache inventoryCache,
                                                     ConsortiumConfigurationService consortiumConfigurationService) {
      return new InventoryItemManager(restClient, configurationEntriesCache, inventoryCache, consortiumConfigurationService);
    }

    @Bean
    public InventoryHoldingManager inventoryHoldingManager(RestClient restClient,
                                                           ConfigurationEntriesCache configurationEntriesCache,
                                                           InventoryCache inventoryCache) {
      return new InventoryHoldingManager(restClient, configurationEntriesCache, inventoryCache);
    }

    @Bean
    public InventoryInstanceManager inventoryInstanceManager(RestClient restClient,
                                                             ConfigurationEntriesCache configurationEntriesCache,
                                                             InventoryCache inventoryCache,
                                                             ConsortiumConfigurationService consortiumConfigurationService,
                                                             SharingInstanceService sharingInstanceService) {
      return new InventoryInstanceManager(restClient, configurationEntriesCache, inventoryCache, sharingInstanceService, consortiumConfigurationService);
    }

    @Bean
    PurchaseOrderLineService purchaseOrderLineService(RestClient restClient, InventoryCache inventoryCache, InventoryHoldingManager inventoryHoldingManager) {
      return new PurchaseOrderLineService(restClient, inventoryCache, inventoryHoldingManager);
    }

    @Bean
    InventoryCache inventoryCache(InventoryService inventoryService) {
      return new InventoryCache(inventoryService);
    }

    @Bean
    ConfigurationEntriesCache configurationEntriesCache(ConfigurationEntriesService configurationEntriesService) {
      return new ConfigurationEntriesCache(configurationEntriesService, 1L);
    }

    @Bean OrderLinePatchOperationService orderLinePatchOperationService(
        RestClient restClient,
        OrderLineUpdateInstanceStrategyResolver orderLineUpdateInstanceStrategyResolver,
        PurchaseOrderLineService purchaseOrderLineService,
        InventoryCache inventoryCache,
        InventoryInstanceManager inventoryInstanceManager) {
      return new OrderLinePatchOperationService(restClient, orderLineUpdateInstanceStrategyResolver, purchaseOrderLineService, inventoryCache, inventoryInstanceManager);
    }

    @Bean OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy(InventoryInstanceManager inventoryInstanceManager,
                                                                                     InventoryItemManager inventoryItemManager,
                                                                                     InventoryHoldingManager inventoryHoldingManager,
                                                                                     PieceStorageService pieceStorageService) {
      return new WithHoldingOrderLineUpdateInstanceStrategy(inventoryInstanceManager, inventoryItemManager, inventoryHoldingManager, pieceStorageService);
    }

    @Bean OrderLineUpdateInstanceStrategyResolver updateInstanceStrategyResolver(OrderLineUpdateInstanceStrategy withHoldingOrderLineUpdateInstanceStrategy,
        OrderLineUpdateInstanceStrategy withoutHoldingOrderLineUpdateInstanceStrategy) {
      Map<CreateInventoryType, OrderLineUpdateInstanceStrategy> strategies = new HashMap<>();

      strategies.put(CreateInventoryType.INSTANCE_HOLDING_ITEM, withHoldingOrderLineUpdateInstanceStrategy);
      strategies.put(CreateInventoryType.INSTANCE_HOLDING, withHoldingOrderLineUpdateInstanceStrategy);
      strategies.put(CreateInventoryType.INSTANCE, withoutHoldingOrderLineUpdateInstanceStrategy);
      strategies.put(CreateInventoryType.NONE, withoutHoldingOrderLineUpdateInstanceStrategy);

      return new OrderLineUpdateInstanceStrategyResolver(strategies);
    }

    @Bean
    SettingsRetriever settingsRetriever(RestClient restClient) {
      return new SettingsRetriever(restClient, 1L);
    }
  }
}
