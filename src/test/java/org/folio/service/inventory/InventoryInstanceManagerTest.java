package org.folio.service.inventory;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import lombok.extern.log4j.Log4j2;
import org.folio.CopilotGenerated;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.models.consortium.SharingInstance;
import org.folio.models.consortium.SharingInstanceCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.SharingInstanceService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.platform.commons.util.StringUtils;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_INSTANCE_STATUS;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_INSTANCE_TYPE;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.COMP_PO_LINES_MOCK_DATA_PATH;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_STATUSES;
import static org.folio.service.inventory.InventoryInstanceManager.INSTANCE_TYPES;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@Log4j2
public class InventoryInstanceManagerTest {

  private static final String PO_LINE_MIN_CONTENT_PATH = COMP_PO_LINES_MOCK_DATA_PATH + "minimalContent.json";
  private static final String MEMBER_TENANT_1 = "tenant1";
  private static final String MEMBER_TENANT_2 = "tenant2";
  private static final String CENTRAL_TENANT = "central_tenant";

  private AutoCloseable mockitoMocks;
  private InventoryInstanceManager inventoryInstanceManager;

  @Mock
  private RequestContext requestContext;
  @Mock
  private RestClient restClient;
  @Mock
  private CommonSettingsCache commonSettingsCache;
  @Mock
  private InventoryCache inventoryCache;
  @Mock
  private SharingInstanceService sharingInstanceService;
  @Mock
  private ConsortiumConfigurationService consortiumConfigurationService;

  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
    inventoryInstanceManager = new InventoryInstanceManager(restClient, commonSettingsCache, inventoryCache, sharingInstanceService, consortiumConfigurationService);
  }

  @AfterEach
  void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void shouldUseExistingInstanceId() {
    String instanceId = UUID.randomUUID().toString();
    PoLine poLine = getPoLine(instanceId, List.of(
      new Location().withTenantId(MEMBER_TENANT_1),
      new Location().withTenantId(CENTRAL_TENANT)));
    doReturn(succeededFuture(Optional.empty())).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);

    inventoryInstanceManager.openOrderHandleInstance(poLine, false, requestContext).result();

    verifyNoInteractions(restClient);
    verifyNoInteractions(sharingInstanceService);
  }

  @Test
  void shouldNotShareInstanceWhenNonEcsMode() {
    PoLine poLine = getPoLine(null, List.of(
      new Location().withTenantId(MEMBER_TENANT_1),
      new Location().withTenantId(CENTRAL_TENANT)));
    doReturn(succeededFuture(Optional.empty())).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);

    JsonObject emptyInstanceCollection = new JsonObject();
    emptyInstanceCollection.put("instances", new JsonArray());
    doReturn(succeededFuture(emptyInstanceCollection))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), any(RequestContext.class));

    inventoryInstanceManager.openOrderHandleInstance(poLine, false, requestContext).result();

    verifyNoInteractions(sharingInstanceService);
  }

  @ParameterizedTest
  @CsvSource({"true,true", "false,false", "false,"})
  @CopilotGenerated(partiallyGenerated = true, model = "Claude Sonnet 3.7")
  void shouldCreateInstanceRecordWithSuppressDiscoveryWhenPoLineSuppressInstanceFromDiscoveryIsTrue(String expected, String input) {
    var poLine = getPoLine(null, List.of(
      new Location().withTenantId(MEMBER_TENANT_1),
      new Location().withTenantId(CENTRAL_TENANT)));
    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(succeededFuture(Optional.empty()));
    if (StringUtils.isNotBlank(input)) {
      poLine.setSuppressInstanceFromDiscovery(Boolean.valueOf(input));
    }

    // Mock the InventoryUtils.getEntryId static method calls
    try (var mockedUtils = Mockito.mockStatic(InventoryUtils.class)) {
      // Mock for instance types
      var instanceTypeJson = new JsonObject().put(INSTANCE_TYPES, "fake-instance-type-id");
      mockedUtils.when(() -> InventoryUtils.getEntryId(eq(commonSettingsCache), eq(inventoryCache),
          eq(INSTANCE_TYPES), eq(MISSING_INSTANCE_TYPE), eq(requestContext)))
        .thenReturn(succeededFuture(instanceTypeJson));

      // Mock for instance statuses
      var statusJson = new JsonObject().put(INSTANCE_STATUSES, "fake-status-id");
      mockedUtils.when(() -> InventoryUtils.getEntryId(eq(commonSettingsCache), eq(inventoryCache),
          eq(INSTANCE_STATUSES), eq(MISSING_INSTANCE_STATUS), eq(requestContext)))
        .thenReturn(succeededFuture(statusJson));

      var emptyInstanceCollection = new JsonObject();
      emptyInstanceCollection.put("instances", new JsonArray());
      when(restClient.getAsJsonObject(any(RequestEntry.class), any(RequestContext.class)))
        .thenReturn(succeededFuture(emptyInstanceCollection));

      // Mock for postJsonObjectAndGetId
      when(restClient.postJsonObjectAndGetId(any(RequestEntry.class), any(JsonObject.class), any(RequestContext.class)))
        .thenReturn(succeededFuture("mock-instance-id"));

      inventoryInstanceManager.openOrderHandleInstance(poLine, false, requestContext).result();

      // Verify instance was created with suppressDiscovery=true
      var instanceCaptor = ArgumentCaptor.forClass(JsonObject.class);
      verify(restClient).postJsonObjectAndGetId(any(RequestEntry.class), instanceCaptor.capture(), any(RequestContext.class));
      var createdInstance = instanceCaptor.getValue();
      var actual = createdInstance.getBoolean(InventoryInstanceManager.INSTANCE_DISCOVERY_SUPPRESS);

      log.info("Created instance with suppressDiscovery: {}", actual);
      assertEquals(Boolean.valueOf(expected), actual);

      verifyNoInteractions(sharingInstanceService);
    }
  }

  @Test
  void shouldNotShareInstanceAmongTenantsWhenInstancesAlreadyShared() {
    String instanceId = UUID.randomUUID().toString();
    PoLine poLine = getPoLine(null, List.of(
      new Location().withTenantId(MEMBER_TENANT_1),
      new Location().withTenantId(CENTRAL_TENANT)));
    Optional<ConsortiumConfiguration> configuration = Optional.of(new ConsortiumConfiguration(CENTRAL_TENANT, UUID.randomUUID().toString()));
    doReturn(succeededFuture(configuration)).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);

    JsonObject emptyInstanceCollection = new JsonObject();
    emptyInstanceCollection.put("instances", new JsonArray());
    JsonObject instanceCollection = new JsonObject();
    JsonObject instanceAsJson = new JsonObject();
    instanceAsJson.put("id", instanceId);
    instanceCollection.put("instances", JsonArray.of(instanceAsJson));
    doReturn(succeededFuture(emptyInstanceCollection))
      .doReturn(succeededFuture(instanceCollection))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), any(RequestContext.class));

    SharingInstanceCollection collection = getSharingInstanceCollection(instanceId);
    doReturn(succeededFuture(collection)).when(sharingInstanceService).getSharingInstances(instanceId, configuration.get(), requestContext);

    inventoryInstanceManager.openOrderHandleInstance(poLine, false, requestContext).result();

    verify(sharingInstanceService, never()).createShadowInstance(anyString(), any(ConsortiumConfiguration.class), any(RequestContext.class));
  }

  @Test
  void shouldShareInstanceWhenFoundInCentralTenant() {
    String centralInstanceId = UUID.randomUUID().toString();
    PoLine poLine = getPoLine(null, List.of(
      new Location().withTenantId(MEMBER_TENANT_1),
      new Location().withTenantId(MEMBER_TENANT_2)));
    JsonObject emptyInstanceCollection = new JsonObject();
    emptyInstanceCollection.put("instances", new JsonArray());
    JsonObject instanceCollection = new JsonObject();
    JsonObject instanceAsJson = new JsonObject();
    instanceAsJson.put("id", centralInstanceId);
    instanceCollection.put("instances", JsonArray.of(instanceAsJson));
    Optional<ConsortiumConfiguration> configuration = Optional.of(new ConsortiumConfiguration(CENTRAL_TENANT, UUID.randomUUID().toString()));

    doReturn(succeededFuture(configuration))
      .when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);
    doReturn(succeededFuture(emptyInstanceCollection))
      .doReturn(succeededFuture(instanceCollection))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), any(RequestContext.class));
    doReturn(succeededFuture(null))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), any(RequestContext.class));

    SharingInstanceCollection collection = new SharingInstanceCollection();
    SharingInstance sharingInstance = new SharingInstance(UUID.fromString(centralInstanceId), MEMBER_TENANT_2, CENTRAL_TENANT);
    collection.setSharingInstances(List.of(sharingInstance));

    doReturn(succeededFuture(collection))
      .when(sharingInstanceService).getSharingInstances(centralInstanceId, configuration.get(), requestContext);

    ArgumentCaptor<RequestContext> requestContextCaptor = ArgumentCaptor.forClass(RequestContext.class);

    inventoryInstanceManager.openOrderHandleInstance(poLine, false, requestContext).result();

    verify(sharingInstanceService, times(1))
      .createShadowInstance(eq(centralInstanceId), eq(configuration.get()), requestContextCaptor.capture());

    RequestContext requestContextForSharing = requestContextCaptor.getValue();
    String tenantId = TenantTool.tenantId(requestContextForSharing.getHeaders());
    assertEquals(MEMBER_TENANT_1, tenantId);
  }

  private PoLine getPoLine(String instanceId, List<Location> locations) {
    PoLine result = getMockAsJson(PO_LINE_MIN_CONTENT_PATH).mapTo(PoLine.class);
    result.setInstanceId(instanceId);
    result.setLocations(locations);
    List<ProductId> productIds = List.of(new ProductId().withProductId("10407849").withProductIdType("913300b2-03ed-469a-8179-c1092c991227"));
    result.setDetails(new Details().withProductIds(productIds));
    return result;
  }

  private SharingInstanceCollection getSharingInstanceCollection(String instanceId) {
    SharingInstanceCollection result = new SharingInstanceCollection();
    SharingInstance sharingInstance = new SharingInstance(UUID.fromString(instanceId), MEMBER_TENANT_1, CENTRAL_TENANT);
    result.setSharingInstances(List.of(sharingInstance));
    return result;
  }
}
