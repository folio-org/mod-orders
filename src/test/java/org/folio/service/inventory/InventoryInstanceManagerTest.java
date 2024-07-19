package org.folio.service.inventory;

import io.vertx.core.json.JsonObject;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.models.consortium.SharingInstance;
import org.folio.models.consortium.SharingInstanceCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.SharingInstanceService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.models.ItemFields.ID;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.COMP_PO_LINES_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

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
  private ConfigurationEntriesCache configurationEntriesCache;
  @Mock
  private InventoryCache inventoryCache;
  @Mock
  private SharingInstanceService sharingInstanceService;
  @Mock
  private ConsortiumConfigurationService consortiumConfigurationService;


  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
    inventoryInstanceManager = new InventoryInstanceManager(restClient, configurationEntriesCache, inventoryCache, sharingInstanceService, consortiumConfigurationService);
  }

  @Test
  void shouldNoShareInstanceWhenNonEcsMode() {
    String instanceId = UUID.randomUUID().toString();
    CompositePoLine compositePoLine = getPoLine(instanceId, List.of(
      new Location().withTenantId(MEMBER_TENANT_1),
      new Location().withTenantId(CENTRAL_TENANT)));
    doReturn(succeededFuture(Optional.empty())).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);

    inventoryInstanceManager.openOrderHandleInstance(compositePoLine, false, requestContext).result();

    verifyNoInteractions(sharingInstanceService);
  }

  @Test
  void shouldNotShareInstanceAmongTenantsWhenInstancesAlreadyShared() {
    String instanceId = UUID.randomUUID().toString();
    CompositePoLine compositePoLine = getPoLine(instanceId, List.of(
      new Location().withTenantId(MEMBER_TENANT_1),
      new Location().withTenantId(CENTRAL_TENANT)));
    Optional<ConsortiumConfiguration> configuration = Optional.of(new ConsortiumConfiguration(CENTRAL_TENANT, UUID.randomUUID().toString()));
    doReturn(succeededFuture(configuration)).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);

    SharingInstanceCollection collection = getSharingInstanceCollection(instanceId, MEMBER_TENANT_1, CENTRAL_TENANT);
    doReturn(succeededFuture(collection)).when(sharingInstanceService).getSharingInstances(instanceId, configuration.get(), requestContext);

    inventoryInstanceManager.openOrderHandleInstance(compositePoLine, false, requestContext).result();

    verify(sharingInstanceService, never()).createShadowInstance(anyString(), any(ConsortiumConfiguration.class), any(RequestContext.class));
  }

  @Test
  void shouldShareInstance() {
    String instanceId = UUID.randomUUID().toString();
    CompositePoLine compositePoLine = getPoLine(instanceId, List.of(
      new Location().withTenantId(MEMBER_TENANT_1),
      new Location().withTenantId(MEMBER_TENANT_2)));
    Optional<ConsortiumConfiguration> configuration = Optional.of(new ConsortiumConfiguration(CENTRAL_TENANT, UUID.randomUUID().toString()));
    doReturn(succeededFuture(configuration)).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);
    doReturn(succeededFuture(null)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), any(RequestContext.class));

    SharingInstanceCollection collection = new SharingInstanceCollection();
    SharingInstance sharingInstance = new SharingInstance(UUID.fromString(instanceId), MEMBER_TENANT_1, CENTRAL_TENANT);
    collection.setSharingInstances(List.of(sharingInstance));

    doReturn(succeededFuture(collection)).when(sharingInstanceService).getSharingInstances(instanceId, configuration.get(), requestContext);

    ArgumentCaptor<RequestContext> requestContextCaptor = ArgumentCaptor.forClass(RequestContext.class);

    inventoryInstanceManager.openOrderHandleInstance(compositePoLine, false, requestContext).result();

    verify(sharingInstanceService, times(1)).createShadowInstance(eq(instanceId), eq(configuration.get()), requestContextCaptor.capture());

    RequestContext requestContextForSharing = requestContextCaptor.getValue();
    String tenantId = TenantTool.tenantId(requestContextForSharing.getHeaders());
    assertEquals(MEMBER_TENANT_2, tenantId);
  }

  private CompositePoLine getPoLine(String instanceId, List<Location> locations) {
    CompositePoLine result = getMockAsJson(PO_LINE_MIN_CONTENT_PATH).mapTo(CompositePoLine.class);
    result.setInstanceId(instanceId);
    result.setLocations(locations);
    return result;
  }

  private SharingInstanceCollection getSharingInstanceCollection(String instanceId, String sourceTenant, String targetTenant) {
    SharingInstanceCollection result = new SharingInstanceCollection();
    SharingInstance sharingInstance = new SharingInstance(UUID.fromString(instanceId), sourceTenant, targetTenant);
    result.setSharingInstances(List.of(sharingInstance));
    return result;
  }
}
