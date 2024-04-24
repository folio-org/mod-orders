package org.folio.service.orders.flows.update.open;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.apache.commons.lang3.RandomStringUtils;
import org.folio.ApiTestSuite;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Location;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.strategies.ProcessInventoryElectronicStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryPhysicalStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.HOLDINGS_OLD_NEW_PATH;
import static org.folio.rest.tools.utils.TenantTool.tenantId;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(VertxExtension.class)
public class OpenCompositeOrderInventoryServiceTest {

  public static final String LINE_ID = "2c4c29fd-fb8a-4c70-8561-931690bae8a4";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";

  @Autowired
  private OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  @Autowired
  private InventoryInstanceManager inventoryInstanceManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private  PieceStorageService pieceStorageService;
  @Autowired
  private ConsortiumConfigurationService consortiumConfigurationService;
  @Autowired
  private ProcessInventoryStrategyResolver processInventoryStrategyResolver;
  @Autowired
  private RestClient restClient;

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
    initSpringContext(OpenCompositeOrderInventoryServiceTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void shouldFoundHoldingIdByLocationId() throws IOException {
    String titleId = UUID.randomUUID().toString();
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    doReturn(succeededFuture(line)).when(inventoryInstanceManager).openOrderHandleInstance(any(), anyBoolean(), eq(requestContext));
    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(), eq(requestContext));
    doReturn(succeededFuture(Optional.empty())).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);

    openCompositeOrderInventoryService.processInventory(line, titleId, false, requestContext).result();

    assertEquals(line.getLocations().get(0).getHoldingId(), "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d63");
    verify(processInventoryStrategyResolver, times(1)).getHoldingAndItemStrategy(any());
  }

  @Test
  void shouldUseClonedRequestContextBasedOnLocationWhenTenantSpecified() throws IOException {
    ArgumentCaptor<RequestContext> requestContextCaptor = ArgumentCaptor.forClass(RequestContext.class);

    String titleId = UUID.randomUUID().toString();
    Location location = new Location().withLocationId(UUID.randomUUID().toString()).withTenantId(RandomStringUtils.random(4));
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setLocations(Collections.singletonList(location));
    Optional<ConsortiumConfiguration> configuration = Optional.of(new ConsortiumConfiguration(UUID.randomUUID().toString(), UUID.randomUUID().toString()));

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    doReturn(succeededFuture(line)).when(inventoryInstanceManager).openOrderHandleInstance(any(), anyBoolean(), eq(requestContext));
    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(), requestContextCaptor.capture());
    doReturn(succeededFuture(configuration)).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);

    openCompositeOrderInventoryService.processInventory(line, titleId, false, requestContext).result();

    RequestContext capturedRequestContext = requestContextCaptor.getValue();
    assertEquals(location.getTenantId(), tenantId(capturedRequestContext.getHeaders()));
  }

  @Test
  void shouldNotUseClonedRequestContextBasedOnLocationWhenTenantNotSpecified() throws IOException {
    ArgumentCaptor<RequestContext> requestContextCaptor = ArgumentCaptor.forClass(RequestContext.class);

    String titleId = UUID.randomUUID().toString();
    Location location = new Location().withLocationId(UUID.randomUUID().toString());
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setLocations(Collections.singletonList(location));
    Optional<ConsortiumConfiguration> configuration = Optional.of(new ConsortiumConfiguration(UUID.randomUUID().toString(), UUID.randomUUID().toString()));

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));

    doReturn(succeededFuture(line)).when(inventoryInstanceManager).openOrderHandleInstance(any(), anyBoolean(), eq(requestContext));
    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(), requestContextCaptor.capture());
    doReturn(succeededFuture(configuration)).when(consortiumConfigurationService).getConsortiumConfiguration(requestContext);

    openCompositeOrderInventoryService.processInventory(line, titleId, false, requestContext).result();

    RequestContext capturedRequestContext = requestContextCaptor.getValue();
    assertEquals(tenantId(okapiHeadersMock), tenantId(capturedRequestContext.getHeaders()));
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  private static class ContextConfiguration {
    @Bean TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean InventoryHoldingManager inventoryHoldingManager() {
      return mock(InventoryHoldingManager.class);
    }

    @Bean InventoryInstanceManager inventoryInstanceManager() {
      return mock(InventoryInstanceManager.class);
    }

    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean ConsortiumConfigurationService consortiumConfigurationService() {
      return mock(ConsortiumConfigurationService.class);
    }
    @Bean OpenCompositeOrderPieceService openCompositeOrderPieceCreateService() {
      return mock(OpenCompositeOrderPieceService.class);
    }

    @Bean ProcessInventoryPhysicalStrategy processInventoryPhysicalStrategy(ConsortiumConfigurationService consortiumConfigurationService) {
      return spy(new ProcessInventoryPhysicalStrategy(consortiumConfigurationService));
    }

    @Bean ProcessInventoryElectronicStrategy processInventoryElectronicStrategy(ConsortiumConfigurationService consortiumConfigurationService) {
      return spy(new ProcessInventoryElectronicStrategy(consortiumConfigurationService));
    }

    @Bean ProcessInventoryStrategyResolver processInventoryStrategyResolver(ProcessInventoryPhysicalStrategy processInventoryPhysicalStrategy,
                                                    ProcessInventoryElectronicStrategy processInventoryElectronicStrategy) {
      Map<String, ProcessInventoryStrategy> strategy = new HashMap<>();
      strategy.put(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE.value(), processInventoryElectronicStrategy);
      strategy.put(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE.value(), processInventoryPhysicalStrategy);
      return spy(new ProcessInventoryStrategyResolver(strategy));
    }

    @Bean OpenCompositeOrderInventoryService openCompositeOrderInventoryService(InventoryItemManager inventoryItemManager,
                                                                                InventoryHoldingManager inventoryHoldingManager,
                                                                                InventoryInstanceManager inventoryInstanceManager,
                                                                                OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                                                                ProcessInventoryStrategyResolver processInventoryStrategyResolver,
                                                                                RestClient restClient) {
      return spy(new OpenCompositeOrderInventoryService(
        inventoryItemManager, inventoryHoldingManager, inventoryInstanceManager,
        openCompositeOrderPieceService, processInventoryStrategyResolver, restClient
      ));
    }
  }
}
