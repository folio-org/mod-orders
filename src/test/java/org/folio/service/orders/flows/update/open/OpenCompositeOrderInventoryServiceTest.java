package org.folio.service.orders.flows.update.open;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.folio.ApiTestSuite;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.strategies.ProcessInventoryElectronicStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryPhysicalStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategy;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.*;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.HOLDINGS_OLD_NEW_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(VertxExtension.class)
public class OpenCompositeOrderInventoryServiceTest {

  public static final String LINE_ID = "2c4c29fd-fb8a-4c70-8561-931690bae8a4";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";

  @Autowired
  private OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private  PieceStorageService pieceStorageService;
  @Autowired
  private ProcessInventoryStrategyResolver processInventoryStrategyResolver;
  @Autowired
  private RestClient restClient;

  @Mock
  private Map<String, String> okapiHeadersMock;
  @Spy
  private Context ctxMock = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
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

    doReturn(succeededFuture(line)).when(inventoryManager).openOrderHandleInstance(any(), anyBoolean(), eq(requestContext));
    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(), eq(requestContext));

    openCompositeOrderInventoryService.processInventory(line, titleId, false, requestContext).result();

    assertEquals(line.getLocations().get(0).getHoldingId(), "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d63");
    verify(processInventoryStrategyResolver, times(1)).getHoldingAndItemStrategy(any());
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  private static class ContextConfiguration {
    @Bean TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean OpenCompositeOrderPieceService openCompositeOrderPieceCreateService() {
      return mock(OpenCompositeOrderPieceService.class);
    }

    @Bean ProcessInventoryPhysicalStrategy processInventoryPhysicalStrategy() {
      return spy(new ProcessInventoryPhysicalStrategy());
    }

    @Bean ProcessInventoryElectronicStrategy processInventoryElectronicStrategy() {
      return spy(new ProcessInventoryElectronicStrategy());
    }

    @Bean ProcessInventoryStrategyResolver processInventoryStrategyResolver(ProcessInventoryPhysicalStrategy processInventoryPhysicalStrategy,
                                                    ProcessInventoryElectronicStrategy processInventoryElectronicStrategy) {
      Map<String, ProcessInventoryStrategy> strategy = new HashMap<>();
      strategy.put(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE.value(), processInventoryElectronicStrategy);
      strategy.put(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE.value(), processInventoryPhysicalStrategy);
      return spy(new ProcessInventoryStrategyResolver(strategy));
    }

    @Bean OpenCompositeOrderInventoryService openCompositeOrderInventoryService(InventoryManager inventoryManager,
                                                                                OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                                                                ProcessInventoryStrategyResolver processInventoryStrategyResolver,
                                                                                RestClient restClient) {
      return spy(new OpenCompositeOrderInventoryService(inventoryManager, openCompositeOrderPieceService, processInventoryStrategyResolver, restClient));
    }
  }
}
