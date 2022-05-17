package org.folio.service.orders.flows.update.open;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.strategies.ProcessInventoryStrategyResolver;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

public class OpenCompositeOrderInventoryServiceTest {

  @Autowired
  private OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private  PieceStorageService pieceStorageService;

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

    @Bean OpenCompositeOrderPieceService openCompositeOrderPieceCreateService() {
      return mock(OpenCompositeOrderPieceService.class);
    }

    @Bean ProcessInventoryStrategyResolver resolver() {
      return mock(ProcessInventoryStrategyResolver.class);
    }

    @Bean OpenCompositeOrderInventoryService openCompositeOrderInventoryService(InventoryManager inventoryManager,
                                                                                OpenCompositeOrderPieceService openCompositeOrderPieceService,
                                                                                ProcessInventoryStrategyResolver processInventoryStrategyResolver) {
      return spy(new OpenCompositeOrderInventoryService(inventoryManager, openCompositeOrderPieceService, processInventoryStrategyResolver));
    }
  }
}
