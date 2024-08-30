package org.folio.service.pieces;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.ProtectionService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

public class PieceServiceTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String PIECE_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private PieceService pieceService;
  @Autowired
  private ProtectionService protectionService;
  @Autowired
  private InventoryItemManager inventoryItemManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  @Autowired
  private PieceUpdateInventoryService pieceUpdateInventoryService;
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
    initSpringContext(PieceServiceTest.ContextConfiguration.class);
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

    @Bean
    public ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }

    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean
    PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean
    InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean
    RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean
    PieceChangeReceiptStatusPublisher receiptStatusPublisher() {
      return mock(PieceChangeReceiptStatusPublisher.class);
    }

    @Bean ReceivingEncumbranceStrategy receivingEncumbranceStrategy() {
      return mock(ReceivingEncumbranceStrategy.class);
    }

    @Bean PurchaseOrderStorageService purchaseOrderService() {
      return mock(PurchaseOrderStorageService.class);
    }

    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean PieceUpdateInventoryService pieceUpdateInventoryService() {
      return mock(PieceUpdateInventoryService.class);
    }

    @Bean PieceService piecesService(PieceChangeReceiptStatusPublisher receiptStatusPublisher) {
      return spy(new PieceService(receiptStatusPublisher));
    }
  }
}
