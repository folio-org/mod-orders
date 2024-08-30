package org.folio.service.orders.flows.update.open;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.mockito.Mockito.mock;

import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderLinesSummaryPopulateService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.orders.flows.update.unopen.UnOpenCompositeOrderManager;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceStorageService;
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

public class OpenCompositeOrderManagerTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";

  @Autowired
  private OpenCompositeOrderManager openCompositeOrderManager;
  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private ProtectionService protectionService;
  @Autowired
  private InventoryItemManager inventoryItemManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  @Autowired
  private OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  @Autowired
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Autowired
  private OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;

  @Mock
  private Map<String, String> okapiHeadersMock;
  private Context ctx = getFirstContextFromVertx(getVertx());

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
    initSpringContext(OpenCompositeOrderManagerTest.ContextConfiguration.class);
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

    @Bean TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean PieceChangeReceiptStatusPublisher receiptStatusPublisher() {
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

    @Bean EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory() {
      return mock(EncumbranceWorkflowStrategyFactory.class);
    }

    @Bean OpenCompositeOrderInventoryService openCompositeOrderInventoryService() {
      return mock(OpenCompositeOrderInventoryService.class);
    }

    @Bean OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator() {
      return mock(OpenCompositeOrderFlowValidator.class);
    }

    @Bean PurchaseOrderLineHelper purchaseOrderLineHelper() {
      return mock(PurchaseOrderLineHelper.class);
    }

    @Bean  EncumbranceService encumbranceService() {
      return mock(EncumbranceService.class);
    }

    @Bean ExpenseClassValidationService expenseClassValidationService() {
      return mock(ExpenseClassValidationService.class);
    }

    @Bean OrderInvoiceRelationService orderInvoiceRelationService() {
      return mock(OrderInvoiceRelationService.class);
    }

    @Bean AcquisitionsUnitsService acquisitionsUnitsService() {
      return mock(AcquisitionsUnitsService.class);
    }

    @Bean CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService() {
      return mock(OrderLinesSummaryPopulateService.class);
    }

    @Bean RestClient restClient() {
      return mock(RestClient.class);
    }


    @Bean OpenCompositeOrderManager openCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
      TitlesService titlesService, OpenCompositeOrderInventoryService openCompositeOrderInventoryService,
      OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator, UnOpenCompositeOrderManager unOpenCompositeOrderManager) {
      return new OpenCompositeOrderManager(purchaseOrderLineService, encumbranceWorkflowStrategyFactory,
          titlesService, openCompositeOrderInventoryService, openCompositeOrderFlowValidator, unOpenCompositeOrderManager);
    }
  }
}
