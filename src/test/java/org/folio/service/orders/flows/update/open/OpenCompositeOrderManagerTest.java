package org.folio.service.orders.flows.update.open;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.service.orders.flows.update.open.OpenCompositeOrderManager.DISABLE_INSTANCE_MARCHING_CONFIG_KEY;
import static org.folio.service.orders.flows.update.open.OpenCompositeOrderManager.DISABLE_INSTANCE_MARCHING_CONFIG_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.ApiTestSuite;
import org.folio.CopilotGenerated;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.settings.CommonSettingsRetriever;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

@ExtendWith(MockitoExtension.class)
@ExtendWith(VertxExtension.class)
public class OpenCompositeOrderManagerTest {

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
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(OpenCompositeOrderManagerTest.ContextConfiguration.class);
  }

  @AfterAll
  static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  @Test
  @CopilotGenerated(model = "Claude 3.5 Sonnet")
  void testUpdateIncomingOrderShouldSetOpenedById(VertxTestContext vertxTestContext) {
    // Given
    var userId = UUID.randomUUID().toString();
    var compPO = new CompositePurchaseOrder()
      .withId(UUID.randomUUID().toString())
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    var poFromStorage = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
    var config = new JsonObject().put(DISABLE_INSTANCE_MARCHING_CONFIG_NAME,
      new JsonObject().put(DISABLE_INSTANCE_MARCHING_CONFIG_KEY, true).encode());

    // Mock headers to return specific user ID
    when(okapiHeadersMock.get(anyString())).thenReturn(userId);

    // Setup other required mocks
    when(openCompositeOrderFlowValidator.validate(any(), any(), any()))
      .thenReturn(succeededFuture());

    Map<String, List<Title>> emptyTitles = new HashMap<>();
    when(titlesService.fetchNonPackageTitles(any(), any()))
      .thenReturn(succeededFuture(emptyTitles));

    when(openCompositeOrderInventoryService.processInventory(any(), any(), anyBoolean(), any()))
      .thenReturn(succeededFuture());

    var mockStrategy = mock(EncumbranceWorkflowStrategy.class);
    when(mockStrategy.processEncumbrances(any(), any(), any()))
      .thenReturn(succeededFuture());
    when(encumbranceWorkflowStrategyFactory.getStrategy(any()))
      .thenReturn(mockStrategy);

    // When
    var future = openCompositeOrderManager.process(compPO, poFromStorage, config, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onSuccess(v -> vertxTestContext.verify(() -> {
        assertEquals(OPEN, compPO.getWorkflowStatus());
        assertEquals(userId, compPO.getOpenedById());
        assertNotNull(compPO.getDateOrdered());
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }

  private static class ContextConfiguration {

    @Bean
    public CommonSettingsRetriever configurationEntriesService() {
      return mock(CommonSettingsRetriever.class);
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

    @Bean
    UnOpenCompositeOrderManager unOpenCompositeOrderManager() {
      return mock(UnOpenCompositeOrderManager.class);
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
