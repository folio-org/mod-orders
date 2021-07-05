package org.folio.service.orders.flows.unopen;

import org.folio.ApiTestSuite;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.TagService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.CombinedOrderDataPopulateService;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderLinesSummaryPopulateService;
import org.folio.service.orders.OrderReEncumberService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PiecesService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import io.vertx.core.Context;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

public class UnOpenCompositeOrderManagerTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Autowired
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  private OpenToPendingEncumbranceStrategy openToPendingEncumbranceStrategy;
  @Mock
  private RestClient restClient;

  private Map<String, String> okapiHeadersMock;
  private Context ctxMock;
  private RequestContext requestContext;

  private HttpClientInterface httpClient;
  private static boolean runningOnOwn;

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(UnOpenCompositeOrderManagerTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @BeforeEach
  void beforeEach() {
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    httpClient = HttpClientFactory.getHttpClient(okapiURL, X_OKAPI_TENANT.getValue());
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
    reset(encumbranceWorkflowStrategyFactory);
  }

//  @Test
//  void testShouldSetEncumbrancesToPending() {
//    //given
//    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
//    UnOpenCompositeOrderManager serviceSpy = spy(
//      new UnOpenCompositeOrderManager(piece, "en", orderLineHelper));
//    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
//
//    doReturn(completedFuture(order)).when(serviceSpy).updateAndGetOrderWithLines(any(CompositePurchaseOrder.class));
//    doReturn(openToPendingEncumbranceStrategy).when(encumbranceWorkflowStrategyFactory).getStrategy(eq(OPEN_TO_PENDING));
//    doReturn(completedFuture(null)).when(openToPendingEncumbranceStrategy).processEncumbrances(eq(order), any());
//    doNothing().when(orderLineHelper).closeHttpClient();
//    doReturn(completedFuture(null)).when(purchaseOrderLineService).updateOrderLine(any(), eq(requestContext));
//    //When
//    serviceSpy.unOpenOrder(order, requestContext).join();
//    //Then
//
//    verify(serviceSpy).unOpenOrderUpdatePoLinesSummary(any(), eq(requestContext));
//  }
//
//  @Test
//  void testShouldEndExceptionallyAndCloseConnection() {
//    //given
//    PurchaseOrderLineHelper orderLineHelper = mock(PurchaseOrderLineHelper.class, CALLS_REAL_METHODS);
//
//    UnOpenCompositeOrderManager serviceSpy = spy(new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en", orderLineHelper));
//    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
//
//    doReturn(completedFuture(order)).when(serviceSpy).updateAndGetOrderWithLines(any(CompositePurchaseOrder.class));
//    doReturn(openToPendingEncumbranceStrategy).when(encumbranceWorkflowStrategyFactory).getStrategy(any());
//    doReturn(completedFuture(null)).when(openToPendingEncumbranceStrategy).processEncumbrances(any(), any());
//
//    doNothing().when(orderLineHelper).closeHttpClient();
//    //When
//    CompletableFuture<Void> act = serviceSpy.unOpenOrder(order, requestContext);
//    //Then
//    assertTrue(act.isCompletedExceptionally());
//  }

  /**
   * Define unit test specific beans to override actual ones
   */
  static class ContextConfiguration {

    @Bean
    public EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory() {
      return mock(EncumbranceWorkflowStrategyFactory.class);
    }

    @Bean
    public ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }

    @Bean
    public EncumbranceService encumbranceService() {
      return mock(EncumbranceService.class);
    }

    @Bean
    public OrderReEncumberService orderReEncumberService() {
      return mock(OrderReEncumberService.class);
    }

    @Bean
    public ExpenseClassValidationService expenseClassValidationService() {
      return mock(ExpenseClassValidationService.class);
    }

    @Bean
    public OrderInvoiceRelationService orderInvoiceRelationService() {
      return mock(OrderInvoiceRelationService.class);
    }

    @Bean
    TagService tagService() {
      return mock(TagService.class);
    }

    @Bean
    public RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean
    CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService() {
      return mock(OrderLinesSummaryPopulateService.class);
    }

    @Bean
    CompositeOrderDynamicDataPopulateService combinedPopulateService() {
      return mock(CombinedOrderDataPopulateService.class);
    }

    @Bean
    PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean
    public TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    public AcquisitionsUnitsService acquisitionsUnitsService() {
      return mock(AcquisitionsUnitsService.class);
    }

    @Bean
    public ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean
    public InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean
    public PiecesService piecesService() {
      return mock(PiecesService.class);
    }
  }
}
