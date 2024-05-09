package org.folio.helper;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockData;
import static org.folio.orders.utils.HelperUtils.ORDER_CONFIG_MODULE_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.models.ItemStatus;
import org.folio.rest.acq.model.OrderInvoiceRelationship;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.ProtectionService;
import org.folio.service.TagService;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.inventory.InventoryItemStatusSyncService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderValidationService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderFlowValidator;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderManager;
import org.folio.service.orders.flows.update.reopen.ReOpenCompositeOrderManager;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.mockito.stubbing.Answer;

@ExtendWith(VertxExtension.class)
public class PurchaseOrderHelperTest {
  private static final Logger logger = LogManager.getLogger();
  public static final String BASE_MOCK_DATA_PATH = "mockdata/";
  private static final String LISTED_PRINT_SERIAL_PATH = "po_listed_print_serial.json";

  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  private AutoCloseable mockitoMocks;
  private PurchaseOrderHelper purchaseOrderHelper;
  @Mock
  private RestClient restClient;
  @Mock
  private RequestContext requestContext;
  @Mock
  PurchaseOrderLineHelper purchaseOrderLineHelper;
  @Mock
  CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService;
  @Mock
  EncumbranceService encumbranceService;
  @Mock
  CompositeOrderDynamicDataPopulateService combinedPopulateService;
  @Mock
  EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Mock
  OrderInvoiceRelationService orderInvoiceRelationService;
  @Mock
  TagService tagService;
  @Mock
  PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  TitlesService titlesService;
  @Mock
  ProtectionService protectionService;
  @Mock
  InventoryItemStatusSyncService itemStatusSyncService;
  @Mock
  OpenCompositeOrderManager openCompositeOrderManager;
  @Mock
  PurchaseOrderStorageService purchaseOrderStorageService;
  @Mock
  ConfigurationEntriesCache configurationEntriesCache;
  @Mock
  PoNumberHelper poNumberHelper;
  @Mock
  OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;
  @Mock
  ReOpenCompositeOrderManager reOpenCompositeOrderManager;
  @Mock
  OrderValidationService orderValidationService;

  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
    purchaseOrderHelper = createPurchaseOrderHelper();
  }

  private PurchaseOrderHelper createPurchaseOrderHelper() {
    return new PurchaseOrderHelper(purchaseOrderLineHelper, orderLinesSummaryPopulateService,
      encumbranceService, combinedPopulateService, encumbranceWorkflowStrategyFactory, orderInvoiceRelationService,
      tagService, purchaseOrderLineService, titlesService, protectionService, itemStatusSyncService,
      openCompositeOrderManager, purchaseOrderStorageService, configurationEntriesCache, poNumberHelper,
      openCompositeOrderFlowValidator, reOpenCompositeOrderManager, orderValidationService);
  }

  @AfterEach
  void resetMocks() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void testPostOpenCompositeOrder() throws Exception {
    logger.info("=== Test post open composite order ===");
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder compositePo = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(compositePo);
    compositePo.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    compositePo.setId(UUID.randomUUID().toString());

    JsonObject tenantConfig = new JsonObject();
    doReturn(succeededFuture(tenantConfig))
      .when(configurationEntriesCache).loadConfiguration(eq(ORDER_CONFIG_MODULE_NAME), eq(requestContext));
    doReturn(succeededFuture(List.of()))
      .when(orderValidationService).validateOrderForPost(any(CompositePurchaseOrder.class), eq(tenantConfig), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).validateOrderForCreation(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(tagService).createTagsIfMissing(any(), eq(requestContext));
    doAnswer((Answer<Future<PurchaseOrder>>) invocation -> {
      PurchaseOrder po = invocation.getArgument(0);
      return succeededFuture(po);
    }).when(purchaseOrderStorageService).createPurchaseOrder(any(PurchaseOrder.class), eq(requestContext));
    doAnswer((Answer<Future<CompositePoLine>>) invocation -> {
      CompositePoLine poLine = invocation.getArgument(0);
      return succeededFuture(poLine);
    }).when(purchaseOrderLineHelper).createPoLine(any(CompositePoLine.class), any(CompositePurchaseOrder.class),
      eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).checkOrderApprovalRequired(any(CompositePurchaseOrder.class), eq(requestContext));
    doAnswer((Answer<Future<CompositePurchaseOrder>>) invocation -> {
      CompositePurchaseOrder po = invocation.getArgument(0);
      return succeededFuture(po);
    }).when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(openCompositeOrderManager).process(any(CompositePurchaseOrder.class), eq(null), eq(tenantConfig),
        eq(requestContext));
    doReturn(succeededFuture(null))
      .when(itemStatusSyncService).updateItemStatusesInInventory(anyList(), any(ItemStatus.class), any(ItemStatus.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderStorageService).saveOrder(any(PurchaseOrder.class), eq(requestContext));
    doAnswer((Answer<Future<CompositeOrderRetrieveHolder>>) invocation -> {
      CompositeOrderRetrieveHolder holder = invocation.getArgument(0);
      return succeededFuture(holder);
    }).when(orderLinesSummaryPopulateService).populate(any(CompositeOrderRetrieveHolder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(encumbranceService).updateEncumbrancesOrderStatusAndReleaseIfClosed(any(CompositePurchaseOrder.class),
        eq(requestContext));

    Future<CompositePurchaseOrder> future = purchaseOrderHelper.postCompositeOrder(compositePo, requestContext);
    assertTrue(future.succeeded());
  }

  @Test
  void testDeleteOrderLinkedToInvoiceWithError(VertxTestContext vertxTestContext) {
    // given
    InvoiceLineService invoiceLineService = new InvoiceLineService(restClient);
    RestClient restClient = mock(RestClient.class, CALLS_REAL_METHODS);
    OrderInvoiceRelationService orderInvoiceRelationService = new OrderInvoiceRelationService(restClient, invoiceLineService);

    // for returning non empty collection
    OrderInvoiceRelationshipCollection oirCollection = new OrderInvoiceRelationshipCollection()
            .withOrderInvoiceRelationships(Collections.singletonList(new OrderInvoiceRelationship()))
            .withTotalRecords(1);

    doReturn(succeededFuture(oirCollection)).when(restClient).get(any(RequestEntry.class), any(), eq(requestContext));

    Future<Void> future = orderInvoiceRelationService.checkOrderInvoiceRelationship(ORDER_ID, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertEquals(ErrorCodes.ORDER_RELATES_TO_INVOICE.getDescription(), result.cause().getMessage());
        vertxTestContext.completeNow();
      });
  }

  private void prepareOrderForPostRequest(CompositePurchaseOrder reqData) {
    reqData.setDateOrdered(null);
    removeAllEncumbranceLinks(reqData);
  }

  private void removeAllEncumbranceLinks(CompositePurchaseOrder reqData) {
    reqData.getCompositePoLines().forEach(poLine ->
      poLine.getFundDistribution().forEach(fundDistribution -> fundDistribution.setEncumbrance(null))
    );
  }

}
