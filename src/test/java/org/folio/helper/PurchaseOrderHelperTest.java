package org.folio.helper;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.CLOSED;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import io.vertx.core.json.JsonObject;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.models.ItemStatus;
import org.folio.rest.acq.model.OrderInvoiceRelationship;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.service.TagService;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.inventory.InventoryItemStatusSyncService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.PoLineValidationService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderValidationService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderManager;
import org.folio.service.orders.flows.update.reopen.ReOpenCompositeOrderManager;
import org.folio.service.orders.flows.update.unopen.UnOpenCompositeOrderManager;
import org.folio.service.ProtectionService;
import org.folio.service.titles.TitlesService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.mockito.stubbing.Answer;
import static org.mockito.ArgumentMatchers.anyBoolean;
import org.folio.helper.PurchaseOrderHelper;
import static org.folio.orders.utils.PermissionsUtil.OKAPI_HEADER_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_REOPEN_PERMISSIONS;
import static org.mockito.Mockito.verifyNoInteractions;
import java.util.HashMap;
import org.folio.rest.core.exceptions.HttpException;
import static org.folio.rest.core.exceptions.ErrorCodes.COMPOSITE_ORDER_MISSING_PO_LINES;
import static org.mockito.Mockito.doThrow;
import static org.folio.rest.core.exceptions.ErrorCodes.PROHIBITED_FIELD_CHANGING;
import static org.folio.rest.core.exceptions.ErrorCodes.APPROVAL_REQUIRED_TO_OPEN;
import org.folio.rest.jaxrs.model.Error;

@ExtendWith(VertxExtension.class)
public class PurchaseOrderHelperTest {

  public static final String BASE_MOCK_DATA_PATH = "mockdata/";
  private static final String LISTED_PRINT_SERIAL_PATH = "po_listed_print_serial.json";
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  private AutoCloseable mockitoMocks;
  @InjectMocks
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
  TagService tagService;
  @Mock
  PurchaseOrderLineService purchaseOrderLineService;
  @Mock
  InventoryItemStatusSyncService itemStatusSyncService;
  @Mock
  OpenCompositeOrderManager openCompositeOrderManager;
  @Mock
  PurchaseOrderStorageService purchaseOrderStorageService;
  @Mock
  CommonSettingsCache commonSettingsCache;
  @Mock
  OrderValidationService orderValidationService;
  @Mock
  PoLineValidationService poLineValidationService;
  @Mock
  ReOpenCompositeOrderManager reOpenCompositeOrderManager;
  @Mock
  UnOpenCompositeOrderManager unOpenCompositeOrderManager;
  @Mock
  ProtectionService protectionService;
  @Mock
  TitlesService titlesService;
  @Mock
  EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;

  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void resetMocks() throws Exception {
    mockitoMocks.close();
  }

  @Test
  @DisplayName("Test POST open composite order")
  void testPostOpenCompositeOrder() throws IOException {
    // Given
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder compPO = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(compPO);
    compPO.setWorkflowStatus(OPEN);
    compPO.setId(UUID.randomUUID().toString());

    JsonObject tenantConfig = new JsonObject();
    doReturn(succeededFuture(tenantConfig))
      .when(commonSettingsCache).loadSettings(eq(requestContext));
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
    doAnswer((Answer<Future<PoLine>>) invocation -> {
      PoLine poLine = invocation.getArgument(0);
      return succeededFuture(poLine);
    }).when(purchaseOrderLineHelper).createPoLineWithOrder(any(PoLine.class), any(CompositePurchaseOrder.class),
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

    // When
    Future<CompositePurchaseOrder> future = purchaseOrderHelper.postCompositeOrder(compPO, requestContext);

    // Then
    assertTrue(future.succeeded());
  }

  @Test
  @DisplayName("Test PUT pending composite order (no change)")
  void testPutPendingCompositeOrder() throws IOException {
    // Given
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder compPO = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(compPO);
    compPO.setId(UUID.randomUUID().toString());
    compPO.getPoLines().forEach(line -> line.withId(UUID.randomUUID().toString()));
    CompositePurchaseOrder poFromStorage = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
    poFromStorage.setPoLines(List.of(getMinimalContentCompositePoLine(order.getString("id"))));

    boolean deleteHoldings = false;

    doReturn(succeededFuture(List.of()))
      .when(orderValidationService).validateOrderForPut(eq(compPO.getId()), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(JsonObject.mapFrom(poFromStorage)))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(compPO.getId()), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).validateOrderForUpdate(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderLineHelper).updatePoLines(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderStorageService).saveOrder(any(PurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(encumbranceService).updateEncumbrancesOrderStatusAndReleaseIfClosed(any(CompositePurchaseOrder.class), eq(requestContext));
    doNothing()
      .when(poLineValidationService).checkPurchaseOrderHasPoLines(any());
    doReturn(succeededFuture(null))
      .when(poLineValidationService).validateUserUnaffiliatedLocations(anyString(), any(), eq(requestContext));

    // When
    Future<Void> future = purchaseOrderHelper.putCompositeOrderById(compPO.getId(), deleteHoldings, compPO, requestContext);

    // Then
    assertTrue(future.succeeded());
    // dateOrdered should remain null for pending order
    assertNull(compPO.getDateOrdered());
  }

  @Test
  @DisplayName("Test PUT pending composite order (no change)")
  void testPutUnOpenOrderValidationThrow() throws IOException {
    var order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder compPO = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(compPO);
    compPO.setId(UUID.randomUUID().toString());
    compPO.getPoLines().forEach(line -> line.withId(UUID.randomUUID().toString()));
    CompositePurchaseOrder poFromStorage = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
    poFromStorage.setPoLines(List.of(getMinimalContentCompositePoLine(order.getString("id"))));

    boolean deleteHoldings = false;

    doReturn(succeededFuture(List.of()))
      .when(orderValidationService).validateOrderForPut(eq(compPO.getId()), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(JsonObject.mapFrom(poFromStorage)))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(compPO.getId()), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(failedFuture("error"))
      .when(poLineValidationService).validateUserUnaffiliatedLocations(anyString(), any(), eq(requestContext));

    // When
    Future<Void> future = purchaseOrderHelper.putCompositeOrderById(compPO.getId(), deleteHoldings, compPO, requestContext);

    // Then
    assertTrue(future.failed());

    verify(orderValidationService, times(0)).validateOrderForUpdate(any(), any(), any());
  }

  @Test
  @DisplayName("Test source validation in line")
  void testSourceValidationInLine() {
    // Note: RMB schema validation is not reliable in unit tests with MockServer (it does not always return the same code),
    // but we can check the same validation using a Validator.
    CompositePurchaseOrder compPO = getMinimalContentCompositePurchaseOrder();
    PoLine poLine = getMinimalContentCompositePoLine();
    poLine.setSource(null);
    compPO.getPoLines().add(poLine);
    try (ValidatorFactory factory = Validation.buildDefaultValidatorFactory()) {
      Validator schemaValidator = factory.getValidator();
      Set<ConstraintViolation<CompositePurchaseOrder>> violations = schemaValidator.validate(compPO);
      assertThat(violations, hasSize(1));
      assertEquals("poLines[0].source", violations.iterator().next().getPropertyPath().toString());
    }
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

  @Test
  @DisplayName("Test PUT order transition to OPEN sets dateOrdered")
  void testPutOrderTransitionToOpenSetsDateOrdered() throws IOException {
    // Given
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder compPO = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(compPO);
    compPO.setId(UUID.randomUUID().toString());
    compPO.setWorkflowStatus(OPEN);
    compPO.getPoLines().forEach(line -> line.withId(UUID.randomUUID().toString()));

    CompositePurchaseOrder poFromStorage = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
    poFromStorage.setWorkflowStatus(PENDING);
    poFromStorage.setDateOrdered(null);
    poFromStorage.setPoLines(compPO.getPoLines());

    boolean deleteHoldings = false;
    JsonObject tenantConfig = new JsonObject();

    doReturn(succeededFuture(List.of()))
      .when(orderValidationService).validateOrderForPut(eq(compPO.getId()), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(JsonObject.mapFrom(poFromStorage)))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(compPO.getId()), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).validateOrderForUpdate(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doNothing()
      .when(poLineValidationService).checkPurchaseOrderHasPoLines(any());
    doReturn(succeededFuture(null))
      .when(poLineValidationService).validateUserUnaffiliatedLocations(anyString(), any(), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderLineHelper).updatePoLines(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).checkOrderApprovalRequired(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(tenantConfig))
      .when(commonSettingsCache).loadSettings(eq(requestContext));
    // Mock OpenCompositeOrderManager to set dateOrdered (mimicking what the real implementation does)
    doAnswer((Answer<Future<Void>>) invocation -> {
      CompositePurchaseOrder po = invocation.getArgument(0);
      po.setDateOrdered(new Date());
      return succeededFuture(null);
    }).when(openCompositeOrderManager).process(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(tenantConfig),
        eq(requestContext));
    doReturn(succeededFuture(compPO.getPoLines()))
      .when(purchaseOrderLineService).getPoLinesByOrderId(eq(compPO.getId()), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(itemStatusSyncService).updateItemStatusesInInventory(anyList(), any(ItemStatus.class), any(ItemStatus.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderStorageService).saveOrder(any(PurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(encumbranceService).updateEncumbrancesOrderStatusAndReleaseIfClosed(any(CompositePurchaseOrder.class), eq(requestContext));

    // When
    Future<Void> future = purchaseOrderHelper.putCompositeOrderById(compPO.getId(), deleteHoldings, compPO, requestContext);

    // Then
    assertTrue(future.succeeded());
    // dateOrdered should be set when opening the order
    assertNotNull(compPO.getDateOrdered(), "dateOrdered should be set when transitioning to OPEN");
  }

  @Test
  @DisplayName("Test PUT order transition to CLOSED preserves dateOrdered")
  void testPutOrderTransitionToClosedPreservesDateOrdered() throws IOException {
    // Given
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder compPO = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(compPO);
    compPO.setId(UUID.randomUUID().toString());
    compPO.setWorkflowStatus(CLOSED);
    compPO.setCloseReason(new org.folio.rest.jaxrs.model.CloseReason().withReason("Complete"));
    compPO.getPoLines().forEach(line -> line.withId(UUID.randomUUID().toString()));

    Date originalDateOrdered = new Date();
    CompositePurchaseOrder poFromStorage = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
    poFromStorage.setWorkflowStatus(OPEN);
    poFromStorage.setDateOrdered(originalDateOrdered);
    poFromStorage.setPoLines(compPO.getPoLines());

    boolean deleteHoldings = false;

    // Mock EncumbranceWorkflowStrategy for closing
    EncumbranceWorkflowStrategy mockStrategy = mock(EncumbranceWorkflowStrategy.class);
    doReturn(succeededFuture(null))
      .when(mockStrategy).processEncumbrances(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(mockStrategy)
      .when(encumbranceWorkflowStrategyFactory).getStrategy(any());

    doReturn(succeededFuture(List.of()))
      .when(orderValidationService).validateOrderForPut(eq(compPO.getId()), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(JsonObject.mapFrom(poFromStorage)))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(compPO.getId()), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).validateOrderForUpdate(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doReturn(succeededFuture(null))
      .when(poLineValidationService).validateUserUnaffiliatedLocations(anyString(), any(), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderLineHelper).updatePoLines(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doReturn(succeededFuture(compPO.getPoLines()))
      .when(purchaseOrderLineService).getPoLinesByOrderId(eq(compPO.getId()), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(itemStatusSyncService).updateItemStatusesInInventory(anyList(), any(ItemStatus.class), any(ItemStatus.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderStorageService).saveOrder(any(PurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(encumbranceService).updateEncumbrancesOrderStatusAndReleaseIfClosed(any(CompositePurchaseOrder.class), eq(requestContext));

    // When
    Future<Void> future = purchaseOrderHelper.putCompositeOrderById(compPO.getId(), deleteHoldings, compPO, requestContext);

    // Then
    assertTrue(future.succeeded());
    // dateOrdered should be preserved from storage when closing the order
    assertEquals(originalDateOrdered, compPO.getDateOrdered(), "dateOrdered should be preserved when transitioning to CLOSED");
  }

  @Test
  @DisplayName("Test PUT order transition to REOPEN preserves original dateOrdered")
  void testPutOrderTransitionToReopenPreservesDateOrdered() throws IOException {
    // Given
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder compPO = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(compPO);
    compPO.setId(UUID.randomUUID().toString());
    compPO.setWorkflowStatus(OPEN);
    compPO.getPoLines().forEach(line -> line.withId(UUID.randomUUID().toString()));

    Date originalDateOrdered = new Date();
    CompositePurchaseOrder poFromStorage = JsonObject.mapFrom(compPO).mapTo(CompositePurchaseOrder.class);
    poFromStorage.setWorkflowStatus(CLOSED);
    poFromStorage.setDateOrdered(originalDateOrdered);
    poFromStorage.setPoLines(compPO.getPoLines());

    boolean deleteHoldings = false;

    // Mock request context headers to include reopen permissions
    Map<String, String> headers = new java.util.HashMap<>();
    headers.put("X-Okapi-Permissions", "[\"orders.item.reopen\"]");
    doReturn(headers).when(requestContext).getHeaders();

    // Mock EncumbranceWorkflowStrategy for reopening
    EncumbranceWorkflowStrategy mockStrategy = mock(EncumbranceWorkflowStrategy.class);
    doReturn(succeededFuture(null))
      .when(mockStrategy).processEncumbrances(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(mockStrategy)
      .when(encumbranceWorkflowStrategyFactory).getStrategy(any());

    doReturn(succeededFuture(List.of()))
      .when(orderValidationService).validateOrderForPut(eq(compPO.getId()), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(JsonObject.mapFrom(poFromStorage)))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(compPO.getId()), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).validateOrderForUpdate(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doReturn(succeededFuture(null))
      .when(poLineValidationService).validateUserUnaffiliatedLocations(anyString(), any(), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(reOpenCompositeOrderManager).process(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderLineHelper).updatePoLines(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class),
        eq(requestContext));
    doReturn(succeededFuture(compPO.getPoLines()))
      .when(purchaseOrderLineService).getPoLinesByOrderId(eq(compPO.getId()), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(itemStatusSyncService).updateItemStatusesInInventory(anyList(), any(ItemStatus.class), any(ItemStatus.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderStorageService).saveOrder(any(PurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(encumbranceService).updateEncumbrancesOrderStatusAndReleaseIfClosed(any(CompositePurchaseOrder.class), eq(requestContext));

    // When
    Future<Void> future = purchaseOrderHelper.putCompositeOrderById(compPO.getId(), deleteHoldings, compPO, requestContext);

    // Then
    assertTrue(future.succeeded());
    // dateOrdered should be preserved from storage when reopening the order
    assertEquals(originalDateOrdered, compPO.getDateOrdered(), "dateOrdered should be preserved when reopening order from CLOSED to OPEN");
  }

    @Test
  @DisplayName("Test updateOrderWithValidation should preserve dateOrdered from storage when request has null")
  void testUpdateOrderWithValidationShouldPreserveDateOrderedFromStorage(VertxTestContext vertxTestContext) {
    // TestMate-5dd4e0ac7bb62b20ae308549c94109ca
    // Given
    String orderId = UUID.randomUUID().toString();
    Date originalDateOrdered = new Date(1705314600000L); // 2024-01-15T10:30:00Z
    CompositePurchaseOrder compPO = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(OPEN)
      .withDateOrdered(null);
    CompositePurchaseOrder poFromStorage = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(OPEN)
      .withDateOrdered(originalDateOrdered);
    JsonObject storageOrderJson = JsonObject.mapFrom(poFromStorage);
    doReturn(succeededFuture(storageOrderJson))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(poLineValidationService).validateUserUnaffiliatedLocations(anyString(), anyList(), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).validateOrderForUpdate(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderLineHelper).updatePoLines(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(purchaseOrderStorageService).saveOrder(any(PurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(encumbranceService).updateEncumbrancesOrderStatusAndReleaseIfClosed(any(CompositePurchaseOrder.class), eq(requestContext));
    // Mock getPoLinesByOrderId which is called inside handleFinalOrderStatus when compPO.getPoLines() is empty
    doReturn(succeededFuture(Collections.emptyList()))
      .when(purchaseOrderLineService).getPoLinesByOrderId(eq(orderId), eq(requestContext));
    // When
    Future<Void> future = purchaseOrderHelper.updateOrderWithValidation(compPO, false, requestContext);
    // Then
    vertxTestContext.assertComplete(future)
      .onSuccess(v -> vertxTestContext.verify(() -> {
        assertNotNull(compPO.getDateOrdered());
        assertEquals(originalDateOrdered, compPO.getDateOrdered());
        vertxTestContext.completeNow();
      }))
      .onFailure(vertxTestContext::failNow);
  }

    @Test
  @DisplayName("Test updateOrderWithValidation when reopening without permission should throw exception")
  void testUpdateOrderWithValidationWhenReopeningWithoutPermissionShouldThrowException(VertxTestContext vertxTestContext) {
    // TestMate-bf0b124dd4ae2b1cc32b0e26960b1ea3
    // Given
    String orderId = "550e8400-e29b-41d4-a716-446655440000";
    CompositePurchaseOrder requestOrder = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(OPEN);
    CompositePurchaseOrder poFromStorage = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(CLOSED);
    JsonObject storageOrderJson = JsonObject.mapFrom(poFromStorage);
    Map<String, String> headers = new HashMap<>();
    headers.put(OKAPI_HEADER_PERMISSIONS, "[\"orders.item.approve\"]");
    doReturn(headers).when(requestContext).getHeaders();
    doReturn(succeededFuture(storageOrderJson))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    // When
    Future<Void> future = purchaseOrderHelper.updateOrderWithValidation(requestOrder, false, requestContext);
    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(403, exception.getCode());
        assertEquals(USER_HAS_NO_REOPEN_PERMISSIONS.getCode(), exception.getError().getCode());
        verify(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
        verifyNoInteractions(orderValidationService);
        verifyNoInteractions(openCompositeOrderManager);
        vertxTestContext.completeNow();
      });
  }

    @Test
  @DisplayName("Test updateOrderWithValidation when opening order without PO Lines should throw exception")
  void testUpdateOrderWithValidationWhenOpeningWithoutPoLinesShouldThrowException(VertxTestContext vertxTestContext) {
    // TestMate-071766ace1b9b005bc19b30674e608d5
    // Given
    String orderId = UUID.randomUUID().toString();
    CompositePurchaseOrder requestOrder = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(OPEN)
      .withPoLines(Collections.emptyList());
    CompositePurchaseOrder poFromStorage = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(PENDING)
      .withPoLines(Collections.emptyList());
    JsonObject storageOrderJson = JsonObject.mapFrom(poFromStorage);
    doReturn(succeededFuture(storageOrderJson))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doThrow(new HttpException(422, COMPOSITE_ORDER_MISSING_PO_LINES))
      .when(poLineValidationService).checkPurchaseOrderHasPoLines(anyList());
    // When
    Future<Void> future = purchaseOrderHelper.updateOrderWithValidation(requestOrder, false, requestContext);
    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        assertEquals(COMPOSITE_ORDER_MISSING_PO_LINES.getCode(), exception.getError().getCode());
        verify(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
        verify(poLineValidationService).checkPurchaseOrderHasPoLines(anyList());
        verifyNoInteractions(orderValidationService);
        vertxTestContext.completeNow();
      });
  }

    @Test
  @DisplayName("Test updateOrderWithValidation when protected fields changed on non-pending order should throw exception")
  void testUpdateOrderWithValidationWhenProtectedFieldsChangedOnNonPendingOrderShouldThrowException(VertxTestContext vertxTestContext) {
    // TestMate-5444b42d34ec4c7dc9dcb43680387ef7
    // Given
    String orderId = "550e8400-e29b-41d4-a716-446655440000";
    CompositePurchaseOrder requestOrder = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN)
      .withPoNumber("NEW-PO-NUMBER");
    JsonObject storageOrderJson = new JsonObject()
      .put("id", orderId)
      .put("workflowStatus", "Open")
      .put("poNumber", "OLD-PO-NUMBER")
      .put("nextPolNumber", 1);
    doReturn(succeededFuture(storageOrderJson))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
    // When
    Future<Void> future = purchaseOrderHelper.updateOrderWithValidation(requestOrder, false, requestContext);
    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(400, exception.getCode());
        assertEquals(PROHIBITED_FIELD_CHANGING.getCode(), exception.getError().getCode());
        verify(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
        verifyNoInteractions(purchaseOrderLineService);
        verifyNoInteractions(openCompositeOrderManager);
        vertxTestContext.completeNow();
      });
  }

    @Test
  @DisplayName("Test updateOrderWithValidation when opening and approval required but not approved should throw exception")
  void testUpdateOrderWithValidationWhenOpeningAndApprovalRequiredButNotApprovedShouldThrowException(VertxTestContext vertxTestContext) {
    // TestMate-be0f677cabad69266ce5b9254e0a7424
    // Given
    String orderId = "550e8400-e29b-41d4-a716-446655440000";
    CompositePurchaseOrder requestOrder = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(OPEN)
      .withApproved(false);
    CompositePurchaseOrder poFromStorage = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(PENDING)
      .withApproved(false);
    JsonObject storageOrderJson = JsonObject.mapFrom(poFromStorage);
    storageOrderJson.put("workflowStatus", PENDING.value());
    doReturn(succeededFuture(storageOrderJson))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(poFromStorage))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(poLineValidationService).validateUserUnaffiliatedLocations(anyString(), anyList(), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(orderValidationService).validateOrderForUpdate(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(failedFuture(new HttpException(400, APPROVAL_REQUIRED_TO_OPEN)))
      .when(orderValidationService).checkOrderApprovalRequired(any(CompositePurchaseOrder.class), eq(requestContext));
    // When
    Future<Void> future = purchaseOrderHelper.updateOrderWithValidation(requestOrder, false, requestContext);
    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(400, exception.getCode());
        assertEquals(APPROVAL_REQUIRED_TO_OPEN.getCode(), exception.getError().getCode());
        verify(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
        verify(orderValidationService).checkOrderApprovalRequired(any(CompositePurchaseOrder.class), eq(requestContext));
        vertxTestContext.completeNow();
      });
  }

    @Test
  @DisplayName("Test updateOrderWithValidation when general update validation fails should throw exception")
  void testUpdateOrderWithValidationWhenGeneralUpdateValidationFailsShouldThrowException(VertxTestContext vertxTestContext) {
    // TestMate-240e65657069105af1c71dd8aff18141
    // Given
    String orderId = UUID.randomUUID().toString();
    CompositePurchaseOrder requestOrder = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(PENDING);
    CompositePurchaseOrder storageOrder = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(PENDING);
    JsonObject storageOrderJson = JsonObject.mapFrom(storageOrder);
    Error validationError = new Error().withCode("validationError").withMessage("Validation Error");
    HttpException validationException = new HttpException(422, validationError);
    doReturn(succeededFuture(storageOrderJson))
      .when(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(storageOrder))
      .when(purchaseOrderLineService).populateOrderLines(any(CompositePurchaseOrder.class), eq(requestContext));
    doReturn(succeededFuture(null))
      .when(poLineValidationService).validateUserUnaffiliatedLocations(anyString(), anyList(), eq(requestContext));
    doReturn(failedFuture(validationException))
      .when(orderValidationService).validateOrderForUpdate(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
    // When
    Future<Void> future = purchaseOrderHelper.updateOrderWithValidation(requestOrder, false, requestContext);
    // Then
    vertxTestContext.assertFailure(future)
      .onComplete(result -> {
        assertTrue(result.failed());
        HttpException exception = (HttpException) result.cause();
        assertEquals(422, exception.getCode());
        assertEquals("validationError", exception.getError().getCode());
        verify(purchaseOrderStorageService).getPurchaseOrderByIdAsJson(eq(orderId), eq(requestContext));
        verify(orderValidationService).validateOrderForUpdate(any(CompositePurchaseOrder.class), any(CompositePurchaseOrder.class), eq(requestContext));
        verifyNoInteractions(openCompositeOrderManager);
        verifyNoInteractions(reOpenCompositeOrderManager);
        vertxTestContext.completeNow();
      });
  }

  private void prepareOrderForPostRequest(CompositePurchaseOrder reqData) {
    reqData.setDateOrdered(null);
    removeAllEncumbranceLinks(reqData);
  }

  private void removeAllEncumbranceLinks(CompositePurchaseOrder reqData) {
    reqData.getPoLines().forEach(poLine ->
      poLine.getFundDistribution().forEach(fundDistribution -> fundDistribution.setEncumbrance(null))
    );
  }
}
