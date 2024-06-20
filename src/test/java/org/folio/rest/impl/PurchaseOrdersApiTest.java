package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static java.util.stream.Collectors.toList;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.containsAny;
import static org.folio.RestTestUtils.checkPreventProtectedFieldsModificationRule;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.RestTestUtils.verifySuccessGet;
import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.*;
import static org.folio.TestUtils.getInstanceId;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.TestUtils.validatePoLineCreationErrorForNonPendingOrder;
import static org.folio.helper.InventoryInteractionTestHelper.joinExistingAndNewItems;
import static org.folio.helper.InventoryInteractionTestHelper.verifyHoldingsCreated;
import static org.folio.helper.InventoryInteractionTestHelper.verifyInstanceLinksForUpdatedOrder;
import static org.folio.helper.InventoryInteractionTestHelper.verifyInventoryInteraction;
import static org.folio.helper.InventoryInteractionTestHelper.verifyInventoryNonInteraction;
import static org.folio.helper.InventoryInteractionTestHelper.verifyItemsCreated;
import static org.folio.helper.InventoryInteractionTestHelper.verifyOpenOrderPiecesCreated;
import static org.folio.helper.InventoryInteractionTestHelper.verifyPiecesCreated;
import static org.folio.helper.InventoryInteractionTestHelper.verifyPiecesQuantityForSuccessCase;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.calculateTotalQuantity;
import static org.folio.orders.utils.PermissionsUtil.OKAPI_HEADER_PERMISSIONS;
import static org.folio.orders.utils.ResourcePathResolver.*;
import static org.folio.rest.RestConstants.ERROR_CAUSE;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_EXPENSE_CLASS_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.CLAIMING_CONFIG_INVALID;
import static org.folio.rest.core.exceptions.ErrorCodes.INACTIVE_EXPENSE_CLASS;
import static org.folio.rest.core.exceptions.ErrorCodes.INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE;
import static org.folio.rest.core.exceptions.ErrorCodes.ISBN_NOT_VALID;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.core.exceptions.ErrorCodes.MISSING_MATERIAL_TYPE;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_CLOSED;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_OPEN;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_VENDOR_IS_INACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_VENDOR_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.ORGANIZATION_NOT_A_VENDOR;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECES_TO_BE_DELETED;
import static org.folio.rest.core.exceptions.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.POL_LINES_LIMIT_EXCEEDED;
import static org.folio.rest.core.exceptions.ErrorCodes.VENDOR_ISSUE;
import static org.folio.rest.impl.MockServer.BUDGET_IS_INACTIVE_TENANT;
import static org.folio.rest.impl.MockServer.BUDGET_NOT_FOUND_FOR_TRANSACTION_TENANT;
import static org.folio.rest.impl.MockServer.FUND_CANNOT_BE_PAID_TENANT;
import static org.folio.rest.impl.MockServer.ITEM_RECORDS;
import static org.folio.rest.impl.MockServer.LEDGER_NOT_FOUND_FOR_TRANSACTION_TENANT;
import static org.folio.rest.impl.MockServer.PO_LINES_EMPTY_COLLECTION_ID;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.getContributorNameTypesSearches;
import static org.folio.rest.impl.MockServer.getCreatedHoldings;
import static org.folio.rest.impl.MockServer.getCreatedInstances;
import static org.folio.rest.impl.MockServer.getCreatedItems;
import static org.folio.rest.impl.MockServer.getCreatedPieces;
import static org.folio.rest.impl.MockServer.getHoldingsSearches;
import static org.folio.rest.impl.MockServer.getInstanceStatusesSearches;
import static org.folio.rest.impl.MockServer.getInstanceTypesSearches;
import static org.folio.rest.impl.MockServer.getInstancesSearches;
import static org.folio.rest.impl.MockServer.getItemUpdates;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getLoanTypesSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPurchaseOrderUpdates;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.folio.rest.jaxrs.model.Piece.Format.OTHER;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.endsWith;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.TestUtils;
import org.folio.config.ApplicationConfig;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineFieldNames;
import org.folio.orders.utils.POProtectedFields;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.acq.model.finance.Metadata;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembershipCollection;
import org.folio.rest.jaxrs.model.CloseReason;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat;
import org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Physical.CreateInventory;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.TransactionService;
import org.hamcrest.beans.HasPropertyWithValue;
import org.hamcrest.core.Every;
import org.hamcrest.core.Is;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class PurchaseOrdersApiTest {
  private static final Logger logger = LogManager.getLogger();

  private static final String PENDING_ORDER_APPROVED_FALSE = "e5ae4afd-3fa9-494e-a972-f541df9b877e";
  private static final String ORDER_WITHOUT_PO_LINES = "order_without_po_lines.json";
  private static final String ORDER_WITHOUT_VENDOR_ID = "order_without_vendor_id.json";
  public static final String ORDER_WITH_PO_LINES_JSON = "put_order_with_po_lines.json";
  private static final String ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON = "put_order_with_mismatch_id_in_po_lines.json";
  private static final String ORDER_WITHOUT_MATERIAL_TYPE_JSON = "order_po_line_without_material_type.json";
  private static final String ORDER_WITH_NOT_VALID_CLAIMING_CONFIG = "order_with_invalid_claiming_config.json";

  static final String ACTIVE_VENDOR_ID = "d0fb5aa0-cdf1-11e8-a8d5-f2801f1b9fd1";
  static final String INACTIVE_VENDOR_ID = "b1ef7e96-98f3-4f0d-9820-98c322c989d2";
  static final String INACTIVE_EXPENSE_CLASS_ID = "2fa1d78f-1f7d-4659-854b-9d03cd06f21c";
  static final String NON_EXIST_VENDOR_ID = "bba87500-6e71-4057-a2a9-a091bac7e0c1";
  static final String MOD_VENDOR_INTERNAL_ERROR_ID = "bba81500-6e41-4057-a2a9-a081bac7e0c1";
  static final String VENDOR_WITH_BAD_CONTENT = "5a34ae0e-5a11-4337-be95-1a20cfdc3161";
  static final String ORGANIZATION_NOT_VENDOR = "52a7669b-0e6d-4513-92be-14086c7d10e6";
  private static final String EXISTING_REQUIRED_VENDOR_UUID = "168f8a86-d26c-406e-813f-c7527f241ac3";
  private static final String FULLY_RECEIVED_POL_UUID = "1b02d557-c7c5-418b-a61a-077f804b630b";
  private static final String AWAITING_RECEIPT_POL_UUID = "d3fb56ac-918e-4d5f-b84d-ce121f9f8a21";
  private static final String EMPTY_RECEIPT_POL_UUID = "e92ef562-c77d-4306-8089-2607eff9b921";

  static final String ID_FOR_PRINT_MONOGRAPH_ORDER = "00000000-1111-2222-8888-999999999999";
  private static final String PO_ID_FOR_FAILURE_CASE = "bad500aa-aaaa-500a-aaaa-aaaaaaaaaaaa";
  private static final String ORDER_ID_WITHOUT_PO_LINES = "50fb922c-3fa9-494e-a972-f2801f1b9fd1";
  private static final String ORDER_WITHOUT_WORKFLOW_STATUS = "41d56e59-46db-4d5e-a1ad-a178228913e5";
  static final String ORDER_WIT_PO_LINES_FOR_SORTING =  "9a952cd0-842b-4e71-bddd-014eb128dc8e";
  static final String VALID_FUND_ID =  "fb7b70f1-b898-4924-a991-0e4b6312bb5f";
  static final String SAMPLE_TITLE_ID = "5489d159-cbbd-417e-a4e3-c6b0f7627f4f";

  public static final String ORDER_WITHOUT_MATERIAL_TYPES_ID =  "0cb6741d-4a00-47e5-a902-5678eb24478d";

  // API paths
  public static final String COMPOSITE_ORDERS_PATH = "/orders/composite-orders";
  private static final String COMPOSITE_ORDERS_BY_ID_PATH = COMPOSITE_ORDERS_PATH + "/%s";

  static final String LISTED_PRINT_MONOGRAPH_PATH = "po_listed_print_monograph.json";
  static final String LISTED_PRINT_SERIAL_RECEIPT_NOT_REQUIRED_PATH = "po_listed_print_serial_with_receipt_payment_not_required.json";
  private static final String ORDERS_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + "getOrders.json";
  private static final String ORDER_FOR_FAILURE_CASE_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + PO_ID_FOR_FAILURE_CASE + ".json";
  private static final String PE_MIX_PATH = "po_listed_print_monograph_pe_mix.json";
  private static final String MONOGRAPH_FOR_CREATE_INVENTORY_TEST = "print_monograph_for_create_inventory_test.json";
  private static final String LISTED_PRINT_SERIAL_PATH = "po_listed_print_serial.json";
  private static final String MINIMAL_ORDER_PATH = "minimal_order.json";
  private static final String ELECTRONIC_FOR_CREATE_INVENTORY_TEST = "po_listed_electronic_monograph.json";
  private static final String PO_FOR_TAGS_INHERITANCE_TEST = "po_tags_inheritance.json";

  private static final String NULL = "null";
  static final String PURCHASE_ORDER_ID = "purchaseOrderId";
  public static final String ORDER_DELETE_ERROR_TENANT = "order_delete_error";
  static final Header ERROR_ORDER_DELETE_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, ORDER_DELETE_ERROR_TENANT);

  public static final Header ALL_DESIRED_PERMISSIONS_HEADER = new Header(OKAPI_HEADER_PERMISSIONS, new JsonArray(AcqDesiredPermissions.getValuesExceptBypass()).encode());
  public static final Header APPROVAL_PERMISSIONS_HEADER = new Header(OKAPI_HEADER_PERMISSIONS, new JsonArray(Collections.singletonList("orders.item.approve")).encode());
  public static final Header UNOPEN_PERMISSIONS_HEADER = new Header(OKAPI_HEADER_PERMISSIONS, new JsonArray(Collections.singletonList("orders.item.unopen")).encode());
  public static final Header REOPEN_PERMISSIONS_HEADER = new Header(OKAPI_HEADER_PERMISSIONS, new JsonArray(Collections.singletonList("orders.item.reopen")).encode());
  static final String ITEMS_NOT_FOUND = UUID.randomUUID().toString();
  private static final String ITEM_MATERIAL_TYPE_ID = "materialTypeId";
  private static final String ITEMS = "items";
  private static final int MAX_IDS_FOR_GET_RQ = 15;
  private static final String ACQUISITIONS_UNIT_IDS = "acqUnitIds";
  private static final String NO_ACQ_UNIT_ASSIGNED_CQL = "cql.allRecords=1 not " + ACQUISITIONS_UNIT_IDS + " <> []";
  private static final String DEFAULT_INSTANCE_STATUS_CODE = "temp";
  private static final String DEFAULT_INSTANCE_TYPE_CODE = "zzz";
  private static final String DEFAULT_LOAN_TYPE_NAME = "Can circulate";
  private static final String INSTANCE_STATUS_ID = "statusId";
  private static final String INSTANCE_TYPE_ID = "instanceTypeId";
  private static final String EXISTING_PO_NUMBER = "oldPoNumber";
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);

  private static boolean runningOnOwn;
  private AutoCloseable mockitoMocks;

  @InjectMocks
  private EncumbranceService encumbranceService;
  @Mock
  private TransactionService transactionService;
  @Mock
  private RestClient restClient;

  private RequestContext requestContext;
  @Mock
  private RequestEntry requestEntry;

  @BeforeEach
  public void initMocks() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
    Context context = Vertx.vertx().getOrCreateContext();
    Map<String, String> okapiHeaders = new HashMap<>();
    okapiHeaders.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeaders.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeaders.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeaders.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(context, okapiHeaders);
  }
  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @AfterEach
  void afterEach() throws Exception {
    clearServiceInteractions();
    mockitoMocks.close();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void testPlaceOrderMinimal() throws Exception {
    logger.info("=== Test Placement of minimal order ===");

    String body = getMockData(MINIMAL_ORDER_PATH);
    JsonObject reqData = new JsonObject(body);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);


    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
    assertEquals(reqData.getJsonArray(COMPOSITE_PO_LINES).size(), resp.getCompositePoLines().size());

    for (int i = 0; i < resp.getCompositePoLines().size(); i++) {
      CompositePoLine line = resp.getCompositePoLines().get(i);
      String polNumber = line.getPoLineNumber();
      String polId = line.getId();

      assertEquals(poId, line.getPurchaseOrderId());
      assertNotNull(polId);
      assertNotNull(polNumber);
      assertTrue(polNumber.startsWith(poNumber));
    }
  }

  @Test
  void testPostOrderFailsWithInvalidPONumber() {
    logger.info("=== Test Placement of minimal order failure with Invalid PO Number===");

    JsonObject request = new JsonObject();
    request.put("poNumber", "1234");
    String body= request.toString();

    verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422);

  }

  @Test
  @Disabled
    //
  void testPostOrderFailsWithExistingPONumber() {
    logger.info("=== Test Placement of minimal order failure with Existing PO Number===");

    JsonObject request = new JsonObject();
    request.put("poNumber", PoNumberApiTest.EXISTING_PO_NUMBER);
    request.put("orderType", CompositePurchaseOrder.OrderType.ONE_TIME.value());
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);
    String body = request.toString();

    verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 400);

  }

  @Test
  void testPostOrderPONumberAutoGenerated() {
    logger.info("=== Test Placement of Empty order with Auto Generated PO Number===");

    JsonObject request = new JsonObject();
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);
    request.put("orderType", CompositePurchaseOrder.OrderType.ONE_TIME.value());
    String body= request.toString();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
  }

  @Test
  void testShouldReturnErrorIfPoNumberDoesNotMatchPattern() {
    logger.info("=== Test PO creation failure ===");
    JsonObject jsonObject = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, "0cb6741d-4a00-47e5-a902-5678eb24478d");

    String poNumber = "asdfgh123456789aaafffgghhhh";
    jsonObject.put("poNumber", poNumber);
    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, jsonObject.toString(),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());

    assertFalse(errors.getErrors().isEmpty());
    assertNotNull(errors.getErrors().get(0));
    assertEquals("must match \"^[a-zA-Z0-9]{1,22}$\"", errors.getErrors().get(0).getMessage());
    assertFalse(errors.getErrors().get(0).getParameters().isEmpty());
    assertNotNull(errors.getErrors().get(0).getParameters().get(0));
    assertEquals("poNumber", errors.getErrors().get(0).getParameters().get(0).getKey());
    assertEquals(poNumber, errors.getErrors().get(0).getParameters().get(0).getValue());
  }

  @Test
  void testShouldCreateOrderIfPoNumberMatchPattern() {
    logger.info("=== Test PO creation failure ===");
    JsonObject jsonObject = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, "0cb6741d-4a00-47e5-a902-5678eb24478d");

    String poNumber = "asdfghj100200jhgfdsa";
    jsonObject.put("poNumber", poNumber);
    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, jsonObject.toString(),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 201).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());

    assertTrue(errors.getErrors().isEmpty());
  }

  @Test
  void testPoCreationWithOverLimitPOLines() throws Exception {
    logger.info("=== Test PO, with over limit lines quantity, creation ===");

    String body = getMockDraftOrder().toString();

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1), APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }


  @Test
  void testPostWithInvalidConfig() throws Exception {
    logger.info("=== Test PO creation fail if config is invalid ===");

    String body = getMockDraftOrder().toString();

    final Errors error = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(INVALID_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 500).body().as(Errors.class);

    assertEquals("Invalid limit value in configuration.", error.getErrors().get(0).getAdditionalProperties().get(ERROR_CAUSE));
  }


  @Test
  void testSubObjectCreationFailure() throws Exception {
    logger.info("=== Test Details creation failure ===");

    String body = getMockDraftOrder().toString();
    int status403 = HttpStatus.HTTP_FORBIDDEN.toInt();
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, new Header(X_ECHO_STATUS, String.valueOf(status403)));

    final Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body, headers, APPLICATION_JSON, 403);

    String respBody = resp.getBody().as(Errors.class).getErrors().get(0).getMessage();
    logger.info(respBody);

    assertEquals("Access requires permission: foo.bar.baz", respBody);
  }

  @Test
  void testGetOrderById() throws IOException {
    logger.info("=== Test Get Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, id);

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(order.getCompositePoLines());
    final CompositePurchaseOrder resp = verifySuccessGet(url, CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(id, resp.getId());
    verifyCalculatedData(resp);
  }

  @Test
  void testGetOrderWithConvertedTotals() throws Exception {
    logger.info("=== Test Get Order With Converted Totals ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, id);

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id).mapTo(CompositePurchaseOrder.class);
    var exchangeRate = new ExchangeRate().withFrom("EUR").withTo("USD").withExchangeRate(1.1d);
  //  doReturn(succeededFuture(exchangeRate)).when(restClient).get(requestEntry, ExchangeRate.class, requestContext);

    order.getCompositePoLines()
      .forEach(poLine -> {
        poLine.getCost().setCurrency("EUR");
        addMockEntry(PO_LINES_STORAGE, poLine);
      });
    MockServer.addMockTitles(order.getCompositePoLines());
    final CompositePurchaseOrder resp = verifySuccessGet(url, CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(id, resp.getId());
    verifyCalculatedData(resp);
  }

  @Test
  void testGetOrderByIdWithPoLinesSorting() {
    logger.info("=== Test Get Order By Id - PoLines sorting ===");

    String[] expectedPoLineNumbers = {"841240-001", "841240-02", "841240-3", "841240-21"};

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(order.getCompositePoLines());
    final CompositePurchaseOrder resp = verifySuccessGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_WIT_PO_LINES_FOR_SORTING), CompositePurchaseOrder.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertArrayEquals(expectedPoLineNumbers, resp.getCompositePoLines().stream().map(CompositePoLine::getPoLineNumber).toArray());
  }

  private void verifyCalculatedData(CompositePurchaseOrder resp) {
    Integer expectedQuantity = resp
      .getCompositePoLines()
      .stream()
      .mapToInt(poLine -> {
        int eQty = ObjectUtils.defaultIfNull(poLine.getCost().getQuantityElectronic(), 0);
        int pQty = ObjectUtils.defaultIfNull(poLine.getCost().getQuantityPhysical(), 0);
        return eQty + pQty;
      })
      .sum();

    //PurchaseOrderHelper serviceSpy = spy(new PurchaseOrderHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    //double expectedPrice = serviceSpy.calculateTotalEstimatedPrice(resp.getCompositePoLines()).result();

    assertThat(resp.getTotalItems(), equalTo(expectedQuantity));
    //assertThat(resp.getTotalEstimatedPrice(), equalTo(expectedPrice));

    resp.getCompositePoLines().forEach(line -> assertThat(line.getCost().getPoLineEstimatedPrice(), greaterThan(0d)));
  }

  @Test
  void testGetOrderByIdWithPoLines() {
    logger.info("=== Test Get Order By Id - PoLines with items ===");

    String[] expectedPoLineNumbers = {"841240-001", "841240-02", "841240-3", "841240-21"};

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING));

    final CompositePurchaseOrder resp = verifySuccessGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_WIT_PO_LINES_FOR_SORTING), CompositePurchaseOrder.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertArrayEquals(expectedPoLineNumbers, resp.getCompositePoLines().stream().map(CompositePoLine::getPoLineNumber).toArray());
  }

  @Test
  void testGetOrderByIdWithPoLinesWithInstanceId() {
    logger.info("=== Test Get Order By Id - PoLines with items ===");

    String[] expectedPoLineNumbers = {"841240-001", "841240-02", "841240-3", "841240-21"};
    String instanceId= UUID.randomUUID().toString();

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> line.setInstanceId(instanceId));
    MockServer.addMockTitles(order.getCompositePoLines());
    order.getCompositePoLines().forEach(line -> line.setInstanceId(null));
    final CompositePurchaseOrder resp = verifySuccessGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_WIT_PO_LINES_FOR_SORTING), CompositePurchaseOrder.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertArrayEquals(expectedPoLineNumbers, resp.getCompositePoLines().stream().map(CompositePoLine::getPoLineNumber).toArray());
    assertThat(resp.getCompositePoLines(), (Every.everyItem(HasPropertyWithValue.hasProperty("instanceId", Is.is(instanceId)))));
  }

  @Test
  void testGetPOByIdTotalItemsWithoutPOLines() {
    logger.info("=== Test Get Order By Id without PO Line totalItems value is 0 ===");

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_ID_WITHOUT_PO_LINES));
    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES);
    final CompositePurchaseOrder resp = verifySuccessGet(url, CompositePurchaseOrder.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(PO_ID_PENDING_STATUS_WITHOUT_PO_LINES, resp.getId());
    assertEquals(0, resp.getTotalItems().intValue());
  }

  @Test
  void testGetOrderByIdWithOnePoLine() {
    logger.info("=== Test Get Order By Id - With one PO Line and empty source ===");

    String id = PO_ID_CLOSED_STATUS;
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(order.getCompositePoLines());
    final CompositePurchaseOrder resp = verifySuccessGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(id, resp.getId());
    assertEquals(1, resp.getCompositePoLines().size());
    assertEquals(calculateTotalQuantity(resp.getCompositePoLines().get(0)), resp.getTotalItems().intValue());
  }

  @Test
  void testGetOrderByIdIncorrectIdFormat() {
    logger.info("=== Test Get Order By Id - Incorrect Id format - 400 ===");

    String id = ID_BAD_FORMAT;
    final Response resp = verifyGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), TEXT_PLAIN, 400);

    String actual = resp.getBody().asString();
    logger.info(actual);

    assertNotNull(actual);
    assertTrue(actual.contains(id));
  }

  @Test
  void testGetOrderByIdNotFound() {
    logger.info("=== Test Get Order By Id - Not Found ===");

    String id = ID_DOES_NOT_EXIST;
    final Response resp = verifyGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), APPLICATION_JSON, 404);

    String actual = resp.getBody().as(Errors.class).getErrors().get(0).getMessage();
    logger.info(actual);

    assertNotNull(actual);
    assertTrue(actual.contains(id));
  }

  @Test
  void testDeleteById() throws IOException {
    logger.info("=== Test Delete Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));

    verifyDeleteResponse(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), "", 204);
  }

  @Test
  void testDeleteByIdNoOrderFound() {
    logger.info("=== Test Delete Order By Id - Not Found ===");
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_DOES_NOT_EXIST, "", 404);
  }

  @Test
  void testDeleteById500Error() {
    logger.info("=== Test Delete Order By Id - Storage Internal Server Error ===");
    Headers headers = prepareHeaders(ERROR_ORDER_DELETE_TENANT_HEADER);
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + MIN_PO_ID, headers, APPLICATION_JSON, 500);
  }

  @Test
  void testDeleteByIdWhenDeletingPoLine500Error() {
    logger.info("=== Test Delete Order By Id - Storage Internal Server Error on PO Line deletion ===");
    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    CompositePoLine line = getMinimalContentCompositePoLine(order.getId());
    line.setId(ID_FOR_INTERNAL_SERVER_ERROR);
    addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(line));

    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + order.getId(), APPLICATION_JSON, 500);
  }

  @Test
  void testPutOrdersByIdWorkflowStatusOpenForStorageAndCurrentRequest() throws IOException {
    logger.info("=== Test Put Order By Id workflowStatus is Open from storage and workflowStatus is Open in current request  ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    JsonObject storageData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    storageData.put("workflowStatus", "Open");
    reqData.put("workflowStatus", "Open");
    verifyPoWithPoLinesUpdate(reqData, storageData);
  }

  private void verifyPoWithPoLinesUpdate(JsonObject reqData, JsonObject storageData) {
    JsonArray poLinesFromRequest = reqData.getJsonArray(COMPOSITE_PO_LINES);
    JsonArray poLinesFromStorage = storageData.getJsonArray(COMPOSITE_PO_LINES);
    int sameLinesCount = 0;
    for (int i = 0; i < poLinesFromRequest.size(); i++) {
      JsonObject lineFromRequest = poLinesFromRequest.getJsonObject(i);
      for (int j = 0; j < poLinesFromStorage.size(); j++) {
        JsonObject lineFromStorage = poLinesFromStorage.getJsonObject(j);
        if (StringUtils.equals(lineFromRequest.getString(ID), lineFromStorage.getString(ID))) {
          sameLinesCount++;
          break;
        }
      }
    }

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT));
    assertEquals(MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.POST).size(), poLinesFromRequest.size() - sameLinesCount);
    assertEquals(MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.DELETE).size(), poLinesFromStorage.size() - sameLinesCount);
    assertNotNull(MockServer.getPoLineUpdates());
    assertEquals(MockServer.getPoLineUpdates().size(), sameLinesCount);
  }

  @Test
  void testUpdateOrderWithDefaultStatus() throws IOException {
    logger.info("=== Test Put Order By Id - Make sure that default status is used ===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(MINIMAL_ORDER_PATH)).mapTo(CompositePurchaseOrder.class);
    reqData.setId(ORDER_WITHOUT_WORKFLOW_STATUS);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    String url = COMPOSITE_ORDERS_PATH + "/" + reqData.getId();
    verifyPut(url, JsonObject.mapFrom(reqData), "", 204);

    List<JsonObject> orderRetrievals = MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.GET);
    assertNotNull(orderRetrievals);
    assertEquals(1, orderRetrievals.size());
    PurchaseOrder storageOrderBeforeUpdate = orderRetrievals.get(0).mapTo(PurchaseOrder.class);
    // Assert default status is Pending
    assertEquals(PurchaseOrder.WorkflowStatus.PENDING, storageOrderBeforeUpdate.getWorkflowStatus());

    List<JsonObject> orderUpdates = MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT);
    assertNotNull(orderUpdates);
    assertEquals(1, orderUpdates.size());

    PurchaseOrder storageUpdatedOrder = orderUpdates.get(0).mapTo(PurchaseOrder.class);
    assertNotNull(storageUpdatedOrder.getWorkflowStatus());

    assertEquals(PurchaseOrder.WorkflowStatus.CLOSED, storageUpdatedOrder.getWorkflowStatus());
  }

  @Test
  void testPutOrdersByIdWithIdMismatch() throws IOException {
    logger.info("=== Test Put Order By Id with id mismatch  ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, APPLICATION_JSON, 422);
  }

  @Test
  void testUpdatePoNumber() throws IOException {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject storData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));
    String newPoNumber = reqData.getString(PO_NUMBER) + "A";
    reqData.put(PO_NUMBER, newPoNumber);
    Pattern poLinePattern = Pattern.compile(String.format("(%s)(-[0-9]{1,3})", newPoNumber));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT));
    assertEquals(MockServer.getPoLineUpdates().size(), storData.getJsonArray(COMPOSITE_PO_LINES).size());
    MockServer.getPoLineUpdates().forEach(poLine -> {
      Matcher matcher = poLinePattern.matcher(poLine.getString(PO_LINE_NUMBER));
      assertTrue(matcher.find());
    });
    assertNull(MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.DELETE));
  }

  @Test
  void testUpdatePoNumberWithPrefixAndSuffix() throws IOException {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(2).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject storeData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT));
    assertEquals(MockServer.getPoLineUpdates().size(), storeData.getJsonArray(COMPOSITE_PO_LINES).size());
    MockServer.getPoLineUpdates().forEach(poLine -> {
    });
    assertNull(MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.DELETE));
  }

  @Test
  void testUpdatePoNumberWithNoPrefixAndSuffix() throws IOException {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject storData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));
    String newPoNumber = reqData.getString(PO_NUMBER) + "A";
    reqData.put(PO_NUMBER, newPoNumber);
    Pattern poLinePattern = Pattern.compile(String.format("(%s)(-[0-9]{1,3})", newPoNumber));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT));
    assertEquals(MockServer.getPoLineUpdates().size(), storData.getJsonArray(COMPOSITE_PO_LINES).size());
    MockServer.getPoLineUpdates().forEach(poLine -> {
      Matcher matcher = poLinePattern.matcher(poLine.getString(PO_LINE_NUMBER));
      assertTrue(matcher.find());
    });
    assertNull(MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.DELETE));
  }

  @Test
  void testUpdatePoNumberWithNoPrefixAndSuffixInCurrentOrder() throws IOException {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(2).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject storeData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));
    String newPoNumber = reqData.getString(PO_NUMBER) + "A";
    reqData.put(PO_NUMBER, newPoNumber);
    Pattern poLinePattern = Pattern.compile(String.format("(%s)(-[0-9]{1,3})", newPoNumber));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT));
    assertEquals(MockServer.getPoLineUpdates().size(), storeData.getJsonArray(COMPOSITE_PO_LINES).size());
    MockServer.getPoLineUpdates().forEach(poLine -> {
    });
    assertNull(MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.DELETE));
  }

  @Test
  void testUpdatePoNumberWithPoLines() throws IOException {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    JsonObject storageData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    String newPoNumber = reqData.getString(PO_NUMBER) + "A";
    reqData.put(PO_NUMBER, newPoNumber);
    reqData.put(VENDOR_ID, EXISTING_REQUIRED_VENDOR_UUID);
    Pattern poLinePattern = Pattern.compile(String.format("(%s)(-[0-9]{1,3})", newPoNumber));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);
    verifyPoWithPoLinesUpdate(reqData, storageData);
    MockServer.getPoLineUpdates().forEach(poLine -> {
      Matcher matcher = poLinePattern.matcher(poLine.getString(PO_LINE_NUMBER));
      assertTrue(matcher.find());
    });
  }

  @Test
  void testPutOrderWithoutPoNumberValidation() throws IOException {
    logger.info("=== Test Put Order By Id without po number validation  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    reqData.remove(PO_NUMBER);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, APPLICATION_JSON, 422);
  }

  @Test
  void testPutOrderFailsWithInvalidPONumber() throws Exception {
    logger.info("=== Test update order failure with Invalid PO Number===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);

    JsonObject request = new JsonObject();
    request.put("poNumber", "1234");

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), request, APPLICATION_JSON, 422);

  }

  @Test
  @Disabled
    //
  void testPutOrderFailsWithExistingPONumber() throws Exception {
    logger.info("=== Test update of order failure with Existing PO Number===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);

    JsonObject request = new JsonObject();
    request.put("poNumber", PoNumberApiTest.EXISTING_PO_NUMBER);
    request.put("orderType", CompositePurchaseOrder.OrderType.ONE_TIME.value());
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), request, APPLICATION_JSON, 400);
  }

  @Test
  void testPoUpdateWithOverLimitPOLines() throws Exception {
    logger.info("=== Test PUT PO, with over limit lines quantity ===");

    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES);
    String body = getMockData(LISTED_PRINT_MONOGRAPH_PATH);
    Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_TOKEN, X_OKAPI_USER_ID);
    final Errors errors = verifyPut(url, body, headers, APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  void testPoUpdateWithOverLimitPOLinesWithDefaultLimit() throws Exception {
    logger.info("=== Test PUT PO, with over limit lines quantity with default limit ===");

    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES);
    String body = getMockData(LISTED_PRINT_MONOGRAPH_PATH);
    Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_USER_ID);
    final Errors errors = verifyPut(url, body, headers, APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  void testOpenOrderWithDifferentPoLineCurrency() throws Exception {
    logger.info("=== Test to try open order with different poLine currency ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.getCompositePoLines().get(0).getCost().setCurrency("EUR");
    preparePiecesForCompositePo(reqData);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().forEach(this::createMockTitle);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
  }

  @Test
  void testOpenOrderForInactiveExpenseClass() throws Exception {
    logger.info("=== Test to try open order with inactive expense class ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setExpenseClassId(INACTIVE_EXPENSE_CLASS_ID);
    preparePiecesForCompositePo(reqData);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().forEach(this::createMockTitle);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 400)
      .as(Errors.class);

    assertThat(errors.getErrors().get(0).getCode(),is(INACTIVE_EXPENSE_CLASS.toError().getCode()));
  }

  @Test
  void testOpenOrderWithExpenseClassNotFound() throws Exception {
    logger.info("=== Test to try open order with expense class not found ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setExpenseClassId(UUID.randomUUID().toString());
    preparePiecesForCompositePo(reqData);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().forEach(this::createMockTitle);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 400)
      .as(Errors.class);

    assertThat(errors.getErrors().get(0).getCode(),is(BUDGET_EXPENSE_CLASS_NOT_FOUND.toError().getCode()));
  }


  @Test
  @Disabled
  //TODO must be fixed in scope of https://issues.folio.org/browse/MODORDERS-587
  void testPutOrdersByIdToChangeStatusToOpen() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

    preparePiecesForCompositePo(reqData);

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // MODORDERS-178 guarantee electronic resource for the second PO Line but set "create items" to NONE
    CompositePoLine line1 = reqData.getCompositePoLines().get(0);
    CompositePoLine line2 = reqData.getCompositePoLines().get(1);

    line1.setPurchaseOrderId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    line2.setPurchaseOrderId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    line2.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    line2.getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    reqData.getCompositePoLines().forEach(s -> s.setReceiptStatus(CompositePoLine.ReceiptStatus.PENDING));
    reqData.getCompositePoLines().forEach(s -> s.setPaymentStatus(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED));

    reqData.getCompositePoLines().forEach(this::createMockTitle);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    List<JsonObject> respOrder =  MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT);
    CompositePurchaseOrder compPo = respOrder.get(0).mapTo(CompositePurchaseOrder.class);
    List<JsonObject> respLines =  MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.PUT);

    CompositePoLine respLine1 = respLines.stream()
      .filter(line -> line.getString(ID).equals(line1.getId()))
      .peek(line -> line.remove("reportingCodes"))
      .map(line -> line.mapTo(CompositePoLine.class))
      .filter(line -> Objects.nonNull(line.getLocations().get(0).getHoldingId()))
      .distinct().findAny().get();

    CompositePoLine respLine2 = respLines.stream()
      .filter(line -> line.getString(ID).equals(line2.getId()))
      .peek(line -> line.remove("reportingCodes"))
      .map(line -> line.mapTo(CompositePoLine.class))
      .findAny().get();

    compPo.setCompositePoLines(List.of(respLine1, respLine2));

    int polCount = reqData.getCompositePoLines().size();

    List<JsonObject> createdInstances = getCreatedInstances();
    assertEquals(1, createdInstances.size(), "Quantity of created instance must be equal of line, if create inventory include instance");
    assertNotNull("Line must be connected to instance, if create inventory include instance", respLine1.getInstanceId());
    assertNull(respLine2.getInstanceId());

    List<JsonObject> createdHoldings = getCreatedHoldings();
    assertEquals(3, createdHoldings.size(), "Quantity of created instance must be depended of quantity in the locations and create inventory include holding");
    verifyHoldingsCreated(3, createdHoldings, respLine1);
    verifyHoldingsCreated(0, createdHoldings, respLine2);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();
    verifyItemsCreated(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, 4, items, respLine1);
    verifyItemsCreated(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, 0, items, respLine2);

    List<JsonObject> createdPieces = getCreatedPieces();
    verifyOpenOrderPiecesCreated(items, compPo.getCompositePoLines(), createdPieces, 0);

    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value(), compPo.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED.value(), compPo.getCompositePoLines().size());
  }

  @Test
  void testPutOrdersByIdToChangeStatusToOpenRestrictedIfExtraPiecesExist() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    assertEquals(2, reqData.getCompositePoLines().size());

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // Create extra-pieces
    preparePiecesForCompositePo(reqData);
    preparePiecesForCompositePo(reqData);

    Errors response = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 422).as(Errors.class);

    verifyInventoryNonInteraction();

    List<Error> errors = response.getErrors();
    assertThat(errors, hasSize(1));
    Error error = errors.get(0);
    assertThat(error.getCode(), is(PIECES_TO_BE_DELETED.getCode()));
  }

  @Test
  void testPutOrdersByIdToOpenOrderWithLocationButCreateInventoryNoneForOneOfResources() {
    logger.info("=== Test Put Order By Id to open order - P/E Mix PO Line, location specified but create inventory is None for electronic resource ===");

    CompositePurchaseOrder reqData = getMinimalContentCompositePurchaseOrder();
    reqData.setId(PO_LINES_EMPTY_COLLECTION_ID);
    CompositePoLine line = getMinimalContentCompositePoLine();

    reqData.withVendor(ACTIVE_VENDOR_ID);
    line.setOrderFormat(OrderFormat.P_E_MIX);
    line.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.NONE).withAccessProvider(reqData.getVendor()));
    line.setPhysical(new Physical().withCreateInventory(CreateInventory.INSTANCE_HOLDING_ITEM).withMaterialType(ITEM_MATERIAL_TYPE_ID));
    line.setPoLineNumber(reqData.getPoNumber() + "-1");

    line.setLocations(Collections.singletonList(new Location().withLocationId(UUID.randomUUID()
      .toString())
      .withQuantity(4)
      .withQuantityElectronic(2)
      .withQuantityPhysical(2)));

    line.setCost(new Cost().withCurrency("USD")
      .withListUnitPrice(1.0)
      .withListUnitPriceElectronic(1.0)
      .withQuantityElectronic(2)
      .withQuantityPhysical(2));

    JsonObject storJson = JsonObject.mapFrom(reqData);
    storJson.remove(COMPOSITE_PO_LINES);
    storJson.remove("totalEstimatedPrice");
    storJson.remove("totalItems");
    PurchaseOrder purchaseOrder = JsonObject.mapFrom(storJson).mapTo(PurchaseOrder.class);
    addMockEntry(PURCHASE_ORDER_STORAGE, purchaseOrder);
    addMockEntry(PO_LINES_STORAGE, line);
    createMockTitle(line);

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    preparePiecesForCompositePo(reqData);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    List<JsonObject> createdPieces = getCreatedPieces();
    verifyPiecesQuantityForSuccessCase(Collections.singletonList(line), createdPieces);
  }

  private void createMockTitle(CompositePoLine line) {
    Title title = new Title().withId(SAMPLE_TITLE_ID).withTitle(line.getTitleOrPackage()).withPoLineId(line.getId());
    addMockEntry(TITLES, JsonObject.mapFrom(title));
  }

  @Test
  void testPutOrdersByIdInstanceCreation() throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    Map<String, String> uuids = new HashMap<>();
    // Populate instanceIds
    reqData.getCompositePoLines().forEach(p -> p.setInstanceId(uuids.compute(p.getId(), (k, v) -> UUID.randomUUID().toString())));
    MockServer.addMockTitles(reqData.getCompositePoLines());
    preparePiecesForCompositePo(reqData);
    // Update order
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
    verifyInstanceLinksForUpdatedOrder(reqData);
    // Verify instanceIds conformity
    reqData.getCompositePoLines().forEach(compPoline -> assertThat(uuids.get(compPoline.getId()), is(compPoline.getInstanceId())));
    // Verify that new instances didn't created
    assertThat(MockServer.getCreatedInstances(), nullValue());

  }

  @Test
  void testPutOrdersWithPackagePolinesAndInstanceIdSpecified() throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);

    reqData.getCompositePoLines()
      .forEach(p -> {
        p.setInstanceId(UUID.randomUUID().toString());
        p.setIsPackage(true);
      });
    // Update order
    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 422)
      .then()
      .extract()
      .as(Errors.class);

    assertEquals(INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE.getCode(), errors.getErrors().get(0).getCode());

    // test with transition to open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 422)
      .then()
      .extract()
      .as(Errors.class);

    assertEquals(INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  void testPostOrdersWithOpenStatusAndCheckinItems() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that mock po has 2 po lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    // Make sure that mock po has the first PO line with 3 locations
    assertThat(reqData.getCompositePoLines().get(0).getLocations(), hasSize(3));

    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set checkin items flag true
    reqData.getCompositePoLines().forEach(s -> s.setCheckinItems(true));

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_TOKEN, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getCreatedHoldings());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  void testPostOrdersCreateInventoryPhysicalNone() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction not required for Physical Resource  ===");

    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create nothing in inventory + fix from MODORDERS-264
    reqData.getCompositePoLines().get(0).setOrderFormat(OrderFormat.PHYSICAL_RESOURCE);
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    reqData.getCompositePoLines().get(0).getCost().setQuantityElectronic(0);
    reqData.getCompositePoLines().get(0).getCost().setListUnitPriceElectronic(0d);
    reqData.getCompositePoLines().get(0).getLocations().get(0).setQuantityElectronic(0);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    verifyInstanceNotCreated();
  }

  @Test
  void testPostOrdersCreateInventoryElcetronicNone() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction not required for Electronic Resource  ===");

    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create nothing in inventory + fix from MODORDERS-264
    reqData.getCompositePoLines().get(0).setOrderFormat(OrderFormat.ELECTRONIC_RESOURCE);
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).getCost().setQuantityPhysical(0);
    reqData.getCompositePoLines().get(0).getCost().setListUnitPrice(0d);
    reqData.getCompositePoLines().get(0).getLocations().get(0).setQuantityPhysical(0);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    verifyInstanceNotCreated();
  }

  private void verifyInstanceNotCreated() {
    assertNull(getInstancesSearches());
    assertNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
    assertNull(getCreatedInstances());
  }

  @Test
  void testPostOrdersCreateInventoryInstance() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create inventory Instance
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE);
    MockServer.addMockTitles(reqData.getCompositePoLines());

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
  }

  @Test
  void testPostOrdersCreateInventoryInstanceHolding() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // Set CreateInventory value to create inventory instances and holdings
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNotNull(getCreatedHoldings());
    assertNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
  }

  @Test
  void testPostOrdersCreateInventoryInstanceHoldingItem() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create inventory instances, holdings and items
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getCreatedHoldings());
    assertNotNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
  }

  @Test
  void testPostOrdersWithEmptyCreateInventory() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Clear CreateInventory for setting default values
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(null);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(null);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNotNull(getCreatedHoldings());
    assertNotNull(getItemsSearches());

    CompositePoLine respLine1 = resp.getCompositePoLines().get(0);
    List<JsonObject> createdInstances = getCreatedInstances();
    assertEquals(1, createdInstances.size(), "Quantity of created instance must be equal of line, if create inventory include instance");
    assertNotNull("Line must be connected to instance, if create inventory include instance", respLine1.getInstanceId());

    List<JsonObject> createdHoldings = getCreatedHoldings();
    assertEquals(1, createdHoldings.size(), "Quantity of created instance must be depended of quantity in the locations and create inventory include holding");
    verifyHoldingsCreated(1, createdHoldings, respLine1);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();
    verifyItemsCreated(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, 2, items, respLine1);

    List<JsonObject> createdPieces = getCreatedPieces();
    verifyOpenOrderPiecesCreated(items, resp.getCompositePoLines(), createdPieces, 0);

    assertNotNull(getCreatedPieces());
  }

  @Test
  void testPostOrdersWithEmptyCreateInventoryAndEmptyConfiguration() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Clear CreateInventory for setting default values
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(null);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(null);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EMPTY_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNotNull(getCreatedHoldings());
    assertNotNull(getItemsSearches());

    // MODORDERS-239/240/241: default values will be used when config is empty
    CompositePoLine respLine1 = resp.getCompositePoLines().get(0);
    List<JsonObject> createdInstances = getCreatedInstances();
    assertEquals(1, createdInstances.size(), "Quantity of created instance must be equal of line, if create inventory include instance");
    assertNotNull("Line must be connected to instance, if create inventory include instance", respLine1.getInstanceId());

    List<JsonObject> createdHoldings = getCreatedHoldings();
    assertEquals(1, createdHoldings.size(), "Quantity of created instance must be depended of quantity in the locations and create inventory include holding");
    verifyHoldingsCreated(1, createdHoldings, respLine1);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();
    verifyItemsCreated(EMPTY_CONFIG_X_OKAPI_TENANT, 1, items, respLine1);

    List<JsonObject> createdPieces = getCreatedPieces();
    verifyOpenOrderPiecesCreated(items, resp.getCompositePoLines(), createdPieces, 1);

    assertNotNull(getCreatedPieces());
  }

  @Test
  void testPutOrdersByIdToChangeStatusToOpenWithCheckinItems() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that mock PO has 2 po lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // MODORDERS-183 Set the second POLine checkinItems true
    CompositePoLine line1 = reqData.getCompositePoLines().get(0);
    CompositePoLine line2 = reqData.getCompositePoLines().get(1);
    line2.setCheckinItems(true);
    reqData.getCompositePoLines().forEach(s -> s.setReceiptStatus(CompositePoLine.ReceiptStatus.PENDING));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    List<JsonObject> respOrder =  MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.GET);
    CompositePurchaseOrder compPo = respOrder.get(0).mapTo(CompositePurchaseOrder.class);
    List<JsonObject> respLines =  MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.PUT);

    CompositePoLine respLine1 = respLines.stream()
      .filter(line -> line.getString(ID).equals(line1.getId()))
      .peek(line -> line.remove("reportingCodes"))
      .map(line -> line.mapTo(CompositePoLine.class))
      .filter(line -> Objects.nonNull(line.getLocations().get(0).getHoldingId()))
      .distinct().findAny().get();

    CompositePoLine respLine2 = respLines.stream()
      .filter(line -> line.getString(ID).equals(line2.getId()))
      .peek(line -> line.remove("reportingCodes"))
      .map(line -> line.mapTo(CompositePoLine.class))
      .filter(line -> Objects.nonNull(line.getLocations().get(0).getHoldingId()))
      .findAny().get();

    compPo.setCompositePoLines(List.of(respLine1, respLine2));

    int polCount = compPo.getCompositePoLines().size();
    List<JsonObject> createdInstances = getCreatedInstances();
    assertEquals(2, createdInstances.size(), "Quantity of created instance must be equal of line, if create inventory include instance");
    assertNotNull("Line must be connected to instance, if create inventory include instance", respLine1.getInstanceId());
    assertNotNull("Line must be connected to instance, if create inventory include instance", respLine2.getInstanceId());

    List<JsonObject> createdHoldings = getCreatedHoldings();
    assertEquals(5, createdHoldings.size(), "Quantity of created instance must be depended of quantity in the locations and create inventory include holding");
    verifyHoldingsCreated(3, createdHoldings, respLine1);
    verifyHoldingsCreated(2, createdHoldings, respLine2);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();
    verifyItemsCreated(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, 4, items, respLine1);
    verifyItemsCreated(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, 0, items, respLine2);

    List<JsonObject> createdPieces = getCreatedPieces();
    verifyOpenOrderPiecesCreated(items, compPo.getCompositePoLines(), createdPieces, 0);

    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value(), compPo.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.AWAITING_PAYMENT.value(), compPo.getCompositePoLines().size());
  }

  @Test
  void testEncumbranceTagInheritance() throws Exception {
    logger.info("=== Test encumbrance tag inheritance upon order opening ===");

    JsonObject order = new JsonObject(getMockData(PO_FOR_TAGS_INHERITANCE_TEST));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    assertThat(reqData.getCompositePoLines(), hasSize(1));
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    //verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201);
    Transaction createdEncumbrance = MockServer.getCreatedEncumbrances().get(0);
    assertEquals(Collections.singletonList("important"), createdEncumbrance.getTags().getTagList());
  }

  @Test
  void testPutOrdersByIdToChangeStatusToOpenWithEmptyPoLines() throws Exception {
    logger.info("=== Test Put (WithEmptyPoLines) Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    preparePiecesForCompositePo(reqData);
    reqData.getCompositePoLines().clear();
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value(), reqData.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.AWAITING_PAYMENT.value(), reqData.getCompositePoLines().size());
  }
  @Test
  void testPutOrdersByIdToChangeStatusToOpenWithPaymentNotRequired() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockOrderWithStatusPaymentNotRequired().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
    verifyReceiptStatusChangedTo(ReceiptStatus.RECEIPT_NOT_REQUIRED.value(), reqData.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED.value(), reqData.getCompositePoLines().size());
  }
  private void verifyReceiptStatusChangedTo(String expectedStatus, int poLinesQuantity) {
    List<JsonObject> polUpdates = MockServer.getPoLineUpdates();
    assertNotNull(polUpdates);
    // check receipt status of the last poLinesQuantity updated polines
    for (JsonObject jsonObj : polUpdates.subList(polUpdates.size() - poLinesQuantity, polUpdates.size())) {
      assertEquals(expectedStatus, jsonObj.getString(RECEIPT_STATUS));
    }
  }

  private void verifyPaymentStatusChangedTo(String expectedStatus, int poLinesQuantity) {
    List<JsonObject> polUpdates = MockServer.getPoLineUpdates();
    assertNotNull(polUpdates);

    for (JsonObject jsonObj : polUpdates.subList(polUpdates.size() - poLinesQuantity, polUpdates.size())) {
      assertEquals(expectedStatus, jsonObj.getString(PAYMENT_STATUS));
    }
  }

  @Test
  @Disabled
    //
  void testUpdateOrderToOpenWithPartialItemsCreation() throws Exception {
    logger.info("=== Test Order update to Open status - Inventory items expected to be created partially ===");

    // One item for each location will be found
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Emulate items creation issue
    reqData.getCompositePoLines().get(0).getPhysical().setMaterialType(ID_FOR_INTERNAL_SERVER_ERROR);
    assertThat(reqData.getCompositePoLines().get(0).getLocations(), hasSize(3));
    // Second location has 2 physical resources
    reqData.getCompositePoLines().get(0).getLocations().get(1).setLocationId(ID_FOR_INTERNAL_SERVER_ERROR);
    // Let's have only one PO Line
    reqData.getCompositePoLines().remove(1);
    // Set status to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // Set specific ID to let items search to return 1 item
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    int polCount = reqData.getCompositePoLines().size();
    // Assert that only one PO line presents
    assertEquals(1, polCount);

    preparePiecesForCompositePo(reqData);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 500);

    // Verify inventory GET and POST requests for instance, holding and item records
    verifyInventoryInteraction(false, false);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();

    // Verify that not all expected items created
    assertThat(items.size(), lessThan(calculateInventoryItemsQuantity(reqData.getCompositePoLines().get(0))));
  }


  @Test
  void testPutOrdersByIdToChangeStatusToOpenButWithFailureFromStorage() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open - Storage errors expected and no interaction with Inventory===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(ORDER_FOR_FAILURE_CASE_MOCK_DATA_PATH)).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    preparePiecesForCompositePo(reqData);

    final Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData),
      APPLICATION_JSON, 500)
      .body()
      .as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertEquals(1, errors.getErrors().size());
    assertNull(getInstancesSearches());
    assertNull(getItemsSearches());
  }

  @Test
  @Disabled
    //
  void testPutOrdersByIdToChangeStatusToOpenButWithErrorCreatingItemsForSecondPOL() throws Exception {
    logger.info("=== Test Put Order By Id to change Order's status to Open - Inventory errors expected on items creation for second POL ===");

    /*==============  Preparation ==============*/

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    MockServer.addMockTitles(reqData.getCompositePoLines());

    int polCount = reqData.getCompositePoLines().size();
    // Make sure that mock PO has 2 lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    // Make sure that inventory interaction is expected for each PO line
    for (CompositePoLine pol : reqData.getCompositePoLines()) {
      assertTrue(calculateInventoryItemsQuantity(pol) > 0);
    }

    // Set location ids to one which emulates item creation failure
    CompositePoLine line1 = reqData.getCompositePoLines().get(0);
    CompositePoLine line2 = reqData.getCompositePoLines().get(1);
    reqData.getCompositePoLines().get(1).getLocations().forEach(location -> location.withLocationId(ID_FOR_INTERNAL_SERVER_ERROR));

    reqData.getCompositePoLines().get(1).getLocations().get(0).setLocationId(UUID.randomUUID().toString());

    preparePiecesForCompositePo(reqData);

    String path = String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId());

    /*==============  Assert result ==============*/

    // Server Error expected as a result because not all items created
    verifyPut(path, JsonObject.mapFrom(reqData), APPLICATION_JSON, 500);

    List<JsonObject> respOrder =  MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.GET);
    CompositePurchaseOrder compPo = respOrder.get(0).mapTo(CompositePurchaseOrder.class);
    List<JsonObject> respLines =  MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.PUT);

    CompositePoLine respLine1 = respLines.stream()
      .filter(line -> line.getString(ID).equals(line1.getId()))
      .peek(line -> line.remove("reportingCodes"))
      .map(line -> line.mapTo(CompositePoLine.class))
      .distinct().findAny().get();

    CompositePoLine respLine2 = respLines.stream()
      .filter(line -> line.getString(ID).equals(line2.getId()))
      .peek(line -> line.remove("reportingCodes"))
      .map(line -> line.mapTo(CompositePoLine.class))
      .findAny().get();

    compPo.setCompositePoLines(List.of(respLine1, respLine2));

    // Check that search of the existing instances and items was done for each PO line
    List<JsonObject> instancesSearches = getInstancesSearches();
    assertNotNull(instancesSearches);
    assertNotNull(getItemsSearches());
    assertNotNull(getPieceSearches());
    assertEquals(polCount, instancesSearches.size());

    // Check that 2 new instances created and items created successfully only for first POL
    List<JsonObject> createdInstances = getCreatedInstances();
    List<JsonObject> createdPieces = getCreatedPieces();
    assertNotNull(createdInstances);
    assertNotNull(getCreatedItems());
    assertNotNull(createdPieces);
    assertEquals(polCount, createdInstances.size());

    List<JsonObject> items = joinExistingAndNewItems();

    // Check instance Ids not exist for polines
    verifyInstanceLinksNotCreatedForPoLine();

    // Verify pieces were created
    assertEquals(calculateTotalQuantity(compPo.getCompositePoLines().get(0))
      + calculateTotalQuantity(compPo.getCompositePoLines().get(1)) - 1, createdPieces.size());

    // Effectively remove non-processed locations with ID_FOR_INTERNAL_SERVER_ERROR to exclude them from
    // created pieces verification
    compPo.getCompositePoLines().forEach(poLine -> poLine.getLocations().removeIf(l -> {
      if (l.getLocationId().equals(ID_FOR_INTERNAL_SERVER_ERROR)) {
        if (poLine.getCost().getQuantityElectronic() != null) {
          poLine.getCost().setQuantityElectronic(poLine.getCost().getQuantityElectronic() - l.getQuantityElectronic());
        }
        if (poLine.getCost().getQuantityPhysical() != null) {
          poLine.getCost().setQuantityPhysical(poLine.getCost().getQuantityPhysical() - l.getQuantityPhysical());
        }
        return true;
      } else {
        return false;
      }
    }));

    verifyPiecesCreated(items, compPo.getCompositePoLines(), createdPieces);
  }

  private void verifyInstanceLinksNotCreatedForPoLine() {
    List<JsonObject> polUpdates = MockServer.getPoLineUpdates();
    assertNotNull(polUpdates);
    boolean instanceIdExists = false;
    for (JsonObject jsonObj : polUpdates) {
      PoLine line = jsonObj.mapTo(PoLine.class);
      if (StringUtils.isNotEmpty(getInstanceId(line))) {
        instanceIdExists = true;
        break;
      }
    }

    assertFalse(instanceIdExists, "The PO Line must NOT contain instance id");
  }

  @Test
  void testPutOrderByIdWithPoLinesInRequestAndNoPoLinesInStorage() throws Exception {
    logger.info("=== Test Put Order By Id with PO lines and without PO lines in order from storage ===");

    JsonObject reqData = getMockDraftOrder();
    String poNumber = reqData.getString(PO_NUMBER);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT));
    List<JsonObject> createdPoLines = MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.POST);
    assertEquals(createdPoLines.size(), reqData.getJsonArray(COMPOSITE_PO_LINES).size());
    createdPoLines.forEach(poLine -> assertEquals(poNumber + "-" + PO_LINE_NUMBER_VALUE, poLine.getString(PO_LINE_NUMBER)));
    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.PENDING));
  }

  @Test
  void testPutOrderByIdWithoutPoLinesInRequestDoesNotDeletePoLinesFromStorage() throws IOException {
    logger.info("=== Test Put Order By Id without PO lines doesn't delete lines from storage ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT));
    assertNull(MockServer.serverRqRs.get(PO_LINES_STORAGE, HttpMethod.DELETE));
  }

  @Test
  void testPutOrderByIdWith404InvalidId() {
    logger.info("=== Test Put Order By Id for 404 with Invalid Id or Order not found ===");

    JsonObject reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, "93f612a9-9a05-4eef-aac5-435be131454b"), reqData, APPLICATION_JSON, 404);
  }

  @Test
  void testPutOrderByIdWithInvalidOrderIdInBody() {
    logger.info("=== Test Put Order By Id for 422 with Invalid Id in the Order ===");

    CompositePurchaseOrder reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH).mapTo(CompositePurchaseOrder.class);
    reqData.setId(PO_ID_PENDING_STATUS_WITH_PO_LINES);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_FOR_FAILURE_CASE),
      JsonObject.mapFrom(reqData), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
  }

  @Test
  void testPutOrderByIdWithInvalidOrderIdInPol() {
    logger.info("=== Test Put Order By Id for 422 with invalid purchase order Id in the POL ===");

    CompositePurchaseOrder reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH).mapTo(CompositePurchaseOrder.class);
    reqData.getCompositePoLines().forEach(line -> line.setPurchaseOrderId(PO_ID_FOR_FAILURE_CASE));

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_PENDING_STATUS_WITH_PO_LINES),
      JsonObject.mapFrom(reqData), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(reqData.getCompositePoLines().size()));
    errors.getErrors().forEach(error -> {
      assertThat(error.getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
      assertThat(error.getParameters(), hasSize(1));
    });
  }

  @Test
  void testPutOrderInOpenStatusAddingNewLine() {
    logger.info("=== Test update Open order - add new line ===");

    validateUpdateRejectedForNonPendingOrder(PO_ID_OPEN_STATUS, ORDER_OPEN.getCode());
  }

  @Test
  void testPutOrderInClosedStatusAddingNewLine() {
    logger.info("=== Test update Closed order - add new line ===");

    validateUpdateRejectedForNonPendingOrder(PO_ID_CLOSED_STATUS, ORDER_CLOSED.getCode());
  }

  private void validateUpdateRejectedForNonPendingOrder(String orderId, String errorCode) {
    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, orderId).mapTo(CompositePurchaseOrder.class);
    if (reqData.getOngoing() != null) {
      reqData.setOrderType(CompositePurchaseOrder.OrderType.ONGOING);
    } else {
      reqData.setOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);
    }
    // Delete PO Line id emulating case when new PO Line is being added
    reqData.getCompositePoLines().forEach(line -> line.setId(null));

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 422).as(Errors.class);
    validatePoLineCreationErrorForNonPendingOrder(errorCode, errors, 4);
  }

  @Test
  void testPostOrdersWithMissingVendorId() throws IOException {
    logger.info("=== Test Post Order with missing Vendor Id ===");

    Errors resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, getMockData(ORDER_WITHOUT_VENDOR_ID),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(VENDOR_ID, resp.getErrors().get(0).getParameters().get(0).getKey());
    assertEquals(NULL, resp.getErrors().get(0).getParameters().get(0).getValue());
  }


  @Test
  @Disabled
    //
  void testGetOrdersNoParameters() {
    logger.info("=== Test Get Orders - With empty query ===");

    addMockEntry(PURCHASE_ORDER_STORAGE, getMinimalContentCompositePurchaseOrder().withId(UUID.randomUUID().toString()));
    addMockEntry(PURCHASE_ORDER_STORAGE, getMinimalContentCompositePurchaseOrder().withId(UUID.randomUUID().toString()));
    addMockEntry(PURCHASE_ORDER_STORAGE, getMinimalContentCompositePurchaseOrder().withId(UUID.randomUUID().toString()));

    final PurchaseOrderCollection purchaseOrders = verifySuccessGet(COMPOSITE_ORDERS_PATH, PurchaseOrderCollection.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
    assertThat(purchaseOrders.getTotalRecords(), is(3));

    List<String> queryParams = getQueryParams(PURCHASE_ORDER_STORAGE);
    assertThat(queryParams, hasSize(1));
    assertThat(queryParams.get(0), equalTo(NO_ACQ_UNIT_ASSIGNED_CQL));
  }

  @Test
  @Disabled
  void testGetOrdersWithParameters() {
    logger.info("=== Test Get Orders - With empty query ===");
    String sortBy = " sortBy poNumber";
    String queryValue = "poNumber==" + EXISTING_PO_NUMBER;
    String endpointQuery = String.format("%s?query=%s%s", COMPOSITE_ORDERS_PATH, queryValue, sortBy);
    final PurchaseOrderCollection purchaseOrders = verifySuccessGet(endpointQuery, PurchaseOrderCollection.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
    assertThat(purchaseOrders.getTotalRecords(), is(1));

    List<String> queryParams = getQueryParams(PURCHASE_ORDER_STORAGE);
    assertThat(queryParams, hasSize(1));
    String queryToStorage = queryParams.get(0);
    assertThat(queryToStorage, containsString("(" + queryValue + ")"));
    assertThat(queryToStorage, not(containsString(ACQUISITIONS_UNIT_IDS + "=")));
    assertThat(queryToStorage, containsString(NO_ACQ_UNIT_ASSIGNED_CQL));
    assertThat(queryToStorage, endsWith(sortBy));
  }

  @Test
  void testGetOrdersForUserAssignedToAcqUnits() {
    logger.info("=== Test Get Orders by query - user assigned to acqUnits ===");

    Headers headers = prepareHeaders(X_OKAPI_URL, NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID_WITH_ACQ_UNITS);
    verifyGet(COMPOSITE_ORDERS_PATH, headers, APPLICATION_JSON, 200);

    assertThat(MockServer.serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(PURCHASE_ORDER_STORAGE);
    assertThat(queryParams, hasSize(1));
    String queryToStorage = queryParams.get(0);
    assertThat(queryToStorage, containsString(ACQUISITIONS_UNIT_IDS + "%3D"));

    MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET)
      .get(0)
      .mapTo(AcquisitionsUnitMembershipCollection.class)
      .getAcquisitionsUnitMemberships()
      .forEach(member -> assertThat(queryToStorage, containsString(member.getAcquisitionsUnitId())));
  }

  @Test
  void testGetOrdersBadQuery() {
    logger.info("=== Test Get Orders by query - unprocessable query to emulate 400 from storage ===");

    String endpointQuery = String.format("%s?query=%s", COMPOSITE_ORDERS_PATH, BAD_QUERY);

    verifyGet(endpointQuery, APPLICATION_JSON, 400);

  }

  @Test
  void testGetOrdersInternalServerError() {
    logger.info("=== Test Get Orders by query - emulating 500 from storage ===");

    String endpointQuery = String.format("%s?query=%s", COMPOSITE_ORDERS_PATH, ID_FOR_INTERNAL_SERVER_ERROR);

    verifyGet(endpointQuery, APPLICATION_JSON, 500);

  }

  @Test
  void testCreatePoWithDifferentVendorStatus() throws Exception {

    logger.info("=== Test POST PO with vendor's status ===");

    CompositePurchaseOrder reqData = getPoWithVendorId(ACTIVE_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B);
    for (int i = 0; i < reqData.getCompositePoLines().size(); i++) {
      reqData.getCompositePoLines().get(i).setPoLineNumber("number-" + i);
    }
    MockServer.addMockTitles(reqData.getCompositePoLines());

    // Purchase order is OK
    Errors activeVendorActiveAccessProviderErrors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(Errors.class);
    assertThat(activeVendorActiveAccessProviderErrors.getErrors(), empty());

    // Internal mod-vendor error
//    reqData.setVendor(MOD_VENDOR_INTERNAL_ERROR_ID);
//    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
//    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
//    reqData.setPoNumber("268759");
//    Errors internalServerError = verifyPostResponseErrors(1, JsonObject.mapFrom(reqData).toString());
//    assertThat(internalServerError.getErrors().get(0).getCode(), equalTo(VENDOR_ISSUE.getCode()));
//    assertThat(internalServerError.getErrors().get(0).getAdditionalProperties().get(ERROR_CAUSE), notNullValue());
//
//    // Non-existed vendor
//    reqData.setVendor(NON_EXIST_VENDOR_ID);
//    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
//    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
//    Errors nonExistedVendorError = verifyPostResponseErrors(1, JsonObject.mapFrom(reqData).toString());
//    checkExpectedError(NON_EXIST_VENDOR_ID, nonExistedVendorError, 0, ORDER_VENDOR_NOT_FOUND, reqData, 0);
//
//    // Active vendor and non-existed access provider
//    reqData.setVendor(ACTIVE_VENDOR_ID);
//    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
//    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(NON_EXIST_ACCESS_PROVIDER_A);
//    Errors inactiveAccessProviderErrors = verifyPostResponseErrors(1, JsonObject.mapFrom(reqData).toString());
//    checkExpectedError(NON_EXIST_ACCESS_PROVIDER_A, inactiveAccessProviderErrors, 0, POL_ACCESS_PROVIDER_NOT_FOUND, reqData, 1);
//
//    // Inactive access provider
//    reqData.setVendor(ACTIVE_VENDOR_ID);
//    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
//    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
//    Errors nonExistedAccessProviderErrors = verifyPostResponseErrors(1, JsonObject.mapFrom(reqData).toString());
//    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, nonExistedAccessProviderErrors, 0, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);

    // Inactive vendor and inactive access providers
    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_B);
    Errors allInactiveErrors = verifyPostResponseErrors(3, JsonObject.mapFrom(reqData).toString());
    checkExpectedError(INACTIVE_VENDOR_ID, allInactiveErrors, 0, ORDER_VENDOR_IS_INACTIVE, reqData, 0);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, allInactiveErrors, 1, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_B, allInactiveErrors, 2, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);

  }

  @Test
  void testPutOrdersByIdToChangeStatusToOpenInactiveVendor() throws Exception {

    logger.info("=== Test Put Order By Id to change status of Order to Open for different vendor's status ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    for (int i = 0; i < reqData.getCompositePoLines().size(); i++) {
      reqData.getCompositePoLines().get(i).setPoLineNumber("number-" + i);
    }

    // Positive cases
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 204);

    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    preparePiecesForCompositePo(reqData);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 204);

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 204);

    // Negative cases
    // Non-existed vendor
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(NON_EXIST_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    Errors nonExistedVendorErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);
    checkExpectedError(NON_EXIST_VENDOR_ID, nonExistedVendorErrors, 0, ORDER_VENDOR_NOT_FOUND, reqData, 0);

    // Inactive access provider
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    Errors inactiveAccessProviderErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, inactiveAccessProviderErrors, 0, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);

    // Inactive vendor and inactive access providers
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_B);
    Errors allInactiveErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);
    checkExpectedError(INACTIVE_VENDOR_ID, allInactiveErrors, 0, ORDER_VENDOR_IS_INACTIVE, reqData, 0);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, allInactiveErrors, 1, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_B, allInactiveErrors, 2, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);
  }

  @Test
  void testPutOrderToChangeStatusToOpenVendorWithUnexpectedContent() throws Exception {

    logger.info("=== Test Put Order to change status of Order to Open - vendor with unexpected content ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Prepare order
    reqData.setVendor(VENDOR_WITH_BAD_CONTENT);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(VENDOR_WITH_BAD_CONTENT);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(2));

    int errorsWithParam = (int) errors
      .getErrors()
      .stream()
      .filter(error -> !error.getParameters().isEmpty())
      .count();

    assertThat(errorsWithParam, is(1));

    errors.getErrors().forEach(error -> {
      assertThat(error.getCode(), equalTo(VENDOR_ISSUE.getCode()));
      assertThat(error.getAdditionalProperties().get(ERROR_CAUSE), notNullValue());
      if (!error.getParameters().isEmpty()) {
        assertThat(error.getParameters(), hasSize(1));
        assertThat(error.getParameters().get(0).getKey(), equalTo(ID));
        assertThat(error.getParameters().get(0).getValue(), equalTo(VENDOR_WITH_BAD_CONTENT));
      }
    });
  }

  @Test
  void testPutOrderToChangeStatusToOpenWithOrganizationNotVendor() throws Exception {

    logger.info("=== Test Put Order to change status of Order to Open - organization which is not vendor ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Prepare order
    reqData.setVendor(ORGANIZATION_NOT_VENDOR);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ORGANIZATION_NOT_VENDOR);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));

    checkExpectedError(ORGANIZATION_NOT_VENDOR, errors, 0, ORGANIZATION_NOT_A_VENDOR, reqData, 0);

  }

  @Test
  void testPutOrderToAutomaticallyChangeStatusFromOpenToClosed() {

    logger.info("===  Test case when order status update is expected from Open to Closed ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setVendor(ACTIVE_VENDOR_ID);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    reqData.setReEncumber(false);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
    assertThat(purchaseOrder.getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.CLOSED));
    assertThat(purchaseOrder.getCloseReason(), notNullValue());
    assertThat(purchaseOrder.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));

  }

  @Test
  void testPostOrderToAutomaticallyChangeStatusFromOpenToClosed() {

    logger.info("===  Test case when order status update is expected from Open to Closed ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setPoNumberPrefix("TestP");
    reqData.setPoNumberSuffix("TestS");
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    CompositePurchaseOrder respData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(respData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    assertThat(respData.getCloseReason(), notNullValue());
    assertThat(respData.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), notNullValue());
    assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==On order", reqData.getCompositePoLines().get(0).getId()));
  }

  @Test
  void testPostOrderToAutomaticallyChangeStatusFromOpenToClosedNoItemsFound() {

    logger.info("===  Test case when order status update is expected from Open to Closed no On order items in inventory ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    reqData.getCompositePoLines().get(0).setId(ITEMS_NOT_FOUND);

    MockServer.addMockTitles(reqData.getCompositePoLines());
    addMockEntry(ITEM_RECORDS, new JsonObject());

    reqData.setVendor(ACTIVE_VENDOR_ID);

    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    CompositePurchaseOrder respData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(respData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    assertThat(respData.getCloseReason(), notNullValue());
    assertThat(respData.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), nullValue());

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==On order", reqData.getCompositePoLines().get(0).getId()));
  }

  @Test
  void testPutOrderWithoutPoLinesToChangeStatusFromOpenToClosedItemsHaveToBeUpdate() {
    logger.info("===  Test case when order status updated from Open to Closed no PO Lines in payload - related items have to be updated ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    List<CompositePoLine> poLines = reqData.getCompositePoLines();
    poLines.forEach(line -> {
      line.setPurchaseOrderId(PO_ID_OPEN_TO_BE_CLOSED);
      line.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
      addMockEntry(PO_LINES_STORAGE, line);
    });
    MockServer.addMockTitles(poLines);

    reqData.setCompositePoLines(Collections.emptyList());
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);
    reqData.setCloseReason(new CloseReason().withReason("Test"));

    reqData.setReEncumber(false);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
    assertThat(purchaseOrder.getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.CLOSED));
    assertThat(purchaseOrder.getCloseReason(), notNullValue());
    assertThat(purchaseOrder.getCloseReason().getReason(), equalTo("Test"));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), notNullValue());
    assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==On order", poLines.get(0).getId()));

  }

  @Test
  void testPutOrderWithoutPoLinesToReOpenItemsHaveToBeUpdate() {
    logger.info("===  Test case when order status updated from Closed to Open no PO Lines in payload - related items have to be updated ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_CLOSED_STATUS).mapTo(CompositePurchaseOrder.class);
    List<CompositePoLine> poLines = reqData.getCompositePoLines();
    poLines.forEach(line -> {
      line.setPurchaseOrderId(PO_ID_CLOSED_STATUS);
      line.setReceiptStatus(ReceiptStatus.FULLY_RECEIVED);
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.getEresource().setAccessProvider(ACTIVE_VENDOR_ID);
      addMockEntry(PO_LINES_STORAGE, line);
    });
    addMockEntry(PURCHASE_ORDER_STORAGE, reqData.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    MockServer.addMockTitles(poLines);

    reqData.setCompositePoLines(Collections.emptyList());
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    reqData.setReEncumber(false);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, REOPEN_PERMISSIONS_HEADER), EMPTY, 204);

    PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
    assertThat(purchaseOrder.getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.OPEN));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), notNullValue());
    assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==Order closed", poLines.get(0).getId()));

  }

  @Test
  void testPutOrderToAutomaticallyChangeStatusFromClosedToOpen() {

    logger.info("=== Test case when order status update is expected from Closed to Open ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_CLOSED_STATUS).mapTo(CompositePurchaseOrder.class);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));


    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.OPEN));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), notNullValue());
    assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==Order closed", reqData.getCompositePoLines().get(0).getId()));
  }

  @Test
  void testUpdateOrderWithoutPoLineToWithClosedStatus() {
    logger.info("===  Test Put Order to change status of Order from Pending to Closed without PO Lines - no items searches and updates ===");

    CompositePurchaseOrder reqData = getMinimalContentCompositePurchaseOrder();
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.CLOSED));

    assertThat(getItemsSearches(), nullValue());
    assertThat(getItemUpdates(), nullValue());
    assertThat(getQueryParams(ITEM_RECORDS), hasSize(0));
  }

  @Test
  void testUpdateNotRequiredForOpenOrder() {
    logger.info("=== Test case when no workflowStatus update is expected for Open order ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_STATUS).mapTo(CompositePurchaseOrder.class);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));


    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.OPEN));
  }

  @Test
  void testNoUpdatesForPendingOrderWithLines() {
    logger.info("=== Test case when no order update is expected for Pending order with a few PO Lines ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_PENDING_STATUS_WITH_PO_LINES).mapTo(CompositePurchaseOrder.class);
    reqData.setAssignedTo(null);
    reqData.setId(PO_ID_PENDING_STATUS_WITH_PO_LINES);
    reqData.getCompositePoLines().forEach(poLine -> poLine.setPurchaseOrderId(PO_ID_PENDING_STATUS_WITH_PO_LINES));
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));


    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.PENDING));
  }

  @Test
  void testUpdateOrderWithProtectedFieldsChanging() {
    logger.info("=== Test case when OPEN order errors if protected fields are changed ===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.OPEN.value()));

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(POProtectedFields.APPROVED.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.PO_NUMBER.getFieldName(), "testPO");
    allProtectedFieldsModification.put(POProtectedFields.MANUAL_PO.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.RE_ENCUMBER.getFieldName(), true);
    checkPreventProtectedFieldsModificationRule(COMPOSITE_ORDERS_BY_ID_PATH, reqData, allProtectedFieldsModification);
  }


  @Test
  void testUpdateOrderCloseOrderWithCloseReason() {
    logger.info("=== Test case: Able to Close Order with close Reason ===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.OPEN.value()));
    reqData.put("workflowStatus", "Closed");
    // close reason must not be checked if the Order is in OPEN status in storage
    CloseReason closeReason = new CloseReason();
    closeReason.setNote("can set close reason");
    closeReason.setReason("Complete");
    reqData.put("closeReason", JsonObject.mapFrom(closeReason));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getString("id")), JsonObject.mapFrom(reqData), "", 204);
  }

  @Test
  void testUpdateOrderCloseOrderWithCloseReasonAndFundDistribution() {
    logger.info("=== Test case: Able to Close Order with close Reason and fund distribution ===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_WFD_ID_OPEN_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.OPEN.value()));
    reqData.put("workflowStatus", "Closed");
    // close reason must not be checked if the Order is in OPEN status in storage
    CloseReason closeReason = new CloseReason();
    closeReason.setNote("can set close reason");
    closeReason.setReason("Complete");
    reqData.put("closeReason", JsonObject.mapFrom(closeReason));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getString("id")), JsonObject.mapFrom(reqData), "", 204);
  }

  @Test
  void testCancelOrder() {
    logger.info("=== Test case: Cancel order ===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_CANCEL);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.OPEN.value()));
    reqData.put("workflowStatus", "Closed");
    CloseReason closeReason = new CloseReason();
    closeReason.setReason("Cancelled");
    reqData.put("closeReason", JsonObject.mapFrom(closeReason));
    reqData.remove("compositePoLines");

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getString("id")), JsonObject.mapFrom(reqData), "", 204);
    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.CLOSED));
    List<JsonObject> polUpdates = MockServer.getPoLineUpdates();
    assertNotNull(polUpdates);
    // check the payment and receipt status of the last 3 updated polines
    JsonObject line1 = polUpdates.stream().filter(pol -> EMPTY_RECEIPT_POL_UUID.equals(pol.getString(HelperUtils.ID))).findFirst().get();
    assertEquals(ReceiptStatus.CANCELLED.value(), line1.getString(RECEIPT_STATUS));
    assertEquals(CompositePoLine.PaymentStatus.CANCELLED.value(), line1.getString(PAYMENT_STATUS));
    JsonObject line2 = polUpdates.stream().filter(pol -> AWAITING_RECEIPT_POL_UUID.equals(pol.getString(HelperUtils.ID))).findFirst().get();
    assertEquals(ReceiptStatus.CANCELLED.value(), line2.getString(RECEIPT_STATUS));
    assertEquals(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED.value(), line2.getString(PAYMENT_STATUS));
    JsonObject line3 = polUpdates.stream().filter(pol -> FULLY_RECEIVED_POL_UUID.equals(pol.getString(HelperUtils.ID))).findFirst().get();
    assertEquals(ReceiptStatus.FULLY_RECEIVED.value(), line3.getString(RECEIPT_STATUS));
    assertEquals(CompositePoLine.PaymentStatus.CANCELLED.value(), line3.getString(PAYMENT_STATUS));
  }

  @Test
  void testUpdateOrderWithLineProtectedFieldsChanging() {
    logger.info("=== Test case when OPEN order errors if protected fields are changed in CompositePoLine===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.OPEN.value()));

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(COMPOSITE_PO_LINES_PREFIX.concat(POLineFieldNames.CHECKIN_ITEMS.getFieldName()), true);
    allProtectedFieldsModification.put(COMPOSITE_PO_LINES_PREFIX.concat(POLineFieldNames.ACQUISITION_METHOD.getFieldName()),
      TestUtils.DEPOSITORY_METHOD);
    allProtectedFieldsModification.put(COMPOSITE_PO_LINES_PREFIX.concat(POLineFieldNames.ERESOURCE_USER_LIMIT.getFieldName()),
      100);

    checkPreventProtectedFieldsModificationRule(COMPOSITE_ORDERS_BY_ID_PATH, reqData, allProtectedFieldsModification);
  }

  @Test
  void testUpdateOrderWithProtectedFieldsChangingForClosedOrder() {
    logger.info("=== Test case when closed order errors if protected fields are changed ===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_CLOSED_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.CLOSED.value()));

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(POProtectedFields.APPROVED.getFieldName(), false);
    allProtectedFieldsModification.put(POProtectedFields.PO_NUMBER.getFieldName(), "testPO");
    allProtectedFieldsModification.put(POProtectedFields.MANUAL_PO.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.RE_ENCUMBER.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.ORDER_TYPE.getFieldName(),
      CompositePurchaseOrder.OrderType.ONE_TIME.value());
    CloseReason closeReason = new CloseReason();
    closeReason.setNote("testing reason on Closed Order");
    closeReason.setReason("Complete");
    allProtectedFieldsModification.put(POProtectedFields.CLOSE_REASON.getFieldName(), JsonObject.mapFrom(closeReason));

    checkPreventProtectedFieldsModificationRule(COMPOSITE_ORDERS_BY_ID_PATH, reqData, allProtectedFieldsModification);
  }

  @Test
  void testPostOpenOrderCacheKayMustBeTenantSpecific() throws Exception {

    MockServer.serverRqRs.clear();
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat( reqData.getCompositePoLines(), hasSize(1));

    Headers headers = prepareHeaders(new Header(OKAPI_HEADER_TENANT, "existReferenceData"), X_OKAPI_USER_ID);

    //Create order first time for tenant, no contributor name type in cache
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);

    assertThat(getLoanTypesSearches().size() > 0, is(true));
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));
    clearServiceInteractions();

    //Create order second time for tenant, cache contains contributor name type for this tenant
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);


    assertThat(getLoanTypesSearches(), nullValue());
    assertThat(getInstanceStatusesSearches(), nullValue());
    assertThat(getInstanceTypesSearches(), nullValue());
    clearServiceInteractions();

    // Prepare X-Okapi-Tenant heander for tenant which has no contributor name type
    headers = prepareHeaders(new Header(OKAPI_HEADER_TENANT, "anotherExistReferenceData"), X_OKAPI_USER_ID);

    //Create order for another tenant, no contributor name type in cache for this tenant
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);

    assertThat(getLoanTypesSearches().size() > 0, is(true));
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));

  }

  @Test
  void testInventoryHelperCacheDoesNotKeepEmptyValuesWhenFails() throws Exception {

    MockServer.serverRqRs.clear();
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat( reqData.getCompositePoLines(), hasSize(1));

    Headers headers = prepareHeaders(NON_EXIST_INSTANCE_STATUS_TENANT_HEADER, X_OKAPI_USER_ID, APPROVAL_PERMISSIONS_HEADER);

    //Create order first time for tenant without instanceStatus, no instanceStatus in cache, so logic should call get /instance-types
    Error err = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 500).getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);

    assertThat(getLoanTypesSearches(), nullValue());
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));

    assertThat(err.getCode(), equalTo(ErrorCodes.MISSING_INSTANCE_STATUS.getCode()));
    assertThat(err.getMessage(), equalTo(ErrorCodes.MISSING_INSTANCE_STATUS.getDescription()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(DEFAULT_INSTANCE_STATUS_CODE));
    clearServiceInteractions();

    //Create order second time for same tenant, still no instanceStatus in cache, logic should call get /instance-types again
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 500);

    assertThat(getLoanTypesSearches(), nullValue());
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), nullValue());

  }

  @Test
  void testInventoryHelperCacheContainsDifferentValuesForInstanceTypeAndInstanceStatusWithSameCode() throws Exception {

    MockServer.serverRqRs.clear();
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat(reqData.getCompositePoLines(), hasSize(1));
    Headers headers = prepareHeaders(INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT_HEADER, X_OKAPI_USER_ID);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);

    assertThat(getLoanTypesSearches().size() > 0, is(true));
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));
    clearServiceInteractions();
    MockServer.addMockTitles(reqData.getCompositePoLines());
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);

    assertThat(getLoanTypesSearches(), nullValue());
    assertThat(getInstanceStatusesSearches(), nullValue());
    assertThat(getInstanceTypesSearches(), nullValue());
    assertThat(getCreatedInstances(), hasSize(1));
    JsonObject instance = getCreatedInstances().get(0);
    String instanceStatusId = instance.getString(INSTANCE_STATUS_ID);
    String instanceTypeId = instance.getString(INSTANCE_TYPE_ID);
    assertThat(instanceStatusId, not(equalTo(instanceTypeId)));

  }

  @Test
  void testInventoryHelperEmptyInstanceTypeThrowsProperError() throws Exception {
    Error err = verifyMissingInventoryEntryErrorHandling(NON_EXIST_INSTANCE_TYPE_TENANT_HEADER);

    assertThat(err.getCode(), equalTo(ErrorCodes.MISSING_INSTANCE_TYPE.getCode()));
    assertThat(err.getMessage(), equalTo(ErrorCodes.MISSING_INSTANCE_TYPE.getDescription()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(DEFAULT_INSTANCE_TYPE_CODE));
  }

  @Test
  void testInventoryHelperEmptyContributors() throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    reqData.getCompositePoLines().get(0).getContributors().clear();

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), headers, APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    int polCount = resp.getCompositePoLines().size();
    CompositePoLine respLine1 = resp.getCompositePoLines().get(0);
    List<JsonObject> createdInstances = getCreatedInstances();
    assertEquals(1, createdInstances.size(), "Quantity of created instance must be equal of line, if create inventory include instance");
    assertNotNull("Line must be connected to instance, if create inventory include instance", respLine1.getInstanceId());

    List<JsonObject> createdHoldings = getCreatedHoldings();
    assertEquals(3, createdHoldings.size(), "Quantity of created instance must be depended of quantity in the locations and create inventory include holding");
    verifyHoldingsCreated(3, createdHoldings, respLine1);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();
    verifyItemsCreated(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, 4, items, respLine1);

    List<JsonObject> createdPieces = getCreatedPieces();
    verifyOpenOrderPiecesCreated(items, resp.getCompositePoLines(), createdPieces, 0);
  }


  @Test
  void testInventoryHelperMissingContributorNameTypeThrowsProperError() throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines()
      .remove(1);
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    reqData.getCompositePoLines().get(0).getContributors().add(new Contributor().withContributor("Test").withContributorNameTypeId(ID_DOES_NOT_EXIST));

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID);

    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), headers, APPLICATION_JSON, 500);

    int expectedContributorNameTypesSearches = Math.toIntExact(reqData.getCompositePoLines().get(0).getContributors().stream()
      .map(Contributor::getContributorNameTypeId)
      .distinct()
      .count() / MAX_IDS_FOR_GET_RQ + 1);

    assertThat(getContributorNameTypesSearches(), hasSize(expectedContributorNameTypesSearches));

    Error err = resp.getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);

    assertThat(err.getCode(), equalTo(ErrorCodes.MISSING_CONTRIBUTOR_NAME_TYPE.getCode()));
    assertThat(err.getMessage(), equalTo(ErrorCodes.MISSING_CONTRIBUTOR_NAME_TYPE.getDescription()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(ID_DOES_NOT_EXIST));
  }

  @Test
  void testInventoryHelperEmptyLoanTypeThrowsProperError() throws Exception {

    Error err = verifyMissingInventoryEntryErrorHandling(NON_EXIST_LOAN_TYPE_TENANT_HEADER);

    assertThat(err.getCode(), equalTo(ErrorCodes.MISSING_LOAN_TYPE.getCode()));
    assertThat(err.getMessage(), equalTo(ErrorCodes.MISSING_LOAN_TYPE.getDescription()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(DEFAULT_LOAN_TYPE_NAME));
  }

  @Test
  void testPostOrdersWithInvalidIsbn() throws Exception {
    logger.info("=== Test Post Order with invalid ISBN ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(reqData);
    String isbn = "1234";

    reqData.getCompositePoLines().get(0).getDetails().getProductIds().get(0).setProductId(isbn);


    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 400);

    Error err = resp.getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);

    assertThat(err.getMessage(), equalTo(ISBN_NOT_VALID.getDescription()));
    assertThat(err.getCode(), equalTo(ISBN_NOT_VALID.getCode()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(isbn));
  }

  @Test
  void testPostOrdersToConvertToIsbn13() throws Exception {
    logger.info("=== Test Post order to verify ISBN 10 is normalized to ISBN 13 ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(reqData);
    String isbn = "0-19-852663-6";

    reqData.getCompositePoLines().get(0).getDetails().getProductIds().get(0).setProductId(isbn);
    CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(resp.getCompositePoLines().get(0).getDetails().getProductIds().get(0).getProductId(), equalTo("9780198526636"));
  }

  @Test
  void testPostOrdersToIgnoreNextPolNumber() throws Exception {
    logger.info("=== Test Post order to verify nextPolNumber is ignored ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(reqData);

    CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(resp.getCompositePoLines().get(0).getPoLineNumber(), equalTo("268758-1"));
  }

  @Test
  void testPutOrdersWithInvalidIsbn() throws Exception {
    logger.info("=== Test Put Order with invalid ISBN ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    String isbn = "1234";

    reqData.getCompositePoLines().get(0).getDetails().getProductIds().get(0).setProductId(isbn);

    Response resp = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 400);

    Error err = resp.getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);


    assertThat(err.getMessage(), equalTo(ISBN_NOT_VALID.getDescription()));
    assertThat(err.getCode(), equalTo(ISBN_NOT_VALID.getCode()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(isbn));
  }

  @Test
  void testPutOrdersToConvertToIsbn13() throws Exception {
    logger.info("=== Test Put order to verify ISBN 10 is normalized to ISBN 13 ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    String isbn = "0-19-852663-6";

    reqData.getCompositePoLines().remove(1);
    reqData.getCompositePoLines().get(0).getDetails().getProductIds().get(0).setProductId(isbn);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData)
      .encodePrettily(), "", 204);

    assertThat(MockServer.getPoLineUpdates().get(0).mapTo(PoLine.class).getDetails().getProductIds().get(0).getProductId(), equalTo("9780198526636"));
  }


  @Test
  void testPostOrderWithUserNotHavingApprovalPermissions() {
    logger.info("===  Test case when approval is required and user does not have required permission===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_USER_ID), APPLICATION_JSON, 403);

    Error err = resp.getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);

    assertThat(err.getCode(), equalTo(ErrorCodes.USER_HAS_NO_APPROVAL_PERMISSIONS.getCode()));
  }

  @Test
  void testPostOrderWithHonorUserUnopenPermissions() {
    logger.info("===  Test case when unopen permission is required and user does not have required permission===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, "c1465131-ed35-4308-872c-d7cdf0afc5f7")
      .mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));
    reqData.setWorkflowStatus(WorkflowStatus.PENDING);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 403).as(Errors.class);

    assertEquals(ErrorCodes.USER_HAS_NO_UNOPEN_PERMISSIONS.getCode(), errors.getErrors().get(0).getCode());

    // set unopen permission header
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, UNOPEN_PERMISSIONS_HEADER), "", 204);
  }

  @Test
  void testPutOrderHonorsUserReopenPermissions() {
    logger.info("===  Test case when reopen permission is required and user does not have required permission===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_CLOSED_STATUS).mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    List<CompositePoLine> poLines = reqData.getCompositePoLines();
    poLines.forEach(line -> line.getEresource().setAccessProvider(ACTIVE_VENDOR_ID));
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    reqData.setWorkflowStatus(WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 403).as(Errors.class);

    assertEquals(ErrorCodes.USER_HAS_NO_REOPEN_PERMISSIONS.getCode(), errors.getErrors().get(0).getCode());

    // set unopen permission header
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, REOPEN_PERMISSIONS_HEADER), "", 204);
  }

  @Test
  void testPutOrderHonorsUserReopenPermissionsWithOngoingSubscription() {
    logger.info("===  Test case when reopen permission is required and user does not have required permission===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_CLOSED_STATUS_WITH_ONGOING).mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    List<CompositePoLine> poLines = reqData.getCompositePoLines();
    poLines.forEach(line -> line.getEresource().setAccessProvider(ACTIVE_VENDOR_ID));
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    reqData.setWorkflowStatus(WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 403).as(Errors.class);

    assertEquals(ErrorCodes.USER_HAS_NO_REOPEN_PERMISSIONS.getCode(), errors.getErrors().get(0).getCode());

    // set unopen permission header
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, REOPEN_PERMISSIONS_HEADER), "", 204);
  }

  @Test
  void testPostOrderToFailOnNonApprovedOrder() {
    logger.info("===  Test case when approval is required to open order and order not approved ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setApproved(false);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_USER_ID, APPROVAL_PERMISSIONS_HEADER), APPLICATION_JSON, 400);

    Error err = resp.getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);

    assertThat(err.getCode(), equalTo(ErrorCodes.APPROVAL_REQUIRED_TO_OPEN.getCode()));
  }

  @Test
  void testPostOrderToSetRequiredFieldsOnApproval() {
    logger.info("===  Test required Fields are set/not set based on approval required field===");

    // -- test config set to "isApprovalRequired":true, approval details are set when order is approved in PENDING state

    CompositePurchaseOrder pendingData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES)
      .mapTo(CompositePurchaseOrder.class);
    pendingData.setVendor(ACTIVE_VENDOR_ID);
    pendingData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    assertThat(pendingData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));

    CompositePurchaseOrder responseData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(pendingData)
        .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_USER_ID, APPROVAL_PERMISSIONS_HEADER),
      APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(responseData.getApprovalDate(), notNullValue());
    assertThat(responseData.getApprovedById(), notNullValue());
    assertThat(responseData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));


    // -- test config set to "isApprovalRequired":false, approval details are not set when order is approved in PENDING state

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES)
      .mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));
    MockServer.serverRqRs.clear();
    CompositePurchaseOrder respData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
        .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, APPROVAL_PERMISSIONS_HEADER),
      APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(respData.getApprovalDate(), nullValue());
    assertThat(respData.getApprovedById(), nullValue());
    assertThat(respData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));
  }


  @Test
  void testPostOrderApprovalNotRequired() {
    logger.info("===  Test case when approval is not required to open order, and order not approved, set required Fields when opening Order ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setApproved(false);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    CompositePurchaseOrder respData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(respData.getApprovalDate(), notNullValue());
    assertThat(respData.getApprovedById(), notNullValue());
  }

  @Test
  void testPutOrderWithUserNotHavingApprovalPermissions() {
    logger.info("=== Test PUT PO, with User not having Approval permission==");

    CompositePurchaseOrder reqData = getMockAsJson(PE_MIX_PATH).mapTo(CompositePurchaseOrder.class);
    reqData.setApproved(true);
    Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_TOKEN, X_OKAPI_USER_ID);
    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, PENDING_ORDER_APPROVED_FALSE);
    verifyPut(url, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 403).body().as(Errors.class);
  }

  @Test
  void testPostOrdersInventoryInteractionWithReceiptNotRequired() throws Exception {
    logger.info("=== Test POST electronic PO, to create Instance and Holding even if receipt not required==");

    JsonObject order = new JsonObject(getMockData(ELECTRONIC_FOR_CREATE_INVENTORY_TEST));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order is Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Set CreateInventory value to create inventory instances and holdings
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    reqData.getCompositePoLines().get(0).setReceiptStatus(ReceiptStatus.RECEIPT_NOT_REQUIRED);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getCreatedInstances());
    assertNotNull(getCreatedHoldings());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  void testPostOrdersInventoryInteractionWithPackagePoLine() throws Exception {
    logger.info("=== Test POST electronic PO, to no interaction with inventory if poLine.isPackage=true ==");

    JsonObject order = new JsonObject(getMockData(ELECTRONIC_FOR_CREATE_INVENTORY_TEST));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order is Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    reqData.getCompositePoLines().get(0).setIsPackage(true);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNull(getCreatedInstances());
    assertNull(getCreatedHoldings());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  void testPostOrdersNoInventoryInteractionWithReceiptNotRequired() throws Exception {
    logger.info("=== Test POST PO, to have no inventory Interaction, with CreateInventory None and receipt not required==");

    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create nothing in inventory
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).setReceiptStatus(ReceiptStatus.RECEIPT_NOT_REQUIRED);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    // No inventory interation necessary with "Receipt Not Required" and CreateInventory "None"
    assertNull(getInstancesSearches());
    assertNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  void testPostOpenOrderWithEncumbranceCreationError() throws Exception {
    logger.info("=== Test Placement of minimal order ===");

    MockServer.serverRqRs.clear();
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat( reqData.getCompositePoLines(), hasSize(1));


    Header errorHeader = new Header(OKAPI_HEADER_TENANT, FUND_CANNOT_BE_PAID_TENANT);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(errorHeader, X_OKAPI_USER_ID), APPLICATION_JSON, 422);
    MockServer.serverRqRs.clear();
    errorHeader = new Header(OKAPI_HEADER_TENANT, BUDGET_IS_INACTIVE_TENANT);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(errorHeader, X_OKAPI_USER_ID), APPLICATION_JSON, 422);
    MockServer.serverRqRs.clear();
    errorHeader = new Header(OKAPI_HEADER_TENANT, LEDGER_NOT_FOUND_FOR_TRANSACTION_TENANT);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(errorHeader, X_OKAPI_USER_ID), APPLICATION_JSON, 422);
    MockServer.serverRqRs.clear();
    errorHeader = new Header(OKAPI_HEADER_TENANT, BUDGET_NOT_FOUND_FOR_TRANSACTION_TENANT);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(errorHeader, X_OKAPI_USER_ID), APPLICATION_JSON, 422);

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


  private Error verifyMissingInventoryEntryErrorHandling(Header header) throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.getCompositePoLines()
      .remove(1);
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    Headers headers = prepareHeaders(header, X_OKAPI_USER_ID, APPROVAL_PERMISSIONS_HEADER);

    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), headers, APPLICATION_JSON, 500);

    int expectedContributorNameTypesSearches = Math.toIntExact(reqData.getCompositePoLines().get(0).getContributors().stream()
      .map(Contributor::getContributorNameTypeId)
      .distinct()
      .count() / MAX_IDS_FOR_GET_RQ + 1);

    assertThat(getContributorNameTypesSearches(), hasSize(expectedContributorNameTypesSearches));
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));

    return resp.getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);
  }

  private Errors verifyPostResponseErrors(int expectedErrorsNumber, String body) {
    Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(errors.getTotalRecords(), equalTo(expectedErrorsNumber));
    assertThat(errors.getErrors(), hasSize(expectedErrorsNumber));
    return errors;
  }


  private CompositePurchaseOrder getPoWithVendorId(String vendorId, String... accessProviderIds) throws Exception {
    CompositePurchaseOrder comPo = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    comPo.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    comPo.setVendor(vendorId);
    for(int i = 0; i < accessProviderIds.length; i++) {
      comPo.getCompositePoLines().get(i).getEresource().setAccessProvider(accessProviderIds[i]);
    }
    return comPo;
  }

  private void checkExpectedError(String id, Errors errors, int index, ErrorCodes expectedErrorCodes, CompositePurchaseOrder purchaseOrder, int expectedPoLineEntriesinErrors) {
    Error error = errors.getErrors().get(index);
    assertThat(error.getCode(), equalTo(expectedErrorCodes.getCode()));
    assertThat(error.getMessage(), equalTo(expectedErrorCodes.getDescription()));
    assertThat(error.getParameters(), hasSize(expectedPoLineEntriesinErrors + 1));
    assertThat(error.getParameters().get(0).getKey(), equalTo(ID));
    assertThat(error.getParameters().get(0).getValue(), equalTo(id));
    if(expectedPoLineEntriesinErrors > 0) {
      List<String> poLineNumbersFromError = error.getParameters().stream().filter(p -> p.getKey().equals("poLineNumber")).map(Parameter::getValue).collect(toList());
      List<String> poLineNumbers = purchaseOrder.getCompositePoLines().stream().map(CompositePoLine::getPoLineNumber).collect(toList());
      poLineNumbersFromError.forEach(p -> assertThat(poLineNumbers.contains(p), is(true)));
    }
  }

  @Test
  void testPostShouldBeSuccessIfOrderInPendingStatusAndMaterialTypeIsAbsentInOrderLine() {
    logger.info("===Test Post Successful test completion if material type is absent and Order in PENDING status ===");
    CompositePurchaseOrder reqData = getMockAsJson(ORDER_WITHOUT_MATERIAL_TYPE_JSON).mapTo(CompositePurchaseOrder.class);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201);
  }

  @Test
  void testPostShouldFailedIfOrderIsNotInPendingStatusAndMaterialTypeIsAbsentInOrderLine() {
    logger.info("===Test Post Failed test completion if material type is absent and Order in OPEN status ===");
    CompositePurchaseOrder reqData = getMockAsJson(ORDER_WITHOUT_MATERIAL_TYPE_JSON).mapTo(CompositePurchaseOrder.class);

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    List<Error> errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(MISSING_MATERIAL_TYPE.getDescription()));
  }

  @Test
  void testPutShouldFailedIfOrderTransitFromPendingToOpenStatusAndMaterialTypeIsAbsentInOrderLine() {
    logger.info("===Test Success Post Failed test completion if material type is absent and Order in PENDING status");
    CompositePurchaseOrder reqData = getMockAsJson(ORDER_WITHOUT_MATERIAL_TYPE_JSON).mapTo(CompositePurchaseOrder.class);
    reqData.getCompositePoLines().forEach(poLine -> {
      removeMaterialType(poLine);
      poLine.setId(UUID.randomUUID().toString());
    });

    CompositePurchaseOrder po = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201)
      .as(CompositePurchaseOrder.class);

    CompositePurchaseOrder putReq = prepareCompositeOrderOpenRequest(po);
    putReq.setCompositePoLines(reqData.getCompositePoLines());
    preparePiecesForCompositePo(putReq);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);


    List<Error> errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_WITHOUT_MATERIAL_TYPES_ID), JsonObject.mapFrom(putReq)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(MISSING_MATERIAL_TYPE.getDescription()));
  }

  @Test
  void testPostShouldFailIfClaimingConfigNotValid() {
    logger.info("===Test Post Failed if claiming active but claiming interval is negative ===");
    CompositePurchaseOrder reqData = getMockAsJson(ORDER_WITH_NOT_VALID_CLAIMING_CONFIG).mapTo(CompositePurchaseOrder.class);

    List<Error> errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(CLAIMING_CONFIG_INVALID.getDescription()));
  }

  @Test
  void testReopenOrderUnreleasesEncumbrancesUnlessInvoiceLineHasReleaseEncumbrance(VertxTestContext vertxTestContext) {
    logger.info("=== Check encumbrances are unreleased when an order is reopened, except for po lines having a linked invoice line with releaseEncumbrance = true ===");

    String purchaseOrderId = "0fb18568-cf8d-442b-b74a-cad7cfa557a0";
    String poLineId1 = "0285a6b6-6693-4ea9-83e1-41d227063d88";
    String transactionId = "41fe8202-f038-4000-a969-4eee74ba8735";
    String fiscalYearId = "ac2164c7-ba3d-1bc2-a12c-e35ceccbfaf2";
    String fundId = "a89eccf0-57a6-495e-898d-32b9b2210f2f";

    List<String> transactionIds = Arrays.asList(transactionId);
    Cost cost = new Cost().withCurrency("USD").withListUnitPrice(10.00).withQuantityElectronic(1);
    FundDistribution fundDistribution = new FundDistribution().withFundId(VALID_FUND_ID)
      .withDistributionType(DistributionType.PERCENTAGE).withValue(100.00).withEncumbrance("eb506834-6c70-4239-8d1a-6414a5b08008");
    CompositePoLine poLines = new CompositePoLine().withId(poLineId1)
      .withOrderFormat(OrderFormat.PHYSICAL_RESOURCE)
      .withPoLineNumber("10233-1")
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.NONE))
      .withPurchaseOrderId(purchaseOrderId).withAcquisitionMethod(TestUtils.PURCHASE_METHOD)
      .withCollection(true).withCost(cost).withFundDistribution(Arrays.asList(fundDistribution));
    CompositePurchaseOrder reqData = new CompositePurchaseOrder().withId(purchaseOrderId).withApproved(true)
      .withPoNumber("S60402").withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME)
      .withVendor("d0fb5aa0-cdf1-11e8-a8d5-f2801f1b9fd1").withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    Encumbrance encumbrance = new Encumbrance().withOrderType(Encumbrance.OrderType.ONE_TIME)
      .withStatus(Encumbrance.Status.UNRELEASED).withSourcePoLineId(poLineId1).withReEncumber(false)
      .withSubscription(false).withSourcePurchaseOrderId(purchaseOrderId).withInitialAmountEncumbered(10.00)
      .withAmountAwaitingPayment(0.0).withAmountExpended(0.0);
    Metadata metadata = new Metadata().withCreatedByUserId("00000001-1111-5555-9999-999999999999")
      .withUpdatedByUsername("00000001-1111-5555-9999-999999999999").withCreatedDate(new Date(2020, 05, 29, 11, 30))
      .withUpdatedDate(new Date(2020, 05, 30, 11, 30));
    Transaction transaction = new Transaction().withId(transactionId).withAmount(10.0).withSource(Transaction.Source.PO_LINE)
      .withCurrency("USD").withToFundId(fundId).withEncumbrance(encumbrance).withFiscalYearId(fiscalYearId)
      .withTransactionType(Transaction.TransactionType.ENCUMBRANCE).withMetadata(metadata);
    MockServer.addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLines));
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(reqData));

    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    reqData.setWorkflowStatus(WorkflowStatus.OPEN);

    // NOTE: permissions are checked in another test
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, REOPEN_PERMISSIONS_HEADER), "", 204);

    // check the order has been reopened, the first encumbrance has been unreleased but not the second one
    PurchaseOrder po = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
    assertThat(po.getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.OPEN));

    List<Transaction> transactions = List.of(transaction);
    TransactionCollection transactionCollection = new TransactionCollection()
      .withTransactions(List.of(transaction))
      .withTotalRecords(1);
    doReturn(succeededFuture())
      .when(transactionService).batchUpdate(eq(transactions), eq(requestContext));
    doReturn(succeededFuture(transactionCollection))
      .when(restClient).get(eq(requestEntry), eq(TransactionCollection.class), eq(requestContext));
    doReturn(succeededFuture(transactions))
      .when(transactionService).getTransactionsByIds(eq(transactionIds), eq(requestContext));

    Future<List<Transaction>> future = encumbranceService.getEncumbrancesByIds(transactionIds, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertThat(result.result().size(), equalTo(1));
        Transaction updatedEncumbrance1 = result.result().get(0);
        assertThat(updatedEncumbrance1.getEncumbrance().getSourcePoLineId(), equalTo(poLineId1));
        assertThat(updatedEncumbrance1.getEncumbrance().getStatus(), equalTo(Encumbrance.Status.UNRELEASED));
        vertxTestContext.completeNow();
      });

  }

  @Test
  void testCreateOrderPopulateSearchLocationIds() throws IOException {
    logger.info("=== Check that searchLocationIds array is populated from locationIds array ===");
    String body = getMockData(ORDER_WITH_PO_LINES_JSON);
    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    resp.getCompositePoLines().forEach(line -> {
      assertEquals(line.getLocations().size(), line.getSearchLocationIds().size());
      assertEquals(line.getLocations().get(0).getLocationId(), line.getSearchLocationIds().get(0));
    });
  }

  private CompositePurchaseOrder prepareCompositeOrderOpenRequest(CompositePurchaseOrder po) {
    CompositePurchaseOrder compositeOrderPutRequest = new CompositePurchaseOrder();
    compositeOrderPutRequest.setId(ORDER_WITHOUT_MATERIAL_TYPES_ID);
    compositeOrderPutRequest.setOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);
    compositeOrderPutRequest.setOrderType(po.getOrderType());
    compositeOrderPutRequest.setApproved(false);
    compositeOrderPutRequest.setPoNumber(po.getPoNumber());
    compositeOrderPutRequest.setTotalEstimatedPrice(po.getTotalEstimatedPrice());
    compositeOrderPutRequest.setTotalItems(po.getTotalItems());
    compositeOrderPutRequest.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    compositeOrderPutRequest.setVendor(po.getVendor());
    compositeOrderPutRequest.setMetadata(po.getMetadata());
    compositeOrderPutRequest.setCompositePoLines(po.getCompositePoLines());

    return compositeOrderPutRequest;
  }

  private void removeMaterialType(CompositePoLine poLine) {
    if (poLine.getPhysical() != null)
      poLine.getPhysical().setMaterialType(null);
    if (poLine.getEresource() != null)
      poLine.getEresource().setMaterialType(null);
  }

  private static JsonObject getMockDraftOrder() throws Exception {
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH));
    order.put("workflowStatus", "Pending");

    return order;
  }
  private static JsonObject getMockOrderWithStatusPaymentNotRequired() throws Exception {
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_RECEIPT_NOT_REQUIRED_PATH));
    order.put("workflowStatus", "Pending");

    return order;
  }

  private void preparePiecesForCompositePo(CompositePurchaseOrder reqData) {
    reqData.getCompositePoLines().forEach(poLine -> poLine.getLocations().forEach(location -> {
      for (int i = 0; i < location.getQuantity(); i++) {
        Title title = new Title().withId(SAMPLE_TITLE_ID)
          .withTitle(poLine.getTitleOrPackage()).withPoLineId(poLine.getId());
        addMockEntry(TITLES, JsonObject.mapFrom(title));
        addMockEntry(PIECES_STORAGE,
          new Piece().withPoLineId(poLine.getId())
            .withLocationId(location.getLocationId()).withFormat(OTHER)
            .withReceivingStatus(Piece.ReceivingStatus.EXPECTED)
            .withTitleId(title.getId()));
      }
    }));
  }
}
