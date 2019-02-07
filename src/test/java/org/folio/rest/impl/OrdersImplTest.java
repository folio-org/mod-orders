package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.DEFAULT_POLINE_LIMIT;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.ResourcePathResolver.*;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.AbstractHelper.PO_LINE_NUMBER;
import static org.folio.rest.impl.AbstractHelper.PO_NUMBER;
import static org.folio.rest.impl.InventoryHelper.ON_ORDER_ITEM_STATUS;
import static org.folio.rest.impl.OrdersImpl.LINES_LIMIT_ERROR_CODE;
import static org.folio.rest.impl.OrdersImpl.OVER_LIMIT_ERROR_MESSAGE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.collect.ImmutableSet;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.RestVerticle;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PoLineCollection;
import org.folio.rest.acq.model.ReceivingHistoryCollection;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;

import io.restassured.RestAssured;
import io.restassured.http.Header;
import io.restassured.response.Response;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.http.HttpServer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;

import javax.ws.rs.core.Response.Status;


@RunWith(VertxUnitRunner.class)
public class OrdersImplTest {

  private static final String BAD_REQUEST = "BadRequest";
  private static final String ORDERS_RECEIVING_HISTORY_ENDPOINT = "/orders/receiving-history";
  private static final String INSTANCE_RECORD = "instance_record";
  private static final String HOLDINGS_RECORD = "holding_record";
  private static final String ITEM_RECORDS = "item_records";
  private static final String PIECES = "pieces";
  private static final String ORDER_WITHOUT_PO_LINES = "order_without_po_lines.json";
  private static final String ORDER_WITH_PO_LINES_JSON = "put_order_with_po_lines.json";
  private static final String ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON = "put_order_with_mismatch_id_in_po_lines.json";
  private static final String PO_NUMBER_VALUE = "228D126";

  static {
    System.setProperty(LoggerFactory.LOGGER_DELEGATE_FACTORY_CLASS_NAME, "io.vertx.core.logging.Log4j2LogDelegateFactory");
  }

  private static final Logger logger = LoggerFactory.getLogger(OrdersImplTest.class);

  private static final String APPLICATION_JSON = "application/json";
  private static final String TEXT_PLAIN = "text/plain";

  private static final int okapiPort = NetworkUtils.nextFreePort();
  private static final int mockPort = NetworkUtils.nextFreePort();

  private static final String EXIST_CONFIG_TENANT_LIMIT_10 = "test_diku_limit_10";
  private static final String EXIST_CONFIG_TENANT_LIMIT_1 = "test_diku_limit_1";
  private static final String INVALID_EXIST_CONFIG_TENANT = "invalid_config";
  private static final String EMPTY_CONFIG_TENANT = "config_empty";
  private static final String NON_EXIST_CONFIG_TENANT = "ordersimpltest";
  private static final String PO_NUMBER_ERROR_TENANT = "po_number_error_tenant";


  private static final Header X_OKAPI_URL = new Header("X-Okapi-Url", "http://localhost:" + mockPort);
  private static final Header NON_EXIST_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, NON_EXIST_CONFIG_TENANT);
  private static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10 = new Header(OKAPI_HEADER_TENANT, EXIST_CONFIG_TENANT_LIMIT_10);
  private static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1 = new Header(OKAPI_HEADER_TENANT, EXIST_CONFIG_TENANT_LIMIT_1);
  private static final Header INVALID_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, INVALID_EXIST_CONFIG_TENANT);
  private static final Header EMPTY_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, EMPTY_CONFIG_TENANT);
  private static final Header PO_NUMBER_ERROR_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, PO_NUMBER_ERROR_TENANT);
  private static final Header X_OKAPI_USER_ID = new Header(OKAPI_USERID_HEADER, "440c89e3-7f6c-578a-9ea8-310dad23605e");
  private static final Header X_OKAPI_TOKEN = new Header(OKAPI_HEADER_TOKEN, "eyJhbGciOiJIUzI1NiJ9");

  private static final String X_ECHO_STATUS = "X-Okapi-Echo-Status";

  private static final String INVALID_LANG = "?lang=english";

  private static final String VALID_ORDER_ID = "07f65192-44a4-483d-97aa-b137bbd96390";
  private static final String PO_ID = "e5ae4afd-3fa9-494e-a972-f541df9b877e";
  private static final String ID_BAD_FORMAT = "123-45-678-90-abc";
  private static final String ID_DOES_NOT_EXIST = "d25498e7-3ae6-45fe-9612-ec99e2700d2f";
  private static final String ID_FOR_INTERNAL_SERVER_ERROR = "168f8a86-d26c-406e-813f-c7527f241ac3";
  private static final String ID_FOR_PENDING_ORDER = "00000000-1111-2222-8888-999999999999";
  private static final String PO_ID_FOR_FAILURE_CASE = "bad500aa-aaaa-500a-aaaa-aaaaaaaaaaaa";
  private static final String PO_LINE_ID_FOR_SUCCESS_CASE = "fca5fa9e-15cb-4a3d-ab09-eeea99b97a47";
  private static final String ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String PO_LINE_ID_WITH_SOME_SUB_OBJECTS_ALREADY_REMOVED = "0009662b-8b80-4001-b704-ca10971f175d";
  private static final String PO_LINE_ID_WITH_SUB_OBJECT_OPERATION_500_CODE = "c2755a78-2f8d-47d0-a218-059a9b7391b4";
  private static final String PO_LINE_ID_WITH_FUND_DISTRIBUTION_404_CODE = "f7223ce8-9e92-4c28-8fd9-097596053b7c";
  private static final String ORDER_ID_WITHOUT_PO_LINES = "50fb922c-3fa9-494e-a972-f2801f1b9fd1";
  private static final String ORDER_ID_WITH_PO_LINES = "ab18897b-0e40-4f31-896b-9c9adc979a87";
  private static final String ORDER_WITHOUT_WORKFLOW_STATUS = "41d56e59-46db-4d5e-a1ad-a178228913e5";
  private static final String RECEIVING_HISTORY_PURCHASE_ORDER_ID = "0804ddec-6545-404a-b54d-a693f505681d";

  // API paths
  private final static String COMPOSITE_ORDERS_PATH = "/orders/composite-orders";
  private final static String COMPOSITE_ORDERS_BY_ID_PATH = "/orders/composite-orders/%s";
  private final static String LINES_PATH = "/orders/order-lines";
  private final static String LINE_BY_ID_PATH = "/orders/order-lines/%s";
  private static final String PONUMBER_VALIDATE_PATH = "/orders/po-number/validate";

  // Mock data paths
  private static final String BASE_MOCK_DATA_PATH = "mockdata/";
  private static final String INSTANCE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instances/";
  private static final String INSTANCE_IDENTIFIERS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "identifierTypes/";
  private static final String INSTANCE_STATUSES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceStatuses/";
  private static final String INSTANCE_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceTypes/";
  private static final String HOLDINGS_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "holdingsRecords/";
  private static final String ITEMS_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "itemsRecords/";
  private static final String LOAN_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "loanTypes/";
  private static final String COMP_ORDER_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/";
  private static final String ORDERS_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + "getOrders.json";
  private static final String ORDER_FOR_FAILURE_CASE_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + PO_ID_FOR_FAILURE_CASE + ".json";
  private static final String PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "lines/";
  private static final String COMP_PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String MOCK_DATA_ROOT_PATH = "src/test/resources/";
  private static final String LISTED_PRINT_MONOGRAPH_PATH = MOCK_DATA_ROOT_PATH + "/po_listed_print_monograph.json";
  private static final String POLINES_COLLECTION = PO_LINES_MOCK_DATA_PATH + "/po_line_collection.json";
  private static final String listedPrintSerialPath = MOCK_DATA_ROOT_PATH + "/po_listed_print_serial.json";
  private static final String MINIMAL_ORDER_PATH = MOCK_DATA_ROOT_PATH + "/minimal_order.json";
  private static final String poCreationFailurePath = MOCK_DATA_ROOT_PATH + "/po_creation_failure.json";
  private static final String poLineCreationFailurePath = MOCK_DATA_ROOT_PATH + "/po_line_creation_failure.json";
  private static final String CONFIG_MOCK_PATH = BASE_MOCK_DATA_PATH + "configurations.entries/%s.json";
  /** The PO Line with minimal required content */
  private static final String PO_LINE_MIN_CONTENT_PATH = COMP_PO_LINES_MOCK_DATA_PATH + "/minimalContent.json";
  private static final String RECEIVING_HISTORY_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "receivingHistory/";

  private static final String QUERY_PARAM_NAME = "query";
  private static final String ID = "id";
  private static final String PURCHASE_ORDER_ID = "purchase_order_id";
  private static final String INCORRECT_LANG_PARAMETER = "'lang' parameter is incorrect. parameter value {english} is not valid: must match \"[a-zA-Z]{2}\"";
  private static final String INTERNAL_SERVER_ERROR = "Internal Server Error";
  private static final String EXISTING_PO_NUMBER = "oldPoNumber";
  private static final String NONEXISTING_PO_NUMBER = "newPoNumber";
  private static final String BAD_QUERY = "unprocessableQuery";

  private static final Set<String> REQUIRED_PO_LINE_PROPERTIES = ImmutableSet.of(SOURCE);

  private static Vertx vertx;
  private static MockServer mockServer;


  @BeforeClass
  public static void setUpOnce(TestContext context) {
    vertx = Vertx.vertx();

    mockServer = new MockServer(mockPort);
    mockServer.start(context);

    RestAssured.baseURI = "http://localhost:" + okapiPort;
    RestAssured.port = okapiPort;
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();

    final JsonObject conf = new JsonObject();
    conf.put("http.port", okapiPort);

    final DeploymentOptions opt = new DeploymentOptions().setConfig(conf);
    vertx.deployVerticle(RestVerticle.class.getName(), opt, context.asyncAssertSuccess());
  }

  @AfterClass
  public static void tearDownOnce(TestContext context) {
    vertx.close(context.asyncAssertSuccess());
    mockServer.close();
  }

  @Before
  public void setUp() {
    MockServer.serverRqRs.clear();
  }

  @Test
  public void testListedPrintMonograph() throws Exception {
    logger.info("=== Test Listed Print Monograph ===");

    JsonObject reqData = getMockDraftOrder();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, reqData.toString(),
      EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

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
      assertNotNull(line.getCost().getId());
      assertNotNull(line.getDetails().getId());
      assertNotNull(line.getLocation().getId());
      assertNull(line.getInstanceId());
    }
  }

  @Test
  public void testListedPrintMonographInOpenStatus() throws Exception {
    logger.info("=== Test Listed Print Monograph in Open status ===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH)).mapTo(CompositePurchaseOrder.class);
    // remove productId from PO line to test scenario when it's not provided so there is no check for existing instance but new one will be created
    reqData.getCompositePoLines().get(0).getDetails().getProductIds().clear();
    // MODORDERS-117 only physical quantity will be used
    reqData.getCompositePoLines().get(0).setOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE);
    // Set status to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

	  LocalDate now = LocalDate.now();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    LocalDate dateOrdered = resp.getDateOrdered().toInstant().atZone(ZoneId.of(ZoneOffset.UTC.getId())).toLocalDate();
    assertThat(dateOrdered, equalTo(now));

    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
    assertEquals(reqData.getCompositePoLines().size(), resp.getCompositePoLines().size());

    for (int i = 0; i < resp.getCompositePoLines().size(); i++) {
      CompositePoLine line = resp.getCompositePoLines().get(i);
      String polNumber = line.getPoLineNumber();
      String polId = line.getId();

      assertEquals(poId, line.getPurchaseOrderId());
      assertNotNull(polId);
      assertNotNull(polNumber);
      assertTrue(polNumber.startsWith(poNumber));
      assertNotNull(line.getCost().getId());
      assertNotNull(line.getDetails().getId());
      assertNotNull(line.getLocation().getId());
      assertNotNull(line.getInstanceId());
    }

    int polCount = resp.getCompositePoLines().size();

    List<JsonObject> instancesSearches = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
    List<JsonObject> holdingsSearches = MockServer.serverRqRs.get(HOLDINGS_RECORD, HttpMethod.GET);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);

    assertNotNull(instancesSearches);
    assertNotNull(holdingsSearches);
    assertNotNull(itemsSearches);

    // Check that search for existing instances was done not for all PO lines
    assertEquals(polCount - 1, instancesSearches.size());
    // Check that search for existing items was done for each PO lines
    assertEquals(polCount, itemsSearches.size());

    verifyInventoryInteraction(resp, polCount);
  }

  @Test
  public void testOrderWithPoLinesWithoutSource() throws Exception {
    logger.info("=== Test Listed Print Monograph with POL without source ===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH)).mapTo(CompositePurchaseOrder.class);
    // Assert that there are 2 lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // remove source to verify validation for first POL
    reqData.getCompositePoLines().get(0).setSource(null);
    // Set source code to null to verify validation for second POL
    reqData.getCompositePoLines().get(1).getSource().setCode(null);

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      NON_EXIST_CONFIG_X_OKAPI_TENANT, APPLICATION_JSON, 422).as(Errors.class);
    assertEquals(reqData.getCompositePoLines().size(), errors.getErrors().size());
  }

  @Test
  public void testDateOrderedIsNotSetForPendingOrder() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH)).mapTo(CompositePurchaseOrder.class);
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that Order moves to Pending
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    // Verify dateOrdered is not set because Workflow status is not OPEN
    assertNull(resp.getDateOrdered());
  }

  @Test
  public void testPostOpenOrderInventoryUpdateOnlyForFirstPOL() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH)).mapTo(CompositePurchaseOrder.class);
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // MODORDERS-117 Setting OrderFormat to OTHER which means create nothing in inventory for the second PO Line
    reqData.getCompositePoLines().get(1).setOrderFormat(CompositePoLine.OrderFormat.OTHER);

	  LocalDate now = LocalDate.now();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    LocalDate dateOrdered = resp.getDateOrdered().toInstant().atZone(ZoneId.of(ZoneOffset.UTC.getId())).toLocalDate();
    assertThat(dateOrdered, equalTo(now));

    // Check that search of the existing instances and items was done for first PO line only
    List<JsonObject> instancesSearches = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
    List<JsonObject> holdingsSearches = MockServer.serverRqRs.get(HOLDINGS_RECORD, HttpMethod.GET);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    assertNotNull(instancesSearches);
    assertNotNull(holdingsSearches);
    assertNotNull(itemsSearches);
    assertEquals(1, instancesSearches.size());
    assertEquals(1, holdingsSearches.size());
    assertEquals(1, itemsSearches.size());

    verifyInventoryInteraction(resp, 1);
  }

  @Test
  public void testPlaceOrderMinimal() throws Exception {
    logger.info("=== Test Placement of minimal order ===");

    String body = getMockData(MINIMAL_ORDER_PATH);
    JsonObject reqData = new JsonObject(body);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      NON_EXIST_CONFIG_X_OKAPI_TENANT, APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);


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
  public void testPostOrderFailsWithInvalidPONumber() {
    logger.info("=== Test Placement of minimal order failure with Invalid PO Number===");

    JsonObject request = new JsonObject();
    request.put("po_number", "1234");
    String body= request.toString();

     verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      NON_EXIST_CONFIG_X_OKAPI_TENANT, APPLICATION_JSON, 422);

  }

  @Test
  public void testPostOrderFailsWithExistingPONumber() {
    logger.info("=== Test Placement of minimal order failure with Existing PO Number===");

    JsonObject request = new JsonObject();
    request.put("po_number", EXISTING_PO_NUMBER);
    String body= request.toString();

     verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      NON_EXIST_CONFIG_X_OKAPI_TENANT, TEXT_PLAIN, 400);

  }

  @Test
  public void testPostOrderPONumberAutoGenerated() {
    logger.info("=== Test Placement of Empty order with Auto Generated PO Number===");

    JsonObject request = new JsonObject();
    String body= request.toString();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
        NON_EXIST_CONFIG_X_OKAPI_TENANT, APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
  }

  @Test
  public void testPoCreationFailure(TestContext ctx) throws Exception {
    logger.info("=== Test PO creation failure ===");

    String body = getMockData(poCreationFailurePath);

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      NON_EXIST_CONFIG_X_OKAPI_TENANT, APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());

    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertNotNull(errors.getErrors().get(0));
    ctx.assertEquals("must match \"^[a-zA-Z0-9]{5,16}$\"", errors.getErrors().get(0).getMessage());
    ctx.assertFalse(errors.getErrors().get(0).getParameters().isEmpty());
    ctx.assertNotNull(errors.getErrors().get(0).getParameters().get(0));
    ctx.assertEquals("poNumber", errors.getErrors().get(0).getParameters().get(0).getKey());
    ctx.assertEquals("123", errors.getErrors().get(0).getParameters().get(0).getValue());
  }

  @Test
  public void testPoCreationWithOverLimitPOLines(TestContext ctx) throws Exception {
    logger.info("=== Test PO, with over limit lines quantity, creation ===");

    String body = getMockDraftOrder().toString();

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(String.format(OVER_LIMIT_ERROR_MESSAGE, 1), errors.getErrors().get(0).getMessage());
  }


  @Test
  public void testPostWithInvalidConfig(TestContext ctx) throws Exception {
    logger.info("=== Test PO creation fail if config is invalid ===");

    String body = getMockDraftOrder().toString();

    final String error = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      INVALID_CONFIG_X_OKAPI_TENANT, TEXT_PLAIN, 500).body().print();

    ctx.assertEquals(error, "Invalid limit value in configuration.");
  }

  @Test
  public void testPostWithEmptyConfigOverLimit(TestContext ctx) {
    logger.info("=== Test PO creation fail with default limit ===");

    JsonObject compPoLineJson = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final Errors errors = verifyPostResponse(LINES_PATH, compPoLineJson.encodePrettily(),
      EMPTY_CONFIG_X_OKAPI_TENANT, APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(String.format(OVER_LIMIT_ERROR_MESSAGE, DEFAULT_POLINE_LIMIT), errors.getErrors().get(0).getMessage());
    ctx.assertEquals(LINES_LIMIT_ERROR_CODE, errors.getErrors().get(0).getCode());
  }


  @Test
  public void testPoLineCreationIfPoAlreadyReachedLimit(TestContext ctx) {
    logger.info("=== Test PO Line over limit creation ===");
    JsonObject compPoLineJson = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final Errors errors = verifyPostResponse(LINES_PATH, compPoLineJson.encodePrettily(),
      EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(String.format(OVER_LIMIT_ERROR_MESSAGE, 1), errors.getErrors().get(0).getMessage());
    ctx.assertEquals(LINES_LIMIT_ERROR_CODE, errors.getErrors().get(0).getCode());
  }


  @Test
  public void testPoLineCreationFailure(TestContext ctx) throws Exception {
    logger.info("=== Test POLine creation failure ===");

    String body = getMockData(poLineCreationFailurePath);

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      NON_EXIST_CONFIG_X_OKAPI_TENANT, APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());

    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertNotNull(errors.getErrors().get(0));
    ctx.assertEquals("must match \"^[a-zA-Z0-9]{5,16}-[0-9]{1,3}$\"", errors.getErrors().get(0).getMessage());
    ctx.assertFalse(errors.getErrors().get(0).getParameters().isEmpty());
    ctx.assertNotNull(errors.getErrors().get(0).getParameters().get(0));
    ctx.assertEquals("compositePoLines[1].poLineNumber", errors.getErrors().get(0).getParameters().get(0).getKey());
    ctx.assertEquals("invalid", errors.getErrors().get(0).getParameters().get(0).getValue());
  }

  @Test
  public void testDetailsCreationFailure() throws Exception {
    logger.info("=== Test Details creation failure ===");

    String body = getMockDraftOrder().toString();

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
        .header(X_OKAPI_USER_ID)
        .header(X_OKAPI_TOKEN)
        .header(X_ECHO_STATUS, 403)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(COMPOSITE_ORDERS_PATH)
        .then()
          .contentType(TEXT_PLAIN)
          .statusCode(500)
            .extract()
              .response();

    String respBody = resp.getBody().asString();
    logger.info(respBody);

    assertEquals("Access requires permission: foo.bar.baz", respBody);
  }

  @Test
  public void testGetOrderById() throws Exception {
    logger.info("=== Test Get Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));

    final CompositePurchaseOrder resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(COMPOSITE_ORDERS_PATH + "/" + id)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(200)
          .extract()
            .response()
              .as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(id, resp.getId());
  }

  @Test
  public void testGetOrderByIdWithOnePoLine() {
    logger.info("=== Test Get Order By Id - With one PO Line and empty source ===");

    String id = VALID_ORDER_ID;
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));

    final CompositePurchaseOrder resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(COMPOSITE_ORDERS_PATH + "/" + id)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(200)
          .extract()
            .response()
              .as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(id, resp.getId());
    assertEquals(1, resp.getCompositePoLines().size());
    // The source set in file to ID_DOES_NOT_EXIST constant value
    assertNull(resp.getCompositePoLines().get(0).getSource());
  }

  @Test
  public void testGetOrderByIdIncorrectIdFormat() {
    logger.info("=== Test Get Order By Id - Incorrect Id format - 400 ===");

    String id = ID_BAD_FORMAT;

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(COMPOSITE_ORDERS_PATH + "/" + id)
        .then()
          // The status code should be 400 once Pattern validation annotation is added to Orders interface methods
          .statusCode(400)
          .extract()
            .response();

    String actual = resp.getBody().asString();
    logger.info(actual);

    assertNotNull(actual);
    assertTrue(actual.contains(id));
  }

  @Test
  public void testGetOrderByIdNotFound() {
    logger.info("=== Test Get Order By Id - Not Found ===");

    String id = ID_DOES_NOT_EXIST;

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(COMPOSITE_ORDERS_PATH + "/" + id)
        .then()
          .contentType(TEXT_PLAIN)
          .statusCode(404)
          .extract()
            .response();

    String actual = resp.getBody().asString();
    logger.info(actual);

    assertEquals(id, actual);
  }

  @Test
  public void testDeleteById() throws Exception {
    logger.info("=== Test Delete Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .delete(COMPOSITE_ORDERS_PATH + "/" + id)
        .then()
          .statusCode(204);
  }

  @Test
  public void testDeleteByIdNoOrderFound() {
    logger.info("=== Test Delete Order By Id - Not Found ===");
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_DOES_NOT_EXIST, TEXT_PLAIN, 404);
  }

  @Test
  public void testDeleteById500Error() {
    logger.info("=== Test Delete Order By Id - Storage Internal Server Error ===");
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_FOR_INTERNAL_SERVER_ERROR, TEXT_PLAIN, 500);
  }

  @Test
  public void testDeleteByIdWithoutOkapiUrlHeader() {
    logger.info("=== Test Delete Order By Id - 500 due to missing Okapi URL header ===");

   RestAssured
      .with()
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .delete(COMPOSITE_ORDERS_PATH + "/" + ID_DOES_NOT_EXIST)
        .then()
          .statusCode(500);
  }

  @Test
  public void testPutOrdersByIdWorkflowStatusOpenForStorageAndCurrentRequest() throws Exception {
    logger.info("=== Test Put Order By Id workflow_status is Open from storage and workflow_status is Open in current request  ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    JsonObject storageData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), "", 204);

    storageData.put("workflow_status", "Open");
    reqData.put("workflow_status", "Open");
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

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    assertEquals(MockServer.serverRqRs.get(PO_LINES, HttpMethod.POST).size(), poLinesFromRequest.size() - sameLinesCount);
    assertEquals(MockServer.serverRqRs.get(PO_LINES, HttpMethod.DELETE).size(), poLinesFromStorage.size() - sameLinesCount);
    assertNotNull(MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT));
    assertEquals(MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT).size(), sameLinesCount);
  }

  @Test
  public void testUpdateOrderWithDefaultStatus() throws Exception {
    logger.info("=== Test Put Order By Id - Make sure that default status is used ===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(MINIMAL_ORDER_PATH)).mapTo(CompositePurchaseOrder.class);
    reqData.setId(ORDER_WITHOUT_WORKFLOW_STATUS);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    String url = COMPOSITE_ORDERS_PATH + "/" + reqData.getId();
    String body = JsonObject.mapFrom(reqData).encode();
    verifyPut(url, body, "", 204);

    List<JsonObject> orderRetrievals = MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET);
    assertNotNull(orderRetrievals);
    assertEquals(1, orderRetrievals.size());
    PurchaseOrder storageOrderBeforeUpdate = orderRetrievals.get(0).mapTo(PurchaseOrder.class);
    // Assert default status is Pending
    assertEquals(PurchaseOrder.WorkflowStatus.PENDING, storageOrderBeforeUpdate.getWorkflowStatus());

    List<JsonObject> orderUpdates = MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT);
    assertNotNull(orderUpdates);
    assertEquals(1, orderUpdates.size());

    PurchaseOrder storageUpdatedOrder = orderUpdates.get(0).mapTo(PurchaseOrder.class);
    assertNotNull(storageUpdatedOrder.getWorkflowStatus());
    assertEquals(CompositePurchaseOrder.WorkflowStatus.CLOSED.value(), storageUpdatedOrder.getWorkflowStatus().value());

  }

  @Test
  public void testPutOrdersByIdWithIdMismatch() throws Exception {
    logger.info("=== Test Put Order By Id with id mismatch  ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON));

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), APPLICATION_JSON, 422);
  }

  @Test
  public void testUpdatePoNumber() throws Exception {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject storData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));
    String newPoNumber = reqData.getString(PO_NUMBER) + "A";
    reqData.put(PO_NUMBER, newPoNumber);
    Pattern poLinePattern = Pattern.compile(String.format("(%s)(-[0-9]{1,3})", newPoNumber));

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    assertEquals(MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT).size(), storData.getJsonArray(COMPOSITE_PO_LINES).size());
    MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT).forEach(poLine -> {
      Matcher matcher = poLinePattern.matcher(poLine.getString(PO_LINE_NUMBER));
      assertTrue(matcher.find());
    });
    assertNull(MockServer.serverRqRs.get(PO_LINES, HttpMethod.DELETE));
  }

  @Test
  public void testUpdatePoNumberWithPoLines() throws Exception {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject storageData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    String newPoNumber = reqData.getString(PO_NUMBER) + "A";
    reqData.put(PO_NUMBER, newPoNumber);
    Pattern poLinePattern = Pattern.compile(String.format("(%s)(-[0-9]{1,3})", newPoNumber));

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), "", 204);
    verifyPoWithPoLinesUpdate(reqData, storageData);
    MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT).forEach(poLine -> {
      Matcher matcher = poLinePattern.matcher(poLine.getString(PO_LINE_NUMBER));
      assertTrue(matcher.find());
    });
  }

  @Test
  public void testPutOrderWithoutPoNumberValidation() throws IOException {
    logger.info("=== Test Put Order By Id without po number validation  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    reqData.remove(PO_NUMBER);

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), APPLICATION_JSON, 422);
  }

  @Test
  public void testPutOrderFailsWithInvalidPONumber() throws Exception {
    logger.info("=== Test update order failure with Invalid PO Number===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);

    JsonObject request = new JsonObject();
    request.put("po_number", "1234");
    String body= request.toString();

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, body, APPLICATION_JSON, 422);

  }

  @Test
  public void testPutOrderFailsWithExistingPONumber() throws Exception {
    logger.info("=== Test update of order failure with Existing PO Number===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);

    JsonObject request = new JsonObject();
    request.put("po_number", EXISTING_PO_NUMBER);
    String body= request.toString();

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, body, TEXT_PLAIN, 400);

  }

  @Test
  public void testPoUpdateWithOverLimitPOLines(TestContext ctx) throws Exception {
    logger.info("=== Test PUT PO, with over limit lines quantity ===");

    String body = getMockData(LISTED_PRINT_MONOGRAPH_PATH);
    final Errors errors = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1)
        .header(X_OKAPI_TOKEN)
        .header(X_OKAPI_USER_ID)
        .contentType(APPLICATION_JSON)
        .body(body)
      .put(COMPOSITE_ORDERS_PATH + "/" + ORDER_ID_WITHOUT_PO_LINES)
        .then()
          .statusCode(422)
            .extract()
              .response()
                .body()
                  .as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(String.format(OVER_LIMIT_ERROR_MESSAGE, 1), errors.getErrors().get(0).getMessage());
    ctx.assertEquals(LINES_LIMIT_ERROR_CODE, errors.getErrors().get(0).getCode());
  }

  @Test
  public void testPoUpdateWithOverLimitPOLinesWithDefaultLimit(TestContext ctx) throws Exception {
    logger.info("=== Test PUT PO, with over limit lines quantity with default limit ===");

    String body = getMockData(LISTED_PRINT_MONOGRAPH_PATH);
    final Errors errors = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .header(X_OKAPI_USER_ID)
        .contentType(APPLICATION_JSON)
        .body(body)
      .put(COMPOSITE_ORDERS_PATH + "/" + ORDER_ID_WITHOUT_PO_LINES)
        .then()
          .statusCode(422)
            .extract()
              .response()
                .body()
                  .as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(String.format(OVER_LIMIT_ERROR_MESSAGE, 1), errors.getErrors().get(0).getMessage());
    ctx.assertEquals(LINES_LIMIT_ERROR_CODE, errors.getErrors().get(0).getCode());
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpen() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH)).mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PENDING_ORDER);
    // Make sure that mock PO has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // MODORDERS-117 guarantee electronic resource for the second PO Line but set "create items" to false
    reqData.getCompositePoLines().get(1).setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    reqData.getCompositePoLines().get(1).getEresource().setCreateInventory(false);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), "", 204);

    int polCount = reqData.getCompositePoLines().size();

    verifyInventoryInteraction(reqData, polCount - 1);
  }

  @Test
  public void testUpdateOrderToOpenWithPartialItemsCreation() throws Exception {
    logger.info("=== Test Order update to Open status - Inventory items expected to be created partially ===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH)).mapTo(CompositePurchaseOrder.class);
    // Emulate items creation issue
    reqData.getCompositePoLines().get(0).getDetails().getMaterialTypes().set(0, ID_FOR_INTERNAL_SERVER_ERROR);
    // Let's have only one PO Line
    reqData.getCompositePoLines().remove(1);
    // Set status to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // Set specific ID to let items search to return 1 item
    reqData.setId(ID_FOR_PENDING_ORDER);

    int polCount = reqData.getCompositePoLines().size();
    // Assert that only one PO line presents
    assertEquals(1, polCount);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), TEXT_PLAIN, 500);

    // Verify inventory GET and POST requests for instance, holding and item records
    verifyInventoryInteraction(false);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();
    // All created pieces
    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);

    // Assert that items quantity equals to created ieces
    assertEquals(createdPieces.size(), items.size());

    // Verify that not all expected items created
    assertThat(items.size(), lessThan(calculateInventoryItemsQuantity(reqData.getCompositePoLines().get(0))));

    // Verify that pieces created for processed items quantity
    verifyPiecesCreated(items, createdPieces);
  }

  private void verifyInventoryInteraction(CompositePurchaseOrder reqData, int createdInstancesCount) {
    // Verify inventory GET and POST requests for instance, holding and item records
    verifyInventoryInteraction(true);

    List<JsonObject> createdInstances = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.POST);
    List<JsonObject> createdHoldings = MockServer.serverRqRs.get(HOLDINGS_RECORD, HttpMethod.POST);
    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);

    assertEquals(createdInstancesCount, createdInstances.size());

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();

    assertEquals(createdPieces.size(), items.size());
    for (CompositePoLine pol : reqData.getCompositePoLines()) {
      verifyInstanceCreated(createdInstances, pol);
      verifyHoldingsCreated(createdHoldings, pol);
      verifyItemsCreated(items, pol, calculateInventoryItemsQuantity(pol));
      verifyPiecesCreated(items, createdPieces);
    }
  }

  private void verifyInventoryInteraction(boolean checkItemsCreated) {
    // Check that search of the existing instances and items was done for each PO line
    List<JsonObject> instancesSearches = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
    List<JsonObject> holdingsSearches = MockServer.serverRqRs.get(HOLDINGS_RECORD, HttpMethod.GET);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> piecesSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    assertNotNull(instancesSearches);
    logger.debug("--------------------------- Instances found -------------------------------\n" + new JsonArray(instancesSearches).encodePrettily());
    assertNotNull(holdingsSearches);
    logger.debug("--------------------------- Holdings found -------------------------------\n" + new JsonArray(holdingsSearches).encodePrettily());
    assertNotNull(itemsSearches);
    logger.debug("--------------------------- Items found -------------------------------\n" + new JsonArray(itemsSearches).encodePrettily());
    assertNotNull(piecesSearches);
    logger.debug("--------------------------- Pieces found -------------------------------\n" + new JsonArray(piecesSearches).encodePrettily());

    // Check that creation of the new instances and items was done
    List<JsonObject> createdInstances = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.POST);
    List<JsonObject> createdHoldings = MockServer.serverRqRs.get(HOLDINGS_RECORD, HttpMethod.POST);
    List<JsonObject> createdItems = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.POST);
    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);
    assertNotNull(createdInstances);
    logger.debug("--------------------------- Instances created -------------------------------\n" + new JsonArray(createdInstances).encodePrettily());
    assertNotNull(createdHoldings);
    logger.debug("--------------------------- Holdings created -------------------------------\n" + new JsonArray(createdHoldings).encodePrettily());
    if (checkItemsCreated) {
      assertNotNull(createdItems);
      logger.debug("--------------------------- Items created -------------------------------\n" + new JsonArray(createdItems).encodePrettily());
    }
    assertNotNull(createdPieces);
    logger.debug("--------------------------- Pieces created -------------------------------\n" + new JsonArray(createdPieces).encodePrettily());
  }

  private List<JsonObject> joinExistingAndNewItems() {
    List<JsonObject> items = new ArrayList<>(CollectionUtils.emptyIfNull(MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.POST)));
    MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET).forEach(json -> {
      JsonArray existingItems = json.getJsonArray("items");
      if (existingItems != null) {
        existingItems.forEach(item -> items.add((JsonObject) item));
      }
    });
    return items;
  }

  private void verifyInstanceCreated(List<JsonObject> inventoryInstances, CompositePoLine pol) {
    boolean verified = false;
    for (JsonObject instance : inventoryInstances) {
      if (pol.getTitle().equals(instance.getString("title"))) {
        verifyInstanceRecordRequest(instance, pol);
        verified = true;
        break;
      }
    }

    int expectedItemsQuantity = calculateInventoryItemsQuantity(pol);
    if (!verified && expectedItemsQuantity > 0) {
      fail("No matching instance for POL: " + JsonObject.mapFrom(pol).encodePrettily());
    }

    if ((!verified && StringUtils.isNotEmpty(pol.getInstanceId()) || (verified && expectedItemsQuantity == 0))) {
      fail("No instance expected for POL: " + JsonObject.mapFrom(pol).encodePrettily());
    }
  }

	private void verifyPiecesCreated(List<JsonObject> inventoryItems, List<JsonObject> pieces) {
    // Collect all item id's
    List<String> itemIds = inventoryItems.stream()
                                    .map(item -> item.getString(ID))
                                    .collect(Collectors.toList());

    for (JsonObject pieceObj : pieces) {
      // Make sure piece data corresponds to schema content
      Piece piece = pieceObj.mapTo(Piece.class);

      // Check if itemId in inventoryItems match itemId in piece record
      assertThat(itemIds, hasItem(piece.getItemId()));
      assertThat(piece.getReceivingStatus(), equalTo(Piece.ReceivingStatus.EXPECTED));
    }
	}

  private void verifyHoldingsCreated(List<JsonObject> holdings, CompositePoLine pol) {
    boolean verified = false;
    for (JsonObject holding : holdings) {
      if (StringUtils.isNotEmpty(pol.getLocation().getLocationId())
          && pol.getLocation().getLocationId().equals(holding.getString("permanentLocationId"))) {
        verified = true;
        break;
      }
    }

    int expectedItemsQuantity = calculateInventoryItemsQuantity(pol);
    if (!verified && expectedItemsQuantity > 0) {
      fail("No matching holdings record for POL: " + JsonObject.mapFrom(pol).encodePrettily());
    }

    if (!verified && StringUtils.isNotEmpty(pol.getInstanceId())) {
      fail("Holdings Record could not be found for POL: " + JsonObject.mapFrom(pol).encodePrettily());
    }
  }

  private void verifyItemsCreated(List<JsonObject> inventoryItems, CompositePoLine pol, int expectedQuantity) {
    int actualQuantity = 0;

    for (JsonObject item : inventoryItems) {
      if (pol.getId().equals(item.getString("purchaseOrderLineIdentifier"))) {
        verifyItemRecordRequest(item, pol);
        actualQuantity++;
      }
    }

    if (expectedQuantity != actualQuantity) {
      fail(String.format("Actual items quantity is %d but expected %d", actualQuantity, expectedQuantity));
    }
  }

  private void verifyInstanceRecordRequest(JsonObject instance, CompositePoLine line) {
    assertThat(instance.getString("title"), equalTo(line.getTitle()));
    assertThat(instance.getString("source"), equalTo(line.getSource().getCode()));
    assertThat(instance.getString("statusId"), equalTo("daf2681c-25af-4202-a3fa-e58fdf806183"));
    assertThat(instance.getString("instanceTypeId"), equalTo("30fffe0e-e985-4144-b2e2-1e8179bdb41f"));
    assertThat(instance.getJsonArray("publication").getJsonObject(0).getString("publisher"), equalTo(line.getPublisher()));
    assertThat(instance.getJsonArray("publication").getJsonObject(0).getString("dateOfPublication"), equalTo(line.getPublicationDate()));
    if (line.getDetails().getProductIds().size() > 0) {
      assertThat(instance.getJsonArray("identifiers").getJsonObject(0).getString("identifierTypeId"), equalTo("8261054f-be78-422d-bd51-4ed9f33c3422"));
      assertThat(instance.getJsonArray("identifiers").getJsonObject(0).getString("value"), equalTo(line.getDetails().getProductIds().get(0).getProductId()));
    }
  }

  private void verifyItemRecordRequest(JsonObject item, CompositePoLine line) {
    assertThat(item.getString("purchaseOrderLineIdentifier"), not(isEmptyOrNullString()));
    assertThat(line.getDetails().getMaterialTypes(), hasItem(item.getString("materialTypeId")));
    assertThat(item.getString("holdingsRecordId"), not(isEmptyOrNullString()));
    assertThat(item.getString("permanentLoanTypeId"), not(isEmptyOrNullString()));
    assertThat(item.getJsonObject("status"), notNullValue());
    assertThat(item.getJsonObject("status").getString("name"), equalTo(ON_ORDER_ITEM_STATUS));
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenButWithFailureFromStorage(TestContext ctx) throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open - Storage errors expected and no interaction with Inventory===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(ORDER_FOR_FAILURE_CASE_MOCK_DATA_PATH)).mapTo(CompositePurchaseOrder.class);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    final Errors errors = verifyPut(
      String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData).toString(),
      APPLICATION_JSON,
      500)
        .body()
          .as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertEquals(3, errors.getErrors().size());
    ctx.assertNull(MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET));
    ctx.assertNull(MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET));
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenButWithErrorCreatingItemsForSecondPOL(TestContext ctx) throws Exception {
    logger.info("=== Test Put Order By Id to change Order's status to Open - Inventory errors expected on items creation for second POL ===");

    /*==============  Preparation ==============*/

    // Get Open Order
    CompositePurchaseOrder reqData = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH)).mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PENDING_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    int polCount = reqData.getCompositePoLines().size();
    // Make sure that mock PO has 2 lines
    assertEquals(2, polCount);
    // Make sure that inventory interaction is expected for each PO line
    for (CompositePoLine pol : reqData.getCompositePoLines()) {
      assertTrue(calculateInventoryItemsQuantity(pol) > 0);
    }

    // Set material type id to one which emulates item creation failure
    reqData.getCompositePoLines().get(1).getDetails().getMaterialTypes().set(0, ID_FOR_INTERNAL_SERVER_ERROR);

    String path = String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId());

    /*==============  Assert result ==============*/

    // Server Error expected as a result because not all items created
    verifyPut(path, JsonObject.mapFrom(reqData).toString(), TEXT_PLAIN, 500);

    // Check that search of the existing instances and items was done for each PO line
    List<JsonObject> instancesSearches = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> piecesSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    ctx.assertNotNull(instancesSearches);
    ctx.assertNotNull(itemsSearches);
    ctx.assertNotNull(piecesSearches);
    assertEquals(polCount, instancesSearches.size());
    assertEquals(polCount, itemsSearches.size());

    // Check that 2 new instances created and items created successfully only for first POL
    List<JsonObject> createdInstances = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.POST);
    List<JsonObject> createdHoldings = MockServer.serverRqRs.get(HOLDINGS_RECORD, HttpMethod.POST);
    List<JsonObject> createdItems = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.POST);
    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);
    assertNotNull(createdInstances);
    assertNotNull(createdItems);
    assertNotNull(createdPieces);
    assertEquals(polCount, createdInstances.size());

    List<JsonObject> items = joinExistingAndNewItems();
    assertEquals(createdPieces.size(), items.size());

    // Check that instance and items created successfully for first POL
    CompositePoLine firstPol = reqData.getCompositePoLines().get(0);
    verifyInstanceCreated(createdInstances, firstPol);
    verifyHoldingsCreated(createdHoldings, firstPol);
    verifyItemsCreated(items, firstPol, calculateInventoryItemsQuantity(firstPol));
    verifyPiecesCreated(items, createdPieces);

    // Check that instance created successfully for second POL but no items created (but expected)
    CompositePoLine secondPol = reqData.getCompositePoLines().get(1);
    verifyInstanceCreated(createdInstances, secondPol);
    verifyHoldingsCreated(createdHoldings, secondPol);
    verifyItemsCreated(items, secondPol, 0);
  }

  @Test
  public void testPutOrderByIdWithPoLinesInRequestAndNoPoLinesInStorage() throws IOException {
    logger.info("=== Test Put Order By Id with PO lines and without PO lines in order from storage ===");

    JsonObject reqData = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH));

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + ORDER_ID_WITHOUT_PO_LINES, reqData.toString(), "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    assertEquals(MockServer.serverRqRs.get(PO_LINES, HttpMethod.POST).size(), reqData.getJsonArray(COMPOSITE_PO_LINES).size());

  }

  @Test
  public void testPutOrderByIdWithoutPoLinesInRequestDoesNotDeletePoLinesFromStorage() throws IOException {
    logger.info("=== Test Put Order By Id without PO lines doesn't delete lines from storage ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    assertNull(MockServer.serverRqRs.get(PO_LINES, HttpMethod.DELETE));
  }

  @Test
  public void testPutOrderByIdWith404InvalidId() throws IOException {
    logger.info("=== Test Put Order By Id for 404 with Invalid Id or Order not found ===");

    JsonObject reqData = new JsonObject(getMockData(listedPrintSerialPath));
    verifyPut(COMPOSITE_ORDERS_PATH + "/" + "93f612a9-9a05-4eef-aac5-435be131454b", reqData.toString(), TEXT_PLAIN, 404);
  }

  @Test
  public void testValidationOnPost() throws IOException {
    logger.info("=== Test validation Annotation on POST API ===");

    logger.info("=== Test validation with no body ===");

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .header(X_OKAPI_USER_ID)
        .contentType(APPLICATION_JSON)
      .post(COMPOSITE_ORDERS_PATH)
        .then()
          .statusCode(400)
          .body(containsString("Json content error HV000116: The object to be validated must not be null"));

    logger.info("=== Test validation on invalid lang query parameter ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .header(X_OKAPI_TOKEN)
        .contentType(APPLICATION_JSON)
        .body(getMockData(MINIMAL_ORDER_PATH))
      .post(COMPOSITE_ORDERS_PATH +INVALID_LANG)
        .then()
          .statusCode(400)
          .body(containsString(INCORRECT_LANG_PARAMETER));

  }

  @Test
  public void testValidationOnGetById() {
    logger.info("=== Test validation Annotation on GET ORDER BY ID API ===");
    String id = "non-existent-po-id";

    logger.info("=== Test validation on invalid lang query parameter ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .header(X_OKAPI_TOKEN)
        .contentType(APPLICATION_JSON)
      .get(COMPOSITE_ORDERS_PATH +"/"+id+INVALID_LANG)
        .then()
          .statusCode(400)
          .body(containsString(INCORRECT_LANG_PARAMETER));
  }

  @Test
  public void testValidationDelete() {
    logger.info("=== Test validation Annotation on DELETE API ===");

    logger.info("=== Test validation on invalid lang query parameter ===");
    RestAssured
     .with()
       .header(X_OKAPI_URL)
       .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
       .contentType(APPLICATION_JSON)
      .delete(COMPOSITE_ORDERS_PATH + "/" + VALID_ORDER_ID + INVALID_LANG)
       .then()
         .statusCode(400)
         .body(containsString(INCORRECT_LANG_PARAMETER));

  }

  @Test
  public void testValidationOnPut() throws IOException {
    logger.info("=== Test validation Annotation on PUT API ===");
    String id = "non-existent-po-id";
    logger.info("=== Test validation with no body ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
      .put(COMPOSITE_ORDERS_PATH +"/"+id)
        .then()
          .statusCode(400)
          .body(containsString("Json content error HV000116: The object to be validated must not be null"));

     logger.info("=== Test validation on invalid lang query parameter ===");
     RestAssured
       .with()
         .header(X_OKAPI_URL)
         .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
         .contentType(APPLICATION_JSON)
         .body(getMockData(MINIMAL_ORDER_PATH))
       .put(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id) + INVALID_LANG)
         .then()
           .statusCode(400)
           .body(containsString(INCORRECT_LANG_PARAMETER));

     logger.info("=== Test validation on no Content-type parameter ===");
     RestAssured
       .with()
         .header(X_OKAPI_URL)
         .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
         .body(getMockData(MINIMAL_ORDER_PATH))
       .put(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id) + INVALID_LANG)
         .then()
           .statusCode(400)
           .body(containsString("Content-type"));

  }

  @Test
  public void testDeleteOrderLineById() {
    logger.info("=== Test Delete Order Line By Id - Success case ===");

    deleteOrderLineByIdSuccess(PO_LINE_ID_FOR_SUCCESS_CASE);
  }

  @Test
  public void testDeleteOrderLineByIdWithPartiallyDeletedSubObjects() {
    logger.info("=== Test Delete Order Line By Id - Success case ===");

    // This test should behave the same as regular test and only warning in log expected
    deleteOrderLineByIdSuccess(PO_LINE_ID_WITH_SOME_SUB_OBJECTS_ALREADY_REMOVED);
  }

  private void deleteOrderLineByIdSuccess(String lineId) {
    String url = String.format(LINE_BY_ID_PATH, lineId);

    final Response resp = verifyDeleteResponse(url, "", 204);
    assertTrue(StringUtils.isEmpty(resp.getBody().asString()));
  }

  @Test
  public void testDeleteOrderLineByIdWithoutOkapiUrlHeader() {
    logger.info("=== Test Delete Order Line By Id - 500 due to missing Okapi URL header ===");
    RestAssured
      .with()
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .delete(String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST))
        .then()
          .statusCode(500);
  }

  @Test
  public void testDeleteOrderLineByIdNotFound() {
    logger.info("=== Test Delete Order Line By Id - Not Found ===");

    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);
    Response actual = verifyDeleteResponse(url, TEXT_PLAIN, 404);

    assertEquals(ID_DOES_NOT_EXIST, actual.asString());
  }

  @Test
  public void testDeleteOrderLineById500FromStorageOnGetPoLine() {
    logger.info("=== Test Delete Order Line By Id - 500 From Storage On Get PO Line ===");

    String url = String.format(LINE_BY_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR);
    Response actual = verifyDeleteResponse(url, TEXT_PLAIN, 500);

    assertNotNull(actual.asString());
  }

  @Test
  public void testDeleteOrderLineById500FromStorageOnSubObjectDeletion() {
    logger.info("=== Test Delete Order Line By Id - 500 From Storage On Sub-Object deletion ===");

    String url = String.format(LINE_BY_ID_PATH, PO_LINE_ID_WITH_SUB_OBJECT_OPERATION_500_CODE);
    Response actual = verifyDeleteResponse(url, TEXT_PLAIN, 500);

    assertNotNull(actual.asString());
  }

  private Response verifyDeleteResponse(String url, String expectedContentType, int expectedCode) {
    return RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .delete(url)
        .then()
          .statusCode(expectedCode)
          .contentType(expectedContentType)
          .extract()
            .response();
  }

  private Response verifyPostResponse(String url, String body, Header tenantHeader, String
    expectedContentType, int expectedCode) {
    return RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TOKEN)
        .header(tenantHeader)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(url)
        .then()
          .log()
            .all()
            .statusCode(expectedCode)
            .contentType(expectedContentType)
            .extract()
            .response();
  }

  @Test
  public void testGetOrderLineById() {
    logger.info("=== Test Get Orderline By Id ===");

    final CompositePoLine resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(String.format(LINE_BY_ID_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE))
        .then()
          .statusCode(200)
          .extract()
          .response()
          .as(CompositePoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE, resp.getId());
  }


  @Test
  public void testGetOrderLineByIdWith404() {
    logger.info("=== Test Get Orderline By Id - With 404 ===");

    String lineId = ID_DOES_NOT_EXIST;

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(String.format(LINE_BY_ID_PATH, lineId))
        .then()
          .statusCode(404)
          .extract()
          .response();

    assertEquals(lineId, resp.getBody().asString());
  }

  @Test
  public void testGetOrderLineByIdWith500() {
    logger.info("=== Test Get Orderline By Id - With 500 ===");

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(String.format(LINE_BY_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR))
        .then()
          .statusCode(500)
          .extract()
          .response();

    assertEquals("Internal Server Error", resp.getBody().print());
  }

  @Test
  public void testPostOrdersLinesById(TestContext ctx) {
    logger.info("=== Test Post Order Lines By Id (expected flow) ===");

    JsonObject reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final CompositePoLine response = verifyPostResponse(LINES_PATH, reqData.encodePrettily(),
      EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, APPLICATION_JSON, 201).as(CompositePoLine.class);

    ctx.assertEquals(reqData.getString(PURCHASE_ORDER_ID), response.getPurchaseOrderId());
    ctx.assertNull(response.getInstanceId());

    Set<String> poLinesIds = new HashSet<>();
    for (Map.Entry<String, List<JsonObject>> obj : MockServer.serverRqRs.column(HttpMethod.POST).entrySet()) {
      String poLineId = obj.getValue().get(0).getString("po_line_id");
      if(poLineId != null) {
        poLinesIds.add(poLineId);
      }
    }

    ctx.assertEquals(1, poLinesIds.size());
  }

  @Test
  public void testPostOrdersLinesByIdPoLineWithoutId(TestContext ctx) throws IOException {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");

    Errors resp = verifyPostResponse(LINES_PATH, getMockData(PO_LINE_MIN_CONTENT_PATH),
      NON_EXIST_CONFIG_X_OKAPI_TENANT, APPLICATION_JSON, 422).as(Errors.class);

    ctx.assertEquals(1, resp.getErrors().size());
  }

  @Test
  public void testValidationOnPutLineWithoutBody() {
    logger.info("=== Test validation on PUT line with no body ===");
    RestAssured
      .with()
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
      .put(String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST))
        .then()
          .statusCode(400)
          .body(containsString("Json content error HV000116: The object to be validated must not be null"));
  }

  @Test
  public void testValidationOnPutWithIncorrectLang() throws IOException {
    logger.info("=== Test validation on PUT line with invalid lang query parameter ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(getMockData(PO_LINE_MIN_CONTENT_PATH))
      .put(String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST) + INVALID_LANG)
        .then()
          .statusCode(400)
          .body(containsString(INCORRECT_LANG_PARAMETER));
  }

  @Test
  public void testValidationOnPutWithIncorrectLineId() throws IOException {
    logger.info("=== Test validation on PUT line with invalid line ID path parameter ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(getMockData(PO_LINE_MIN_CONTENT_PATH))
      .put(String.format(LINE_BY_ID_PATH, ID_BAD_FORMAT))
        .then()
          .statusCode(400)
          .body(containsString("parameter is incorrect"));
  }

  @Test
  public void testPutOrderLineByIdWithEmptyBody() throws IOException {
    logger.info("=== Test PUT Order Line By Id With Empty Json - validation error because of required properties like PO ID ===");

    String url = String.format(LINE_BY_ID_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);

    Errors resp = verifyPut(url, getMockData(PO_LINE_MIN_CONTENT_PATH), "", 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
  }

  @Test
  public void testPutOrderLineById() {
    logger.info("=== Test PUT Order Line By Id - Success case ===");

    String lineId = PO_LINE_ID_FOR_SUCCESS_CASE;
    JsonObject body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId);

    String url = String.format(LINE_BY_ID_PATH, lineId);

    final Response resp = verifyPut(url, body.encodePrettily(), "", 204);

    assertTrue(StringUtils.isEmpty(resp.getBody().asString()));

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertEquals(2, column.size());
    assertThat(column.keySet(), containsInAnyOrder(REPORTING_CODES, SOURCE));
    assertThat(column.get(REPORTING_CODES), hasSize(3));

    column = MockServer.serverRqRs.column(HttpMethod.DELETE);
    assertEquals(1, column.size());
    assertThat(column.keySet(), containsInAnyOrder(CLAIMS));

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertEquals(10, column.size());
    assertThat(column.keySet(), containsInAnyOrder(ADJUSTMENT, COST, DETAILS, ERESOURCE, LOCATION, PHYSICAL, VENDOR_DETAIL, CLAIMS, FUND_DISTRIBUTION, PO_LINES));
  }

  @Test
  public void testPutOrderLineByIdWithPartiallyDeletedSubObjects() {
    logger.info("=== Test PUT Order Line By Id - Line refers to not existing sub-objects ===");

    String lineId = PO_LINE_ID_WITH_SOME_SUB_OBJECTS_ALREADY_REMOVED;
    JsonObject body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    final Response resp = verifyPut(url, body.encodePrettily(), APPLICATION_JSON, 500);
    assertEquals(3, resp.as(Errors.class).getErrors().size());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertEquals(3, column.size());
    assertThat(column.keySet(), containsInAnyOrder(CLAIMS, FUND_DISTRIBUTION, SOURCE));

    column = MockServer.serverRqRs.column(HttpMethod.DELETE);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertEquals(8, column.size());
    assertThat(column.keySet(), containsInAnyOrder(ADJUSTMENT, COST, DETAILS, ERESOURCE, LOCATION, PHYSICAL, VENDOR_DETAIL, PO_LINES));

    List<JsonObject> jsonObjects = column.get(PO_LINES);
    assertThat(jsonObjects, hasSize(1));
    JsonObject entry = jsonObjects.get(0);

    // Verify that references to failed objects are not persisted
    assertFalse(entry.containsKey(COST));
    assertFalse(entry.containsKey(DETAILS));
  }

  @Test
  public void testPutOrderLineByIdWithOneIncorrectFundDistroSubObject() {
    logger.info("=== Test PUT Order Line By Id - Line refers to not existing fund_distribution ===");

    String lineId = PO_LINE_ID_WITH_FUND_DISTRIBUTION_404_CODE;
    JsonObject body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId);

    String url = String.format(LINE_BY_ID_PATH, lineId);

    final Response resp = verifyPut(url, body.encodePrettily(), APPLICATION_JSON, 500);
    assertEquals(2, resp.as(Errors.class).getErrors().size());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.DELETE);
    assertEquals(1, column.size());
    assertThat(column, hasKey(FUND_DISTRIBUTION));

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertEquals(13, column.size());
    assertThat(column.keySet(), containsInAnyOrder(ADJUSTMENT, ALERTS, CLAIMS, COST, DETAILS, ERESOURCE, FUND_DISTRIBUTION, LOCATION, PHYSICAL, REPORTING_CODES, SOURCE, VENDOR_DETAIL, PO_LINES));

    List<JsonObject> jsonObjects = column.get(PO_LINES);
    assertThat(jsonObjects, hasSize(1));
    JsonObject entry = jsonObjects.get(0);

    // Verify that reference failed to be deleted still presented
    assertTrue(entry.containsKey(FUND_DISTRIBUTION));
    assertEquals(2, entry.getJsonArray(FUND_DISTRIBUTION).size());
  }

  @Test
  public void testPutOrderLineByIdWithoutOkapiUrlHeader() throws IOException {
    logger.info("=== Test PUT Order Line By Id - 500 due to missing Okapi URL header ===");

    String lineId = ID_DOES_NOT_EXIST;

    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId, ID_DOES_NOT_EXIST);

    RestAssured
      .with()
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(body)
      .put(url)
        .then()
          .statusCode(500)
          .contentType(TEXT_PLAIN);


    // Verify that no calls reached mock server
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testPutOrderLineByIdNotFound() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Not Found ===");

    String lineId = ID_DOES_NOT_EXIST;
    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId, PO_ID);

    Response actual = verifyPut(url, body, TEXT_PLAIN, 404);

    assertEquals(lineId, actual.asString());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.DELETE);
    assertTrue(column.isEmpty());
  }

  @Test
  public void testPutOrderLineByIdWithInvalidContentInBody() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Body Validation Error ===");

    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);
    String body = getPoLineWithMinContentAndIds(ID_BAD_FORMAT, PO_ID);

    Response resp = verifyPut(url, body, APPLICATION_JSON, 422);

    assertEquals(1, resp.as(Errors.class).getErrors().size());
    assertNotNull(resp.as(Errors.class).getErrors().get(0).getMessage());

    // Verify that no calls reached mock server
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testPutOrderLineByIdWithIdMismatch() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Ids mismatch ===");

    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);
    String body = getPoLineWithMinContentAndIds(PO_LINE_ID_FOR_SUCCESS_CASE, PO_ID);

    Response resp = verifyPut(url, body, APPLICATION_JSON, 422);

    assertEquals(1, resp.as(Errors.class).getErrors().size());
    assertNotNull(resp.as(Errors.class).getErrors().get(0).getMessage());

    // Verify that no calls reached mock server
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testPutOrderLineById500FromStorageOnGetPoLine() throws IOException {
    logger.info("=== Test PUT Order Line By Id - 500 From Storage On Get PO Line ===");

    String lineId = ID_FOR_INTERNAL_SERVER_ERROR;

    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId, PO_ID);

    Response actual = verifyPut(url, body, TEXT_PLAIN, 500);

    assertNotNull(actual.asString());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.DELETE);
    assertTrue(column.isEmpty());
  }

  @Test
  public void testPutOrderLineById500FromStorageOnSubObjectDeletion() throws IOException {
    logger.info("=== Test PUT Order Line By Id - 500 From Storage On Sub-Object deletion ===");

    String lineId = PO_LINE_ID_WITH_SUB_OBJECT_OPERATION_500_CODE;
    org.folio.rest.acq.model.PoLine line = getMockLine(lineId);
    String orderId = line.getPurchaseOrderId();
    String poLineNumber = line.getPoLineNumber();

    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId, orderId);
    Response actual = verifyPut(url, body, APPLICATION_JSON, 500);

    assertEquals(2, actual.as(Errors.class).getErrors().size());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.DELETE);
    assertThat(column.keySet(), containsInAnyOrder(ADJUSTMENT, COST, DETAILS, ERESOURCE, LOCATION, PHYSICAL, VENDOR_DETAIL));

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertThat(column.keySet(), containsInAnyOrder(PO_LINES, SOURCE));

    JsonObject lineWithIds = column.get(PO_LINES).get(0);

    // Verify that object has only PO and PO line ids
    assertEquals(lineId, lineWithIds.remove(ID));
    assertEquals(orderId, lineWithIds.remove(PURCHASE_ORDER_ID));
    assertEquals(poLineNumber, lineWithIds.remove(PO_LINE_NUMBER));
    lineWithIds.stream().forEach(entry -> {
      Object value = entry.getValue();
      // Required properties
      if (REQUIRED_PO_LINE_PROPERTIES.contains(entry.getKey())) {
        assertThat(value, is(notNullValue()));
      } else {
        assertTrue(Objects.isNull(value) || (value instanceof Iterable && !((Iterable) value).iterator().hasNext()));
      }
    });
  }

  private String getPoLineWithMinContentAndIds(String lineId, String orderId) throws IOException {
    CompositePoLine poLine = new JsonObject(getMockData(PO_LINE_MIN_CONTENT_PATH)).mapTo(CompositePoLine.class);
    poLine.setId(lineId);
    poLine.setPurchaseOrderId(orderId);
    return JsonObject.mapFrom(poLine).encode();
  }

  @Test
  public void testPoNumberValidatewithExistingPONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", EXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, TEXT_PLAIN, 400);
  }


  @Test
  public void testPoNumberValidatewithUniquePONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", NONEXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, "", 204);
  }

  @Test
  public void testPoNumberValidatewithInvalidPattern()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", "11");
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, APPLICATION_JSON, 422);
  }

  @Test
  public void testGetOrdersNoParameters() {
    logger.info("=== Test Get Orders - With empty query ===");

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(COMPOSITE_ORDERS_PATH)
        .then()
          .statusCode(200)
          .extract()
          .response();

    assertEquals(3, resp.getBody().as(PurchaseOrders.class).getTotalRecords().intValue());
  }

  @Test
  public void testGetOrdersBadQuery() {
    logger.info("=== Test Get Orders by query - unprocessable query to emulate 400 from storage ===");

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
        .param(QUERY_PARAM_NAME, BAD_QUERY)
      .get(COMPOSITE_ORDERS_PATH)
        .then()
          .statusCode(400)
          .contentType(TEXT_PLAIN);
  }

  @Test
  public void testGetOrdersInternalServerError() {
    logger.info("=== Test Get Orders by query - emulating 500 from storage ===");

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
        .param(QUERY_PARAM_NAME, ID_FOR_INTERNAL_SERVER_ERROR)
      .get(COMPOSITE_ORDERS_PATH)
        .then()
          .statusCode(500)
          .contentType(TEXT_PLAIN);
  }

  @Test
  public void testGetOrderLinessBadQuery() {
    logger.info("=== Test Get Order Lines by query - unprocessable query to emulate 400 from storage ===");

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
        .param(QUERY_PARAM_NAME, BAD_QUERY)
      .get(LINES_PATH)
        .then()
          .statusCode(400)
          .contentType(TEXT_PLAIN);
  }

  @Test
  public void testGetOrderLinesInternalServerError() {
    logger.info("=== Test Get Order Lines by query - emulating 500 from storage ===");

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
        .param(QUERY_PARAM_NAME, ID_FOR_INTERNAL_SERVER_ERROR)
      .get(LINES_PATH)
        .then()
          .statusCode(500)
          .contentType(TEXT_PLAIN);
  }

  @Test
  public void testGetOrderPOLinesByPoId() {
    logger.info("=== Test Get Orders lines - With empty query ===");

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
        .param(QUERY_PARAM_NAME, "purchase_order_id==" + ORDER_ID_WITH_PO_LINES)
      .get(LINES_PATH)
        .then()
          .statusCode(200)
          .extract()
          .response();

    assertEquals(2, resp.getBody().as(PoLineCollection.class).getTotalRecords().intValue());
  }

  @Test
  public void testGetPoNumber() {
    logger.info("=== Test Get PO Number (generate po_number) ===");

    final Response response = RestAssured
      .with()
      .header(X_OKAPI_URL)
      .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get("/orders/po-number")
      .then()
      .statusCode(200)
      .extract()
      .response();

    String actualResponse = response.getBody().asString();
    logger.info(actualResponse);

    PoNumber poNumber = response.as(PoNumber.class);
    String actualPoNumberValue = poNumber.getPoNumber();

    assertEquals(PO_NUMBER_VALUE, actualPoNumberValue);
    assertNotNull(actualResponse);
  }

  @Test
  public void testGetPoNumberError() {
    logger.info("=== Test Get PO Number (generate po_number) - fail ===");

    RestAssured
      .with()
      .header(X_OKAPI_URL)
      .header(PO_NUMBER_ERROR_X_OKAPI_TENANT)
      .get("/orders/po-number")
      .then()
      .statusCode(500)
      .extract()
      .response();
  }

  @Test
  public void testGetReceivingHistory() {
    logger.info("=== Test Get Receiving History - With empty query ===");

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(ORDERS_RECEIVING_HISTORY_ENDPOINT)
        .then()
          .statusCode(200)
          .extract()
          .response();

    assertEquals(0, resp.getBody().as(ReceivingHistoryCollection.class).getTotalRecords().intValue());
  }

  @Test
  public void testGetReceivingHistoryForPurchaseOrder() {
    logger.info("=== Test Get Receiving History - With purchase Order query ===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, RECEIVING_HISTORY_PURCHASE_ORDER_ID);

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(endpointQuery)
        .then()
          .statusCode(200)
          .extract()
          .response();

    assertEquals(1, resp.getBody().as(ReceivingHistoryCollection.class).getTotalRecords().intValue());
  }

  @Test
  public void testGetReceivingHistoryForPurchaseOrderWithError() {
    logger.info("=== Test Get Receiving History - With purchase Order query Error===");
    String endpointQuery = String.format("%s?query=purchaseOrderId=%s", ORDERS_RECEIVING_HISTORY_ENDPOINT, INTERNAL_SERVER_ERROR);

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(endpointQuery)
        .then()
          .statusCode(500)
          .extract()
          .response();

  }

  @Test
  public void testGetReceivingHistoryBadRequest() {
    logger.info("=== Test Get Receiving History - With Bad Request");

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get(ORDERS_RECEIVING_HISTORY_ENDPOINT+"?query="+BAD_REQUEST)
        .then()
          .statusCode(400)
          .extract()
          .response();

  }

  private org.folio.rest.acq.model.PoLine getMockLine(String id) {
    return getMockAsJson(PO_LINES_MOCK_DATA_PATH, id).mapTo(org.folio.rest.acq.model.PoLine.class);
  }

  private JsonObject getMockAsJson(String path, String id) {
    try {
      return new JsonObject(getMockData(String.format("%s%s.json", path, id)));
    } catch (IOException e) {
      fail(e.getMessage());
    }
    return new JsonObject();
  }

  private Response verifyPut(String url, String body, String expectedContentType, int expectedCode) {
    return RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
        .header(X_OKAPI_TOKEN)
        .contentType(APPLICATION_JSON)
        .body(body)
      .put(url)
        .then()
          .statusCode(expectedCode)
          .contentType(expectedContentType)
          .extract()
            .response();
  }

  private JsonObject getMockDraftOrder() throws Exception {
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH));
    order.put("workflow_status", "Pending");

    return order;
  }

  public static String getMockData(String path) throws IOException {
    try (InputStream resourceAsStream = OrdersImplTest.class.getClassLoader().getResourceAsStream(path)) {
      if (resourceAsStream != null) {
        return IOUtils.toString(resourceAsStream, StandardCharsets.UTF_8);
      } else {
        StringBuilder sb = new StringBuilder();
        try (Stream<String> lines = Files.lines(Paths.get(path))) {
          lines.forEach(sb::append);
        }
        return sb.toString();
      }
    }
  }

  public static class MockServer {

    private static final String TOTAL_RECORDS = "total_records";
    static Table<String, HttpMethod, List<JsonObject>> serverRqRs = HashBasedTable.create();
    private static final Logger logger = LoggerFactory.getLogger(MockServer.class);

    final int port;
    final Vertx vertx;

    MockServer(int port) {
      this.port = port;
      this.vertx = Vertx.vertx();
    }

    void close() {
      vertx.close(res -> {
        if (res.failed()) {
          logger.error("Failed to shut down mock server", res.cause());
          fail(res.cause().getMessage());
        } else {
          logger.info("Successfully shut down mock server");
        }
      });
    }

    Router defineRoutes() {
      Router router = Router.router(vertx);

      router.route().handler(BodyHandler.create());
      router.route(HttpMethod.POST, resourcesPath(PURCHASE_ORDER)).handler(this::handlePostPurchaseOrder);
      router.route(HttpMethod.POST, "/inventory/instances").handler(this::handlePostInstanceRecord);
      router.route(HttpMethod.POST, "/item-storage/items").handler(this::handlePostItemRecord);
      router.route(HttpMethod.POST, "/holdings-storage/holdings").handler(this::handlePostHoldingRecord);
      router.route(HttpMethod.POST, resourcesPath(PO_LINES)).handler(this::handlePostPOLine);
      router.route(HttpMethod.POST, resourcesPath(ADJUSTMENT)).handler(ctx -> handlePostGenericSubObj(ctx, ADJUSTMENT));
      router.route(HttpMethod.POST, resourcesPath(ALERTS)).handler(ctx -> handlePostGenericSubObj(ctx, ALERTS));
      router.route(HttpMethod.POST, resourcesPath(CLAIMS)).handler(ctx -> handlePostGenericSubObj(ctx, CLAIMS));
      router.route(HttpMethod.POST, resourcesPath(COST)).handler(ctx -> handlePostGenericSubObj(ctx, COST));
      router.route(HttpMethod.POST, resourcesPath(DETAILS)).handler(ctx -> handlePostGenericSubObj(ctx, DETAILS));
      router.route(HttpMethod.POST, resourcesPath(ERESOURCE)).handler(ctx -> handlePostGenericSubObj(ctx, ERESOURCE));
      router.route(HttpMethod.POST, resourcesPath(FUND_DISTRIBUTION)).handler(ctx -> handlePostGenericSubObj(ctx, FUND_DISTRIBUTION));
      router.route(HttpMethod.POST, resourcesPath(LOCATION)).handler(ctx -> handlePostGenericSubObj(ctx, LOCATION));
      router.route(HttpMethod.POST, resourcesPath(PHYSICAL)).handler(ctx -> handlePostGenericSubObj(ctx, PHYSICAL));
      router.route(HttpMethod.POST, resourcesPath(REPORTING_CODES)).handler(ctx -> handlePostGenericSubObj(ctx, REPORTING_CODES));
      router.route(HttpMethod.POST, resourcesPath(SOURCE)).handler(ctx -> handlePostGenericSubObj(ctx, SOURCE));
      router.route(HttpMethod.POST, resourcesPath(VENDOR_DETAIL)).handler(ctx -> handlePostGenericSubObj(ctx, VENDOR_DETAIL));
      router.route(HttpMethod.POST, resourcesPath(PIECES)).handler(ctx -> handlePostGenericSubObj(ctx, PIECES));

      router.route(HttpMethod.GET, resourcesPath(PURCHASE_ORDER)+"/:id").handler(this::handleGetPurchaseOrderById);
      router.route(HttpMethod.GET, resourcesPath(PURCHASE_ORDER)).handler(this::handleGetPurchaseOrderByQuery);
      router.route(HttpMethod.GET, "/instance-types").handler(this::handleGetInstanceType);
      router.route(HttpMethod.GET, "/instance-statuses").handler(this::handleGetInstanceStatus);
      router.route(HttpMethod.GET, "/identifier-types").handler(this::handleGetIdentifierType);
      router.route(HttpMethod.GET, "/inventory/instances").handler(this::handleGetInstanceRecord);
      router.route(HttpMethod.GET, "/item-storage/items").handler(this::handleGetItemsRecords);
      router.route(HttpMethod.GET, "/holdings-storage/holdings").handler(this::handleGetHoldingRecord);
      router.route(HttpMethod.GET, "/loan-types").handler(this::handleGetLoanType);
      router.route(HttpMethod.GET, resourcesPath(PO_LINES)).handler(this::handleGetPoLines);
      router.route(HttpMethod.GET, resourcePath(PO_LINES)).handler(this::handleGetPoLineById);
      router.route(HttpMethod.GET, resourcePath(ADJUSTMENT)).handler(this::handleGetAdjustment);
      router.route(HttpMethod.GET, resourcePath(ALERTS)).handler(ctx -> handleGetGenericSubObj(ctx, ALERTS));
      router.route(HttpMethod.GET, resourcePath(CLAIMS)).handler(ctx -> handleGetGenericSubObj(ctx, CLAIMS));
      router.route(HttpMethod.GET, resourcePath(COST)).handler(ctx -> handleGetGenericSubObj(ctx, COST));
      router.route(HttpMethod.GET, resourcePath(DETAILS)).handler(ctx -> handleGetGenericSubObj(ctx, DETAILS));
      router.route(HttpMethod.GET, resourcePath(ERESOURCE)).handler(ctx -> handleGetGenericSubObj(ctx, ERESOURCE));
      router.route(HttpMethod.GET, resourcePath(FUND_DISTRIBUTION)).handler(ctx -> handleGetGenericSubObj(ctx, FUND_DISTRIBUTION));
      router.route(HttpMethod.GET, resourcePath(LOCATION)).handler(this::handleGetLocation);
      router.route(HttpMethod.GET, resourcePath(PHYSICAL)).handler(ctx -> handleGetGenericSubObj(ctx, PHYSICAL));
      router.route(HttpMethod.GET, resourcePath(REPORTING_CODES)).handler(ctx -> handleGetGenericSubObj(ctx, REPORTING_CODES));
      router.route(HttpMethod.GET, resourcePath(SOURCE)).handler(ctx -> handleGetGenericSubObj(ctx, SOURCE));
      router.route(HttpMethod.GET, resourcePath(VENDOR_DETAIL)).handler(ctx -> handleGetGenericSubObj(ctx, VENDOR_DETAIL));
      router.route(HttpMethod.GET, resourcesPath(PO_NUMBER)).handler(this::handleGetPoNumber);
      router.route(HttpMethod.GET, resourcesPath(PIECES)).handler(ctx -> handleGetGenericPieceObj(ctx, PIECES));
      router.route(HttpMethod.GET, resourcesPath(RECEIVING_HISTORY)).handler(this::handleGetReceivingHistory);

      router.route(HttpMethod.PUT, resourcePath(PURCHASE_ORDER)).handler(ctx -> handlePutGenericSubObj(ctx, PURCHASE_ORDER));
      router.route(HttpMethod.PUT, resourcePath(PO_LINES)).handler(ctx -> handlePutGenericSubObj(ctx, PO_LINES));
      router.route(HttpMethod.PUT, resourcePath(ADJUSTMENT)).handler(ctx -> handlePutGenericSubObj(ctx, ADJUSTMENT));
      router.route(HttpMethod.PUT, resourcePath(ALERTS)).handler(ctx -> handlePutGenericSubObj(ctx, ALERTS));
      router.route(HttpMethod.PUT, resourcePath(CLAIMS)).handler(ctx -> handlePutGenericSubObj(ctx, CLAIMS));
      router.route(HttpMethod.PUT, resourcePath(COST)).handler(ctx -> handlePutGenericSubObj(ctx, COST));
      router.route(HttpMethod.PUT, resourcePath(DETAILS)).handler(ctx -> handlePutGenericSubObj(ctx, DETAILS));
      router.route(HttpMethod.PUT, resourcePath(ERESOURCE)).handler(ctx -> handlePutGenericSubObj(ctx, ERESOURCE));
      router.route(HttpMethod.PUT, resourcePath(FUND_DISTRIBUTION)).handler(ctx -> handlePutGenericSubObj(ctx, FUND_DISTRIBUTION));
      router.route(HttpMethod.PUT, resourcePath(LOCATION)).handler(ctx -> handlePutGenericSubObj(ctx, LOCATION));
      router.route(HttpMethod.PUT, resourcePath(PHYSICAL)).handler(ctx -> handlePutGenericSubObj(ctx, PHYSICAL));
      router.route(HttpMethod.PUT, resourcePath(REPORTING_CODES)).handler(ctx -> handlePutGenericSubObj(ctx, REPORTING_CODES));
      router.route(HttpMethod.PUT, resourcePath(SOURCE)).handler(ctx -> handlePutGenericSubObj(ctx, SOURCE));
      router.route(HttpMethod.PUT, resourcePath(VENDOR_DETAIL)).handler(ctx -> handlePutGenericSubObj(ctx, VENDOR_DETAIL));


      router.route(HttpMethod.DELETE, resourcesPath(PURCHASE_ORDER)+"/:id").handler(ctx -> handleDeleteGenericSubObj(ctx, PURCHASE_ORDER));
      router.route(HttpMethod.DELETE, resourcePath(PO_LINES)).handler(ctx -> handleDeleteGenericSubObj(ctx, PO_LINES));
      router.route(HttpMethod.DELETE, resourcePath(ADJUSTMENT)).handler(ctx -> handleDeleteGenericSubObj(ctx, ADJUSTMENT));
      router.route(HttpMethod.DELETE, resourcePath(ALERTS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ALERTS));
      router.route(HttpMethod.DELETE, resourcePath(CLAIMS)).handler(ctx -> handleDeleteGenericSubObj(ctx, CLAIMS));
      router.route(HttpMethod.DELETE, resourcePath(COST)).handler(ctx -> handleDeleteGenericSubObj(ctx, COST));
      router.route(HttpMethod.DELETE, resourcePath(DETAILS)).handler(ctx -> handleDeleteGenericSubObj(ctx, DETAILS));
      router.route(HttpMethod.DELETE, resourcePath(ERESOURCE)).handler(ctx -> handleDeleteGenericSubObj(ctx, ERESOURCE));
      router.route(HttpMethod.DELETE, resourcePath(LOCATION)).handler(ctx -> handleDeleteGenericSubObj(ctx, LOCATION));
      router.route(HttpMethod.DELETE, resourcePath(PHYSICAL)).handler(ctx -> handleDeleteGenericSubObj(ctx, PHYSICAL));
      router.route(HttpMethod.DELETE, resourcePath(REPORTING_CODES)).handler(ctx -> handleDeleteGenericSubObj(ctx, REPORTING_CODES));
      router.route(HttpMethod.DELETE, resourcePath(SOURCE)).handler(ctx -> handleDeleteGenericSubObj(ctx, SOURCE));
      router.route(HttpMethod.DELETE, resourcePath(VENDOR_DETAIL)).handler(ctx -> handleDeleteGenericSubObj(ctx, VENDOR_DETAIL));
      router.route(HttpMethod.DELETE, resourcePath(FUND_DISTRIBUTION)).handler(ctx -> handleDeleteGenericSubObj(ctx, FUND_DISTRIBUTION));

      router.get("/configurations/entries").handler(this::handleConfigurationModuleResponse);
      return router;
    }

    private void handlePostInstanceRecord(RoutingContext ctx) {
      logger.info("handlePostInstanceRecord got: " + ctx.getBodyAsString());
      JsonObject body = ctx.getBodyAsJson();
      addServerRqRsData(HttpMethod.POST, INSTANCE_RECORD, body);

      ctx.response()
        .setStatusCode(201)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .putHeader(HttpHeaders.LOCATION, ctx.request().absoluteURI() + "/" + UUID.randomUUID().toString())
        .end();
    }

    private void handlePostHoldingRecord(RoutingContext ctx) {
      logger.info("handlePostHoldingsRecord got: " + ctx.getBodyAsString());
      JsonObject body = ctx.getBodyAsJson();
      addServerRqRsData(HttpMethod.POST, HOLDINGS_RECORD, body);

      ctx.response()
        .setStatusCode(201)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .putHeader(HttpHeaders.LOCATION, ctx.request().absoluteURI() + "/" + UUID.randomUUID().toString())
        .end();
    }

    private void handlePostItemRecord(RoutingContext ctx) {
      String bodyAsString = ctx.getBodyAsString();
      logger.info("handlePostItemRecord got: " + bodyAsString);

      if (bodyAsString.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        serverResponse(ctx, 500, TEXT_PLAIN, INTERNAL_SERVER_ERROR);
      } else {
        JsonObject bodyAsJson = ctx.getBodyAsJson();
        bodyAsJson.put(ID, UUID.randomUUID().toString());
        addServerRqRsData(HttpMethod.POST, ITEM_RECORDS, bodyAsJson);
        ctx.response()
           .setStatusCode(201)
           .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
           .putHeader(HttpHeaders.LOCATION, ctx.request().absoluteURI() + "/" + bodyAsJson.getString(ID))
           .end(bodyAsJson.encode());
      }
    }

    private void handleGetInstanceRecord(RoutingContext ctx) {
      logger.info("handleGetInstanceRecord got: " + ctx.request().path());

      try {
        JsonObject instance;
        if (ctx.request().getParam("query").contains("ocn956625961")) {
          instance = new JsonObject(getMockData(INSTANCE_RECORDS_MOCK_DATA_PATH + "instance.json"));
        } else {
          instance = new JsonObject().put("instances", new JsonArray());
        }
        addServerRqRsData(HttpMethod.GET, INSTANCE_RECORD, instance);
        serverResponse(ctx, 200, APPLICATION_JSON, instance.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(404)
          .end();
      }
    }

    private void handleGetHoldingRecord(RoutingContext ctx) {
      logger.info("handleGetHoldingRecord got: " + ctx.request().path());

      JsonObject instance = new JsonObject().put("holdingsRecords", new JsonArray());

      addServerRqRsData(HttpMethod.GET, HOLDINGS_RECORD, instance);
      serverResponse(ctx, 200, APPLICATION_JSON, instance.encodePrettily());
    }

    private void handleGetItemsRecords(RoutingContext ctx) {
      logger.info("handleGetItemsRecords got: " + ctx.request().path());

      try {
        JsonObject items;
        if (ctx.request().getParam("query").contains(PO_LINE_ID_FOR_SUCCESS_CASE)) {
          items = new JsonObject(getMockData(ITEMS_RECORDS_MOCK_DATA_PATH + "itemsRecords-1.json"));
        } else {
          items = new JsonObject().put("items", new JsonArray());
        }
        addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, items);
        serverResponse(ctx, 200, APPLICATION_JSON, items.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(404)
          .end();
      }
    }

    private void handleGetLoanType(RoutingContext ctx) {
      logger.info("handleGetLoanType got: " + ctx.request().path());

      try {
        JsonObject po = new JsonObject(getMockData(LOAN_TYPES_MOCK_DATA_PATH + "Can circulate.json"));
        serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
      } catch (IOException e) {
        ctx.response()
           .setStatusCode(404)
           .end();
      }
    }

    private void handleGetIdentifierType(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());

      try {
        JsonObject po = new JsonObject(getMockData(INSTANCE_IDENTIFIERS_MOCK_DATA_PATH + "ISBN.json"));
        serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(404)
          .end();
      }
    }

    private void handleGetInstanceStatus(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());

      try {
        JsonObject po = new JsonObject(getMockData(INSTANCE_STATUSES_MOCK_DATA_PATH + "temp.json"));
        serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(404)
          .end();
      }
    }

    private void handleGetInstanceType(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());

      try {
        JsonObject po = new JsonObject(getMockData(INSTANCE_TYPES_MOCK_DATA_PATH + "zzz.json"));
        serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(404)
          .end();
      }
    }

    private void handleGetReceivingHistory(RoutingContext ctx) {
      logger.info("handleGetItemsRecords got: " + ctx.request().path());
      String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
      try {
        JsonObject receivingHistory;
        if (queryParam.contains(RECEIVING_HISTORY_PURCHASE_ORDER_ID)) {
          receivingHistory = new JsonObject(getMockData(RECEIVING_HISTORY_MOCK_DATA_PATH + "receivingHistory.json"));
        } else if(queryParam.contains(INTERNAL_SERVER_ERROR)) {
          throw new HttpException(500, "Exception in orders-storage module");
        }
        else if(queryParam.contains(BAD_REQUEST)) {
          throw new HttpException(400, "QueryValidationException");
        }
        else {
          receivingHistory = new JsonObject();
          receivingHistory.put("receiving_history", new JsonArray());
          receivingHistory.put("total_records", 0);
        }
        addServerRqRsData(HttpMethod.GET, RECEIVING_HISTORY, receivingHistory);
        serverResponse(ctx, 200, APPLICATION_JSON, receivingHistory.encodePrettily());
      } catch (IOException e) {
        ctx.response()
           .setStatusCode(404)
           .end();
      } catch (HttpException e) {
        ctx.response()
           .setStatusCode(e.getCode())
           .end();
      }
    }

    private String resourcePath(String subObjName) {
      return resourceByIdPath(subObjName) + ":id";
    }

    private void handleConfigurationModuleResponse(RoutingContext ctx) {
      try {
        String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT) ;
        serverResponse(ctx, 200, APPLICATION_JSON, getMockData(String.format(CONFIG_MOCK_PATH, tenant)));
      } catch (IOException e) {
        serverResponse(ctx, 500, TEXT_PLAIN, INTERNAL_SERVER_ERROR);
      }

    }

    private void handleDeleteGenericSubObj(RoutingContext ctx, String subObj) {
      String id = ctx.request().getParam(ID);

      addServerRqRsData(HttpMethod.DELETE, subObj, new JsonObject().put(ID, id));

      if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
        serverResponse(ctx, 500, TEXT_PLAIN, INTERNAL_SERVER_ERROR);
      } else {
        ctx.response()
           .setStatusCode(204)
           .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
           .end();
      }
    }

    void start(TestContext context) {
      // Setup Mock Server...
      HttpServer server = vertx.createHttpServer();

      final Async async = context.async();
      server.requestHandler(defineRoutes()::accept).listen(port, result -> {
        if (result.failed()) {
          logger.warn(result.cause());
        }
        context.assertTrue(result.succeeded());
        async.complete();
      });
    }

    private void handleGetPoLines(RoutingContext ctx) {
      logger.info("handleGetPoLines got: {}?{}", ctx.request().path(), ctx.request().query());

      String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
      if (queryParam.contains(BAD_QUERY)) {
        serverResponse(ctx, 400, TEXT_PLAIN, Status.BAD_REQUEST.getReasonPhrase());
      } else if (queryParam.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        serverResponse(ctx, 500, TEXT_PLAIN, Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
      } else {
        String id = queryParam.split("purchase_order_id==")[1];
        String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT);

        try {
          JsonObject po_lines;
          if (id.equals(ORDER_ID_WITH_PO_LINES)) {
            po_lines = new JsonObject(getMockData(POLINES_COLLECTION));
          } else {
            String filePath;
            if (ID_FOR_PENDING_ORDER.equals(id)) {
              filePath = LISTED_PRINT_MONOGRAPH_PATH;
            } else {
              filePath = String.format("%s%s.json", COMP_ORDER_MOCK_DATA_PATH, id);
            }
            JsonObject compPO = new JsonObject(getMockData(filePath));
            // Build PoLineCollection to make sure content is valid
            PoLineCollection poLines = buildPoLineCollection(tenant, compPO.getJsonArray(COMPOSITE_PO_LINES));
            po_lines = JsonObject.mapFrom(poLines);
          }

          logger.info(po_lines.encodePrettily());

          serverResponse(ctx, 200, APPLICATION_JSON, po_lines.encode());
        } catch (IOException e) {
          serverResponse(ctx, 404, TEXT_PLAIN, id);
        }
      }
    }

    private PoLineCollection buildPoLineCollection(String tenant, JsonArray lines) {
      PoLineCollection result = new PoLineCollection();
      if (lines == null || lines.isEmpty()) {
        result.setTotalRecords(0);
        result.setFirst(0);
        result.setLast(0);
      } else {
        // Transform composite PO Lines to storage representation
        List<org.folio.rest.acq.model.PoLine> poLines = lines
          .stream()
          .map(l -> (JsonObject) l)
          .map(line -> {
            replaceObjectById(line, ADJUSTMENT, COST, DETAILS, ERESOURCE, LOCATION, PHYSICAL, SOURCE, VENDOR_DETAIL);
            replaceObjectsByIds(line, ALERTS, CLAIMS, FUND_DISTRIBUTION, REPORTING_CODES);
            return line.mapTo(org.folio.rest.acq.model.PoLine.class);
          })
          .collect(Collectors.toList());

        // Set PO Line number if empty
        for (org.folio.rest.acq.model.PoLine line : poLines) {
          if (StringUtils.isEmpty(line.getPoLineNumber())) {
            line.setPoLineNumber(PO_NUMBER_VALUE + "-1");
          }
        }

        result.setPoLines(poLines);
        result.setFirst(1);
        result.setLast(lines.size());
        if (EMPTY_CONFIG_TENANT.equals(tenant)) {
          result.setTotalRecords(Integer.parseInt(DEFAULT_POLINE_LIMIT));
        } else {
          result.setTotalRecords(lines.size());
        }
      }
      return result;
    }

    private void replaceObjectsByIds(JsonObject line, String... property) {
      for (String prop : property) {
        List<?> objs = ((List<?>) line.remove(prop));
        if (objs != null) {
          line.put(prop, new JsonArray(objs.stream()
                                           .map(o -> ((Map<?, ?>) o).get(ID))
                                           .filter(Objects::nonNull)
                                           .collect(Collectors.toList())));
        }
      }
    }

    private void replaceObjectById(JsonObject line, String... property) {
      for (String prop : property) {
        try {
          Map<?, ?> obj = (Map<?, ?>) line.remove(prop);
          if (obj != null && obj.containsKey(ID)) {
            line.put(prop, obj.get(ID));
          }
        } catch (Exception e) {
          logger.error("Error replacing content for '{}' sub-object", prop);
          throw e;
        }
      }
    }

    private void handleGetPoLineById(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam(ID);
      logger.info("id: " + id);

      addServerRqRsData(HttpMethod.GET, PO_LINES, new JsonObject().put(ID, id));

      if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
        serverResponse(ctx, 500, TEXT_PLAIN, INTERNAL_SERVER_ERROR);
      } else {
        try {

          JsonObject po = null;

          // Attempt to find POLine in mock server memory
          Map<String, List<JsonObject>> column = serverRqRs.column(HttpMethod.POST);
          if (MapUtils.isNotEmpty(column) && CollectionUtils.isNotEmpty(column.get(PO_LINES))) {
            List<JsonObject> objects = new ArrayList<>(column.get(PO_LINES));
            Comparator<JsonObject> comparator = Comparator.comparing(o -> o.getString(ID));
            objects.sort(comparator);
            int ind = Collections.binarySearch(objects, new JsonObject().put(ID, id), comparator);
            if(ind > -1) {
              po = objects.get(ind);
            }
          }

          // If previous step has no result then attempt to find POLine in stubs
          if (po == null) {
            po = new JsonObject(getMockData(String.format("%s%s.json", PO_LINES_MOCK_DATA_PATH, id)));
          }

          serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
        } catch (IOException e) {
          serverResponse(ctx, 404, TEXT_PLAIN, id);
        }
      }
    }

    private void serverResponse(RoutingContext ctx, int statusCode, String contentType, String body) {
      ctx.response()
         .setStatusCode(statusCode)
         .putHeader(HttpHeaders.CONTENT_TYPE, contentType)
         .end(body);
    }

    private void handleGetGenericSubObj(RoutingContext ctx, String subObj) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam(ID);
      logger.info("id: " + id);

      JsonObject data = new JsonObject().put(ID, id);
      addServerRqRsData(HttpMethod.GET, subObj, data);

      if (ID_DOES_NOT_EXIST.equals(id)) {
        serverResponse(ctx, 404, TEXT_PLAIN, id);
      } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
        serverResponse(ctx, 500, TEXT_PLAIN, INTERNAL_SERVER_ERROR);
      } else {
        ctx.response()
           .setStatusCode(200)
           .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
           .end(data.encodePrettily());
      }
    }

    private void handleGetGenericPieceObj(RoutingContext ctx, String subObj) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam("query").split("poLineId=")[1];
      logger.info("id: " + id);

      JsonArray pieces = new JsonArray();
      JsonObject data = new JsonObject().put("pieces", pieces).put("total_records", 0);
      addServerRqRsData(HttpMethod.GET, subObj, data);

      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(data.encodePrettily());
    }

    private void handlePutGenericSubObj(RoutingContext ctx, String subObj) {
      logger.info("handlePutGenericSubObj got: PUT " + ctx.request().path());
      String id = ctx.request().getParam(ID);

      JsonObject body = ctx.getBodyAsJson();
      logger.info("body: " + body.encodePrettily());

      addServerRqRsData(HttpMethod.PUT, subObj, body);

      if (ID_DOES_NOT_EXIST.equals(id)) {
        serverResponse(ctx, 404, TEXT_PLAIN, id);
      } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
        serverResponse(ctx, 500, TEXT_PLAIN, INTERNAL_SERVER_ERROR);
      } else {
        ctx.response()
           .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
           .end();
      }
    }

    private void addServerRqRsData(HttpMethod method, String objName, JsonObject data) {
      List<JsonObject> entries = serverRqRs.get(objName, method);
      if (entries == null) {
        entries = new ArrayList<>();
      }
      entries.add(data);
      serverRqRs.put(objName, method, entries);
    }

    private void handleGetAdjustment(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam(ID);
      logger.info("id: " + id);

      addServerRqRsData(HttpMethod.GET, ADJUSTMENT, new JsonObject().put(ID, id));

      Adjustment a = new Adjustment();
      a.setId(id);
      a.setCredit(1d);
      a.setDiscount(2d);
      a.setInsurance(3d);
      a.setOverhead(4d);
      a.setShipment(5d);
      a.setTax1(6d);
      a.setTax2(7d);
      a.setInvoiceId(UUID.randomUUID().toString());

      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(a).encodePrettily());
    }

    private void handleGetPurchaseOrderById(RoutingContext ctx) {
      logger.info("handleGetPurchaseOrderById got: GET " + ctx.request().path());
      String id = ctx.request().getParam(ID);
      logger.info("id: " + id);

      try {
        String filePath;
        if (ID_FOR_PENDING_ORDER.equals(id)) {
          filePath = LISTED_PRINT_MONOGRAPH_PATH;
        } else {
          filePath = String.format("%s%s.json", COMP_ORDER_MOCK_DATA_PATH, id);
        }
        JsonObject po = new JsonObject(getMockData(filePath));
        po.remove(ADJUSTMENT);
        po.remove(COMPOSITE_PO_LINES);
        po.put(ADJUSTMENT, UUID.randomUUID().toString());

        // Validate the content against schema
        org.folio.rest.acq.model.PurchaseOrder order = po.mapTo(org.folio.rest.acq.model.PurchaseOrder.class);
        order.setId(id);
        po = JsonObject.mapFrom(order);
        addServerRqRsData(HttpMethod.GET, PURCHASE_ORDER, po);
        serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(404)
          .end(id);
      }
    }

    private void handleGetPurchaseOrderByQuery(RoutingContext ctx) {

      String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
      if (queryParam.contains(BAD_QUERY)) {
        serverResponse(ctx, 400, TEXT_PLAIN, Status.BAD_REQUEST.getReasonPhrase());
      } else if (queryParam.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        serverResponse(ctx, 500, TEXT_PLAIN, Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
      } else {
        JsonObject po = new JsonObject();
        addServerRqRsData(HttpMethod.GET, PURCHASE_ORDER, po);
        final String PO_NUMBER_QUERY = "po_number==";
        switch (queryParam) {
          case PO_NUMBER_QUERY + EXISTING_PO_NUMBER:
            po.put(TOTAL_RECORDS, 1);
            break;
          case PO_NUMBER_QUERY + NONEXISTING_PO_NUMBER:
            po.put(TOTAL_RECORDS, 0);
            break;
          case StringUtils.EMPTY:
            po.put(TOTAL_RECORDS, 3);
            break;
          default:
            //modify later as needed
            po.put(TOTAL_RECORDS, 0);
        }
        addServerRqRsData(HttpMethod.GET, PURCHASE_ORDER, po);
        serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
      }
    }

    private void handlePostPurchaseOrder(RoutingContext ctx) {
      logger.info("got: " + ctx.getBodyAsString());
      JsonObject body = ctx.getBodyAsJson();
      org.folio.rest.acq.model.PurchaseOrder po = body.mapTo(org.folio.rest.acq.model.PurchaseOrder.class);
      po.setId(UUID.randomUUID().toString());
      addServerRqRsData(HttpMethod.POST, PURCHASE_ORDER, body);

      ctx.response()
        .setStatusCode(201)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(po).encodePrettily());
    }

    private void handlePostGenericSubObj(RoutingContext ctx, String subObj) {
      logger.info("got: " + ctx.getBodyAsString());

      String echoStatus = ctx.request().getHeader(X_ECHO_STATUS);

      int status = 201;
      String respBody = "";
      String contentType = TEXT_PLAIN;

      if (echoStatus != null) {
        try {
          status = Integer.parseInt(echoStatus);
        } catch (NumberFormatException e) {
          logger.error("Exception parsing " + X_ECHO_STATUS, e);
        }
      }
      ctx.response().setStatusCode(status);

      JsonObject body = null;
      switch (status) {
        case 201:
          contentType = APPLICATION_JSON;
          body = JsonObject.mapFrom(ctx.getBodyAsJson().mapTo(getSubObjClass(subObj))).put(ID, UUID.randomUUID().toString());
          respBody = body.encodePrettily();
          break;
        case 403:
          respBody = "Access requires permission: foo.bar.baz";
          break;
        case 500:
          respBody = INTERNAL_SERVER_ERROR;
          break;
      }

      addServerRqRsData(HttpMethod.POST, subObj, body);
      serverResponse(ctx, status, contentType, respBody);
    }

    private Class<?> getSubObjClass(String subObj) {
      switch (subObj) {
        case ADJUSTMENT:
          return org.folio.rest.acq.model.Adjustment.class;
        case ALERTS:
          return org.folio.rest.acq.model.Alert.class;
        case CLAIMS:
          return org.folio.rest.acq.model.Claim.class;
        case COST:
          return org.folio.rest.acq.model.Cost.class;
        case DETAILS:
          return org.folio.rest.acq.model.Details.class;
        case ERESOURCE:
          return org.folio.rest.acq.model.Eresource.class;
        case FUND_DISTRIBUTION:
          return org.folio.rest.acq.model.FundDistribution.class;
        case LOCATION:
          return org.folio.rest.acq.model.Location.class;
        case PHYSICAL:
          return org.folio.rest.acq.model.Physical.class;
        case REPORTING_CODES:
          return org.folio.rest.acq.model.ReportingCode.class;
        case SOURCE:
          return org.folio.rest.acq.model.Source.class;
        case VENDOR_DETAIL:
          return org.folio.rest.acq.model.VendorDetail.class;
        case PIECES:
            return org.folio.rest.acq.model.Piece.class;
      }

      fail("The sub-object is unknown");
      return null;
    }

    private void handlePostPOLine(RoutingContext ctx) {
      logger.info("got po_line: " + ctx.getBodyAsString());
      JsonObject body = ctx.getBodyAsJson();
      org.folio.rest.acq.model.PoLine pol = body.mapTo(org.folio.rest.acq.model.PoLine.class);

      if (pol.getId() == null) {
        pol.setId(UUID.randomUUID().toString());
      }


      if (ID_FOR_INTERNAL_SERVER_ERROR.equals(pol.getPurchaseOrderId())) {
        ctx.response()
          .setStatusCode(500)
          .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
          .end();
      } else {
        ctx.response()
          .setStatusCode(201)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(JsonObject.mapFrom(pol).encodePrettily());
      }

      addServerRqRsData(HttpMethod.POST, PO_LINES, body);
    }

    private void handleGetLocation(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam(ID);
      logger.info("id: " + id);

      addServerRqRsData(HttpMethod.GET, LOCATION, new JsonObject().put(ID, id));

      Location location = new Location();
      location.setId(id);
      location.setLocationId("123");
      location.setPoLineId("123");
      location.setQuantity(3);
      location.setQuantityElectronic(1);
      location.setQuantityPhysical(2);

      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(location).encodePrettily());
    }

    private void handleGetPoNumber(RoutingContext ctx) {
      if(PO_NUMBER_ERROR_TENANT.equals(ctx.request().getHeader(OKAPI_HEADER_TENANT))) {
        ctx.response()
          .setStatusCode(500)
          .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
          .end();
      } else {
        SequenceNumber seqNumber = new SequenceNumber();
        seqNumber.setSequenceNumber(PO_NUMBER_VALUE);
        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
          .end(JsonObject.mapFrom(seqNumber).encodePrettily());
      }
    }

  }
}
