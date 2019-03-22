package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.orders.utils.ErrorCodes.COST_ADDITIONAL_COST_INVALID;
import static org.folio.orders.utils.ErrorCodes.COST_DISCOUNT_INVALID;
import static org.folio.orders.utils.ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID;
import static org.folio.orders.utils.ErrorCodes.COST_UNIT_PRICE_INVALID;
import static org.folio.orders.utils.ErrorCodes.ELECTRONIC_LOC_QTY_EXCEEDS_COST;
import static org.folio.orders.utils.ErrorCodes.GENERIC_ERROR_CODE;
import static org.folio.orders.utils.ErrorCodes.ITEM_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.ITEM_NOT_RETRIEVED;
import static org.folio.orders.utils.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY;
import static org.folio.orders.utils.ErrorCodes.NON_ZERO_COST_PHYSICAL_QTY;
import static org.folio.orders.utils.ErrorCodes.ORDER_CLOSED;
import static org.folio.orders.utils.ErrorCodes.ORDER_OPEN;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.PHYSICAL_LOC_QTY_EXCEEDS_COST;
import static org.folio.orders.utils.ErrorCodes.PIECE_ALREADY_RECEIVED;
import static org.folio.orders.utils.ErrorCodes.PIECE_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.PIECE_NOT_RETRIEVED;
import static org.folio.orders.utils.ErrorCodes.PIECE_POL_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.PIECE_UPDATE_FAILED;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.POL_LINES_LIMIT_EXCEEDED;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_ELECTRONIC_QTY;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_PHYSICAL_QTY;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_QTY;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.DEFAULT_POLINE_LIMIT;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.calculateTotalQuantity;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.groupLocationsById;
import static org.folio.orders.utils.ResourcePathResolver.*;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.InventoryHelper.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
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
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.restassured.http.Headers;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.RestVerticle;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.acq.model.ReceivingHistoryCollection;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat;
import org.folio.rest.jaxrs.model.Error;
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
  private static final String INSTANCE_RECORD = "instanceRecord";
  private static final String HOLDINGS_RECORD = "holdingRecord";
  private static final String ITEM_RECORDS = "itemRecords";
  private static final String PIECES = "pieces";
  private static final String ORDER_WITHOUT_PO_LINES = "order_without_po_lines.json";
  private static final String ORDER_WITH_PO_LINES_JSON = "put_order_with_po_lines.json";
  private static final String ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON = "put_order_with_mismatch_id_in_po_lines.json";
  private static final String PO_NUMBER_VALUE = "228D126";
  private static final String PO_LINE_NUMBER_VALUE = "1";

  private static final String ACTIVE_VENDOR_ID = "d0fb5aa0-cdf1-11e8-a8d5-f2801f1b9fd1";
  private static final String INACTIVE_VENDOR_ID = "b1ef7e96-98f3-4f0d-9820-98c322c989d2";
  private static final String PENDING_VENDOR_ID = "160501b3-52dd-41ec-a0ce-17762e7a9b47";
  private static final String NON_EXIST_VENDOR_ID = "bba87500-6e71-4057-a2a9-a091bac7e0c1";
  private static final String MOD_VENDOR_INTERNAL_ERROR_ID = "bba81500-6e41-4057-a2a9-a081bac7e0c1";

  private static final String ACTIVE_ACCESS_PROVIDER_A = "858e80d2-f562-4c54-9934-6e274dee511d";
  private static final String ACTIVE_ACCESS_PROVIDER_B = "d1b79c8d-4950-482f-8e42-04f9aae3cb40";
  private static final String INACTIVE_ACCESS_PROVIDER_A = "f6cd1850-2587-4f6c-b680-9b27ff26d619";
  private static final String INACTIVE_ACCESS_PROVIDER_B = "f64bbcae-e5ea-42b6-8236-55fefed0fb8f";
  private static final String NON_EXIST_ACCESS_PROVIDER_A = "160501b3-52dd-31ec-a0ce-17762e6a9b47";

  static {
    System.setProperty(LoggerFactory.LOGGER_DELEGATE_FACTORY_CLASS_NAME, "io.vertx.core.logging.Log4j2LogDelegateFactory");
  }

  private static final Logger logger = LoggerFactory.getLogger(OrdersImplTest.class);

  private static final String APPLICATION_JSON = "application/json";

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

  private static final String ID_BAD_FORMAT = "123-45-678-90-abc";
  private static final String ID_DOES_NOT_EXIST = "d25498e7-3ae6-45fe-9612-ec99e2700d2f";
  private static final String ID_FOR_INTERNAL_SERVER_ERROR = "168f8a86-d26c-406e-813f-c7527f241ac3";
  private static final String ID_FOR_PRINT_MONOGRAPH_ORDER = "00000000-1111-2222-8888-999999999999";
  private static final String PO_ID = "e5ae4afd-3fa9-494e-a972-f541df9b877e";
  private static final String PO_ID_OPEN_STATUS = "c1465131-ed35-4308-872c-d7cdf0afc5f7";
  private static final String PO_ID_CLOSED_STATUS = "07f65192-44a4-483d-97aa-b137bbd96390";
  private static final String PO_ID_FOR_FAILURE_CASE = "bad500aa-aaaa-500a-aaaa-aaaaaaaaaaaa";
  private static final String PO_LINE_ID_FOR_SUCCESS_CASE = "fca5fa9e-15cb-4a3d-ab09-eeea99b97a47";
  private static final String ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String PO_LINE_ID_WITH_SOME_SUB_OBJECTS_ALREADY_REMOVED = "0009662b-8b80-4001-b704-ca10971f175d";
  private static final String ORDER_ID_WITHOUT_PO_LINES = "50fb922c-3fa9-494e-a972-f2801f1b9fd1";
  private static final String ORDER_ID_WITH_PO_LINES = "ab18897b-0e40-4f31-896b-9c9adc979a87";
  private static final String ORDER_ID_WITH_MINIMAL_PO_LINE = "11111111-dddd-4444-9999-ffffffffffff";
  private static final String ORDER_WITHOUT_WORKFLOW_STATUS = "41d56e59-46db-4d5e-a1ad-a178228913e5";
  private static final String RECEIVING_HISTORY_PURCHASE_ORDER_ID = "0804ddec-6545-404a-b54d-a693f505681d";

  // API paths
  private final static String COMPOSITE_ORDERS_PATH = "/orders/composite-orders";
  private final static String COMPOSITE_ORDERS_BY_ID_PATH = "/orders/composite-orders/%s";
  private final static String LINES_PATH = "/orders/order-lines";
  private final static String LINE_BY_ID_PATH = "/orders/order-lines/%s";
  private static final String PONUMBER_VALIDATE_PATH = "/orders/po-number/validate";
  private static final String ORDERS_RECEIVING_HISTORY_ENDPOINT = "/orders/receiving-history";
  private static final String ORDERS_RECEIVING_ENDPOINT = "/orders/receive";
  private static final String PIECES_ENDPOINT = "/orders/pieces";
  private static final String ORDERS_CHECKIN_ENDPOINT = "/orders/check-in";

  // Mock data paths
  private static final String BASE_MOCK_DATA_PATH = "mockdata/";
  private static final String INSTANCE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instances/";
  private static final String INSTANCE_IDENTIFIERS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "identifierTypes/";
  private static final String INSTANCE_STATUSES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceStatuses/";
  private static final String INSTANCE_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceTypes/";
  private static final String ITEMS_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "itemsRecords/";
  private static final String PIECE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String RECEIVING_RQ_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "receiving/";
  private static final String LOAN_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "loanTypes/";
  private static final String COMP_ORDER_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/";
  private static final String PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "lines/";
  private static final String VENDORS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "vendors/";
  private static final String ORDERS_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + "getOrders.json";
  private static final String ORDER_FOR_FAILURE_CASE_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + PO_ID_FOR_FAILURE_CASE + ".json";
  private static final String COMP_PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String LISTED_PRINT_MONOGRAPH_PATH = "po_listed_print_monograph.json";
  private static final String PE_MIX_PATH = "po_listed_print_monograph_pe_mix.json";
  private static final String POLINES_COLLECTION = PO_LINES_MOCK_DATA_PATH + "/po_line_collection.json";
  private static final String LISTED_PRINT_SERIAL_PATH = "po_listed_print_serial.json";
  private static final String MINIMAL_ORDER_PATH = "minimal_order.json";
  private static final String poCreationFailurePath = "po_creation_failure.json";
  private static final String poLineCreationFailurePath = "po_line_creation_failure.json";
  private static final String CONFIG_MOCK_PATH = BASE_MOCK_DATA_PATH + "configurations.entries/%s.json";
  /** The PO Line with minimal required content */
  private static final String PO_LINE_MIN_CONTENT_PATH = COMP_PO_LINES_MOCK_DATA_PATH + "minimalContent.json";
  private static final String RECEIVING_HISTORY_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "receivingHistory/";
  private static final String CHECKIN_RQ_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "checkIn/";

  private static final String QUERY_PARAM_NAME = "query";
  private static final String ID = "id";
  private static final String PURCHASE_ORDER_ID = "purchaseOrderId";
  private static final String INCORRECT_LANG_PARAMETER = "'lang' parameter is incorrect. parameter value {english} is not valid: must match \"[a-zA-Z]{2}\"";
  private static final String INTERNAL_SERVER_ERROR = "Internal Server Error";
  private static final String EXISTING_PO_NUMBER = "oldPoNumber";
  private static final String NONEXISTING_PO_NUMBER = "newPoNumber";
  private static final String BAD_QUERY = "unprocessableQuery";

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

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);

    // Make sure expected number of PO Lines available
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    assertThat(reqData.getCompositePoLines().get(0).getOrderFormat(), equalTo(OrderFormat.P_E_MIX));
    assertThat(reqData.getCompositePoLines().get(1).getOrderFormat(), equalTo(OrderFormat.ELECTRONIC_RESOURCE));

    // Prepare cost details for the first PO Line (see MODORDERS-180 and MODORDERS-181)
    Cost cost = reqData.getCompositePoLines().get(0).getCost();
    cost.setAdditionalCost(10d);
    cost.setDiscount(3d);
    cost.setDiscountType(Cost.DiscountType.PERCENTAGE);
    cost.setQuantityElectronic(1);
    cost.setListUnitPriceElectronic(5.5d);
    cost.setQuantityPhysical(3);
    cost.setListUnitPrice(9.99d);
    cost.setPoLineEstimatedPrice(null);
    double expectedTotalPoLine1 = 44.41d;

    // Prepare cost details for the second PO Line (see MODORDERS-180 and MODORDERS-181)
    cost = reqData.getCompositePoLines().get(1).getCost();
    cost.setAdditionalCost(2d);
    cost.setDiscount(5d);
    cost.setDiscountType(Cost.DiscountType.AMOUNT);
    cost.setQuantityElectronic(3);
    cost.setListUnitPriceElectronic(11.99d);
    cost.setPoLineEstimatedPrice(null);
    double expectedTotalPoLine2 = 32.97d;

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertThat(poId, notNullValue());
    assertThat(poNumber, notNullValue());
    assertThat(resp.getCompositePoLines(), hasSize(2));

    for (int i = 0; i < resp.getCompositePoLines().size(); i++) {
      CompositePoLine line = resp.getCompositePoLines().get(i);
      String polNumber = line.getPoLineNumber();
      String polId = line.getId();

      assertThat(line.getPurchaseOrderId(), equalTo(poId));
      assertThat(polId, notNullValue());
      assertThat(polNumber, notNullValue());
      assertThat(polNumber, startsWith(poNumber));
      assertThat(line.getInstanceId(), nullValue());
    }

    // see MODORDERS-180 and MODORDERS-181
    CompositePoLine compositePoLine1 = resp.getCompositePoLines().get(0);
    CompositePoLine compositePoLine2 = resp.getCompositePoLines().get(1);
    assertThat(compositePoLine1.getCost().getPoLineEstimatedPrice(), equalTo(expectedTotalPoLine1));
    assertThat(compositePoLine2.getCost().getPoLineEstimatedPrice(), equalTo(expectedTotalPoLine2));
    assertThat(resp.getTotalEstimatedPrice(), equalTo(expectedTotalPoLine1 + expectedTotalPoLine2));

    List<JsonObject> poLines = MockServer.serverRqRs.get(PO_LINES, HttpMethod.POST);
    assertThat(poLines, hasSize(2));
    poLines.forEach(line -> {
      PoLine poLine = line.mapTo(PoLine.class);
      Double poLineEstimatedPrice = poLine.getCost().getPoLineEstimatedPrice();
      if (compositePoLine1.getId().equals(poLine.getId())) {
        assertThat(poLineEstimatedPrice, equalTo(expectedTotalPoLine1));
      } else {
        assertThat(poLineEstimatedPrice, equalTo(expectedTotalPoLine2));
      }
    });
  }

  @Test
  public void testPostOrderWithIncorrectCost() throws Exception {
    logger.info("=== Test Order creation - Cost validation fails ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Assert that there are 2 lines
    assertEquals(2, reqData.getCompositePoLines().size());

    // Set incorrect quantities for the first PO Line
    CompositePoLine firstPoLine = reqData.getCompositePoLines().get(0);
    firstPoLine.setOrderFormat(CompositePoLine.OrderFormat.P_E_MIX);
    firstPoLine.getCost().setQuantityPhysical(1);
    firstPoLine.getCost().setQuantityElectronic(0);
    firstPoLine.getCost().setListUnitPrice(-10d);
    firstPoLine.getCost().setListUnitPriceElectronic(-5d);
    firstPoLine.getLocations().forEach(location -> {
      location.setQuantityElectronic(1);
      location.setQuantityPhysical(2);
    });

    // Set incorrect quantities for the second PO Line
    CompositePoLine secondPoLine = reqData.getCompositePoLines().get(1);
    secondPoLine.setOrderFormat(CompositePoLine.OrderFormat.OTHER);
    secondPoLine.getCost().setQuantityPhysical(0);
    secondPoLine.getCost().setQuantityElectronic(1);
    secondPoLine.getCost().setListUnitPrice(-1d);
    secondPoLine.getCost().setListUnitPriceElectronic(10d);
    secondPoLine.getLocations().forEach(location -> {
      location.setQuantityElectronic(0);
      location.setQuantityPhysical(1);
    });

    final Errors response = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(10));
    Set<String> errorCodes = response.getErrors()
                                     .stream()
                                     .map(Error::getCode)
                                     .collect(Collectors.toSet());

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_PHYSICAL_QTY.getCode(),
                                              ZERO_COST_ELECTRONIC_QTY.getCode(),
                                              NON_ZERO_COST_ELECTRONIC_QTY.getCode(),
                                              ELECTRONIC_LOC_QTY_EXCEEDS_COST.getCode(),
                                              PHYSICAL_LOC_QTY_EXCEEDS_COST.getCode(),
                                              COST_UNIT_PRICE_ELECTRONIC_INVALID.getCode(),
                                              COST_UNIT_PRICE_INVALID.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testPutOrderWithIncorrectQuantities() throws Exception {
    logger.info("=== Test Order update - Quantity validation fails for the first PO Line ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Set incorrect quantities for the first PO Line
    CompositePoLine firstPoLine = reqData.getCompositePoLines().get(0);
    firstPoLine.setOrderFormat(CompositePoLine.OrderFormat.P_E_MIX);
    firstPoLine.getCost().setQuantityPhysical(0);
    firstPoLine.getCost().setQuantityElectronic(0);
    firstPoLine.getLocations().forEach(location -> {
      location.setQuantityElectronic(1);
      location.setQuantityPhysical(1);
    });

    final Errors response = verifyPut(COMPOSITE_ORDERS_PATH + "/" + reqData.getId(), JsonObject.mapFrom(reqData).encode(),
      APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(3));
    Set<String> errorCodes = response.getErrors()
                                     .stream()
                                     .map(Error::getCode)
                                     .collect(Collectors.toSet());

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_QTY.getCode(),
                                              ELECTRONIC_LOC_QTY_EXCEEDS_COST.getCode(),
                                              PHYSICAL_LOC_QTY_EXCEEDS_COST.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testListedPrintMonographInOpenStatus() throws Exception {
    logger.info("=== Test Listed Print Monograph in Open status ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    CompositePoLine firstPoLine = reqData.getCompositePoLines().get(0);
    // remove productId from PO line to test scenario when it's not provided so there is no check for existing instance but new one will be created
    firstPoLine.getDetails().getProductIds().clear();
    // MODORDERS-117 only physical quantity will be used
    firstPoLine.setOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE);
    // Set locations quantities
    int totalQty = 0;
    for (int i = 0; i < firstPoLine.getLocations().size(); i++) {
      Location location = firstPoLine.getLocations().get(i);
      int quantityPhysical = i * (i + 1);

      location.setQuantityElectronic(0);
      location.setQuantityPhysical(quantityPhysical);
      totalQty += quantityPhysical;
    }
    // Set cost quantities
    firstPoLine.getCost().setQuantityPhysical(totalQty);
    firstPoLine.getCost().setQuantityElectronic(0);
    firstPoLine.getCost().setListUnitPrice(10d);
    firstPoLine.getCost().setListUnitPriceElectronic(0d);

    // Set status to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    LocalDate now = LocalDate.now();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    LocalDate dateOrdered = resp.getDateOrdered().toInstant().atZone(ZoneId.of(ZoneOffset.UTC.getId())).toLocalDate();
    assertThat(dateOrdered.getMonth(), equalTo(now.getMonth()));
    assertThat(dateOrdered.getYear(), equalTo(now.getYear()));

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
      assertNotNull(line.getInstanceId());
    }

    int polCount = resp.getCompositePoLines().size();

    List<JsonObject> instancesSearches = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
    assertNotNull(instancesSearches);
    // Check that search for existing instances was done not for all PO lines
    assertEquals(polCount - 1, instancesSearches.size());

    verifyInventoryInteraction(resp, polCount);
    verifyCalculatedData(resp);
  }

  @Test
  public void testOrderWithPoLinesWithoutSource() throws Exception {
    logger.info("=== Test Listed Print Monograph with POL without source ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Assert that there are 2 lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // remove source to verify validation for first POL
    reqData.getCompositePoLines().get(0).setSource(null);
    // Set source code to null to verify validation for second POL
    reqData.getCompositePoLines().get(1).getSource().setCode(null);

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);
    assertEquals(reqData.getCompositePoLines().size(), errors.getErrors().size());
  }

  @Test
  public void testDateOrderedIsNotSetForPendingOrder() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - Date Ordered is empty ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that Order moves to Pending
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    // Verify dateOrdered is not set because Workflow status is not OPEN
    assertNull(resp.getDateOrdered());
  }

  @Test
  public void testPostOpenOrderInventoryUpdateOnlyForFirstPOL() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that mock po has the first PO line with 3 locations
    assertEquals(3, reqData.getCompositePoLines().get(0).getLocations().size());

    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set the same location id for all locations of the first PO Line to confirm that only one holding created
    String locationId = reqData.getCompositePoLines().get(0).getLocations().get(0).getLocationId();
    reqData.getCompositePoLines().get(0).getLocations().forEach(location -> location.setLocationId(locationId));

    // Prepare second POL
    CompositePoLine secondPol = reqData.getCompositePoLines().get(1);
    List<Location> secondPolLocations = secondPol.getLocations();
    // MODORDERS-117 Setting OrderFormat to OTHER which means create nothing in inventory for the second PO Line
    secondPol.setOrderFormat(CompositePoLine.OrderFormat.OTHER);
    // Specify correct quantities for OTHER format
    secondPol.getCost().setQuantityElectronic(0);
    secondPol.getCost().setListUnitPriceElectronic(null);
    secondPol.getCost().setListUnitPrice(10d);
    secondPol.getCost().setQuantityPhysical(secondPolLocations.size());
    secondPolLocations.forEach(location -> {
      location.setQuantityElectronic(0);
      location.setQuantityPhysical(1);
    });

          LocalDate now = LocalDate.now();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);
    LocalDate dateOrdered = resp.getDateOrdered().toInstant().atZone(ZoneId.of(ZoneOffset.UTC.getId())).toLocalDate();
    assertThat(dateOrdered.getMonth(), equalTo(now.getMonth()));
    assertThat(dateOrdered.getYear(), equalTo(now.getYear()));

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
    verifyCalculatedData(resp);
  }

  @Test
  public void testPutOrdersByIdPEMixFormat() throws Exception {
    logger.info("=== Test Put Order By Id create Pieces with P/E Mix format ===");
    JsonObject order = new JsonObject(getMockData(PE_MIX_PATH));
    order.put("workflowStatus", "Pending");
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);

    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 1 po lines
    assertEquals(1, reqData.getCompositePoLines().size());

    CompositePoLine compositePoLine = reqData.getCompositePoLines().get(0);

    compositePoLine.getEresource().setCreateInventory(false);
    compositePoLine.getCost().setQuantityPhysical(3);
    compositePoLine.getCost().setQuantityElectronic(2);
    compositePoLine.setOrderFormat(OrderFormat.P_E_MIX);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), "", 204);

    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);
    List<JsonObject> createdItems = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.POST);
    int piecesSize = createdPieces != null ? createdPieces.size() : 0;
    int itemSize = createdItems != null ? createdItems.size() : 0;
    logger.debug("------------------- piecesSize, itemSize --------------------\n" + piecesSize + " " + itemSize);
    // Verify total number of pieces created should be equal to total quantity
    assertEquals(calculateTotalQuantity(compositePoLine), piecesSize);

    if (createdItems != null) {
      verifyPiecesCreated(createdItems, reqData.getCompositePoLines(), createdPieces);
    }
  }

  @Test
  public void testPutOrdersByIdTotalPiecesEqualsTotalQuantityWhenCreateInventoryIsFalse() throws Exception {
    logger.info("=== Test Put Order By Id create Pieces when Item record does not exist ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

    reqData.getCompositePoLines().get(1).getEresource().setCreateInventory(false);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), "", 204);
    List<JsonObject> items = joinExistingAndNewItems();
    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);
    verifyPiecesQuantityForSuccessCase(reqData, createdPieces);
    verifyPiecesCreated(items, reqData.getCompositePoLines(), createdPieces);
  }

  @Test
  public void testPlaceOrderMinimal() throws Exception {
    logger.info("=== Test Placement of minimal order ===");

    String body = getMockData(MINIMAL_ORDER_PATH);
    JsonObject reqData = new JsonObject(body);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);


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
    request.put("poNumber", "1234");
    String body= request.toString();

     verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
       prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422);

  }

  @Test
  public void testPostOrderFailsWithExistingPONumber() {
    logger.info("=== Test Placement of minimal order failure with Existing PO Number===");

    JsonObject request = new JsonObject();
    request.put("poNumber", EXISTING_PO_NUMBER);
    String body= request.toString();

     verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
       prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 400);

  }

  @Test
  public void testPostOrderPONumberAutoGenerated() {
    logger.info("=== Test Placement of Empty order with Auto Generated PO Number===");

    JsonObject request = new JsonObject();
    String body= request.toString();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

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
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).body().as(Errors.class);

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
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1), APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }


  @Test
  public void testPostWithInvalidConfig(TestContext ctx) throws Exception {
    logger.info("=== Test PO creation fail if config is invalid ===");

    String body = getMockDraftOrder().toString();

    final Errors error = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(INVALID_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 500).body().as(Errors.class);

    ctx.assertEquals(error.getErrors().get(0).getMessage(), "Invalid limit value in configuration.");
  }

  @Test
  public void testPostWithEmptyConfigOverLimit(TestContext ctx) {
    logger.info("=== Test PO creation fail with default limit ===");

    JsonObject compPoLineJson = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final Errors errors = verifyPostResponse(LINES_PATH, compPoLineJson.encodePrettily(),
      prepareHeaders(EMPTY_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }


  @Test
  public void testPoLineCreationIfPoAlreadyReachedLimit(TestContext ctx) {
    logger.info("=== Test PO Line over limit creation ===");
    JsonObject compPoLineJson = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final Errors errors = verifyPostResponse(LINES_PATH, compPoLineJson.encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1), APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }


  @Test
  public void testPoLineCreationFailure(TestContext ctx) throws Exception {
    logger.info("=== Test POLine creation failure ===");

    String body = getMockData(poLineCreationFailurePath);

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).body().as(Errors.class);


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
  public void testSubObjectCreationFailure() throws Exception {
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
          .contentType(APPLICATION_JSON)
          .statusCode(500)
            .extract()
              .response();

    String respBody = resp.getBody().as(Errors.class).getErrors().get(0).getMessage();
    logger.info(respBody);

    assertEquals("Access requires permission: foo.bar.baz", respBody);
  }

  @Test
  public void testGetOrderById() throws Exception {
    logger.info("=== Test Get Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
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
    verifyCalculatedData(resp);
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

    double expectedPrice = resp
      .getCompositePoLines()
      .stream()
      .map(CompositePoLine::getCost)
      .mapToDouble(Cost::getPoLineEstimatedPrice)
      .sum();

    assertThat(resp.getTotalItems(), equalTo(expectedQuantity));
    assertThat(resp.getTotalEstimatedPrice(), equalTo(expectedPrice));

    resp.getCompositePoLines().forEach(line -> {
      assertThat(line.getCost().getPoLineEstimatedPrice(), greaterThan(0d));
    });
  }

  @Test
  public void testGetOrderByIdWithOnePoLineWithNullCost() {
    logger.info("=== Test Get Order By Id with one PO Line without cost totalItems value is 0 ===");

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_ID_WITH_MINIMAL_PO_LINE));

    final CompositePurchaseOrder resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(COMPOSITE_ORDERS_PATH + "/" + ORDER_ID_WITH_MINIMAL_PO_LINE)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(200)
          .extract()
            .response()
              .as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ORDER_ID_WITH_MINIMAL_PO_LINE, resp.getId());
    assertEquals(0, resp.getTotalItems().intValue());
  }

  @Test
  public void testGetPOByIdTotalItemsWithoutPOLines() {
    logger.info("=== Test Get Order By Id without PO Line totalItems value is 0 ===");

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_ID_WITHOUT_PO_LINES));

    final CompositePurchaseOrder resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(NON_EXIST_CONFIG_X_OKAPI_TENANT)
      .get(COMPOSITE_ORDERS_PATH + "/" + ORDER_ID_WITHOUT_PO_LINES)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(200)
          .extract()
            .response()
              .as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ORDER_ID_WITHOUT_PO_LINES, resp.getId());
    assertEquals(0, resp.getTotalItems().intValue());
  }

  @Test
  public void testGetOrderByIdWithOnePoLine() {
    logger.info("=== Test Get Order By Id - With one PO Line and empty source ===");

    String id = PO_ID_CLOSED_STATUS;
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
    assertEquals(calculateTotalQuantity(resp.getCompositePoLines().get(0)), resp.getTotalItems().intValue());
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
          .contentType(APPLICATION_JSON)
          .statusCode(404)
          .extract()
            .response();

    String actual = resp.getBody().as(Errors.class).getErrors().get(0).getMessage();
    logger.info(actual);

    assertEquals(id, actual);
  }

  @Test
  public void testDeleteById() throws Exception {
    logger.info("=== Test Delete Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
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
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_DOES_NOT_EXIST, "", 204);
  }

  @Test
  public void testDeleteById500Error() {
    logger.info("=== Test Delete Order By Id - Storage Internal Server Error ===");
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_FOR_INTERNAL_SERVER_ERROR, APPLICATION_JSON, 500);
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
    logger.info("=== Test Put Order By Id workflowStatus is Open from storage and workflowStatus is Open in current request  ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    JsonObject storageData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), "", 204);

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
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON));

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), APPLICATION_JSON, 422);
  }

  @Test
  public void testUpdatePoNumber() throws Exception {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
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
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
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
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    reqData.remove(PO_NUMBER);

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), APPLICATION_JSON, 422);
  }

  @Test
  public void testPutOrderFailsWithInvalidPONumber() throws Exception {
    logger.info("=== Test update order failure with Invalid PO Number===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);

    JsonObject request = new JsonObject();
    request.put("poNumber", "1234");
    String body= request.toString();

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, body, APPLICATION_JSON, 422);

  }

  @Test
  public void testPutOrderFailsWithExistingPONumber() throws Exception {
    logger.info("=== Test update of order failure with Existing PO Number===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);

    JsonObject request = new JsonObject();
    request.put("poNumber", EXISTING_PO_NUMBER);
    String body= request.toString();

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, body, APPLICATION_JSON, 400);

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
      .put(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES))
        .then()
          .statusCode(422)
            .extract()
              .response()
                .body()
                  .as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
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
      .put(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES))
        .then()
          .statusCode(422)
            .extract()
              .response()
                .body()
                  .as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    ctx.assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpen() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // MODORDERS-117 guarantee electronic resource for the second PO Line but set "create items" to false
    reqData.getCompositePoLines().get(1).setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    reqData.getCompositePoLines().get(1).getEresource().setCreateInventory(false);
    reqData.getCompositePoLines().forEach(s -> s.setReceiptStatus(CompositePoLine.ReceiptStatus.PENDING));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), "", 204);

    int polCount = reqData.getCompositePoLines().size();

    verifyInstanceLinksForUpdatedOrder(reqData);
    verifyInventoryInteraction(reqData, polCount - 1);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value());
  }

  @Test
  public void testPostOrdersWithOpenStatusAndCheckinItems() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that mock po has the first PO line with 3 locations
    assertEquals(3, reqData.getCompositePoLines().get(0).getLocations().size());

    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set checkin items flag true
    reqData.getCompositePoLines().forEach(s -> s.setCheckinItems(true));

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    List<JsonObject> instancesSearches = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
    List<JsonObject> holdingsSearches = MockServer.serverRqRs.get(HOLDINGS_RECORD, HttpMethod.GET);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);

    assertNotNull(instancesSearches);
    assertNull(holdingsSearches);
    assertNull(itemsSearches);
    assertNull(createdPieces);
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenWithCheckinItems() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Make sure that mock PO has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // MODORDERS-183 Set the second POLine checkinItems true
    reqData.getCompositePoLines().get(1).setCheckinItems(true);
    reqData.getCompositePoLines().forEach(s -> s.setReceiptStatus(CompositePoLine.ReceiptStatus.PENDING));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), "", 204);

    int polCount = reqData.getCompositePoLines().size();
    verifyInstanceLinksForUpdatedOrder(reqData);
    verifyInventoryInteraction(reqData, polCount);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value());
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenWithEmptyPoLines() throws Exception {
    logger.info("=== Test Put (WithEmptyPoLines) Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.getCompositePoLines().clear();
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), "", 204);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value());
  }

  private void verifyReceiptStatusChangedTo(String expectedStatus) {
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);
    assertNotNull(polUpdates);
    // check receipt status of the last 2 updated polines
    for (JsonObject jsonObj : polUpdates.subList(polUpdates.size() - 2, polUpdates.size())) {
      assertEquals(expectedStatus, jsonObj.getString(RECEIPT_STATUS));
    }
  }

  @Test
  public void testUpdateOrderToOpenWithPartialItemsCreation() throws Exception {
    logger.info("=== Test Order update to Open status - Inventory items expected to be created partially ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Emulate items creation issue
    reqData.getCompositePoLines().get(0).getDetails().getMaterialTypes().set(0, ID_FOR_INTERNAL_SERVER_ERROR);
    // Let's have only one PO Line
    reqData.getCompositePoLines().remove(1);
    // Set status to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // Set specific ID to let items search to return 1 item
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    int polCount = reqData.getCompositePoLines().size();
    // Assert that only one PO line presents
    assertEquals(1, polCount);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), APPLICATION_JSON, 500);

    // Verify inventory GET and POST requests for instance, holding and item records
    verifyInventoryInteraction(false);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();
    // All created pieces
    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);

    // Verify that not all expected items created
    assertThat(items.size(), lessThan(calculateInventoryItemsQuantity(reqData.getCompositePoLines().get(0))));

    // Verify that pieces created for processed items quantity
    verifyPiecesCreated(items, reqData.getCompositePoLines().subList(0, 1), createdPieces);
  }

  private void verifyInstanceLinksForUpdatedOrder(CompositePurchaseOrder reqData) {
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);
    assertNotNull(polUpdates);
    for (CompositePoLine compLine : reqData.getCompositePoLines()) {
      int itemsQuantity = calculateInventoryItemsQuantity(compLine);
      if (itemsQuantity > 0) {
        boolean instanceLinked = false;
        for (JsonObject jsonObj : polUpdates) {
          PoLine line = jsonObj.mapTo(PoLine.class);
          if (StringUtils.equals(line.getId(), compLine.getId()) && StringUtils.isNotEmpty(line.getInstanceId())) {
            instanceLinked = true;
            // Populate instance id in the req data for further validation
            compLine.setInstanceId(line.getInstanceId());
            break;
          }
        }

        assertTrue("The PO Line must contain instance id", instanceLinked);
      }
    }
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

    verifyPiecesQuantityForSuccessCase(reqData, createdPieces);

    for (CompositePoLine pol : reqData.getCompositePoLines()) {
      verifyInstanceCreated(createdInstances, pol);
      verifyHoldingsCreated(createdHoldings, pol);
      verifyItemsCreated(items, pol, calculateInventoryItemsQuantity(pol));
      verifyPiecesCreated(items, reqData.getCompositePoLines(), createdPieces);
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

  private void verifyPiecesQuantityForSuccessCase(CompositePurchaseOrder reqData, List<JsonObject> createdPieces) {
    int totalQuantity = 0;
    for (CompositePoLine poLine : reqData.getCompositePoLines()) {
      if (poLine.getCheckinItems() != null && poLine.getCheckinItems()) continue;
      totalQuantity += calculateTotalQuantity(poLine);
    }
    assertEquals(totalQuantity, createdPieces.size());
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
    if (pol.getCheckinItems() != null && pol.getCheckinItems()) return;

    int expectedItemsQuantity = calculateInventoryItemsQuantity(pol);
    if (!verified && expectedItemsQuantity > 0) {
      fail("No matching instance for POL: " + JsonObject.mapFrom(pol).encodePrettily());
    }

    if ((!verified && StringUtils.isNotEmpty(pol.getInstanceId()) || (verified && expectedItemsQuantity == 0))) {
      fail("No instance expected for POL: " + JsonObject.mapFrom(pol).encodePrettily());
    }
  }

  private void verifyPiecesCreated(List<JsonObject> inventoryItems, List<CompositePoLine> compositePoLines,
      List<JsonObject> pieces) {
    // Collect all item id's
    List<String> itemIds = inventoryItems.stream()
                                    .map(item -> item.getString(ID))
                                    .collect(Collectors.toList());

    // Verify quantity of created pieces
    int expectedPiecesQuantity = itemIds.size();
    for (CompositePoLine poLine : compositePoLines) {
      expectedPiecesQuantity += HelperUtils.calculateExpectedQuantityOfPiecesWithoutItemCreation(poLine, poLine.getLocations());
    }
    assertEquals(expectedPiecesQuantity, pieces.size());

    List<String> locationIds = compositePoLines.stream()
      .flatMap(compositePoLine -> compositePoLine.getLocations().stream())
      .map(Location::getLocationId)
      .distinct()
      .collect(Collectors.toList());

    for (JsonObject pieceObj : pieces) {
      // Make sure piece data corresponds to schema content
      Piece piece = pieceObj.mapTo(Piece.class);

      // Check if itemId in inventoryItems match itemId in piece record
      assertThat(locationIds, hasItem(piece.getLocationId()));
      if(piece.getItemId()!=null)
        assertThat(itemIds, hasItem(piece.getItemId()));
      assertThat(piece.getReceivingStatus(), equalTo(Piece.ReceivingStatus.EXPECTED));
    }
        }

  private void verifyHoldingsCreated(List<JsonObject> holdings, CompositePoLine pol) {
    Map<String, List<Location>> groupedLocations = groupLocationsById(pol);

    long actualQty = 0;
    for (JsonObject holding : holdings) {
      if (groupedLocations.keySet().contains(holding.getString(HOLDING_PERMANENT_LOCATION_ID))
        && StringUtils.equals(pol.getInstanceId(), holding.getString(HOLDING_INSTANCE_ID))) {
        actualQty++;
      }
    }

    int itemsQuantity = calculateInventoryItemsQuantity(pol);
    if (itemsQuantity == 0) {
      assertEquals("No holdings expected", 0, actualQty);
    } else {
      long expectedQty = groupedLocations
        .values()
        .stream()
        .mapToInt(locations -> calculateInventoryItemsQuantity(pol, locations))
        .filter(qty -> qty > 0)
        .count();
      assertEquals("Quantity of holdings does not match to expected", expectedQty, actualQty);
    }
  }

  private void verifyItemsCreated(List<JsonObject> inventoryItems, CompositePoLine pol, int expectedQuantity) {
    int actualQuantity = 0;

    for (JsonObject item : inventoryItems) {
      if (pol.getId().equals(item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER))) {
        verifyItemRecordRequest(item, pol);
        actualQuantity++;
      }
    }

    if (expectedQuantity != actualQuantity) {
      fail(String.format("Actual items quantity is %d but expected %d", actualQuantity, expectedQuantity));
    }
  }

  private void verifyInstanceRecordRequest(JsonObject instance, CompositePoLine line) {
    assertThat(instance.getString(INSTANCE_TITLE), equalTo(line.getTitle()));
    assertThat(instance.getString(INSTANCE_SOURCE), equalTo(line.getSource().getCode()));
    assertThat(instance.getString(INSTANCE_STATUS_ID), equalTo("daf2681c-25af-4202-a3fa-e58fdf806183"));
    assertThat(instance.getString(INSTANCE_TYPE_ID), equalTo("30fffe0e-e985-4144-b2e2-1e8179bdb41f"));
    assertThat(instance.getJsonArray(INSTANCE_PUBLICATION).getJsonObject(0).getString(INSTANCE_PUBLISHER), equalTo(line.getPublisher()));
    assertThat(instance.getJsonArray(INSTANCE_PUBLICATION).getJsonObject(0).getString(INSTANCE_DATE_OF_PUBLICATION), equalTo(line.getPublicationDate()));
    if (line.getDetails().getProductIds().size() > 0) {
      assertThat(instance.getJsonArray(INSTANCE_IDENTIFIERS).getJsonObject(0).getString(INSTANCE_IDENTIFIER_TYPE_ID), equalTo("8261054f-be78-422d-bd51-4ed9f33c3422"));
      assertThat(instance.getJsonArray(INSTANCE_IDENTIFIERS).getJsonObject(0).getString(INSTANCE_IDENTIFIER_TYPE_VALUE), equalTo(line.getDetails().getProductIds().get(0).getProductId()));
    }
  }

  private void verifyItemRecordRequest(JsonObject item, CompositePoLine line) {
    assertThat(item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER), not(isEmptyOrNullString()));
    assertThat(line.getDetails().getMaterialTypes(), hasItem(item.getString(ITEM_MATERIAL_TYPE_ID)));
    assertThat(item.getString(ITEM_HOLDINGS_RECORD_ID), not(isEmptyOrNullString()));
    assertThat(item.getString(ITEM_PERMANENT_LOAN_TYPE_ID), not(isEmptyOrNullString()));
    assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
    assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ITEM_STATUS_ON_ORDER));
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
    ctx.assertEquals(2, errors.getErrors().size());
    ctx.assertNull(MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET));
    ctx.assertNull(MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET));
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenButWithErrorCreatingItemsForSecondPOL(TestContext ctx) throws Exception {
    logger.info("=== Test Put Order By Id to change Order's status to Open - Inventory errors expected on items creation for second POL ===");

    /*==============  Preparation ==============*/

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
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
    verifyPut(path, JsonObject.mapFrom(reqData).toString(), APPLICATION_JSON, 500);

    // Check that search of the existing instances and items was done for each PO line
    List<JsonObject> instancesSearches = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> piecesSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    ctx.assertNotNull(instancesSearches);
    ctx.assertNotNull(itemsSearches);
    ctx.assertNotNull(piecesSearches);
    assertEquals(polCount, instancesSearches.size());

    // Check that 2 new instances created and items created successfully only for first POL
    List<JsonObject> createdInstances = MockServer.serverRqRs.get(INSTANCE_RECORD, HttpMethod.POST);
    List<JsonObject> createdItems = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.POST);
    List<JsonObject> createdPieces = MockServer.serverRqRs.get(PIECES, HttpMethod.POST);
    assertNotNull(createdInstances);
    assertNotNull(createdItems);
    assertNotNull(createdPieces);
    assertEquals(polCount, createdInstances.size());

    List<JsonObject> items = joinExistingAndNewItems();

    // Check instance Ids not exist for polines
    verifyInstanceLinksNotCreatedForPoLine();

    // Verify pieces were created
    assertEquals(calculateTotalQuantity(reqData.getCompositePoLines().get(0)), createdPieces.size());

    verifyPiecesCreated(items, reqData.getCompositePoLines().subList(0, 1), createdPieces);
  }

  private void verifyInstanceLinksNotCreatedForPoLine() {
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);
    assertNotNull(polUpdates);
    boolean instanceIdExists = false;
    for (JsonObject jsonObj : polUpdates) {
      PoLine line = jsonObj.mapTo(PoLine.class);
      if (StringUtils.isNotEmpty(line.getInstanceId())) {
        instanceIdExists = true;
        break;
      }
    }

    assertFalse("The PO Line must NOT contain instance id", instanceIdExists);
  }

  @Test
  public void testPutOrderByIdWithPoLinesInRequestAndNoPoLinesInStorage() throws Exception {
    logger.info("=== Test Put Order By Id with PO lines and without PO lines in order from storage ===");

    JsonObject reqData = getMockDraftOrder();
    String poNumber = reqData.getString(PO_NUMBER);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES), reqData.toString(), "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    List<JsonObject> createdPoLines = MockServer.serverRqRs.get(PO_LINES, HttpMethod.POST);
    assertEquals(createdPoLines.size(), reqData.getJsonArray(COMPOSITE_PO_LINES).size());
    createdPoLines.forEach(poLine -> assertEquals(poNumber + "-" + PO_LINE_NUMBER_VALUE, poLine.getString(PO_LINE_NUMBER)));
  }

  @Test
  public void testPutOrderByIdWithoutPoLinesInRequestDoesNotDeletePoLinesFromStorage() throws IOException {
    logger.info("=== Test Put Order By Id without PO lines doesn't delete lines from storage ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));

    verifyPut(COMPOSITE_ORDERS_PATH + "/" + id, reqData.toString(), "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    assertNull(MockServer.serverRqRs.get(PO_LINES, HttpMethod.DELETE));
  }

  @Test
  public void testPutOrderByIdWith404InvalidId() {
    logger.info("=== Test Put Order By Id for 404 with Invalid Id or Order not found ===");

    JsonObject reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, "93f612a9-9a05-4eef-aac5-435be131454b"), reqData.toString(), APPLICATION_JSON, 404);
  }

  @Test
  public void testPutOrderByIdWithInvalidOrderIdInBody() {
    logger.info("=== Test Put Order By Id for 422 with Invalid Id in the Order ===");

    CompositePurchaseOrder reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH).mapTo(CompositePurchaseOrder.class);
    reqData.setId(PO_ID);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_FOR_FAILURE_CASE),
      JsonObject.mapFrom(reqData).encodePrettily(), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
  }

  @Test
  public void testPutOrderByIdWithInvalidOrderIdInPol() {
    logger.info("=== Test Put Order By Id for 422 with invalid purchase order Id in the POL ===");

    CompositePurchaseOrder reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH).mapTo(CompositePurchaseOrder.class);
    reqData.getCompositePoLines().forEach(line -> line.setPurchaseOrderId(PO_ID_FOR_FAILURE_CASE));

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID),
      JsonObject.mapFrom(reqData).encodePrettily(), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(reqData.getCompositePoLines().size()));
    errors.getErrors().forEach(error -> {
      assertThat(error.getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
      assertThat(error.getParameters(), hasSize(1));
    });
  }

  @Test
  public void testPutOrderInOpenStatusAddingNewLine() {
    logger.info("=== Test update Open order - add new line ===");

    validateUpdateRejectedForNonPendingOrder(PO_ID_OPEN_STATUS, ORDER_OPEN.getCode());
  }

  @Test
  public void testPutOrderInClosedStatusAddingNewLine() {
    logger.info("=== Test update Closed order - add new line ===");

    validateUpdateRejectedForNonPendingOrder(PO_ID_CLOSED_STATUS, ORDER_CLOSED.getCode());
  }

  private void validateUpdateRejectedForNonPendingOrder(String orderId, String errorCode) {
    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, orderId).mapTo(CompositePurchaseOrder.class);

    // Delete PO Line id emulating case when new PO Line is being added
    reqData.getCompositePoLines().forEach(line -> line.setId(null));

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(), APPLICATION_JSON, 422).as(Errors.class);
    validatePoLineCreationErrorForNonPendingOrder(errorCode, errors);
  }

  private void validatePoLineCreationErrorForNonPendingOrder(String errorCode, Errors errors) {
    assertEquals(1, errors.getErrors().size());
    assertEquals(errorCode, errors.getErrors().get(0).getCode());
    // Assert that only PO Lines limit (count of existing Lines) and GET PO requests made
    assertEquals(2, MockServer.serverRqRs.size());
    assertEquals(2, MockServer.serverRqRs.rowKeySet().size());
    assertEquals(1, MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET).size());
    assertEquals(1, MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET).size());
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
      .delete(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_CLOSED_STATUS) + INVALID_LANG)
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
    Response actual = verifyDeleteResponse(url, APPLICATION_JSON, 404);

    assertEquals(ID_DOES_NOT_EXIST, actual.as(Errors.class).getErrors().get(0).getMessage());
  }

  @Test
  public void testDeleteOrderLineById500FromStorageOnSubObjectDeletion() {
    logger.info("=== Test Delete Order Line By Id - 500 From Storage On Sub-Object deletion ===");

    String url = String.format(LINE_BY_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR);
    Response actual = verifyDeleteResponse(url, APPLICATION_JSON, 500);

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

  private Response verifyPostResponse(String url, String body, Headers headers, String
    expectedContentType, int expectedCode) {
    return RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TOKEN)
        .headers(headers)
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

  private Headers prepareHeaders(Header... headers) {
    return new Headers(headers);
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

    assertEquals(lineId, resp.getBody().as(Errors.class).getErrors().get(0).getMessage());
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

    assertEquals("Internal Server Error", resp.getBody().as(Errors.class).getErrors().get(0).getMessage());
  }

  @Test
  public void testPostOrderLine() {
    logger.info("=== Test Post Order Line (expected flow) ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    reqData.getCost().setPoLineEstimatedPrice(null);

    final CompositePoLine response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePoLine.class);

    assertThat(response.getPurchaseOrderId(), equalTo(reqData.getPurchaseOrderId()));
    assertThat(response.getInstanceId(), nullValue());

    JsonObject purchaseOrderOwner = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, reqData.getPurchaseOrderId());
    String expectedPoLineNumber = purchaseOrderOwner.getString(PO_NUMBER) + "-" + PO_LINE_NUMBER_VALUE;
    assertThat(response.getPoLineNumber(), equalTo(expectedPoLineNumber));
    // See MODORDERS-180
    assertThat(response.getCost().getPoLineEstimatedPrice(), equalTo(49.98d));
  }

  @Test
  public void testPostOrdersLinePhysicalFormatIncorrectQuantity() {
    logger.info("=== Test Post Physical Order Line - incorrect quantity ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE);
    reqData.getCost().setQuantityPhysical(0);
    reqData.getCost().setQuantityElectronic(1);
    reqData.getLocations().get(0).setQuantityPhysical(1);

    final Errors response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(3));
    List<String> errorCodes = response.getErrors()
                                      .stream()
                                      .map(Error::getCode)
                                      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_PHYSICAL_QTY.getCode(), NON_ZERO_COST_ELECTRONIC_QTY.getCode(), PHYSICAL_LOC_QTY_EXCEEDS_COST.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testPostOrdersLinePhysicalFormatDiscount() {
    logger.info("=== Test Post Physical Order Line - discount exceeds total price ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE);
    reqData.getCost().setQuantityPhysical(2);
    reqData.getCost().setListUnitPrice(10d);
    reqData.getCost().setDiscount(100d);
    reqData.getCost().setDiscountType(Cost.DiscountType.AMOUNT);
    reqData.getCost().setQuantityElectronic(0);
    reqData.getLocations().get(0).setQuantityPhysical(2);

    final Errors response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    assertThat(response.getErrors().get(0).getCode(), equalTo(COST_DISCOUNT_INVALID.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testPutOrderLineElectronicFormatIncorrectQuantityAndPrice() {
    logger.info("=== Test Put Electronic Order Line - incorrect quantity and Price ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    reqData.setId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    Cost cost = reqData.getCost();
    cost.setQuantityPhysical(1);
    cost.setQuantityElectronic(0);
    cost.setListUnitPriceElectronic(-1d);
    cost.setListUnitPrice(1d);
    cost.setAdditionalCost(-1d);
    cost.setDiscountType(Cost.DiscountType.PERCENTAGE);
    cost.setDiscount(100.1d);
    reqData.getLocations().get(0).setQuantityElectronic(1);

    final Errors response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encodePrettily(),
      APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(7));
    List<String> errorCodes = response.getErrors()
                                      .stream()
                                      .map(Error::getCode)
                                      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_ELECTRONIC_QTY.getCode(),
                                              NON_ZERO_COST_PHYSICAL_QTY.getCode(),
                                              COST_UNIT_PRICE_INVALID.getCode(),
                                              COST_UNIT_PRICE_ELECTRONIC_INVALID.getCode(),
                                              COST_ADDITIONAL_COST_INVALID.getCode(),
                                              COST_DISCOUNT_INVALID.getCode(),
                                              ELECTRONIC_LOC_QTY_EXCEEDS_COST.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testPostOrdersLinesByIdPoLineWithoutId(TestContext ctx) throws IOException {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");

    Errors resp = verifyPostResponse(LINES_PATH, getMockData(PO_LINE_MIN_CONTENT_PATH),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    ctx.assertEquals(1, resp.getErrors().size());
    ctx.assertEquals(ErrorCodes.MISSING_ORDER_ID_IN_POL.getCode(), resp.getErrors().get(0).getCode());
  }

  @Test
  public void testPostOrdersLinesByIdPoLineWithNonExistPurchaseOrder(TestContext ctx) {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");
    JsonObject reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);
    reqData.put(PURCHASE_ORDER_ID, ID_DOES_NOT_EXIST);

    Errors resp = verifyPostResponse(LINES_PATH, reqData.encodePrettily(),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    ctx.assertEquals(1, resp.getErrors().size());
    ctx.assertEquals(ErrorCodes.ORDER_NOT_FOUND.getCode(), resp.getErrors().get(0).getCode());
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
    CompositePoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(CompositePoLine.class);

    // See MODORDERS-180
    Cost cost = body.getCost();
    cost.setDiscount(10d);
    cost.setDiscountType(Cost.DiscountType.PERCENTAGE);
    cost.setListUnitPrice(9.99d);
    // set wrong estimated price
    cost.setPoLineEstimatedPrice(100d);
    double expectedTotalPoLine = 8.99d;

    String url = String.format(LINE_BY_ID_PATH, lineId);

    final Response resp = verifyPut(url, JsonObject.mapFrom(body).encodePrettily(), "", 204);

    assertTrue(StringUtils.isEmpty(resp.getBody().asString()));

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertEquals(1, column.size());
    assertThat(column.keySet(), containsInAnyOrder(REPORTING_CODES));
    assertThat(column.get(REPORTING_CODES), hasSize(3));

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertEquals(1, column.size());
    assertThat(column.keySet(), containsInAnyOrder(PO_LINES));

    // See MODORDERS-180
    PoLine poLine = column.get(PO_LINES).get(0).mapTo(PoLine.class);
    assertThat(poLine.getCost().getPoLineEstimatedPrice(), equalTo(expectedTotalPoLine));
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
          .contentType(APPLICATION_JSON);


    // Verify that no calls reached mock server
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testPutOrderLineByIdNotFound() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Not Found ===");

    String lineId = ID_DOES_NOT_EXIST;
    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId, PO_ID);

    Response actual = verifyPut(url, body, APPLICATION_JSON, 404);

    assertEquals(lineId, actual.as(Errors.class).getErrors().get(0).getMessage());

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

    Response actual = verifyPut(url, body, APPLICATION_JSON, 500);

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
  public void testAddOrderLineToOrderInOpenStatus() {
    logger.info("=== Test add new line to Open order ===");

    validateNoLineCreatedForNonPendingOrder(PO_ID_OPEN_STATUS, ORDER_OPEN.getCode());
  }

  @Test
  public void testAddOrderLineToOrderInClosedStatus() {
    logger.info("=== Test add new line to Closed order ===");

    validateNoLineCreatedForNonPendingOrder(PO_ID_CLOSED_STATUS, ORDER_CLOSED.getCode());
  }

  private void validateNoLineCreatedForNonPendingOrder(String orderId, String errorCode) {
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, orderId).mapTo(CompositePurchaseOrder.class);

    CompositePoLine poLine = order.getCompositePoLines().get(0);
    poLine.setId(null);

    Errors errors = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(poLine).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);
    validatePoLineCreationErrorForNonPendingOrder(errorCode, errors);
  }

  @Test
  public void testCreatePoLineWithGetPoLineNumberError() throws IOException {
    logger.info("=== Test Create PO Line - fail on PO Line number generation ===");
    String body = getMockData(String.format("%s%s.json", COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_FOR_SUCCESS_CASE));
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TOKEN)
        .header(PO_NUMBER_ERROR_X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(LINES_PATH)
        .then()
          .statusCode(500);
  }

  private String getPoLineWithMinContentAndIds(String lineId, String orderId) throws IOException {
    CompositePoLine poLine = new JsonObject(getMockData(PO_LINE_MIN_CONTENT_PATH)).mapTo(CompositePoLine.class);
    poLine.setId(lineId);
    poLine.setPurchaseOrderId(orderId);
    return JsonObject.mapFrom(poLine).encode();
  }

  @Test
  public void testPoNumberValidateWithExistingPONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", EXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 400);
  }


  @Test
  public void testPoNumberValidateWithUniquePONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", NONEXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), "", 204);
  }

  @Test
  public void testPoNumberValidateWithInvalidPattern()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", "11");
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422);
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
          .contentType(APPLICATION_JSON);
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
          .contentType(APPLICATION_JSON);
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
          .contentType(APPLICATION_JSON);
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
          .contentType(APPLICATION_JSON);
  }

  @Test
  public void testGetOrderPOLinesByPoId() {
    logger.info("=== Test Get Orders lines - With empty query ===");

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
        .param(QUERY_PARAM_NAME, PURCHASE_ORDER_ID + "==" + ORDER_ID_WITH_PO_LINES)
      .get(LINES_PATH)
        .then()
          .statusCode(200)
          .extract()
          .response();

    assertEquals(2, resp.getBody().as(PoLineCollection.class).getTotalRecords().intValue());
  }

  @Test
  public void testGetPoNumber() {
    logger.info("=== Test Get PO Number (generate poNumber) ===");

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
    logger.info("=== Test Get PO Number (generate poNumber) - fail ===");

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
  public void testPostPiece() {
    logger.info("=== Test POST Piece (Create Piece) ===");

    Piece postPieceRq = getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json").mapTo(Piece.class);

    // Positive cases
    // Piece id is null initially
    assertThat(postPieceRq.getId(), nullValue());

    Piece postPieceRs = verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Piece.class);

    // Piece id not null
    assertThat(postPieceRs.getId(), notNullValue());

    // Negative cases
    // Unable to create piece test
    int status400 = HttpStatus.HTTP_BAD_REQUEST.toInt();
    verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10,
      new Header(X_ECHO_STATUS, String.valueOf(status400))), APPLICATION_JSON, status400);

    // Internal error on mod-orders-storage test
    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10,
      new Header(X_ECHO_STATUS, String.valueOf(status500))), APPLICATION_JSON, status500);
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

  @Test
  public void testPostReceivingPhysicalAll() {
    logger.info("=== Test POST Receiving - Receive physical resources");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-all-resources.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receivingRq.getTotalRecords(), CheckinReceivePiecesHelper.MAX_IDS_FOR_GET_RQ) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(itemsSearches, hasSize(expectedSearchRqQty));
    assertThat(itemUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));
    assertThat(polUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      assertThat(item.getString(ITEM_BARCODE), not(isEmptyString()));
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo("Received"));
    });
    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.FULLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    });
  }

  @Test
  public void testPostReceivingElectronicPartially() {
    logger.info("=== Test POST Receiving - Receive partially electronic resources");

    ReceivingCollection receiving = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-electronic-5-of-10-resources-no-items.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receiving).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receiving.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, is(nullValue()));
    assertThat(itemUpdates, is(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receiving.getTotalRecords(), CheckinReceivePiecesHelper.MAX_IDS_FOR_GET_RQ) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(receiving.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));
    assertThat(polUpdates, hasSize(pieceIdsByPol.size()));

    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });
  }

  @Test
  public void testPostReceivingPhysicalWithErrors() {
    logger.info("=== Test POST Receiving - Receive physical resources with different errors");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-resources-6-of-10-with-errors.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(receivingRq.getTotalRecords()));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    assertThat(receivingResult.getPoLineId(), not(isEmptyString()));
    assertThat(receivingResult.getProcessedSuccessfully(), is(4));
    assertThat(receivingResult.getProcessedWithError(), is(6));

    Set<String> errorCodes = new HashSet<>();
    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
      assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
      if (receivingItemResult.getProcessingStatus().getType() == ProcessingStatus.Type.SUCCESS) {
        assertThat(receivingItemResult.getProcessingStatus().getError(), nullValue());
      } else {
        assertThat(receivingItemResult.getProcessingStatus().getError(), not(nullValue()));
        errorCodes.add(receivingItemResult.getProcessingStatus().getError().getCode());
      }
    }

    assertThat(errorCodes, containsInAnyOrder(PIECE_ALREADY_RECEIVED.getCode(),
      PIECE_POL_MISMATCH.getCode(), PIECE_NOT_FOUND.getCode(), ITEM_UPDATE_FAILED.getCode(), ITEM_NOT_FOUND.getCode(),
      PIECE_UPDATE_FAILED.getCode()));

    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);
    assertThat(polSearches, hasSize(1));
    assertThat(polUpdates, hasSize(1));

    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });
  }

  @Test
  public void testPostReceivingWithErrorSearchingForPiece() {
    logger.info("=== Test POST Receiving - Receive resources with error searching for piece");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-500-error-for-pieces-lookup.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(1));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);
      assertThat(receivingResult.getPoLineId(), not(isEmptyString()));
      assertThat(receivingResult.getProcessedSuccessfully(), is(0));
      assertThat(receivingResult.getProcessedWithError(), is(1));

      for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
        assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
        assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
        assertThat(receivingItemResult.getProcessingStatus().getType(), is(ProcessingStatus.Type.FAILURE));
        assertThat(receivingItemResult.getProcessingStatus().getError().getCode(), is(PIECE_NOT_RETRIEVED.getCode()));
    }

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, is(nullValue()));
    assertThat(itemsSearches, is(nullValue()));
    assertThat(itemUpdates, is(nullValue()));
    assertThat(polSearches, is(nullValue()));
    assertThat(polUpdates, is(nullValue()));
  }

  @Test
  public void testPostReceivingWithErrorSearchingForItem() {
    logger.info("=== Test POST Receiving - Receive resources with error searching for item");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-500-error-for-items-lookup.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(1));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);
      assertThat(receivingResult.getPoLineId(), not(isEmptyString()));
      assertThat(receivingResult.getProcessedSuccessfully(), is(0));
      assertThat(receivingResult.getProcessedWithError(), is(1));

      for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
        assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
        assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
        assertThat(receivingItemResult.getProcessingStatus().getType(), is(ProcessingStatus.Type.FAILURE));
        assertThat(receivingItemResult.getProcessingStatus().getError().getCode(), is(ITEM_NOT_RETRIEVED.getCode()));
    }

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(pieceUpdates, is(nullValue()));
    assertThat(itemUpdates, is(nullValue()));
    assertThat(polSearches, is(nullValue()));
    assertThat(polUpdates, is(nullValue()));
  }

  @Test
  public void testPostReceivingRevertMixedResources() {
    logger.info("=== Test POST Receiving - Revert received P/E Mix resources");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "revert-pe-mix-4-of-5-resources.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    // The piece searches should be made 3 times: 1st time to get all required piece records, 2nd and 3rd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(3));
    // In total 4 pieces required update
    assertThat(pieceUpdates, hasSize(4));
    assertThat(itemsSearches, hasSize(1));
    // There are 3 piece records with item id's
    assertThat(itemUpdates, hasSize(3));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));
    assertThat(polUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ITEM_STATUS_ON_ORDER));
    });
    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });
  }

  @Test
  public void testPostReceivingRevertElectronicResource() {
    logger.info("=== Test POST Receiving - Revert received electronic resource");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "revert-electronic-1-of-1-resource.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    // The piece searches should be made 3 times: 1st time to get piece record, 2nd and 3rd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(3));
    assertThat(pieceUpdates, hasSize(1));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(1));
    assertThat(polUpdates, hasSize(1));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ITEM_STATUS_ON_ORDER));
    });
    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.AWAITING_RECEIPT));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });
  }

  private Map<String, Set<String>> verifyReceivingSuccessRs(ReceivingResults results) {
    Map<String, Set<String>> pieceIdsByPol = new HashMap<>();
    for (ReceivingResult receivingResult : results.getReceivingResults()) {
      assertThat(receivingResult.getPoLineId(), not(isEmptyString()));
      assertThat(receivingResult.getProcessedSuccessfully(), is(receivingResult.getReceivingItemResults().size()));
      assertThat(receivingResult.getProcessedWithError(), is(0));

      for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
        assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
        assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
        assertThat(receivingItemResult.getProcessingStatus().getType(), is(ProcessingStatus.Type.SUCCESS));
        assertThat(receivingItemResult.getProcessingStatus().getError(), nullValue());

        pieceIdsByPol.computeIfAbsent(receivingResult.getPoLineId(), k -> new HashSet<>())
                     .add(receivingItemResult.getPieceId());
      }
    }
    return pieceIdsByPol;
  }

  @Test
  public void testCreatePoWithDifferentVendorStatus() throws Exception {

    logger.info("=== Test POST PO with vendor's status ===");

    // Purchase order is OK
    Errors activeVendorActiveAccessProviderErrors = verifyPostResponse(COMPOSITE_ORDERS_PATH, getPoWithVendorId(ACTIVE_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(Errors.class);
    assertThat(activeVendorActiveAccessProviderErrors.getErrors(), empty());

    // Internal mod-vendor error
    Errors internalServerError = verifyPostResponseErrors(1, MOD_VENDOR_INTERNAL_ERROR_ID, ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B);
    assertEquals(GENERIC_ERROR_CODE.getCode(), internalServerError.getErrors().get(0).getCode());

    // Non-existed vendor
    Errors nonExistedVendorError = verifyPostResponseErrors(1, NON_EXIST_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B);
    checkExpectedError(NON_EXIST_VENDOR_ID, nonExistedVendorError, 0, ORDER_VENDOR_NOT_FOUND);

    // Active vendor and non-existed access provider
    Errors inactiveAccessProviderErrors = verifyPostResponseErrors(1, ACTIVE_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, NON_EXIST_ACCESS_PROVIDER_A);
    checkExpectedError(NON_EXIST_ACCESS_PROVIDER_A, inactiveAccessProviderErrors, 0, POL_ACCESS_PROVIDER_NOT_FOUND);

    // Inactive access provider
    Errors nonExistedAccessProviderErrors = verifyPostResponseErrors(1, ACTIVE_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_A);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, nonExistedAccessProviderErrors, 0, POL_ACCESS_PROVIDER_IS_INACTIVE);

    // Inactive vendor and inactive access providers
    Errors allInactiveErrors = verifyPostResponseErrors(3, INACTIVE_VENDOR_ID, INACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_B);
    checkExpectedError(INACTIVE_VENDOR_ID, allInactiveErrors, 0, ORDER_VENDOR_IS_INACTIVE);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, allInactiveErrors, 1, POL_ACCESS_PROVIDER_IS_INACTIVE);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_B, allInactiveErrors, 2, POL_ACCESS_PROVIDER_IS_INACTIVE);

  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenInactiveVendor() throws Exception {

    logger.info("=== Test Put Order By Id to change status of Order to Open for different vendor's status ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Positive cases
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), EMPTY, 204);

    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), EMPTY, 204);

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), EMPTY, 204);

    // Negative cases
    // Non-existed vendor
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(NON_EXIST_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    Errors nonExistedVendorErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), EMPTY, 422).as(Errors.class);
    checkExpectedError(NON_EXIST_VENDOR_ID, nonExistedVendorErrors, 0, ORDER_VENDOR_NOT_FOUND);

    // Inactive access provider
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    Errors inactiveAccessProviderErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), EMPTY, 422).as(Errors.class);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, inactiveAccessProviderErrors, 0, POL_ACCESS_PROVIDER_IS_INACTIVE);

    // Inactive vendor and inactive access providers
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_B);
    Errors allInactiveErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).toString(), EMPTY, 422).as(Errors.class);
    checkExpectedError(INACTIVE_VENDOR_ID, allInactiveErrors, 0, ORDER_VENDOR_IS_INACTIVE);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, allInactiveErrors, 1, POL_ACCESS_PROVIDER_IS_INACTIVE);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_B, allInactiveErrors, 2, POL_ACCESS_PROVIDER_IS_INACTIVE);
  }


  @Test
  public void testPostCheckInElectronicWithNoItems() {
    logger.info("=== Test POST Checkin - CheckIn Electronic resource");

    CheckinCollection checkInRq = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + "checkin-pe-mix-2-electronic-resources.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkInRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkInRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches,is(nullValue()));
    assertThat(itemUpdates, is(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(checkInRq.getTotalRecords(), CheckinReceivePiecesHelper.MAX_IDS_FOR_GET_RQ) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(checkInRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));
    assertThat(polUpdates, hasSize(pieceIdsByPol.size()));

    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    });
  }

  @Test
  public void testPostCheckInPhysicalWithItemsPartialSuccess() {
    logger.info("=== Test POST Checkin - CheckIn physical resource with only one item updated");

    CheckinCollection checkInRq = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + "checkin-pe-mix-2-physical-resources.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkInRq).encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkInRq.getTotalRecords()));
    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    Set<String> errorCodes = new HashSet<>();
    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
      assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
      if (receivingItemResult.getProcessingStatus().getType() == ProcessingStatus.Type.SUCCESS) {
        assertThat(receivingItemResult.getProcessingStatus().getError(), nullValue());
      } else {
        assertThat(receivingItemResult.getProcessingStatus().getError(), not(nullValue()));
        errorCodes.add(receivingItemResult.getProcessingStatus().getError().getCode());
      }
    }

    assertThat(errorCodes, containsInAnyOrder(ITEM_NOT_FOUND.getCode()));

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    assertThat(pieceSearches, hasSize(2));
    assertThat(pieceUpdates, hasSize(1));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(1));
    assertThat(polUpdates, hasSize(1));

    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    });
  }

  @Test
  public void testPostCheckinRevertPhysicalResource() {
    logger.info("=== Test POST Check-in - Revert received Physical resource");

    CheckinCollection checkinReq = getMockAsJson(
        CHECKIN_RQ_MOCK_DATA_PATH + "revert-checkin-physical-1-resource.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkinReq).encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkinReq.getTotalRecords()));

    verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = MockServer.serverRqRs.get(PIECES, HttpMethod.GET);
    List<JsonObject> pieceUpdates = MockServer.serverRqRs.get(PIECES, HttpMethod.PUT);
    List<JsonObject> itemsSearches = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
    List<JsonObject> itemUpdates = MockServer.serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
    List<JsonObject> polSearches = MockServer.serverRqRs.get(PO_LINES, HttpMethod.GET);
    List<JsonObject> polUpdates = MockServer.serverRqRs.get(PO_LINES, HttpMethod.PUT);

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    // The piece searches should be made 3 times: 1st time to get piece record,
    // 2nd and 3rd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(3));
    assertThat(pieceUpdates, hasSize(1));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(1));
    assertThat(polUpdates, hasSize(1));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ITEM_STATUS_ON_ORDER));
    });
    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.AWAITING_RECEIPT));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });
  }

  private Errors verifyPostResponseErrors(int expectedErrorsNumber, String vendorId, String... accessProviderIds) throws Exception {
    Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, getPoWithVendorId(vendorId, accessProviderIds),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);
    assertEquals(errors.getTotalRecords(), new Integer(expectedErrorsNumber));
    assertThat(errors.getErrors(), hasSize(expectedErrorsNumber));
    return errors;
  }

  private String getPoWithVendorId(String vendorId, String... accessProviderIds) throws Exception {
    CompositePurchaseOrder comPo = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    comPo.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    comPo.setVendor(vendorId);
    for(int i = 0; i < accessProviderIds.length; i++) {
      comPo.getCompositePoLines().get(i).getEresource().setAccessProvider(accessProviderIds[i]);
    }
    return JsonObject.mapFrom(comPo).toString();
  }

  private void checkExpectedError(String id, Errors errors, int index, ErrorCodes expectedErrorCodes) {
    assertEquals(id, errors.getErrors().get(index).getAdditionalProperties().get(ID));
    assertEquals(expectedErrorCodes.getCode(), errors.getErrors().get(index).getCode());
    assertEquals(expectedErrorCodes.getDescription(), errors.getErrors().get(index).getMessage());
  }

  private JsonObject getMockAsJson(String path, String id) {
    return getMockAsJson(String.format("%s%s.json", path, id));
  }

  private JsonObject getMockAsJson(String fullPath) {
    try {
      return new JsonObject(getMockData(fullPath));
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
    order.put("workflowStatus", "Pending");

    return order;
  }

  public static String getMockData(String path) throws IOException {
    logger.info("Using mock datafile: {}", path);
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

    private static final String TOTAL_RECORDS = "totalRecords";
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
      router.route(HttpMethod.POST, "/item-storage/items").handler(this::handlePostItemStorRecord);
      router.route(HttpMethod.POST, "/holdings-storage/holdings").handler(this::handlePostHoldingRecord);
      router.route(HttpMethod.POST, resourcesPath(PO_LINES)).handler(this::handlePostPOLine);
      router.route(HttpMethod.POST, resourcesPath(ALERTS)).handler(ctx -> handlePostGenericSubObj(ctx, ALERTS));
      router.route(HttpMethod.POST, resourcesPath(REPORTING_CODES)).handler(ctx -> handlePostGenericSubObj(ctx, REPORTING_CODES));

      router.route(HttpMethod.POST, resourcesPath(PIECES)).handler(ctx -> handlePostGenericSubObj(ctx, PIECES));

      router.route(HttpMethod.GET, resourcesPath(PURCHASE_ORDER)+"/:id").handler(this::handleGetPurchaseOrderById);
      router.route(HttpMethod.GET, resourcesPath(PURCHASE_ORDER)).handler(this::handleGetPurchaseOrderByQuery);
      router.route(HttpMethod.GET, "/instance-types").handler(this::handleGetInstanceType);
      router.route(HttpMethod.GET, "/instance-statuses").handler(this::handleGetInstanceStatus);
      router.route(HttpMethod.GET, "/identifier-types").handler(this::handleGetIdentifierType);
      router.route(HttpMethod.GET, "/inventory/instances").handler(this::handleGetInstanceRecord);
      router.route(HttpMethod.GET, "/item-storage/items").handler(this::handleGetItemRecordsFromStorage);
      router.route(HttpMethod.GET, "/inventory/items").handler(this::handleGetInventoryItemRecords);
      router.route(HttpMethod.GET, "/holdings-storage/holdings").handler(this::handleGetHoldingRecord);
      router.route(HttpMethod.GET, "/loan-types").handler(this::handleGetLoanType);
      router.route(HttpMethod.GET, "/vendor-storage/vendors/:id").handler(this::handleGetVendorById);
      router.route(HttpMethod.GET, "/vendor-storage/vendors").handler(this::handleGetAccessProviders);
      router.route(HttpMethod.GET, resourcesPath(PO_LINES)).handler(this::handleGetPoLines);
      router.route(HttpMethod.GET, resourcePath(PO_LINES)).handler(this::handleGetPoLineById);
      router.route(HttpMethod.GET, resourcePath(ALERTS)).handler(ctx -> handleGetGenericSubObj(ctx, ALERTS));
      router.route(HttpMethod.GET, resourcePath(REPORTING_CODES)).handler(ctx -> handleGetGenericSubObj(ctx, REPORTING_CODES));
      router.route(HttpMethod.GET, resourcesPath(PO_NUMBER)).handler(this::handleGetPoNumber);
      router.route(HttpMethod.GET, resourcesPath(PIECES)).handler(this::handleGetPieces);
      router.route(HttpMethod.GET, resourcesPath(RECEIVING_HISTORY)).handler(this::handleGetReceivingHistory);
      router.route(HttpMethod.GET, resourcesPath(PO_LINE_NUMBER)).handler(this::handleGetPoLineNumber);

      router.route(HttpMethod.PUT, resourcePath(PURCHASE_ORDER)).handler(ctx -> handlePutGenericSubObj(ctx, PURCHASE_ORDER));
      router.route(HttpMethod.PUT, resourcePath(PO_LINES)).handler(ctx -> handlePutGenericSubObj(ctx, PO_LINES));
      router.route(HttpMethod.PUT, resourcePath(PIECES)).handler(ctx -> handlePutGenericSubObj(ctx, PIECES));
      router.route(HttpMethod.PUT, resourcePath(REPORTING_CODES)).handler(ctx -> handlePutGenericSubObj(ctx, REPORTING_CODES));
      router.route(HttpMethod.PUT, resourcePath(ALERTS)).handler(ctx -> handlePutGenericSubObj(ctx, ALERTS));
      router.route(HttpMethod.PUT, "/inventory/items/:id").handler(ctx -> handlePutGenericSubObj(ctx, ITEM_RECORDS));

      router.route(HttpMethod.DELETE, resourcesPath(PURCHASE_ORDER)+"/:id").handler(ctx -> handleDeleteGenericSubObj(ctx, PURCHASE_ORDER));
      router.route(HttpMethod.DELETE, resourcePath(PO_LINES)).handler(ctx -> handleDeleteGenericSubObj(ctx, PO_LINES));
      router.route(HttpMethod.DELETE, resourcePath(ALERTS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ALERTS));
      router.route(HttpMethod.DELETE, resourcePath(REPORTING_CODES)).handler(ctx -> handleDeleteGenericSubObj(ctx, REPORTING_CODES));

      router.get("/configurations/entries").handler(this::handleConfigurationModuleResponse);
      return router;
    }

    private void handleGetPoLineNumber(RoutingContext ctx) {
      if(PO_NUMBER_ERROR_TENANT.equals(ctx.request().getHeader(OKAPI_HEADER_TENANT))) {
        ctx.response()
          .setStatusCode(500)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end();
      } else {
        SequenceNumber seqNumber = new SequenceNumber();
        seqNumber.setSequenceNumber(PO_LINE_NUMBER_VALUE);
        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(JsonObject.mapFrom(seqNumber).encodePrettily());
      }
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

    private void handlePostItemStorRecord(RoutingContext ctx) {
      String bodyAsString = ctx.getBodyAsString();
      logger.info("handlePostItemRecord got: " + bodyAsString);

      if (bodyAsString.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR);
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

    private void handleGetItemRecordsFromStorage(RoutingContext ctx) {
      logger.info("handleGetItemRecordsFromStorage got: " + ctx.request().path());

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

    private void handleGetInventoryItemRecords(RoutingContext ctx) {
      logger.info("handleGetInventoryItemRecords got: " + ctx.request().path());

      String query = ctx.request().getParam("query");

      if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, new JsonObject());
        serverResponse(ctx, 500, APPLICATION_JSON, Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
      } else {
        try {
          JsonObject items = new JsonObject(getMockData(ITEMS_RECORDS_MOCK_DATA_PATH + "inventoryItemsCollection.json"));
          JsonArray jsonArray = items.getJsonArray(ITEMS);

          if (query.startsWith("id==")) {
            List<String> itemIds = extractIdsFromQuery(query);
            final Iterator iterator = jsonArray.iterator();
            while (iterator.hasNext()) {
              JsonObject item = (JsonObject) iterator.next();
              if (!itemIds.contains(item.getString(ID))) {
                iterator.remove();
              }
            }
          }

          items.put("totalRecords", jsonArray.size());
          addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, items);

          ctx.response()
             .setStatusCode(200)
             .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
             .end(items.encodePrettily());
        } catch (Exception e) {
          serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR);
        }
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

    private void handleGetAccessProviders(RoutingContext ctx) {
      logger.info("handleGetAccessProviders got: " + ctx.request().path());
      String query = ctx.request().getParam("query");
      JsonObject body = new JsonObject();

      try {
        if(getQuery(ACTIVE_ACCESS_PROVIDER_A, NON_EXIST_ACCESS_PROVIDER_A).equals(query)) {
          body = new JsonObject(getMockData(VENDORS_MOCK_DATA_PATH + "one_access_provider_not_found.json"));
        }  else if(getQuery(ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B).equals(query)
          || getQuery(ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_A).equals(query)
          || getQuery(ACTIVE_ACCESS_PROVIDER_B, ACTIVE_ACCESS_PROVIDER_A).equals(query)) {
          body = new JsonObject(getMockData(VENDORS_MOCK_DATA_PATH + "all_access_providers_active.json"));
        } else if(getQuery(ACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
          body = new JsonObject(getMockData(VENDORS_MOCK_DATA_PATH + "active_inactive_access_providers.json"));
        } else if(getQuery(INACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_B).equals(query)
        || getQuery(INACTIVE_ACCESS_PROVIDER_B, INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
          body = new JsonObject(getMockData(VENDORS_MOCK_DATA_PATH + "all_inactive_access_providers.json"));
        } else if(getQuery(ACTIVE_ACCESS_PROVIDER_A).equals(query)) {
          body = new JsonObject(getMockData(VENDORS_MOCK_DATA_PATH + "one_access_provider_not_found.json"));
        } else {
          ctx.response()
            .setStatusCode(HttpStatus.HTTP_NOT_FOUND.toInt())
            .end();
          return;
        }
      } catch(IOException e) {
        ctx.response()
          .setStatusCode(HttpStatus.HTTP_NOT_FOUND.toInt())
          .end();
      }
      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
    }

    private void handleGetVendorById(RoutingContext ctx) {
      logger.info("handleGetVendorById got: " + ctx.request().path());
      String vendorId = ctx.request().getParam(ID);
      JsonObject body;
      try {
        if (NON_EXIST_VENDOR_ID.equals(vendorId)) {
          serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "vendor not found");
        } else if (MOD_VENDOR_INTERNAL_ERROR_ID.equals(vendorId)) {
          serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), APPLICATION_JSON, "internal server error, contact administrator");
        } else {
          switch (vendorId) {
            case ACTIVE_VENDOR_ID:
              body = new JsonObject(getMockData(VENDORS_MOCK_DATA_PATH + "active_vendor.json"));
              break;
            case INACTIVE_VENDOR_ID:
              body = new JsonObject(getMockData(VENDORS_MOCK_DATA_PATH + "inactive_vendor.json"));
              break;
            case PENDING_VENDOR_ID:
              body = new JsonObject(getMockData(VENDORS_MOCK_DATA_PATH + "pending_vendor.json"));
              break;
            default :
              serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "vendor not found");
              return;
          }
          serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
        }
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt())
          .end();
      }
    }

    private String getQuery(String... accessProviders) {
      return convertIdsToCqlQuery(Arrays.asList(accessProviders));
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
          receivingHistory.put("receivingHistory", new JsonArray());
          receivingHistory.put("totalRecords", 0);
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
        if (PO_NUMBER_ERROR_X_OKAPI_TENANT.getValue().equals(tenant)) {
          tenant = EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10.getValue();
        }
        serverResponse(ctx, 200, APPLICATION_JSON, getMockData(String.format(CONFIG_MOCK_PATH, tenant)));
      } catch (IOException e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR);
      }

    }

    private void handleDeleteGenericSubObj(RoutingContext ctx, String subObj) {
      String id = ctx.request().getParam(ID);

      addServerRqRsData(HttpMethod.DELETE, subObj, new JsonObject().put(ID, id));
      if (ID_DOES_NOT_EXIST.equals(id)) {
        serverResponse(ctx, 404, TEXT_PLAIN, id);
      } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR);
      } else {
        ctx.response()
           .setStatusCode(204)
           .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
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
        serverResponse(ctx, 400, APPLICATION_JSON, Status.BAD_REQUEST.getReasonPhrase());
      } else if (queryParam.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        serverResponse(ctx, 500, APPLICATION_JSON, Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
      } else {
        String poId = EMPTY;
        String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT);
        List<String> polIds = Collections.emptyList();

        if (queryParam.contains(PURCHASE_ORDER_ID)) {
          poId = queryParam.split(PURCHASE_ORDER_ID + "==")[1];
        } else if (queryParam.startsWith("id==")) {
          polIds = extractIdsFromQuery(queryParam);
        }

        try {
          PoLineCollection poLineCollection;
          if (poId.equals(ORDER_ID_WITH_PO_LINES) || !polIds.isEmpty()) {
            poLineCollection = new JsonObject(getMockData(POLINES_COLLECTION)).mapTo(PoLineCollection.class);

            // Filter PO Lines either by PO id or by expected line ids
            Iterator<PoLine> iterator = poLineCollection.getPoLines().iterator();
            while (iterator.hasNext()) {
              PoLine poLine = iterator.next();
              if (polIds.isEmpty() ? !poId.equals(poLine.getPurchaseOrderId()) : !polIds.contains(poLine.getId())) {
                iterator.remove();
              }
            }
            poLineCollection.setTotalRecords(poLineCollection.getPoLines().size());
          } else {
            String filePath;
            if (ID_FOR_PRINT_MONOGRAPH_ORDER.equals(poId)) {
              filePath = LISTED_PRINT_MONOGRAPH_PATH;
            } else {
              filePath = String.format("%s%s.json", COMP_ORDER_MOCK_DATA_PATH, poId);
            }
            JsonObject compPO = new JsonObject(getMockData(filePath));
            // Build PoLineCollection to make sure content is valid
            poLineCollection = buildPoLineCollection(tenant, compPO.getJsonArray(COMPOSITE_PO_LINES));
          }

          // Update calculated data
          updatePoLineCalculatedData(poLineCollection);

          JsonObject po_lines = JsonObject.mapFrom(poLineCollection);
          logger.info(po_lines.encodePrettily());

          addServerRqRsData(HttpMethod.GET, PO_LINES, po_lines);
          serverResponse(ctx, 200, APPLICATION_JSON, po_lines.encode());
        } catch (NoSuchFileException e) {
          PoLineCollection poLineCollection = new PoLineCollection();

          // Attempt to find POLine in mock server memory
          List<JsonObject> postedPoLines = serverRqRs.column(HttpMethod.POST).get(PO_LINES);

          if (postedPoLines != null) {
            List<PoLine> poLines = postedPoLines.stream()
              .map(jsonObj -> jsonObj.mapTo(PoLine.class))
              .collect(Collectors.toList());

            for (PoLine poLine : poLines) {
              if (poId.equals(poLine.getPurchaseOrderId())) {
                poLineCollection.getPoLines().add(poLine);
              }
            }
          }
          poLineCollection.setTotalRecords(poLineCollection.getPoLines().size());
          serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(poLineCollection).encodePrettily());
        } catch (IOException e) {
          PoLineCollection poLineCollection = new PoLineCollection();
          poLineCollection.setTotalRecords(0);
          serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(poLineCollection).encodePrettily());
        }
      }
    }

    private void updatePoLineCalculatedData(PoLineCollection poLineCollection) {
      poLineCollection.getPoLines().forEach(this::updatePoLineEstimatedPrice);
    }

    private void updatePoLineEstimatedPrice(PoLine line) {
      if (line.getCost() != null) {
        Cost cost = JsonObject.mapFrom(line.getCost()).mapTo(Cost.class);
        line.getCost().setPoLineEstimatedPrice(calculateEstimatedPrice(cost));
      }
    }

    private PoLineCollection buildPoLineCollection(String tenant, JsonArray lines) {
      PoLineCollection result = new PoLineCollection();
      if (lines == null || lines.isEmpty()) {
        result.setTotalRecords(0);
      } else {
        // Transform composite PO Lines to storage representation
        List<PoLine> poLines = lines
          .stream()
          .map(l -> (JsonObject) l)
          .map(line -> {
            replaceObjectsByIds(line, ALERTS, REPORTING_CODES);
            return line.mapTo(PoLine.class);
          })
          .collect(Collectors.toList());

        // Set PO Line number if empty
        for (PoLine line : poLines) {
          if (StringUtils.isEmpty(line.getPoLineNumber())) {
            line.setPoLineNumber(PO_NUMBER_VALUE + "-1");
          }
        }

        result.setPoLines(poLines);

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

    private void handleGetPoLineById(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam(ID);
      logger.info("id: " + id);

      addServerRqRsData(HttpMethod.GET, PO_LINES, new JsonObject().put(ID, id));

      if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR);
      } else {
        try {

          JsonObject pol = null;

          // Attempt to find POLine in mock server memory
          Map<String, List<JsonObject>> column = serverRqRs.column(HttpMethod.POST);
          if (MapUtils.isNotEmpty(column) && CollectionUtils.isNotEmpty(column.get(PO_LINES))) {
            List<JsonObject> objects = new ArrayList<>(column.get(PO_LINES));
            Comparator<JsonObject> comparator = Comparator.comparing(o -> o.getString(ID));
            objects.sort(comparator);
            int ind = Collections.binarySearch(objects, new JsonObject().put(ID, id), comparator);
            if(ind > -1) {
              pol = objects.get(ind);
            }
          }

          // If previous step has no result then attempt to find POLine in stubs
          if (pol == null) {
            PoLine poLine = new JsonObject(getMockData(String.format("%s%s.json", PO_LINES_MOCK_DATA_PATH, id))).mapTo(PoLine.class);
            updatePoLineEstimatedPrice(poLine);
            pol = JsonObject.mapFrom(poLine);
          }

          serverResponse(ctx, 200, APPLICATION_JSON, pol.encodePrettily());
        } catch (IOException e) {
          serverResponse(ctx, 404, APPLICATION_JSON, id);
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
        serverResponse(ctx, 404, APPLICATION_JSON, id);
      } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR);
      } else {
        ctx.response()
           .setStatusCode(200)
           .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
           .end(data.encodePrettily());
      }
    }

    private void handleGetPieces(RoutingContext ctx) {
      logger.info("handleGetPieces got: " + ctx.request().path());
      String query = ctx.request().getParam("query");
      if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        addServerRqRsData(HttpMethod.GET, PIECES, new JsonObject());
        serverResponse(ctx, 500, APPLICATION_JSON, Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
      } else {
        PieceCollection pieces;
        try {
          if (query.contains("poLineId==")) {
            List<String> conditions = StreamEx
              .split(query, " or ")
              .flatMap(s -> StreamEx.split(s, " and "))
              .toList();

            String polId = EMPTY;
            String status = EMPTY;
            for (String condition : conditions) {
              if (condition.startsWith("poLineId")) {
                polId = condition.split("poLineId==")[1];
              } else if (condition.startsWith("receivingStatus")) {
                status = condition.split("receivingStatus==")[1];
              }
            }
            logger.info("poLineId: " + polId);
            logger.info("receivingStatus: " + status);

            String path = PIECE_RECORDS_MOCK_DATA_PATH + String.format("pieceRecords-%s.json", polId);
            pieces = new JsonObject(getMockData(path)).mapTo(PieceCollection.class);

            // Filter piece records by receiving status
            if (StringUtils.isNotEmpty(status)) {
              Piece.ReceivingStatus receivingStatus = Piece.ReceivingStatus.fromValue(status);
              pieces.getPieces()
                    .removeIf(piece -> receivingStatus != piece.getReceivingStatus());
            }
          } else {
            pieces = new JsonObject(getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecordsCollection.json")).mapTo(PieceCollection.class);

            if (query.contains("id==")) {
              List<String> pieceIds = extractIdsFromQuery(query);
              pieces.getPieces()
                    .removeIf(piece -> !pieceIds.contains(piece.getId()));
            }
          }

          pieces.setTotalRecords(pieces.getPieces()
                                       .size());

        } catch (Exception e) {
          pieces = new PieceCollection();
          pieces.setTotalRecords(0);
        }

        JsonObject data = JsonObject.mapFrom(pieces);
        addServerRqRsData(HttpMethod.GET, PIECES, data);

        ctx.response()
           .setStatusCode(200)
           .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
           .end(data.encodePrettily());
      }
    }

    private List<String> extractIdsFromQuery(String query) {
      return StreamEx
        .split(query, " or ")
        .flatMap(s -> StreamEx.split(s, "=="))
        .map(String::trim)
        .filter(s -> !ID.equals(s))
        .toList();
    }

    private void handlePutGenericSubObj(RoutingContext ctx, String subObj) {
      logger.info("handlePutGenericSubObj got: PUT " + ctx.request().path());
      String id = ctx.request().getParam(ID);

      addServerRqRsData(HttpMethod.PUT, subObj, ctx.getBodyAsJson());

      if (ID_DOES_NOT_EXIST.equals(id)) {
        serverResponse(ctx, 404, APPLICATION_JSON, id);
      } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id) || ctx.getBodyAsString().contains("500500500500")) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR);
      } else {
        ctx.response()
           .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
           .setStatusCode(204)
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

    private void handleGetPurchaseOrderById(RoutingContext ctx) {
      logger.info("handleGetPurchaseOrderById got: GET " + ctx.request().path());
      String id = ctx.request().getParam(ID);
      logger.info("id: " + id);

      try {
        String filePath;
        if (ID_FOR_PRINT_MONOGRAPH_ORDER.equals(id)) {
          filePath = LISTED_PRINT_MONOGRAPH_PATH;
        } else {
          filePath = String.format("%s%s.json", COMP_ORDER_MOCK_DATA_PATH, id);
        }
        JsonObject po = new JsonObject(getMockData(filePath));
        po.remove(COMPOSITE_PO_LINES);

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
        serverResponse(ctx, 400, APPLICATION_JSON, Status.BAD_REQUEST.getReasonPhrase());
      } else if (queryParam.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        serverResponse(ctx, 500, APPLICATION_JSON, Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
      } else {
        JsonObject po = new JsonObject();
        addServerRqRsData(HttpMethod.GET, PURCHASE_ORDER, po);
        final String PO_NUMBER_QUERY = "poNumber==";
        switch (queryParam) {
          case PO_NUMBER_QUERY + EXISTING_PO_NUMBER:
            po.put(TOTAL_RECORDS, 1);
            break;
          case PO_NUMBER_QUERY + NONEXISTING_PO_NUMBER:
            po.put(TOTAL_RECORDS, 0);
            break;
          case EMPTY:
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
      String id = UUID.randomUUID().toString();
      JsonObject body = ctx.getBodyAsJson();
      body.put(ID, id);
      org.folio.rest.acq.model.PurchaseOrder po = body.mapTo(org.folio.rest.acq.model.PurchaseOrder.class);
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
      String contentType = APPLICATION_JSON;

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
        case 400:
          respBody = "Unable to add -- malformed JSON at 13:3";
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
        case ALERTS:
          return org.folio.rest.acq.model.Alert.class;
        case REPORTING_CODES:
          return org.folio.rest.acq.model.ReportingCode.class;
        case PIECES:
            return org.folio.rest.acq.model.Piece.class;
      }

      fail("The sub-object is unknown");
      return null;
    }

    private void handlePostPOLine(RoutingContext ctx) {
      logger.info("got poLine: " + ctx.getBodyAsString());
      JsonObject body = ctx.getBodyAsJson();
      org.folio.rest.acq.model.PoLine pol = body.mapTo(org.folio.rest.acq.model.PoLine.class);

      if (pol.getId() == null) {
        pol.setId(UUID.randomUUID().toString());
      }


      if (ID_FOR_INTERNAL_SERVER_ERROR.equals(pol.getPurchaseOrderId())) {
        ctx.response()
          .setStatusCode(500)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end();
      } else {
        ctx.response()
          .setStatusCode(201)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(JsonObject.mapFrom(pol).encodePrettily());
      }

      addServerRqRsData(HttpMethod.POST, PO_LINES, body);
    }

    private void handleGetPoNumber(RoutingContext ctx) {
      if(PO_NUMBER_ERROR_TENANT.equals(ctx.request().getHeader(OKAPI_HEADER_TENANT))) {
        ctx.response()
          .setStatusCode(500)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end();
      } else {
        SequenceNumber seqNumber = new SequenceNumber();
        seqNumber.setSequenceNumber(PO_NUMBER_VALUE);
        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(JsonObject.mapFrom(seqNumber).encodePrettily());
      }
    }
  }
}