package org.folio.rest.impl;

import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.awaitility.Awaitility.await;
import static org.folio.orders.utils.ErrorCodes.PROHIBITED_FIELD_CHANGING;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.AbstractHelper.ORDER_IDS;
import static org.folio.rest.impl.AcquisitionsMembershipsTests.USER_ID_ASSIGNED_TO_ACQ_UNITS;
import static org.folio.rest.impl.ApiTestSuite.mockPort;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.iterableWithSize;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.rest.jaxrs.model.ToBeReceived;
import org.folio.rest.tools.parser.JsonPathParser;
import org.hamcrest.Matchers;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import io.restassured.RestAssured;
import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.Handler;
import io.vertx.core.eventbus.Message;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class ApiTestBase {

  static {
    System.setProperty(LoggerFactory.LOGGER_DELEGATE_FACTORY_CLASS_NAME, "io.vertx.core.logging.Log4j2LogDelegateFactory");
  }

  private static final Logger logger = LoggerFactory.getLogger(ApiTestBase.class);

  public static final String ORDERS_RECEIVING_ENDPOINT = "/orders/receive";
  public static final String ORDERS_CHECKIN_ENDPOINT = "/orders/check-in";

  static final String PO_LINE_NUMBER_VALUE = "1";

  static final String INVALID_LANG = "?lang=english";
  static final String BAD_QUERY = "unprocessableQuery";
  static final String ID = "id";

  static final String ID_BAD_FORMAT = "123-45-678-90-abc";
  protected static final String ID_DOES_NOT_EXIST = "d25498e7-3ae6-45fe-9612-ec99e2700d2f";
  protected static final String ID_FOR_INTERNAL_SERVER_ERROR = "168f8a86-d26c-406e-813f-c7527f241ac3";
  protected static final String PO_ID_GET_LINES_INTERNAL_SERVER_ERROR = "bad500bb-bbbb-500b-bbbb-bbbbbbbbbbbb";
  protected static final String PO_ID_PENDING_STATUS_WITH_PO_LINES = "e5ae4afd-3fa9-494e-a972-f541df9b877e";
  protected static final String PO_ID_PENDING_STATUS_WITHOUT_PO_LINES = "50fb922c-3fa9-494e-a972-f2801f1b9fd1";
  protected static final String PO_ID_OPEN_STATUS = "c1465131-ed35-4308-872c-d7cdf0afc5f7";
  protected static final String PO_ID_CLOSED_STATUS = "07f65192-44a4-483d-97aa-b137bbd96390";
  protected static final String PO_ID_OPEN_TO_BE_CLOSED = "9d56b621-202d-414b-9e7f-5fefe4422ab3";
  static final String PO_LINE_ID_FOR_SUCCESS_CASE = "fca5fa9e-15cb-4a3d-ab09-eeea99b97a47";
  static final String MIN_PO_ID = UUID.randomUUID().toString();
  static final String MIN_PO_LINE_ID = UUID.randomUUID().toString();

  static final String COMP_ORDER_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/";

  static final String X_ECHO_STATUS = "X-Okapi-Echo-Status";
  static final String EMPTY_CONFIG_TENANT = "config_empty";
  static final String NON_EXIST_CONTRIBUTOR_NAME_TYPE_TENANT = "nonExistContributorNameType";
  static final String INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT = "hasCodeLikeInstanceStatus";
  static final String NON_EXIST_INSTANCE_STATUS_TENANT = "nonExistInstanceStatus";
  static final String NON_EXIST_INSTANCE_TYPE_TENANT = "nonExistInstanceType";
  static final String NON_EXIST_LOAN_TYPE_TENANT = "nonExistLoanType";
  static final String COMPOSITE_PO_LINES_PREFIX = "compositePoLines[0].";

  protected static final Header X_OKAPI_URL = new Header("X-Okapi-Url", "http://localhost:" + mockPort);

  static final Header INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT);
  static final Header NON_EXIST_INSTANCE_STATUS_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_INSTANCE_STATUS_TENANT);
  static final Header NON_EXIST_INSTANCE_TYPE_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_INSTANCE_TYPE_TENANT);
  static final Header NON_EXIST_LOAN_TYPE_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_LOAN_TYPE_TENANT);
  public static final Header NON_EXIST_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "ordersimpltest");
  public static final Header X_OKAPI_USER_ID = new Header(OKAPI_USERID_HEADER, "440c89e3-7f6c-578a-9ea8-310dad23605e");
  static final Header X_OKAPI_USER_ID_WITH_ACQ_UNITS = new Header(OKAPI_USERID_HEADER, USER_ID_ASSIGNED_TO_ACQ_UNITS);
  protected static final Header X_OKAPI_TOKEN = new Header(OKAPI_HEADER_TOKEN, "eyJhbGciOiJIUzI1NiJ9");
  protected static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10 = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_10");
  static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1 = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_1");
  static final Header INVALID_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "invalid_config");
  static final Header EMPTY_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, EMPTY_CONFIG_TENANT);
  public static final String PROTECTED_READ_ONLY_TENANT = "protected_read";

  private static final String STATUS_RECEIVED = "Received";
  private static final String LOCATION_ID = "f34d27c6-a8eb-461b-acd6-5dea81771e70";

  private static boolean runningOnOwn;

  // The variable is defined in main thread but the value is going to be inserted in vert.x event loop thread
  private static volatile List<Message<JsonObject>> eventMessages = new ArrayList<>();

  /**
   * Define unit test specific beans to override actual ones
   */
  @Configuration
  static class ContextConfiguration {

    @Bean("orderStatusHandler")
    @Primary
    public Handler<Message<JsonObject>> mockedOrderStatusHandler() {
      // As an implementation just add received message to list
      return message -> {
        logger.info("New message sent to {} address", message.address());
        eventMessages.add(message);
      };
    }

    @Bean("receiptStatusHandler")
    @Primary
    public Handler<Message<JsonObject>> mockedReceiptStatusHandler() {
      // As an implementation just add received message to list
      return message -> {
        logger.info("New message sent to {} address", message.address());
        eventMessages.add(message);
      };
    }
  }

  @BeforeClass
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {

    if(ApiTestSuite.isNotInitialised()) {
      System.out.println("Running test on own, initialising suite manually");
      runningOnOwn = true;
      ApiTestSuite.before();
    }
  }

  @Before
  public void setUp() {
    clearServiceInteractions();
  }

  protected void clearServiceInteractions() {
    eventMessages.clear();
    MockServer.release();
  }

  @AfterClass
  public static void after() {

    if(runningOnOwn) {
      System.out.println("Running test on own, un-initialising suite manually");
      ApiTestSuite.after();
    }
  }


  protected static String getMockData(String path) throws IOException {
    logger.info("Using mock datafile: {}", path);
    try (InputStream resourceAsStream = PurchaseOrdersApiTest.class.getClassLoader().getResourceAsStream(path)) {
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

  static JsonObject getMockAsJson(String path, String id) {
    return getMockAsJson(String.format("%s%s.json", path, id));
  }

  protected static JsonObject getMockAsJson(String fullPath) {
    try {
      return new JsonObject(getMockData(fullPath));
    } catch (IOException e) {
      fail(e.getMessage());
    }
    return new JsonObject();
  }

  public Response verifyPostResponse(String url, String body, Headers headers, String
    expectedContentType, int expectedCode) {
    Response response = RestAssured
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

    // Verify no messages sent via event bus on POST (except receiving/check-in)
    if (!(url.startsWith(ORDERS_RECEIVING_ENDPOINT) || url.startsWith(ORDERS_CHECKIN_ENDPOINT))) {
      verifyOrderStatusUpdateEvent(0);
    }

    return response;
  }


  Response verifyPut(String url, JsonObject body, String expectedContentType, int expectedCode) {
    return verifyPut(url, body.encodePrettily(), expectedContentType, expectedCode);
  }

  Response verifyPut(String url, String body, String expectedContentType, int expectedCode) {
    return verifyPut(url, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), expectedContentType, expectedCode);
  }

  public Response verifyPut(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
    Response response = RestAssured
      .with()
        .header(X_OKAPI_TOKEN)
        .header(X_OKAPI_URL)
        .headers(headers)
        .body(body)
        .contentType(APPLICATION_JSON)
      .put(url)
        .then()
          .statusCode(expectedCode)
          .contentType(expectedContentType)
          .extract()
            .response();

    // Verify no messages sent via event bus on PUT if there is an error
    if (expectedCode != 204) {
      verifyOrderStatusUpdateEvent(0);
    }

    return response;
  }

  public Response verifyGet(String url, String expectedContentType, int expectedCode) {
    Headers headers = prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT);
    return verifyGet(url, headers, expectedContentType, expectedCode);
  }

  public Response verifyGet(String url, String expectedContentType, int expectedCode, String tenant) {
    Headers headers = prepareHeaders(new Header(OKAPI_HEADER_TENANT, tenant));
    return verifyGet(url, headers, expectedContentType, expectedCode);
  }

  public Response verifyGet(String url, Headers headers, String expectedContentType, int expectedCode) {
    return RestAssured
      .with()
      .header(X_OKAPI_URL)
        .headers(headers)
      .get(url)
        .then()
          .statusCode(expectedCode)
          .contentType(expectedContentType)
          .extract()
            .response();
  }

  <T> T verifySuccessGet(String url, Class<T> clazz) {
    return verifyGet(url, APPLICATION_JSON, 200).as(clazz);
  }

  <T> T verifySuccessGet(String url, Class<T> clazz, String tenant) {
    return verifyGet(url, APPLICATION_JSON, 200, tenant).as(clazz);
  }

  public Response verifyDeleteResponse(String url, String expectedContentType, int expectedCode) {
    Headers headers =  prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT);
    return verifyDeleteResponse(url, headers, expectedContentType, expectedCode);
  }

  public Response verifyDeleteResponse(String url, Headers headers, String expectedContentType, int expectedCode) {
    Response response = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .headers(headers)
      .delete(url)
        .then()
          .statusCode(expectedCode)
          .contentType(expectedContentType)
          .extract()
            .response();

    // Verify no messages sent via event bus
    verifyOrderStatusUpdateEvent(0);

    return response;
  }

  public Headers prepareHeaders(Header... headers) {
    return new Headers(headers);
  }

  static void validatePoLineCreationErrorForNonPendingOrder(String errorCode, Errors errors, int externalAPICalls) {
    assertEquals(1, errors.getErrors().size());
    assertEquals(errorCode, errors.getErrors().get(0).getCode());
    // Assert that only PO Lines limit (count of existing Lines) , GET PO and ISBN validation requests made
    assertEquals(externalAPICalls, MockServer.serverRqRs.size());
    assertEquals(externalAPICalls, MockServer.serverRqRs.rowKeySet().size());
    assertEquals(1, MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET).size());
    assertEquals(1, getPoLineSearches().size());
  }

  void verifyLocationQuantity(Location location, CompositePoLine.OrderFormat orderFormat) {
    switch (orderFormat) {
      case P_E_MIX:
        assertEquals(location.getQuantityPhysical() + location.getQuantityElectronic(), location.getQuantity().intValue());
        break;
      case ELECTRONIC_RESOURCE:
        assertEquals(location.getQuantityElectronic(), location.getQuantity());
        break;
      case PHYSICAL_RESOURCE:
        assertEquals(location.getQuantityPhysical(), location.getQuantity());
        break;
      case OTHER:
        assertEquals(location.getQuantityPhysical(), location.getQuantity());
        break;
    }
  }

  void verifyOrderStatusUpdateEvent(int msgQty) {
    logger.debug("Verifying event bus messages");
    // Wait until event bus registers message
    await().atLeast(50, MILLISECONDS)
           .atMost(1, SECONDS)
           .until(() -> eventMessages, hasSize(msgQty));
    for (int i = 0; i < msgQty; i++) {
      Message<JsonObject> message = eventMessages.get(i);
      assertThat(message.address(), equalTo(MessageAddress.ORDER_STATUS.address));
      assertThat(message.headers(), not(emptyIterable()));
      assertThat(message.body(), notNullValue());
      assertThat(message.body().getJsonArray(ORDER_IDS), iterableWithSize(1));
      assertThat(message.body().getString(HelperUtils.LANG), not(isEmptyOrNullString()));
    }
  }

  void verifyReceiptStatusUpdateEvent(int msgQty) {
    logger.debug("Verifying event bus messages");
    // Wait until event bus registers message
    await().atLeast(50, MILLISECONDS)
           .atMost(1, SECONDS)
           .until(() -> eventMessages, hasSize(msgQty));
    for (int i = 0; i < msgQty; i++) {
      Message<JsonObject> message = eventMessages.get(i);
      assertThat(message.address(), equalTo(MessageAddress.RECEIPT_STATUS.address));
      assertThat(message.headers(), not(emptyIterable()));
      assertThat(message.body(), notNullValue());
      assertThat(message.body().getString("poLineIdUpdate"), not(isEmptyOrNullString()));
      assertThat(message.body().getString(HelperUtils.LANG), not(isEmptyOrNullString()));
    }
  }

  void checkPreventProtectedFieldsModificationRule(String path, JsonObject compPO, Map<String, Object> updatedFields) {
    JsonObject compPOJson = JsonObject.mapFrom(compPO);
    JsonPathParser compPOParser = new JsonPathParser(compPOJson);
    for (Map.Entry<String, Object> m : updatedFields.entrySet()) {
      compPOParser.setValueAt(m.getKey(), m.getValue());
    }
    Errors errors = verifyPut(String.format(path, compPO.getString("id")), compPOJson, "", HttpStatus.HTTP_BAD_REQUEST.toInt())
      .as(Errors.class);

    // Only one error expected
    assertThat(errors.getErrors(), hasSize(1));

    Error error = errors.getErrors()
      .get(0);
    assertThat(error.getCode(), equalTo(PROHIBITED_FIELD_CHANGING.getCode()));

    Object[] failedFieldNames = getModifiedProtectedFields(error);
    Object[] expected = updatedFields.keySet()
      .stream()
      .map(fieldName -> fieldName.replace(COMPOSITE_PO_LINES_PREFIX, StringUtils.EMPTY))
      .toArray();
    assertThat(failedFieldNames.length, is(expected.length));
    assertThat(expected, Matchers.arrayContainingInAnyOrder(failedFieldNames));
  }

  Object[] getModifiedProtectedFields(Error error) {
    return Optional.of(error.getAdditionalProperties()
      .get("protectedAndModifiedFields"))
      .map(obj -> (List) obj)
      .get()
      .toArray();
  }

  public static Piece getMinimalContentPiece(String poLineId) {
    return new Piece()
      .withId(UUID.randomUUID().toString())
      .withReceivingStatus(Piece.ReceivingStatus.RECEIVED)
      .withFormat(Piece.Format.PHYSICAL)
      .withPoLineId(poLineId);
  }


  public static CompositePoLine getMinimalContentCompositePoLine() {
    return getMinimalContentCompositePoLine(MIN_PO_ID);
  }

  public static CompositePoLine getMinimalContentCompositePoLine(String orderId) {
    return new CompositePoLine().withSource(CompositePoLine.Source.EDI)
      .withId(MIN_PO_LINE_ID)
      .withOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withAcquisitionMethod(CompositePoLine.AcquisitionMethod.PURCHASE)
      .withPhysical(new Physical().withMaterialType("2d1398ae-e1aa-4c7c-b9c9-15adf8cf6425"))
      .withCost(new Cost().withCurrency("EUR").withQuantityPhysical(1).withListUnitPrice(10.0))
      .withLocations(Collections.singletonList(new Location().withLocationId("2a00b0be-1447-42a1-a112-124450991899").withQuantityPhysical(1)))
      .withTitle("Title")
      .withPurchaseOrderId(orderId);
  }

  public static CompositePurchaseOrder getMinimalContentCompositePurchaseOrder() {
    return new CompositePurchaseOrder()
      .withId(MIN_PO_ID)
      .withPoNumber("TestNumber")
      .withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME)
      .withVendor("7d232b43-bf9a-4301-a0ce-9e076298632e");
  }

  public static String encodePrettily(Object entity) {
    return JsonObject.mapFrom(entity).encodePrettily();
  }

  public static List<CheckInPiece> getCheckInPieces(CheckInPiece... checkInPieces) {
    return Arrays.asList(checkInPieces);
  }

  public static List<ReceivedItem> getReceivedItems(ReceivedItem... receivedItems) {
    return Arrays.asList(receivedItems);
  }

  public static CheckInPiece getCheckInPiece(String id) {
    return new CheckInPiece().withItemStatus(STATUS_RECEIVED).withLocationId(LOCATION_ID).withId(id);
  }

  public static ReceivedItem getReceivedItem(String pieceId) {
    return new ReceivedItem().withItemStatus(STATUS_RECEIVED).withLocationId(LOCATION_ID).withPieceId(pieceId);
  }

  public static ToBeCheckedIn getToBeCheckedIn(String poLineId, String pieceId) {
    return new ToBeCheckedIn()
      .withPoLineId(poLineId)
      .withCheckInPieces(getCheckInPieces(getCheckInPiece(pieceId)));
  }

  public static ToBeReceived getToBeReceived(String poLineId, String pieceId) {
    return new ToBeReceived()
      .withPoLineId(poLineId)
      .withReceivedItems(getReceivedItems(getReceivedItem(pieceId)));
  }

  public static String getRandomId() {
    return UUID.randomUUID().toString();
  }
}
