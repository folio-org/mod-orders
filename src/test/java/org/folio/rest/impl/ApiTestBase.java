package org.folio.rest.impl;

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
import org.apache.commons.io.IOUtils;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Location;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.awaitility.Awaitility.await;
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
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.iterableWithSize;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class ApiTestBase {

  static {
    System.setProperty(LoggerFactory.LOGGER_DELEGATE_FACTORY_CLASS_NAME, "io.vertx.core.logging.Log4j2LogDelegateFactory");
  }

  private static final Logger logger = LoggerFactory.getLogger(ApiTestBase.class);

  static final String ORDERS_RECEIVING_ENDPOINT = "/orders/receive";
  static final String ORDERS_CHECKIN_ENDPOINT = "/orders/check-in";

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

  static final String COMP_ORDER_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/";

  static final String X_ECHO_STATUS = "X-Okapi-Echo-Status";
  static final String EMPTY_CONFIG_TENANT = "config_empty";
  static final String NON_EXIST_CONTRIBUTOR_NAME_TYPE_TENANT = "nonExistContributorNameType";
  static final String INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT = "hasCodeLikeInstanceStatus";
  static final String NON_EXIST_INSTANCE_STATUS_TENANT = "nonExistInstanceStatus";
  static final String NON_EXIST_INSTANCE_TYPE_TENANT = "nonExistInstanceType";
  static final String NON_EXIST_LOAN_TYPE_TENANT = "nonExistLoanType";

  protected static final Header X_OKAPI_URL = new Header("X-Okapi-Url", "http://localhost:" + mockPort);

  static final Header INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT);
  static final Header NON_EXIST_INSTANCE_STATUS_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_INSTANCE_STATUS_TENANT);
  static final Header NON_EXIST_INSTANCE_TYPE_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_INSTANCE_TYPE_TENANT);
  static final Header NON_EXIST_LOAN_TYPE_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_LOAN_TYPE_TENANT);
  static final Header NON_EXIST_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "ordersimpltest");
  static final Header X_OKAPI_USER_ID = new Header(OKAPI_USERID_HEADER, "440c89e3-7f6c-578a-9ea8-310dad23605e");
  static final Header X_OKAPI_USER_ID_WITH_ACQ_UNITS = new Header(OKAPI_USERID_HEADER, USER_ID_ASSIGNED_TO_ACQ_UNITS);
  protected static final Header X_OKAPI_TOKEN = new Header(OKAPI_HEADER_TOKEN, "eyJhbGciOiJIUzI1NiJ9");
  protected static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10 = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_10");
  static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1 = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_1");
  static final Header INVALID_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "invalid_config");
  static final Header EMPTY_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, EMPTY_CONFIG_TENANT);

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
    MockServer.serverRqRs.clear();
    MockServer.serverRqQueries.clear();
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

  static JsonObject getMockAsJson(String fullPath) {
    try {
      return new JsonObject(getMockData(fullPath));
    } catch (IOException e) {
      fail(e.getMessage());
    }
    return new JsonObject();
  }

  Response verifyPostResponse(String url, String body, Headers headers, String
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
    return verifyPut(url, body, prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_TOKEN), expectedContentType, expectedCode);
  }

  Response verifyPut(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
    Response response = RestAssured
      .with()
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

  Response verifyGet(String url, String expectedContentType, int expectedCode) {
    Headers headers = prepareHeaders(X_OKAPI_URL, NON_EXIST_CONFIG_X_OKAPI_TENANT);
    return verifyGet(url, headers, expectedContentType, expectedCode);
  }

  Response verifyGet(String url, Headers headers, String expectedContentType, int expectedCode) {
    return RestAssured
      .with()
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

  Response verifyDeleteResponse(String url, String expectedContentType, int expectedCode) {
    Headers headers =  prepareHeaders(X_OKAPI_URL, NON_EXIST_CONFIG_X_OKAPI_TENANT);
    return verifyDeleteResponse(url, headers, expectedContentType, expectedCode);
  }

  Response verifyDeleteResponse(String url, Headers headers, String expectedContentType, int expectedCode) {
    Response response = RestAssured
      .with()
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

  Headers prepareHeaders(Header... headers) {
    return new Headers(headers);
  }

  static void validatePoLineCreationErrorForNonPendingOrder(String errorCode, Errors errors) {
    assertEquals(1, errors.getErrors().size());
    assertEquals(errorCode, errors.getErrors().get(0).getCode());
    // Assert that only PO Lines limit (count of existing Lines) and GET PO requests made
    assertEquals(2, MockServer.serverRqRs.size());
    assertEquals(2, MockServer.serverRqRs.rowKeySet().size());
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
}
