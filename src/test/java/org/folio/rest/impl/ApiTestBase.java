package org.folio.rest.impl;

import io.restassured.RestAssured;
import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.apache.commons.io.IOUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Location;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.getPolSearches;
import static org.folio.rest.impl.PurchaseOrdersApiApiTest.APPLICATION_JSON;
import static org.folio.rest.impl.ApiTestSuite.mockPort;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(ApiTestBase.class);

  static final String PO_LINE_NUMBER_VALUE = "1";
  static final String INVALID_LANG = "?lang=english";
  static final String ID_BAD_FORMAT = "123-45-678-90-abc";
  static final String ID_DOES_NOT_EXIST = "d25498e7-3ae6-45fe-9612-ec99e2700d2f";
  static final String ID_FOR_INTERNAL_SERVER_ERROR = "168f8a86-d26c-406e-813f-c7527f241ac3";
  static final String PO_ID = "e5ae4afd-3fa9-494e-a972-f541df9b877e";
  static final String PO_ID_OPEN_STATUS = "c1465131-ed35-4308-872c-d7cdf0afc5f7";
  static final String PO_ID_CLOSED_STATUS = "07f65192-44a4-483d-97aa-b137bbd96390";
  static final String COMP_ORDER_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/";

  static final Header X_OKAPI_URL = new Header("X-Okapi-Url", "http://localhost:" + mockPort);
  static final Header NON_EXIST_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "ordersimpltest");
  static final Header X_OKAPI_USER_ID = new Header(OKAPI_USERID_HEADER, "440c89e3-7f6c-578a-9ea8-310dad23605e");
  static final Header X_OKAPI_TOKEN = new Header(OKAPI_HEADER_TOKEN, "eyJhbGciOiJIUzI1NiJ9");
  static final String EMPTY_CONFIG_TENANT = "config_empty";
  static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10 = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_10");
  static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1 = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_1");
  static final Header INVALID_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "invalid_config");
  static final Header EMPTY_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, EMPTY_CONFIG_TENANT);
  static final String LINE_BY_ID_PATH = "/orders/order-lines/%s";
  static final String COMP_PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  static final String PO_LINE_ID_FOR_SUCCESS_CASE = "fca5fa9e-15cb-4a3d-ab09-eeea99b97a47";

  private static boolean runningOnOwn;

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
    MockServer.serverRqRs.clear();
  }

  @AfterClass
  public static void after() {

    if(runningOnOwn) {
      System.out.println("Running test on own, un-initialising suite manually");
      ApiTestSuite.after();
    }
  }

  public static String getMockData(String path) throws IOException {
    logger.info("Using mock datafile: {}", path);
    try (InputStream resourceAsStream = PurchaseOrdersApiApiTest.class.getClassLoader().getResourceAsStream(path)) {
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

  JsonObject getMockAsJson(String path, String id) {
    return getMockAsJson(String.format("%s%s.json", path, id));
  }

  JsonObject getMockAsJson(String fullPath) {
    try {
      return new JsonObject(getMockData(fullPath));
    } catch (IOException e) {
      fail(e.getMessage());
    }
    return new JsonObject();
  }

  Response verifyPostResponse(String url, String body, Headers headers, String
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

  Response verifyPut(String url, JsonObject body, String expectedContentType, int expectedCode) {
    return verifyPut(url, body.encodePrettily(), expectedContentType, expectedCode);
  }

  Response verifyPut(String url, String body, String expectedContentType, int expectedCode) {
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


  Response verifyDeleteResponse(String url, String expectedContentType, int expectedCode) {
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
    assertEquals(1, getPolSearches().size());
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

}
