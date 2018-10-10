package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.getMockData;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.UUID;

import org.apache.log4j.Logger;
import org.folio.rest.RestVerticle;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

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
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;

@RunWith(VertxUnitRunner.class)
public class OrdersResourceImplTest {

  private static final Logger logger = Logger.getLogger(OrdersResourceImplTest.class);

  private static final String APPLICATION_JSON = "application/json";
  private static final String TEXT_PLAIN = "text/plain";

  private static final int okapiPort = NetworkUtils.nextFreePort();
  private static final int mockPort = NetworkUtils.nextFreePort();

  private final Header X_OKAPI_URL = new Header("X-Okapi-Url", "http://localhost:" + mockPort);
  private final Header X_OKAPI_TENANT = new Header("X-Okapi-Tenant", "ordersresourceimpltest");

  public final static String X_ECHO_STATUS = "X-Okapi-Echo-Status";

  // API paths
  private final String rootPath = "/orders";

  // Mock data paths
  private final String mockDataRootPath = "src/test/resources/";
  private final String listedPrintMonographPath = mockDataRootPath + "/po_listed_print_monograph.json";
  private final String poCreationFailurePath = mockDataRootPath + "/po_creation_failure.json";
  private final String poLineCreationFailurePath = mockDataRootPath + "/po_line_creation_failure.json";

  private static Vertx vertx;
  private static MockServer mockServer;

  @BeforeClass
  public static void setUpOnce(TestContext context) throws Exception {
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

  @Test
  public void testListedPrintMonograph(TestContext ctx) throws Exception {
    logger.info("=== Test Listed Print Monograph ===");

    String body = getMockData(listedPrintMonographPath);
    JsonObject reqData = new JsonObject(body);

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(rootPath)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(201)
          .extract()
            .response();

    String respBody = resp.getBody().asString();

    JsonObject json = new JsonObject(respBody);
    logger.info(json.encodePrettily());

    JsonObject po = json.getJsonObject("purchase_order");
    String poId = po.getString("id");
    String poNumber = po.getString("po_number");

    assertNotNull(poId);
    assertNotNull(poNumber);

    assertEquals(reqData.getJsonArray("po_lines").size(), json.getJsonArray("po_lines").size());

    for (int i = 0; i < json.getJsonArray("po_lines").size(); i++) {
      JsonObject line = json.getJsonArray("po_lines").getJsonObject(i);
      String polNumber = line.getString("po_line_number");

      assertEquals(poId, line.getString("purchase_order_id"));
      assertNotNull(line.getString("id"));
      assertNotNull(polNumber);
      assertTrue(polNumber.startsWith(poNumber));
      assertNotNull(line.getJsonObject("cost").getString("id"));
      assertNotNull(line.getJsonObject("details").getString("id"));
      assertNotNull(line.getJsonObject("location").getString("id"));
      assertNotNull(line.getJsonObject("vendor").getString("id"));
    }
  }

  @Test
  public void testPoCreationFailure(TestContext ctx) throws Exception {
    logger.info("=== Test PO creation failure ===");

    String body = getMockData(poCreationFailurePath);

    final Errors errors = RestAssured
      .given()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(rootPath)
        .then()
          .log()
            .all()
          .contentType(APPLICATION_JSON)
          .statusCode(422)
          .extract()
            .response()
              .body()
                .as(Errors.class);

    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertNotNull(errors.getErrors().get(0));
    ctx.assertEquals("must match \"^[a-zA-Z0-9]{5,16}$\"", errors.getErrors().get(0).getMessage());
    ctx.assertFalse(errors.getErrors().get(0).getParameters().isEmpty());
    ctx.assertNotNull(errors.getErrors().get(0).getParameters().get(0));
    ctx.assertEquals("purchaseOrder.poNumber", errors.getErrors().get(0).getParameters().get(0).getKey());
    ctx.assertEquals("123", errors.getErrors().get(0).getParameters().get(0).getValue());
  }

  @Test
  public void testPoLineCreationFailure(TestContext ctx) throws Exception {
    logger.info("=== Test POLine creation failure ===");

    String body = getMockData(poLineCreationFailurePath);

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(rootPath)
        .then()
          .contentType(TEXT_PLAIN)
          .statusCode(400)
          .extract()
            .response();

    String respBody = resp.getBody().asString();
    logger.info(respBody);

    assertEquals("Invalid barcode", respBody);
  }

  @Test
  public void testDetailsCreationFailure(TestContext ctx) throws Exception {
    logger.info("=== Test Details creation failure ===");

    String body = getMockData(listedPrintMonographPath);

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .header(X_ECHO_STATUS, 403)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(rootPath)
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
  public void testGetOrders(TestContext ctx) throws Exception {
    logger.info("=== Test Get Orders ===");

    String expected = new JsonObject(getMockData(GetOrdersHelper.MOCK_DATA_PATH)).encodePrettily();

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .queryParam("query", "approval_status%3D%22Pending%22")
        .queryParam("limit", "30")
      .get(rootPath)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(200)
          .extract()
            .response();

    String actual = new JsonObject(resp.getBody().asString()).encodePrettily();
    logger.info(actual);

    assertEquals(expected, actual);
  }

  @Test
  public void testGetOrderById(TestContext ctx) throws Exception {
    logger.info("=== Test Get Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(GetOrdersHelper.MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getJsonObject("purchase_order").getString("id");
    logger.info("using mock datafile: " + GetOrderHelper.BASE_MOCK_DATA_PATH + id + ".json");
    JsonObject expected = new JsonObject(getMockData(GetOrderHelper.BASE_MOCK_DATA_PATH + id + ".json"));

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
      .get(rootPath + "/" + id)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(200)
          .extract()
            .response();

    JsonObject actual = new JsonObject(resp.getBody().asString());
    logger.info(actual);

    JsonObject expPo = expected.getJsonObject("purchase_order");
    JsonObject actPo = actual.getJsonObject("purchase_order");

    actPo.fieldNames().forEach(field -> {
      Object actValue = actPo.getValue(field);
      if (!(actValue instanceof JsonObject || actValue instanceof JsonArray)) {
        logger.info("checking purchase_order field: " + field);
        assertEquals(expPo.getValue(field), actValue);
      }
    });

    JsonArray expPoLines = expected.getJsonArray("po_lines");
    JsonArray actPoLines = actual.getJsonArray("po_lines");

    assertEquals(expPoLines.size(), actPoLines.size());
    for (int i = 0; i < expPoLines.size(); i++) {
      JsonObject expPoLine = expPoLines.getJsonObject(i);
      JsonObject actPoLine = actPoLines.getJsonObject(i);

      assertEquals(expPoLine.getString("id"), actPoLine.getString("id"));
      actPoLine.fieldNames().forEach(field -> {
        Object actValue = actPoLine.getValue(field);
        if (!(actValue instanceof JsonObject || actValue instanceof JsonArray)) {
          logger.info("checking po_line field: " + field);
          assertEquals(expPoLine.getValue(field), actValue);
        }
      });
    }
  }

  @Test
  public void testGetOrderByIdNotFound(TestContext ctx) throws Exception {
    logger.info("=== Test Get Order By Id - Not Found ===");

    String id = "non-existent-po-id";
    String expected = id;

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
      .get(rootPath + "/" + id)
        .then()
          .contentType(TEXT_PLAIN)
          .statusCode(404)
          .extract()
            .response();

    String actual = resp.getBody().asString();
    logger.info(actual);

    assertEquals(expected, actual);
  }

  public static class MockServer {

    private static final Logger logger = Logger.getLogger(MockServer.class);

    public final int port;
    public final Vertx vertx;

    public MockServer(int port) {
      this.port = port;
      this.vertx = Vertx.vertx();
    }

    public void close() {
      vertx.close(res -> {
        if (res.failed()) {
          logger.error("Failed to shut down mock server", res.cause());
          fail(res.cause().getMessage());
        } else {
          logger.info("Successfully shut down mock server");
        }
      });
    }

    protected Router defineRoutes() {
      Router router = Router.router(vertx);

      router.route().handler(BodyHandler.create());
      router.route(HttpMethod.POST, "/purchase_order").handler(this::handlePostPurchaseOrder);
      router.route(HttpMethod.POST, "/po_line").handler(this::handlePostPOLine);
      router.route(HttpMethod.POST, "/details").handler(this::handlePostAssignId);
      router.route(HttpMethod.POST, "/cost").handler(this::handlePostAssignId);
      router.route(HttpMethod.POST, "/vendor_detail").handler(this::handlePostAssignId);
      router.route(HttpMethod.POST, "/eresource").handler(this::handlePostAssignId);
      router.route(HttpMethod.POST, "/location").handler(this::handlePostAssignId);

      return router;
    }

    public void start(TestContext context) {

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

    private void handlePostPurchaseOrder(RoutingContext ctx) {
      logger.info("got: " + ctx.getBodyAsString());

      JsonObject body = ctx.getBodyAsJson();
      body.put("id", UUID.randomUUID().toString());

      if ("invalid".equals(body.getString("po_number"))) {
        ctx.response()
          .setStatusCode(400)
          .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
          .end("Invalid po_number");
      } else {
        ctx.response()
          .setStatusCode(201)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(body.encodePrettily());
      }
    }

    private void handlePostAssignId(RoutingContext ctx) {
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

      switch (status) {
      case 201:
        contentType = APPLICATION_JSON;
        JsonObject body = ctx.getBodyAsJson();
        body.put("id", UUID.randomUUID().toString());
        respBody = body.encodePrettily();
        break;
      case 403:
        respBody = "Access requires permission: foo.bar.baz";
        break;
      case 500:
        respBody = "Internal Server Error";
        break;
      }

      ctx.response()
        .setStatusCode(status)
        .putHeader(HttpHeaders.CONTENT_TYPE, contentType)
        .end(respBody);
    }

    private void handlePostPOLine(RoutingContext ctx) {
      logger.info("got: " + ctx.getBodyAsString());

      JsonObject body = ctx.getBodyAsJson();
      body.put("id", UUID.randomUUID().toString());

      if ("invalid".equals(body.getString("barcode"))) {
        ctx.response()
          .setStatusCode(400)
          .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
          .end("Invalid barcode");
      } else {
        ctx.response()
          .setStatusCode(201)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(body.encodePrettily());
      }
    }
  }

}
