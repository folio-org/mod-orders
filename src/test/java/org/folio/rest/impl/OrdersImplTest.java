package org.folio.rest.impl;

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
import org.folio.rest.RestVerticle;
import org.folio.rest.acq.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Adjustment;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.folio.orders.utils.HelperUtils.getMockData;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

@RunWith(VertxUnitRunner.class)
public class OrdersImplTest {

  private static final Logger logger = LoggerFactory.getLogger(OrdersImplTest.class);

  private static final String APPLICATION_JSON = "application/json";
  private static final String TEXT_PLAIN = "text/plain";
  private static final String BASE_MOCK_DATA_PATH = "mockdata/";

  private static final int okapiPort = NetworkUtils.nextFreePort();
  private static final int mockPort = NetworkUtils.nextFreePort();

  private static final Header X_OKAPI_URL = new Header("X-Okapi-Url", "http://localhost:" + mockPort);
  private static final Header X_OKAPI_TENANT = new Header("X-Okapi-Tenant", "ordersimpltest");
  private static final Header X_OKAPI_TOKEN = new Header("X-Okapi-Token", "authorized_user");
  private static final Header TMP_ORDER_HEADER = new Header("X-Okapi_Tmp", "tmp_order");
  private static JsonObject tmpOrder;



  private static final String X_ECHO_STATUS = "X-Okapi-Echo-Status";
  private static final String MOCK_DATA_PATH = "mockdata/getOrders.json";

  private static final String INVALID_LANG = "?lang=english";

  private static final String INVALID_OFFSET = "?offset=-1";
  private static final String INVALID_LIMIT = "?limit=-1";



  // API paths
  private final String rootPath = "/orders";

  // Mock data paths
  private final String mockDataRootPath = "src/test/resources/";
  private final String listedPrintMonographPath = mockDataRootPath + "/po_listed_print_monograph.json";
  private final String minimalOrderPath = mockDataRootPath + "/minimal_order.json";
  private final String existedOrder = mockDataRootPath + "/existed_order.json";
  private final String poCreationFailurePath = mockDataRootPath + "/po_creation_failure.json";
  private final String poLineCreationFailurePath = mockDataRootPath + "/po_line_creation_failure.json";

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

  @Test
  public void testListedPrintMonograph(TestContext ctx) throws Exception {
    logger.info("=== Test Listed Print Monograph ===");

    String body = getMockData(listedPrintMonographPath);
    JsonObject reqData = new JsonObject(body);

    final CompositePurchaseOrder resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .header(X_OKAPI_TOKEN)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(rootPath)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(201)
          .extract()
            .response()
              .as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
    assertEquals(reqData.getJsonArray("po_lines").size(), resp.getPoLines().size());

    for (int i = 0; i < resp.getPoLines().size(); i++) {
      PoLine line = resp.getPoLines().get(i);
      String polNumber = line.getPoLineNumber();
      String polId = line.getId();

      assertEquals(poId, line.getPurchaseOrderId());
      assertNotNull(polId);
      assertNotNull(polNumber);
      assertTrue(polNumber.startsWith(poNumber));
      assertNotNull(line.getCost().getId());
      assertNotNull(line.getDetails().getId());
      assertNotNull(line.getLocation().getId());
    }
  }

  @Test
  public void testPlaceOrderMinimal(TestContext ctx) throws Exception {
    logger.info("=== Test Placement of minimal order ===");

    String body = getMockData(minimalOrderPath);
    JsonObject reqData = new JsonObject(body);

    final CompositePurchaseOrder resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .header(X_OKAPI_TOKEN)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(rootPath)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(201)
          .extract()
            .response()
              .as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
    assertEquals(reqData.getJsonArray("po_lines").size(), resp.getPoLines().size());

    for (int i = 0; i < resp.getPoLines().size(); i++) {
      PoLine line = resp.getPoLines().get(i);
      String polNumber = line.getPoLineNumber();
      String polId = line.getId();

      assertEquals(poId, line.getPurchaseOrderId());
      assertNotNull(polId);
      assertNotNull(polNumber);
      assertTrue(polNumber.startsWith(poNumber));
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
  public void testPoLineCreationFailure(TestContext ctx) throws Exception {
    logger.info("=== Test POLine creation failure ===");

    String body = getMockData(poLineCreationFailurePath);

    final Errors errors = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(rootPath)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(422)
          .extract()
            .response()
              .as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());

    ctx.assertFalse(errors.getErrors().isEmpty());
    ctx.assertNotNull(errors.getErrors().get(0));
    ctx.assertEquals("must match \"^[a-zA-Z0-9]{5,16}-[0-9]{1,3}$\"", errors.getErrors().get(0).getMessage());
    ctx.assertFalse(errors.getErrors().get(0).getParameters().isEmpty());
    ctx.assertNotNull(errors.getErrors().get(0).getParameters().get(0));
    ctx.assertEquals("poLines[1].poLineNumber", errors.getErrors().get(0).getParameters().get(0).getKey());
    ctx.assertEquals("invalid", errors.getErrors().get(0).getParameters().get(0).getValue());
  }

  @Test
  public void testDetailsCreationFailure(TestContext ctx) throws Exception {
    logger.info("=== Test Details creation failure ===");

    String body = getMockData(listedPrintMonographPath);

    final Response resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .header(X_OKAPI_TOKEN)
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
  public void testGetOrderById(TestContext ctx) throws Exception {
    logger.info("=== Test Get Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString("id");
    logger.info(String.format("using mock datafile: %s%s.json", BASE_MOCK_DATA_PATH, id));

    final CompositePurchaseOrder resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
      .get(rootPath + "/" + id)
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

  @Test
  public void testDeleteById() throws Exception {
    logger.info("=== Test Delete Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString("id");
    logger.info(String.format("using mock datafile: %s%s.json", BASE_MOCK_DATA_PATH, id));

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
      .delete(rootPath + "/" + id)
        .then()
          .statusCode(204);

  }

  @Test
  public void putOrdersById(TestContext ctx) throws Exception {
    logger.info("=== Test Put Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("composite_purchase_orders").getJsonObject(0).getString("id");
    logger.info(String.format("using mock datafile: %s%s.json", BASE_MOCK_DATA_PATH, id));
    String body = getMockData(listedPrintMonographPath);
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body(body)
      .put(rootPath + "/" + id)
        .then()
          .statusCode(204);
  }

  @Test
  public void testPutOrdersByIdDoesNotAffectGeneratedData(TestContext ctx) throws Exception {
    logger.info("=== Test Put Order By Id doesn't affect generated data ===");

    tmpOrder = new JsonObject(getMockData(existedOrder));
    PurchaseOrder initialOrder = tmpOrder.mapTo(PurchaseOrder.class);
    CompositePurchaseOrder puttedOrder =  tmpOrder.mapTo(CompositePurchaseOrder.class);
    puttedOrder.setApproved(false);
    puttedOrder.setCreated(new Date());
    puttedOrder.setCreatedBy("440c89e3-7f6c-578a-9ea8-310dad23605e");
    String body = JsonObject.mapFrom(puttedOrder).toString();

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .header(TMP_ORDER_HEADER)
        .contentType(APPLICATION_JSON)
      .body(body)
        .put(rootPath + "/1ab7ef6a-d1d4-4a4f-90a2-882aed18af14")
          .then()
            .statusCode(204);

    PurchaseOrder changedOrder =  tmpOrder.mapTo(PurchaseOrder.class);

    assertThat(initialOrder.getCreated(), equalTo(changedOrder.getCreated()));
    assertThat(initialOrder.getCreatedBy(), equalTo(changedOrder.getCreatedBy()));
    assertThat(initialOrder.getApproved(), not(equalTo(changedOrder.getApproved())));
    assertThat(puttedOrder.getApproved(), equalTo(changedOrder.getApproved()));
    assertThat(puttedOrder.getCreatedBy(), not(equalTo(changedOrder.getCreatedBy())));

  }

  @Test
  public void testIgnoringGeneratedDataInResponseOnPost() throws IOException {
    logger.info("=== Test ignoring \"Created on\" from request on POST API ===");

    String body = getMockData(minimalOrderPath);
    JsonObject reqData = new JsonObject(body);
    CompositePurchaseOrder compPo = reqData.mapTo(CompositePurchaseOrder.class);
    Date dateFromRequest = compPo.getCreated();
    String createdByFromRequest = compPo.getCreatedBy();
    String poNumberFromRequest = compPo.getPoNumber();
    final CompositePurchaseOrder resp = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .header(X_OKAPI_TOKEN)
        .contentType(APPLICATION_JSON)
        .body(body)
      .post(rootPath)
        .then()
          .contentType(APPLICATION_JSON)
          .statusCode(201)
          .extract()
            .response()
              .as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp));
    Date dateFromResponse = resp.getCreated();
    String createdByFromResponse = resp.getCreatedBy();
    String poNumberFromResponse = resp.getPoNumber();
    assertNotNull(dateFromResponse);
    assertNotNull(createdByFromResponse);
    assertNotNull(poNumberFromResponse);
    assertThat(poNumberFromRequest, not(equalTo(poNumberFromResponse)));
    assertThat(dateFromResponse, not(equalTo(dateFromRequest)));
    assertThat(createdByFromResponse, not(equalTo(createdByFromRequest)));
  }

  @Test
  public void testValidationOnPost() throws Exception {
    logger.info("=== Test validation Annotation on POST API ===");

    logger.info("=== Test validation with no body ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
      .post(rootPath)
        .then()
          .statusCode(400)
          .body(containsString("Json content error HV000116: The object to be validated must not be null"));

    logger.info("=== Test validation on invalid lang query parameter ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
        .body("{}")
      .post(rootPath+INVALID_LANG)
        .then()
          .statusCode(400)
          .body(containsString("'lang' parameter is incorrect. parameter value {english} is not valid: must match \"[a-zA-Z]{2}\""));

  }

  @Test
  public void testValidationOnGetById() throws Exception {
    logger.info("=== Test validation Annotation on GET ORDER BY ID API ===");
    String id = "non-existent-po-id";

    logger.info("=== Test validation on invalid lang query parameter ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
      .get(rootPath+"/"+id+INVALID_LANG)
        .then()
          .statusCode(400)
          .body(containsString("'lang' parameter is incorrect. parameter value {english} is not valid: must match \"[a-zA-Z]{2}\""));

  }

  @Test
  public void testValidationDelete() throws Exception {
    logger.info("=== Test validation Annotation on DELETE API ===");
    String id = "non-existent-po-id";

    logger.info("=== Test validation on invalid lang query parameter ===");
    RestAssured
     .with()
       .header(X_OKAPI_URL)
       .header(X_OKAPI_TENANT)
       .contentType(APPLICATION_JSON)
     .delete(rootPath+"/"+id+INVALID_LANG)
       .then()
         .statusCode(400)
         .body(containsString("'lang' parameter is incorrect. parameter value {english} is not valid: must match \"[a-zA-Z]{2}\""));

  }

  @Test
  public void testValidationOnPut() throws Exception {
    logger.info("=== Test validation Annotation on PUT API ===");
    String id = "non-existent-po-id";
    logger.info("=== Test validation with no body ===");
    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(X_OKAPI_TENANT)
        .contentType(APPLICATION_JSON)
      .put(rootPath+"/"+id)
        .then()
          .statusCode(400)
          .body(containsString("Json content error HV000116: The object to be validated must not be null"));

     logger.info("=== Test validation on invalid lang query parameter ===");
     RestAssured
       .with()
         .header(X_OKAPI_URL)
         .header(X_OKAPI_TENANT)
         .contentType(APPLICATION_JSON)
         .body("{}")
       .put(rootPath+"/"+id+INVALID_LANG)
         .then()
           .statusCode(400)
           .body(containsString("'lang' parameter is incorrect. parameter value {english} is not valid: must match \"[a-zA-Z]{2}\""));

     logger.info("=== Test validation on no Content-type parameter ===");
     RestAssured
       .with()
         .header(X_OKAPI_URL)
         .header(X_OKAPI_TENANT)
         .body("{}")
       .put(rootPath+"/"+id+INVALID_LANG)
         .then()
           .statusCode(400)
           .body(containsString("Content-type"));

  }

  public static class MockServer {

    private static final Logger logger = LoggerFactory.getLogger(MockServer.class);


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
      router.route(HttpMethod.POST, "/adjustment").handler(ctx -> handlePostGenericSubObj(ctx, org.folio.rest.acq.model.Adjustment.class));
      router.route(HttpMethod.POST, "/cost").handler(ctx -> handlePostGenericSubObj(ctx, org.folio.rest.acq.model.Cost.class));
      router.route(HttpMethod.POST, "/details").handler(ctx -> handlePostGenericSubObj(ctx, org.folio.rest.acq.model.Details.class));
      router.route(HttpMethod.POST, "/eresource").handler(ctx -> handlePostGenericSubObj(ctx, org.folio.rest.acq.model.Eresource.class));
      router.route(HttpMethod.POST, "/location").handler(ctx -> handlePostGenericSubObj(ctx, org.folio.rest.acq.model.Location.class));
      router.route(HttpMethod.POST, "/physical").handler(ctx -> handlePostGenericSubObj(ctx, org.folio.rest.acq.model.Physical.class));
      router.route(HttpMethod.POST, "/vendor_detail").handler(ctx -> handlePostGenericSubObj(ctx, org.folio.rest.acq.model.VendorDetail.class));

      router.route(HttpMethod.GET, "/purchase_order/:id").handler(this::handleGetPurchaseOrderById);
      router.route(HttpMethod.GET, "/po_line").handler(this::handleGetPoLine);

      router.route(HttpMethod.GET, "/adjustment/:id").handler(this::handleGetAdjustment);
      router.route(HttpMethod.GET, "/alerts/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/cost/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/claims/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/details/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/eresource/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/fund_distribution/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/location/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/physical/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/renewal/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/source/:id").handler(this::handleGetGenericSubObj);
      router.route(HttpMethod.GET, "/vendor_detail/:id").handler(this::handleGetGenericSubObj);


      router.route(HttpMethod.DELETE, "/purchase_order/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/po_line/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/adjustment/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/cost/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/details/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/eresource/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/location/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/physical/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/renewal/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/source/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/vendor_detail/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/alerts/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/claims/:id").handler(this::handleDeleteGenericSubObj);
      router.route(HttpMethod.DELETE, "/fund_distribution/:id").handler(this::handleDeleteGenericSubObj);

      router.route(HttpMethod.GET, "/bl-users/_self").handler(this::handleGetUserId);

      return router;
    }

    private void handleDeleteGenericSubObj(RoutingContext ctx) {

      if (shouldWorkWithTmpOrder(ctx)) {
        tmpOrder = null;
      }

      ctx.response()
        .setStatusCode(204)
        .putHeader(HttpHeaders.CONTENT_TYPE, TEXT_PLAIN)
        .end();

    }

    private void handleGetUserId(RoutingContext ctx) {
      String token = ctx.request().getHeader(X_OKAPI_TOKEN.getName());
      try {
        JsonObject userInfo = new JsonObject(getMockData(String.format("%s.json", token)));
        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(userInfo.encodePrettily());
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
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

    private void handleGetPoLine(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam("query").split("purchase_order_id==")[1];
      try {
        JsonObject compPO = new JsonObject(getMockData(String.format("%s%s.json", BASE_MOCK_DATA_PATH, id)));
        JsonArray lines = compPO.getJsonArray("po_lines");

        lines.forEach(l -> {
          JsonObject line = (JsonObject) l;
          line.put("adjustment", ((Map<?, ?>) line.remove("adjustment")).get("id"));
          line.put("cost", ((Map<?, ?>) line.remove("cost")).get("id"));
          line.put("details", ((Map<?, ?>) line.remove("details")).get("id"));
          line.put("eresource", ((Map<?, ?>) line.remove("eresource")).get("id"));
          line.put("location", ((Map<?, ?>) line.remove("location")).get("id"));
          line.put("physical", ((Map<?, ?>) line.remove("physical")).get("id"));
          if (line.containsKey("renewal")) {
            line.put("renewal", ((Map<?, ?>) line.remove("renewal")).get("id"));
          }
          line.put("source", ((Map<?, ?>) line.remove("source")).get("id"));
          line.put("vendor_detail", ((Map<?, ?>) line.remove("vendor_detail")).get("id"));

          List<?> alerts = ((List<?>) line.remove("alerts"));
          line.put("alerts", new JsonArray());
          alerts.forEach(a ->
            line.getJsonArray("alerts").add(((Map<?, ?>) a).get("id"))
          );

          List<?> claims = ((List<?>) line.remove("claims"));
          line.put("claims", new JsonArray());
          claims.forEach(c ->
            line.getJsonArray("claims").add(((Map<?, ?>) c).get("id"))
          );

          List<?> fund_distribution = ((List<?>) line.remove("fund_distribution"));
          line.put("fund_distribution", new JsonArray());
          fund_distribution.forEach(f ->
            line.getJsonArray("fund_distribution").add(((Map<?, ?>) f).get("id"))
          );
        });

        JsonObject po_lines = new JsonObject()
          .put("po_lines", lines)
          .put("total_records", lines.size())
          .put("first", 0)
          .put("last", lines.size());

        logger.info(po_lines.encodePrettily());

        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(po_lines.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(404)
          .end(id);
      }
    }

    private void handleGetGenericSubObj(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam("id");
      logger.info("id: " + id);

      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(new JsonObject().put("id", id).encodePrettily());
    }

    private void handleGetAdjustment(RoutingContext ctx) {
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam("id");
      logger.info("id: " + id);

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
      logger.info("got: " + ctx.request().path());
      String id = ctx.request().getParam("id");
      logger.info("id: " + id);

      try {
        JsonObject po;
        if (shouldWorkWithTmpOrder(ctx)) {
          po = tmpOrder;
        } else {
          po = new JsonObject(getMockData(String.format("%s%s.json", BASE_MOCK_DATA_PATH, id)));
        }
        po.remove("adjustment");
        po.remove("po_lines");
        po.put("adjustment", UUID.randomUUID().toString());

        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(po.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(404)
          .end(id);
      }
    }

    private void handlePostPurchaseOrder(RoutingContext ctx) {
      logger.info("got: " + ctx.getBodyAsString());

      org.folio.rest.acq.model.PurchaseOrder po = ctx.getBodyAsJson().mapTo(org.folio.rest.acq.model.PurchaseOrder.class);
      po.setId(UUID.randomUUID().toString());
      if (shouldWorkWithTmpOrder(ctx)) {
        tmpOrder = ctx.getBodyAsJson();
      }

      ctx.response()
        .setStatusCode(201)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(po).encodePrettily());
    }

    private void handlePostGenericSubObj(RoutingContext ctx, Class<?> clazz) {
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
          JsonObject body = JsonObject.mapFrom(ctx.getBodyAsJson().mapTo(clazz)).put("id", UUID.randomUUID().toString());
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
      logger.info("got po_line: " + ctx.getBodyAsString());

      org.folio.rest.acq.model.PoLine pol = ctx.getBodyAsJson().mapTo(org.folio.rest.acq.model.PoLine.class);
      pol.setId(UUID.randomUUID().toString());

      ctx.response()
        .setStatusCode(201)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(pol).encodePrettily());
    }

    private boolean shouldWorkWithTmpOrder(RoutingContext ctx) {
      return TMP_ORDER_HEADER.getValue().equals(ctx.request().getHeader(TMP_ORDER_HEADER.getName()));
    }

  }

}
