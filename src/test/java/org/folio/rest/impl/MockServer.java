package org.folio.rest.impl;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;
import io.restassured.http.Header;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.http.HttpServer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;

import javax.ws.rs.core.Response;
import java.io.IOException;
import java.nio.file.NoSuchFileException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.DEFAULT_POLINE_LIMIT;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.ApiTestBase.COMP_ORDER_MOCK_DATA_PATH;
import static org.folio.rest.impl.ApiTestBase.ID;
import static org.folio.rest.impl.ApiTestBase.ID_DOES_NOT_EXIST;
import static org.folio.rest.impl.ApiTestBase.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.rest.impl.ApiTestBase.PO_LINE_NUMBER_VALUE;
import static org.folio.rest.impl.InventoryHelper.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.rest.impl.InventoryHelper.ITEMS;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ACTIVE_ACCESS_PROVIDER_A;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ACTIVE_ACCESS_PROVIDER_B;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ACTIVE_VENDOR_ID;
import static org.folio.rest.impl.ApiTestBase.BAD_QUERY;
import static org.folio.rest.impl.PurchaseOrdersApiTest.EMPTY_CONFIG_TENANT;
import static org.folio.rest.impl.PoNumberApiTest.EXISTING_PO_NUMBER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ID_FOR_PRINT_MONOGRAPH_ORDER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.INACTIVE_ACCESS_PROVIDER_A;
import static org.folio.rest.impl.PurchaseOrdersApiTest.INACTIVE_ACCESS_PROVIDER_B;
import static org.folio.rest.impl.PurchaseOrdersApiTest.INACTIVE_VENDOR_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.LISTED_PRINT_MONOGRAPH_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.MOD_VENDOR_INTERNAL_ERROR_ID;
import static org.folio.rest.impl.PoNumberApiTest.NONEXISTING_PO_NUMBER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.NON_EXIST_ACCESS_PROVIDER_A;
import static org.folio.rest.impl.PurchaseOrdersApiTest.NON_EXIST_VENDOR_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.PURCHASE_ORDER_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.VENDOR_WITH_BAD_CONTENT;
import static org.folio.rest.impl.ApiTestBase.X_ECHO_STATUS;
import static org.folio.rest.impl.ReceivingHistoryApiTest.RECEIVING_HISTORY_PURCHASE_ORDER_ID;
import static org.junit.Assert.fail;

public class MockServer {

  private static final Logger logger = LoggerFactory.getLogger(MockServer.class);

  // Mock data paths
  static final String BASE_MOCK_DATA_PATH = "mockdata/";
  private static final String CONTRIBUTOR_NAME_TYPES_PATH = BASE_MOCK_DATA_PATH + "contributorNameTypes/contributorPersonalNameType.json";
  private static final String CONFIG_MOCK_PATH = BASE_MOCK_DATA_PATH + "configurations.entries/%s.json";
  private static final String LOAN_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "loanTypes/";
  private static final String ITEMS_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "itemsRecords/";
  private static final String INSTANCE_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceTypes/";
  private static final String INSTANCE_STATUSES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceStatuses/";
  private static final String INSTANCE_IDENTIFIERS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "identifierTypes/";
  private static final String INSTANCE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instances/";
  static final String PIECE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "lines/";
  private static final String RECEIVING_HISTORY_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "receivingHistory/";
  private static final String VENDORS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "vendors/";
  private static final String POLINES_COLLECTION = PO_LINES_MOCK_DATA_PATH + "/po_line_collection.json";

  static final String INTERNAL_SERVER_ERROR = "Internal Server Error";
  private static final String PENDING_VENDOR_ID = "160501b3-52dd-41ec-a0ce-17762e7a9b47";
  private static final String ORDER_ID_WITH_PO_LINES = "ab18897b-0e40-4f31-896b-9c9adc979a87";
  static final String PO_NUMBER_VALUE = "228D126";

  private static final String PO_NUMBER_ERROR_TENANT = "po_number_error_tenant";
  static final Header PO_NUMBER_ERROR_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, PO_NUMBER_ERROR_TENANT);

  private static final String TOTAL_RECORDS = "totalRecords";
  private static final String ITEM_RECORDS = "itemRecords";
  private static final String INSTANCE_RECORD = "instanceRecord";
  private static final String HOLDINGS_RECORD = "holdingRecord";

  static Table<String, HttpMethod, List<JsonObject>> serverRqRs = HashBasedTable.create();

  private final int port;
  private final Vertx vertx;

  MockServer(int port) {
    this.port = port;
    this.vertx = Vertx.vertx();
  }

  void start() throws InterruptedException, ExecutionException, TimeoutException {
    // Setup Mock Server...
    HttpServer server = vertx.createHttpServer();
    CompletableFuture<HttpServer> deploymentComplete = new CompletableFuture<>();
    server.requestHandler(defineRoutes()::accept).listen(port, result -> {
      if(result.succeeded()) {
        deploymentComplete.complete(result.result());
      }
      else {
        deploymentComplete.completeExceptionally(result.cause());
      }
    });
    deploymentComplete.get(60, TimeUnit.SECONDS);
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

  static List<JsonObject> getPoLineUpdates() {
    return serverRqRs.get(PO_LINES, HttpMethod.PUT);
  }

  static List<JsonObject> getPoLineSearches() {
    return serverRqRs.get(PO_LINES, HttpMethod.GET);
  }

  static List<JsonObject> getPieceUpdates() {
    return serverRqRs.get(PIECES, HttpMethod.PUT);
  }

  static List<JsonObject> getHoldingsSearches() {
    return serverRqRs.get(HOLDINGS_RECORD, HttpMethod.GET);
  }

  static List<JsonObject> getCreatedHoldings() {
    return serverRqRs.get(HOLDINGS_RECORD, HttpMethod.POST);
  }

  static List<JsonObject> getInstancesSearches() {
    return serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
  }

  static List<JsonObject> getCreatedInstances() {
    return serverRqRs.get(INSTANCE_RECORD, HttpMethod.POST);
  }

  static List<JsonObject> getItemsSearches() {
    return serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
  }

  static List<JsonObject> getItemUpdates() {
    return serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
  }

  static List<JsonObject> getCreatedItems() {
    return serverRqRs.get(ITEM_RECORDS, HttpMethod.POST);
  }

  static List<JsonObject> getCreatedPieces() {
    return serverRqRs.get(PIECES, HttpMethod.POST);
  }

  static List<JsonObject> getPieceSearches() {
    return serverRqRs.get(PIECES, HttpMethod.GET);
  }

  private Router defineRoutes() {
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
    router.route(HttpMethod.GET, "/contributor-name-types").handler(this::handleGetContributorNameTypes);

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

    // the case when item creation is expected to fail for particular holding
    String id = body.getString(HOLDING_PERMANENT_LOCATION_ID).equals(ID_FOR_INTERNAL_SERVER_ERROR)
      ? ID_FOR_INTERNAL_SERVER_ERROR
      : UUID.randomUUID().toString();

    ctx.response()
      .setStatusCode(201)
      .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
      .putHeader(HttpHeaders.LOCATION, ctx.request().absoluteURI() + "/" + id)
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
        instance = new JsonObject(ApiTestBase.getMockData(INSTANCE_RECORDS_MOCK_DATA_PATH + "instance.json"));
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

    JsonObject items;
    items = new JsonObject().put("items", new JsonArray());
    addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, items);
    serverResponse(ctx, 200, APPLICATION_JSON, items.encodePrettily());
  }

  private void handleGetInventoryItemRecords(RoutingContext ctx) {
    logger.info("handleGetInventoryItemRecords got: " + ctx.request().path());

    String query = ctx.request().getParam("query");

    if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, new JsonObject());
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        JsonObject items = new JsonObject(ApiTestBase.getMockData(ITEMS_RECORDS_MOCK_DATA_PATH + "inventoryItemsCollection.json"));
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
      JsonObject po = new JsonObject(ApiTestBase.getMockData(LOAN_TYPES_MOCK_DATA_PATH + "Can circulate.json"));
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
    JsonObject body = null;

    try {
      if (getQuery(ACTIVE_ACCESS_PROVIDER_A, NON_EXIST_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "one_access_provider_not_found.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B).equals(query)
        || getQuery(ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_A).equals(query)
        || getQuery(ACTIVE_ACCESS_PROVIDER_B, ACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "all_access_providers_active.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "active_inactive_access_providers.json"));
      } else if (getQuery(INACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_B).equals(query)
        || getQuery(INACTIVE_ACCESS_PROVIDER_B, INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "all_inactive_access_providers.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "one_access_provider_not_found.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_B).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "one_access_providers_active.json"));
      } else {
        JsonArray vendors = new JsonArray();

        // Search for vendors by id
        extractIdsFromQuery(query)
          .stream()
          .map(this::getVendorById)
          .filter(Objects::nonNull)
          .forEach(vendors::add);

        if (!vendors.isEmpty()) {
          body = new JsonObject().put(VendorHelper.VENDORS, vendors);
        }
      }
    } catch(IOException e) {
      ctx.response()
        .setStatusCode(HttpStatus.HTTP_NOT_FOUND.toInt())
        .end();
    }

    if (body != null) {
      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
    } else {
      ctx.response()
        .setStatusCode(HttpStatus.HTTP_NOT_FOUND.toInt())
        .end();
    }
  }

  private void handleGetVendorById(RoutingContext ctx) {
    logger.info("handleGetVendorById got: " + ctx.request().path());
    String vendorId = ctx.request().getParam(ID);
    JsonObject body;
    if (NON_EXIST_VENDOR_ID.equals(vendorId)) {
      serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "vendor not found");
    } else if (MOD_VENDOR_INTERNAL_ERROR_ID.equals(vendorId)) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), APPLICATION_JSON, "internal server error, contact administrator");
    } else {
      body = getVendorById(vendorId);
      if (body != null) {
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
      } else {
        serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "vendor not found");
      }
    }
  }

  private JsonObject getVendorById(String vendorId) {
    logger.debug("Searching for vendor by id={}", vendorId);
    JsonObject body;
    try {
      switch (vendorId) {
        case ACTIVE_VENDOR_ID:
          body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "active_vendor.json"));
          break;
        case INACTIVE_VENDOR_ID:
          body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "inactive_vendor.json"));
          break;
        case PENDING_VENDOR_ID:
          body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "pending_vendor.json"));
          break;
        case ACTIVE_ACCESS_PROVIDER_B:
          body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "one_access_providers_active.json"))
            .getJsonArray(VendorHelper.VENDORS).getJsonObject(0);
          break;
        case VENDOR_WITH_BAD_CONTENT:
          body = new JsonObject(ApiTestBase.getMockData(VENDORS_MOCK_DATA_PATH + "vendor_bad_content.json"));
          break;
        default:
          body = null;
      }
    } catch (IOException e) {
      body = null;
    }
    return body;
  }

  private String getQuery(String... accessProviders) {
    return convertIdsToCqlQuery(Arrays.asList(accessProviders));
  }

  private void handleGetIdentifierType(RoutingContext ctx) {
    logger.info("got: " + ctx.request().path());

    try {
      JsonObject po = new JsonObject(ApiTestBase.getMockData(INSTANCE_IDENTIFIERS_MOCK_DATA_PATH + "ISBN.json"));
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
      JsonObject po = new JsonObject(ApiTestBase.getMockData(INSTANCE_STATUSES_MOCK_DATA_PATH + "temp.json"));
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
      JsonObject po = new JsonObject(ApiTestBase.getMockData(INSTANCE_TYPES_MOCK_DATA_PATH + "zzz.json"));
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
        receivingHistory = new JsonObject(ApiTestBase.getMockData(RECEIVING_HISTORY_MOCK_DATA_PATH + "receivingHistory.json"));
      } else if(queryParam.contains(INTERNAL_SERVER_ERROR)) {
        throw new HttpException(500, "Exception in orders-storage module");
      }
      else if(queryParam.contains(BAD_QUERY)) {
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
      serverResponse(ctx, 200, APPLICATION_JSON, ApiTestBase.getMockData(String.format(CONFIG_MOCK_PATH, tenant)));
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

  private void handleGetPoLines(RoutingContext ctx) {
    logger.info("handleGetPoLines got: {}?{}", ctx.request().path(), ctx.request().query());

    String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    if (queryParam.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else if (queryParam.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
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
          poLineCollection = new JsonObject(ApiTestBase.getMockData(POLINES_COLLECTION)).mapTo(PoLineCollection.class);

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
          JsonObject compPO = new JsonObject(ApiTestBase.getMockData(filePath));
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
          PoLine poLine = new JsonObject(ApiTestBase.getMockData(String.format("%s%s.json", PO_LINES_MOCK_DATA_PATH, id))).mapTo(PoLine.class);
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
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
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
          pieces = new JsonObject(ApiTestBase.getMockData(path)).mapTo(PieceCollection.class);

          // Filter piece records by receiving status
          if (StringUtils.isNotEmpty(status)) {
            Piece.ReceivingStatus receivingStatus = Piece.ReceivingStatus.fromValue(status);
            pieces.getPieces()
              .removeIf(piece -> receivingStatus != piece.getReceivingStatus());
          }
        } else {
          pieces = new JsonObject(ApiTestBase.getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecordsCollection.json")).mapTo(PieceCollection.class);

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
      JsonObject po = new JsonObject(ApiTestBase.getMockData(filePath));
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
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else if (queryParam.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
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

  private void handleGetContributorNameTypes(RoutingContext ctx) {
    String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    try {
      if(("name==Personal name").equals(queryParam)) {
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, ApiTestBase.getMockData(CONTRIBUTOR_NAME_TYPES_PATH));
      } else {
        serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Illegal query");
      }
    } catch (IOException e) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Mock-server error");
    }
  }
}
