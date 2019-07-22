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
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.EncumbranceCollection;
import org.folio.rest.jaxrs.model.*;

import javax.ws.rs.core.Response;
import java.io.IOException;
import java.nio.file.NoSuchFileException;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.orders.utils.HelperUtils.*;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNIT_ASSIGNMENTS;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.FINANCE_STORAGE_ENCUMBRANCES;
import static org.folio.orders.utils.ResourcePathResolver.SEARCH_ORDERS;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_LINES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.ApiTestBase.*;
import static org.folio.rest.impl.ApiTestBase.ID;
import static org.folio.rest.impl.InventoryHelper.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.rest.impl.InventoryHelper.ITEMS;
import static org.folio.rest.impl.InventoryHelper.LOAN_TYPES;
import static org.folio.rest.impl.ProtectedEntities.*;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ACTIVE_ACCESS_PROVIDER_A;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ACTIVE_ACCESS_PROVIDER_B;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ACTIVE_VENDOR_ID;
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
import static org.folio.rest.impl.PurchaseOrdersApiTest.ORGANIZATION_NOT_VENDOR;
import static org.folio.rest.impl.ReceivingHistoryApiTest.RECEIVING_HISTORY_PURCHASE_ORDER_ID;
import static org.junit.Assert.fail;

public class MockServer {

  private static final Logger logger = LoggerFactory.getLogger(MockServer.class);

  // Mock data paths
  static final String BASE_MOCK_DATA_PATH = "mockdata/";
  private static final String CONTRIBUTOR_NAME_TYPES_PATH = BASE_MOCK_DATA_PATH + "contributorNameTypes/contributorPersonalNameType.json";
  static final String CONFIG_MOCK_PATH = BASE_MOCK_DATA_PATH + "configurations.entries/%s.json";
  static final String LOAN_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "loanTypes/";
  private static final String ITEMS_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "itemsRecords/";
  static final String INSTANCE_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceTypes/";
  static final String INSTANCE_STATUSES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceStatuses/";
  private static final String INSTANCE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instances/";
  private static final String ENCUMBRANCE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "encumbrances/";
  public static final String PIECE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "lines/";
  private static final String ACQUISITIONS_UNITS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "acquisitionsUnits/units";
  private static final String ACQUISITIONS_UNIT_ASSIGNMENTS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "acquisitionsUnitAssignments/assignments";
  private static final String RECEIVING_HISTORY_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "receivingHistory/";
  private static final String ORGANIZATIONS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "organizations/";
  static final String POLINES_COLLECTION = PO_LINES_MOCK_DATA_PATH + "/po_line_collection.json";
  static final String ACQUISITIONS_UNITS_COLLECTION = ACQUISITIONS_UNITS_MOCK_DATA_PATH + "/units.json";
  static final String ACQUISITIONS_UNIT_ASSIGNMENTS_COLLECTION = ACQUISITIONS_UNIT_ASSIGNMENTS_MOCK_DATA_PATH + "/assignments.json";
  static final String ACQUISITIONS_MEMBERSHIPS_COLLECTION = ACQUISITIONS_UNITS_MOCK_DATA_PATH + "/memberships.json";

  static final String INTERNAL_SERVER_ERROR = "Internal Server Error";
  static final String HEADER_SERVER_ERROR = "X-Okapi-InternalServerError";
  private static final String PENDING_VENDOR_ID = "160501b3-52dd-41ec-a0ce-17762e7a9b47";
  static final String ORDER_ID_WITH_PO_LINES = "ab18897b-0e40-4f31-896b-9c9adc979a87";
  private static final String PIECE_POLINE_CONSISTENT_RECEIPT_STATUS_ID = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  private static final String PIECE_POLINE_CONSISTENCY_404_POLINE_NOT_FOUND_ID = "5b454292-6aaa-474f-9510-b59a564e0c8d";
  static final String PO_NUMBER_VALUE = "228D126";

  private static final String PO_NUMBER_ERROR_TENANT = "po_number_error_tenant";
  static final Header PO_NUMBER_ERROR_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, PO_NUMBER_ERROR_TENANT);

  private static final String TOTAL_RECORDS = "totalRecords";
  private static final String ITEM_RECORDS = "itemRecords";
  private static final String INSTANCE_RECORD = "instanceRecord";
  private static final String HOLDINGS_RECORD = "holdingRecord";
  private static final String CONTRIBUTOR_NAME_TYPES = "contributorNameTypes";
  private static final String INSTANCE_TYPES = "instanceTypes";
  private static final String INSTANCE_STATUSES = "instanceStatuses";

  static Table<String, HttpMethod, List<JsonObject>> serverRqRs = HashBasedTable.create();
  static HashMap<String, List<String>> serverRqQueries = new HashMap<>();
  private static Table<String, String, String> pieceLineOrderComplianceMatrix = HashBasedTable.create();

  private final int port;
  private final Vertx vertx;

  static {
    pieceLineOrderComplianceMatrix.put("dd6c7335-a8e5-440d-86ba-657240867de5", PO_LINE_FOR_ORDER_WITH_NON_PROTECTED_UNITS_ID, ORDER_WITH_NON_PROTECTED_UNITS_ID);
    pieceLineOrderComplianceMatrix.put("63bd5119-e177-490b-8eef-dadbb18f4473", PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID, ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID);
    pieceLineOrderComplianceMatrix.put("67c97a0c-4824-4a3b-9b2c-1b4a4ea80ded", PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID, ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID);
  }

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

  public static List<JsonObject> getPoLineUpdates() {
    return serverRqRs.get(PO_LINES, HttpMethod.PUT);
  }

  public static List<JsonObject> getPoLineSearches() {
    return serverRqRs.get(PO_LINES, HttpMethod.GET);
  }

  public static List<JsonObject> getPurchaseOrderRetrievals() {
    return serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET);
  }

  public static List<JsonObject> getPurchaseOrderUpdates() {
    return serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT);
  }

  public static List<JsonObject> getPieceUpdates() {
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

  public static List<JsonObject> getPieceSearches() {
    return serverRqRs.get(PIECES, HttpMethod.GET);
  }

  static List<JsonObject> getOrderLineSearches() {
    return serverRqRs.get(ORDER_LINES, HttpMethod.GET);
  }

  static List<JsonObject> getLoanTypesSearches() {
    return serverRqRs.get(LOAN_TYPES, HttpMethod.GET);
  }

  static List<JsonObject> getInstanceStatusesSearches() {
    return serverRqRs.get(INSTANCE_STATUSES, HttpMethod.GET);
  }

  static List<JsonObject> getContributorNameTypesSearches() {
    return serverRqRs.get(CONTRIBUTOR_NAME_TYPES, HttpMethod.GET);
  }

  static List<JsonObject> getInstanceTypesSearches() {
    return serverRqRs.get(INSTANCE_TYPES, HttpMethod.GET);
  }

  static List<String> getQueryParams(String resourceType) {
    return serverRqQueries.getOrDefault(resourceType, Collections.emptyList());
  }

  static List<Encumbrance> getCreatedEncumbrances() {
    List<JsonObject> jsonObjects = serverRqRs.get(FINANCE_STORAGE_ENCUMBRANCES, HttpMethod.POST);
    return jsonObjects == null ? Collections.emptyList()
        : jsonObjects.stream()
          .map(json -> json.mapTo(Encumbrance.class))
          .collect(Collectors.toList());
  }

  private static String getPoIdByPoLineId(String poLineId) {
    return new ArrayList<>(pieceLineOrderComplianceMatrix.column(poLineId).values()).get(0);
  }

  private static String getPoLineIdByPieceId(String pieceId) {
    return new ArrayList<>(pieceLineOrderComplianceMatrix.row(pieceId).values()).get(0);
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
    router.route(HttpMethod.POST, resourcesPath(FINANCE_STORAGE_ENCUMBRANCES))
      .handler(ctx -> handlePostGeneric(ctx, FINANCE_STORAGE_ENCUMBRANCES));
    router.route(HttpMethod.POST, resourcesPath(ACQUISITIONS_UNITS)).handler(ctx -> handlePostGenericSubObj(ctx, ACQUISITIONS_UNITS));
    router.route(HttpMethod.POST, resourcesPath(ACQUISITIONS_UNIT_ASSIGNMENTS)).handler(ctx -> handlePostGenericSubObj(ctx, ACQUISITIONS_UNIT_ASSIGNMENTS));
    router.route(HttpMethod.POST, resourcesPath(ACQUISITIONS_MEMBERSHIPS)).handler(ctx -> handlePostGenericSubObj(ctx, ACQUISITIONS_MEMBERSHIPS));

    router.route(HttpMethod.GET, resourcePath(PURCHASE_ORDER)).handler(this::handleGetPurchaseOrderById);
    router.route(HttpMethod.GET, resourcesPath(PURCHASE_ORDER)).handler(ctx -> handleGetPurchaseOrderByQuery(ctx, PURCHASE_ORDER));
    router.route(HttpMethod.GET, resourcesPath(SEARCH_ORDERS)).handler(ctx -> handleGetPurchaseOrderByQuery(ctx, SEARCH_ORDERS));
    router.route(HttpMethod.GET, "/instance-types").handler(this::handleGetInstanceType);
    router.route(HttpMethod.GET, "/instance-statuses").handler(this::handleGetInstanceStatus);
    router.route(HttpMethod.GET, "/inventory/instances").handler(this::handleGetInstanceRecord);
    router.route(HttpMethod.GET, "/item-storage/items").handler(this::handleGetItemRecordsFromStorage);
    router.route(HttpMethod.GET, "/inventory/items").handler(this::handleGetInventoryItemRecords);
    router.route(HttpMethod.GET, "/holdings-storage/holdings").handler(this::handleGetHoldingsRecords);
    router.route(HttpMethod.GET, "/holdings-storage/holdings/:id").handler(this::handleGetHolding);
    router.route(HttpMethod.GET, "/loan-types").handler(this::handleGetLoanType);
    router.route(HttpMethod.GET, "/organizations-storage/organizations/:id").handler(this::getOrganizationById);
    router.route(HttpMethod.GET, "/organizations-storage/organizations").handler(this::handleGetAccessProviders);
    router.route(HttpMethod.GET, resourcesPath(PO_LINES)).handler(ctx -> handleGetPoLines(ctx, PO_LINES));
    router.route(HttpMethod.GET, resourcePath(PO_LINES)).handler(this::handleGetPoLineById);
    router.route(HttpMethod.GET, resourcePath(ALERTS)).handler(ctx -> handleGetGenericSubObj(ctx, ALERTS));
    router.route(HttpMethod.GET, resourcePath(REPORTING_CODES)).handler(ctx -> handleGetGenericSubObj(ctx, REPORTING_CODES));
    router.route(HttpMethod.GET, resourcesPath(PO_NUMBER)).handler(this::handleGetPoNumber);
    router.route(HttpMethod.GET, resourcesPath(PIECES)).handler(this::handleGetPieces);
    router.route(HttpMethod.GET, resourcesPath(PIECES)+"/:id").handler(this::handleGetPieceById);
    router.route(HttpMethod.GET, resourcesPath(RECEIVING_HISTORY)).handler(this::handleGetReceivingHistory);
    router.route(HttpMethod.GET, resourcesPath(PO_LINE_NUMBER)).handler(this::handleGetPoLineNumber);
    router.route(HttpMethod.GET, "/contributor-name-types").handler(this::handleGetContributorNameTypes);
    router.route(HttpMethod.GET, resourcesPath(ORDER_LINES)).handler(ctx -> handleGetPoLines(ctx, ORDER_LINES));
    router.route(HttpMethod.GET, resourcesPath(FINANCE_STORAGE_ENCUMBRANCES)).handler(this::handleGetEncumbrances);
    router.route(HttpMethod.GET, resourcesPath(ACQUISITIONS_UNITS)).handler(this::handleGetAcquisitionsUnits);
    router.route(HttpMethod.GET, resourcePath(ACQUISITIONS_UNITS)).handler(this::handleGetAcquisitionsUnit);
    router.route(HttpMethod.GET, resourcesPath(ACQUISITIONS_UNIT_ASSIGNMENTS)).handler(this::handleGetAcquisitionsUnitAssignments);
    router.route(HttpMethod.GET, resourcePath(ACQUISITIONS_UNIT_ASSIGNMENTS)).handler(this::handleGetAcquisitionsUnitAssignment);
    router.route(HttpMethod.GET, resourcesPath(ACQUISITIONS_MEMBERSHIPS)).handler(this::handleGetAcquisitionsMemberships);
    router.route(HttpMethod.GET, resourcePath(ACQUISITIONS_MEMBERSHIPS)).handler(this::handleGetAcquisitionsMembership);

    router.route(HttpMethod.PUT, resourcePath(PURCHASE_ORDER)).handler(ctx -> handlePutGenericSubObj(ctx, PURCHASE_ORDER));
    router.route(HttpMethod.PUT, resourcePath(PO_LINES)).handler(ctx -> handlePutGenericSubObj(ctx, PO_LINES));
    router.route(HttpMethod.PUT, resourcePath(PIECES)).handler(ctx -> handlePutGenericSubObj(ctx, PIECES));
    router.route(HttpMethod.PUT, resourcePath(REPORTING_CODES)).handler(ctx -> handlePutGenericSubObj(ctx, REPORTING_CODES));
    router.route(HttpMethod.PUT, resourcePath(ALERTS)).handler(ctx -> handlePutGenericSubObj(ctx, ALERTS));
    router.route(HttpMethod.PUT, "/inventory/items/:id").handler(ctx -> handlePutGenericSubObj(ctx, ITEM_RECORDS));
    router.route(HttpMethod.PUT, "/holdings-storage/holdings/:id").handler(ctx -> handlePutGenericSubObj(ctx, ITEM_RECORDS));
    router.route(HttpMethod.PUT, resourcePath(ACQUISITIONS_UNITS)).handler(ctx -> handlePutGenericSubObj(ctx, ACQUISITIONS_UNITS));
    router.route(HttpMethod.PUT, resourcePath(ACQUISITIONS_UNIT_ASSIGNMENTS)).handler(ctx -> handlePutGenericSubObj(ctx, ACQUISITIONS_UNIT_ASSIGNMENTS));
    router.route(HttpMethod.PUT, resourcePath(ACQUISITIONS_MEMBERSHIPS)).handler(ctx -> handlePutGenericSubObj(ctx, ACQUISITIONS_MEMBERSHIPS));

    router.route(HttpMethod.DELETE, resourcePath(PURCHASE_ORDER)).handler(ctx -> handleDeleteGenericSubObj(ctx, PURCHASE_ORDER));
    router.route(HttpMethod.DELETE, resourcePath(PO_LINES)).handler(ctx -> handleDeleteGenericSubObj(ctx, PO_LINES));
    router.route(HttpMethod.DELETE, resourcePath(ALERTS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ALERTS));
    router.route(HttpMethod.DELETE, resourcePath(REPORTING_CODES)).handler(ctx -> handleDeleteGenericSubObj(ctx, REPORTING_CODES));
    router.route(HttpMethod.DELETE, resourcePath(PIECES)).handler(ctx -> handleDeleteGenericSubObj(ctx, PIECES));
    router.route(HttpMethod.DELETE, resourcePath(ACQUISITIONS_UNITS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ACQUISITIONS_UNITS));
    router.route(HttpMethod.DELETE, resourcePath(ACQUISITIONS_UNIT_ASSIGNMENTS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ACQUISITIONS_UNIT_ASSIGNMENTS));
    router.route(HttpMethod.DELETE, resourcePath(ACQUISITIONS_MEMBERSHIPS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ACQUISITIONS_MEMBERSHIPS));

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

  private void handleGetHoldingsRecords(RoutingContext ctx) {
    logger.info("handleGetHoldingRecord got: " + ctx.request().path());

    JsonObject instance = new JsonObject().put("holdingsRecords", new JsonArray());

    addServerRqRsData(HttpMethod.GET, HOLDINGS_RECORD, instance);
    serverResponse(ctx, 200, APPLICATION_JSON, instance.encodePrettily());
  }

  private void handleGetHolding(RoutingContext ctx) {
    logger.info("got: " + ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: " + id);

    JsonObject holding = new JsonObject();
    holding.put("id", id);
    holding.put("hrid", "ho00000001");
    holding.put("holdingsItems", new JsonArray());

    addServerRqRsData(HttpMethod.GET, HOLDINGS_RECORD, holding);
    serverResponse(ctx, 200, APPLICATION_JSON, holding.encodePrettily());
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
    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    try {
      if (NON_EXIST_LOAN_TYPE_TENANT.equals(tenantId)) {
        String body = buildEmptyCollection(LOAN_TYPES);
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, LOAN_TYPES, new JsonObject(body));
      } else {
        // Filter result based on name from query
        String name = ctx.request().getParam("query").split("==")[1];
        JsonObject entries = new JsonObject(ApiTestBase.getMockData(LOAN_TYPES_MOCK_DATA_PATH + "types.json"));
        filterByKeyValue("name", name, entries.getJsonArray(LOAN_TYPES));

        serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
        addServerRqRsData(HttpMethod.GET, LOAN_TYPES, entries);
      }
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
        body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "one_access_provider_not_found.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B).equals(query)
        || getQuery(ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_A).equals(query)
        || getQuery(ACTIVE_ACCESS_PROVIDER_B, ACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "all_access_providers_active.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "active_inactive_access_providers.json"));
      } else if (getQuery(INACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_B).equals(query)
        || getQuery(INACTIVE_ACCESS_PROVIDER_B, INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "all_inactive_access_providers.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "one_access_provider_not_found.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_B).equals(query)) {
        body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "one_access_providers_active.json"));
      } else if (getQuery(ORGANIZATION_NOT_VENDOR).equals(query)) {
        body = new JsonObject(
            ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "not_vendor.json"));
      }
      else {
        JsonArray organizations = new JsonArray();

        // Search for Organizations by id
        extractIdsFromQuery(query)
          .stream()
          .map(this::getOrganizationById)
          .filter(Objects::nonNull)
          .forEach(organizations::add);

        if (!organizations.isEmpty()) {
          body = new JsonObject().put(VendorHelper.ORGANIZATIONS, organizations);
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

  private void getOrganizationById(RoutingContext ctx) {
    logger.info("handleGetOrganizationById got: " + ctx.request().path());
    String vendorId = ctx.request().getParam(ID);
    JsonObject body;
    if (NON_EXIST_VENDOR_ID.equals(vendorId)) {
      serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "vendor not found");
    } else if (MOD_VENDOR_INTERNAL_ERROR_ID.equals(vendorId)) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), APPLICATION_JSON, "internal server error, contact administrator");
    } else {
      body = getOrganizationById(vendorId);
      if (body != null) {
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
      } else {
        serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "vendor not found");
      }
    }
  }

  private JsonObject getOrganizationById(String organizationId) {
    logger.debug("Searching for organization by id={}", organizationId);
    JsonObject body;
    try {
      switch (organizationId) {
        case ACTIVE_VENDOR_ID:
          body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "active_vendor.json"));
          break;
        case INACTIVE_VENDOR_ID:
          body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "inactive_vendor.json"));
          break;
        case PENDING_VENDOR_ID:
          body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "pending_vendor.json"));
          break;
        case ACTIVE_ACCESS_PROVIDER_B:
          body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "one_access_providers_active.json"))
            .getJsonArray(VendorHelper.ORGANIZATIONS).getJsonObject(0);
          break;
        case ORGANIZATION_NOT_VENDOR:
          body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "not_vendor.json"));
          break;
        case VENDOR_WITH_BAD_CONTENT:
          body = new JsonObject(ApiTestBase.getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "vendor_bad_content.json"));
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

  private void handleGetInstanceStatus(RoutingContext ctx) {
    logger.info("got: " + ctx.request().path());

    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    try {
      if (NON_EXIST_INSTANCE_STATUS_TENANT.equals(tenantId)) {
        String body = buildEmptyCollection(INSTANCE_STATUSES);
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, INSTANCE_STATUSES, new JsonObject(body));
      } else {
        // Filter result based on code from query
        String code = ctx.request().getParam("query").split("==")[1];
        JsonObject entries = new JsonObject(ApiTestBase.getMockData(INSTANCE_STATUSES_MOCK_DATA_PATH + "types.json"));
        filterByKeyValue("code", code, entries.getJsonArray(INSTANCE_STATUSES));

        serverResponse(ctx, 200, APPLICATION_JSON, entries.encode());
        addServerRqRsData(HttpMethod.GET, INSTANCE_STATUSES, entries);
      }
    } catch (IOException e) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Mock-server error");
    }
  }

  private void handleGetInstanceType(RoutingContext ctx) {
    logger.info("got: " + ctx.request().path());

    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    try {
      if (INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT.equals(tenantId)) {
        String body = ApiTestBase.getMockData(INSTANCE_TYPES_MOCK_DATA_PATH + "temp.json");
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, INSTANCE_TYPES, new JsonObject(body));
      } else if (NON_EXIST_INSTANCE_TYPE_TENANT.equals(tenantId)) {
        String body = buildEmptyCollection(INSTANCE_TYPES);
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, INSTANCE_TYPES, new JsonObject(body));
      } else {
        // Filter result based on code from query
        String code = ctx.request().getParam("query").split("==")[1];
        JsonObject entries = new JsonObject(ApiTestBase.getMockData(INSTANCE_TYPES_MOCK_DATA_PATH + "types.json"));
        filterByKeyValue("code", code, entries.getJsonArray(INSTANCE_TYPES));

        serverResponse(ctx, 200, APPLICATION_JSON, entries.encode());
        addServerRqRsData(HttpMethod.GET, INSTANCE_TYPES, entries);
      }
    } catch (IOException e) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Mock-server error");
    }
  }

  private String buildEmptyCollection(String entryType) {
    JsonObject result = new JsonObject();
    result.put(entryType, new JsonArray());
    result.put("totalRecords", 0);
    return result.encodePrettily();
  }

  private void filterByKeyValue(String key, String value, JsonArray entries) {
    Iterator<Object> iterator = entries.iterator();
    while (iterator.hasNext()) {
      JsonObject obj = (JsonObject) iterator.next();
      if (!StringUtils.equals(value, obj.getString(key))) {
        iterator.remove();
      }
    }
  }

  private void handleGetReceivingHistory(RoutingContext ctx) {
    logger.info("handleGetItemsRecords got: " + ctx.request().path());
    String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    addServerRqQuery(RECEIVING_HISTORY, queryParam);
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

  private void handleGetPoLines(RoutingContext ctx, String type) {
    logger.info("handleGetPoLines got: {}?{}", ctx.request().path(), ctx.request().query());

    String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    addServerRqQuery(type, queryParam);
    if (queryParam.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else if (queryParam.contains(ID_FOR_INTERNAL_SERVER_ERROR) || queryParam.contains(PO_ID_GET_LINES_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      String poId = EMPTY;
      String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT);
      List<String> polIds = Collections.emptyList();

      if (queryParam.contains(PURCHASE_ORDER_ID)) {
        Matcher matcher = Pattern.compile(".*" + PURCHASE_ORDER_ID + "==(\\S+).*").matcher(queryParam);
        poId = matcher.find() ? matcher.group(1) : EMPTY;
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

        addServerRqRsData(HttpMethod.GET, type, po_lines);
        serverResponse(ctx, 200, APPLICATION_JSON, po_lines.encode());
      } catch (NoSuchFileException e) {
        PoLineCollection poLineCollection = new PoLineCollection();

        // Attempt to find POLine in mock server memory
        List<JsonObject> postedPoLines = serverRqRs.column(HttpMethod.POST).get(type);

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

        JsonObject entries = JsonObject.mapFrom(poLineCollection);
        addServerRqRsData(HttpMethod.GET, type, entries);
        serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
      } catch (IOException e) {
        PoLineCollection poLineCollection = new PoLineCollection();
        poLineCollection.setTotalRecords(0);

        JsonObject entries = JsonObject.mapFrom(poLineCollection);
        addServerRqRsData(HttpMethod.GET, type, entries);
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
      line.getCost().setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
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

    if(pieceLineOrderComplianceMatrix.containsColumn(id)) {
      serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(getMinimalContentCompositePoLine().withPurchaseOrderId(getPoIdByPoLineId(id))).encodePrettily());
    } else {
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

  private void handleGetPieceById(RoutingContext ctx) {

    logger.info("handleGetPiecesById got: " + ctx.request()
      .path());
    String pieceId = ctx.request()
      .getParam(ID);
    JsonObject body;
    try {
      if (PIECE_POLINE_CONSISTENCY_404_POLINE_NOT_FOUND_ID.equals(pieceId)) {
        body = new JsonObject(ApiTestBase
          .getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-poline-not-exists-5b454292-6aaa-474f-9510-b59a564e0c8d.json"));
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());

      } else if (PIECE_POLINE_CONSISTENT_RECEIPT_STATUS_ID.equals(pieceId)) {
        body = new JsonObject(ApiTestBase.getMockData(PIECE_RECORDS_MOCK_DATA_PATH
            + "pieceRecord-received-consistent-receipt-status-5b454292-6aaa-474f-9510-b59a564e0c8d2.json"));
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());

      } else {
        body = new JsonObject(
            ApiTestBase.getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-af372ac8-5ffb-4560-8b96-3945a12e121b.json"));
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
      }
    } catch (IOException e) {
      fail(e.getMessage());
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
    return extractIdsFromQuery(ID, query);
  }


  private List<String> extractIdsFromQuery(String fieldName, String query) {
    Matcher matcher = Pattern.compile(".*" + fieldName + "==\\((.+)\\).*").matcher(query);
    if (matcher.find()) {
      return StreamEx.split(matcher.group(1), " or ").toList();
    } else {
      return Collections.emptyList();
    }
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

  private void addServerRqQuery(String objName, String query) {
    serverRqQueries.computeIfAbsent(objName, key -> new ArrayList<>())
      .add(query);
  }

  private void handleGetPurchaseOrderById(RoutingContext ctx) {
    logger.info("handleGetPurchaseOrderById got: GET " + ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: " + id);

    if(pieceLineOrderComplianceMatrix.values().contains(id)) {
      serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(getMinimalContentCompositePurchaseOrder().withId(id)).encodePrettily());


    } else {
      try {
        String filePath;
        if (ID_FOR_PRINT_MONOGRAPH_ORDER.equals(id)) {
          filePath = LISTED_PRINT_MONOGRAPH_PATH;
        } else {
          filePath = String.format("%s%s.json", COMP_ORDER_MOCK_DATA_PATH, id);
        }
        JsonObject po = new JsonObject(ApiTestBase.getMockData(filePath));
        po.remove(COMPOSITE_PO_LINES);
        po.remove(ACQ_UNIT_IDS);

        // Validate the content against schema
        org.folio.rest.acq.model.PurchaseOrder order = po.mapTo(org.folio.rest.acq.model.PurchaseOrder.class);
        order.setId(id);
        po = JsonObject.mapFrom(order);
        addServerRqRsData(HttpMethod.GET, PURCHASE_ORDER, po);
        serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
      } catch (IOException e) {
        ctx.response()
          .setStatusCode(ID_FOR_INTERNAL_SERVER_ERROR.equals(id) ? 500 : 404)
          .end(id);
      }

    }





  }

  private void handleGetPurchaseOrderByQuery(RoutingContext ctx, String orderType) {

    String query = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    addServerRqQuery(orderType, query);
    if (query.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      JsonObject po = new JsonObject();
      addServerRqRsData(HttpMethod.GET, orderType, po);

      Matcher matcher = Pattern.compile(".*poNumber==(\\S+).*").matcher(query);
      final String poNumber = matcher.find() ? matcher.group(1) : EMPTY;
      switch (poNumber) {
        case EXISTING_PO_NUMBER:
          po.put(TOTAL_RECORDS, 1);
          break;
        case NONEXISTING_PO_NUMBER:
          po.put(TOTAL_RECORDS, 0);
          break;
        case EMPTY:
          po.put(TOTAL_RECORDS, 3);
          break;
        default:
          //modify later as needed
          po.put(TOTAL_RECORDS, 0);
      }

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

  private void handlePostGeneric(RoutingContext ctx, String objectType) {
    logger.info("handlePostGeneric {} got: {}", objectType, ctx.getBodyAsString());

    if (objectType.equals(ctx.request().getHeader(HEADER_SERVER_ERROR))) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR);
    } else {
      String id = UUID.randomUUID().toString();
      JsonObject body = ctx.getBodyAsJson();
      body.put(ID, id);
      addServerRqRsData(HttpMethod.POST, objectType, body);

      serverResponse(ctx, 201, APPLICATION_JSON, body.encodePrettily());
    }
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
      case ACQUISITIONS_UNITS:
        return AcquisitionsUnit.class;
      case ACQUISITIONS_UNIT_ASSIGNMENTS:
        return AcquisitionsUnitAssignment.class;
      case ACQUISITIONS_MEMBERSHIPS:
        return AcquisitionsUnitMembership.class;
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
    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);

    if (NON_EXIST_CONTRIBUTOR_NAME_TYPE_TENANT.equals(tenantId)) {
      String body = buildEmptyCollection(CONTRIBUTOR_NAME_TYPES);

      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
      addServerRqRsData(HttpMethod.GET, CONTRIBUTOR_NAME_TYPES, new JsonObject(body));
    } else if (queryParam.startsWith("id==")) {
      List<String> contributorNameTypeIds = extractIdsFromQuery(queryParam);

      JsonObject contributorNameTypeCollection = getMockAsJson(CONTRIBUTOR_NAME_TYPES_PATH);
      List<JsonObject> contributorNameTypes = contributorNameTypeCollection.getJsonArray(CONTRIBUTOR_NAME_TYPES)
        .stream()
        .map(o -> ((JsonObject) o))
        .filter(contributorNameType -> contributorNameTypeIds.contains(contributorNameType.getString(ID)))
        .collect(Collectors.toList());

      contributorNameTypeCollection.put(CONTRIBUTOR_NAME_TYPES, contributorNameTypes);

      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, contributorNameTypeCollection.encodePrettily());
      addServerRqRsData(HttpMethod.GET, CONTRIBUTOR_NAME_TYPES, contributorNameTypeCollection);
    } else {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Illegal query");
    }

  }

  private void handleGetOrderLines(RoutingContext ctx) {
    logger.info("handleGetOrderLines got: {}?{}", ctx.request().path(), ctx.request().query());

    String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    if (queryParam.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    }
    String poId = null;
    if (queryParam.contains(PURCHASE_ORDER_ID)) {
      poId = queryParam.split(PURCHASE_ORDER_ID + "==")[1];
    }

    try {
      PoLineCollection poLineCollection = null;
      if (ORDER_ID_WITH_PO_LINES.equals(poId)) {
        poLineCollection = new JsonObject(ApiTestBase.getMockData(POLINES_COLLECTION)).mapTo(PoLineCollection.class);
        Iterator<PoLine> iterator = poLineCollection.getPoLines().iterator();
        while (iterator.hasNext()) {
          PoLine poLine = iterator.next();
          if (!poId.equals(poLine.getPurchaseOrderId())) {
            iterator.remove();
          }
        }
        poLineCollection.setTotalRecords(poLineCollection.getPoLines().size());
      }

      // Update calculated data
      updatePoLineCalculatedData(poLineCollection);

      JsonObject po_lines = JsonObject.mapFrom(poLineCollection);
      logger.info(po_lines.encodePrettily());

      addServerRqRsData(HttpMethod.GET, ORDER_LINES, po_lines);
      serverResponse(ctx, 200, APPLICATION_JSON, po_lines.encode());
    } catch (IOException e) {
      PoLineCollection poLineCollection = new PoLineCollection();
      poLineCollection.setTotalRecords(0);
      serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(poLineCollection).encodePrettily());
    }
  }

  private void handleGetEncumbrances(RoutingContext ctx) {
    logger.info("handleGetEncumbrances got: " + ctx.request()
      .path());

    String polId = ctx.request()
      .getParam("query")
      .replace("poLineId==", "");
    EncumbranceCollection encumbrances;
    try {
      encumbrances = new JsonObject(ApiTestBase.getMockData(ENCUMBRANCE_RECORDS_MOCK_DATA_PATH + "encumbrances.json"))
        .mapTo(EncumbranceCollection.class);
    } catch (IOException e) {
      encumbrances = new EncumbranceCollection();
    }

    encumbrances.getEncumbrances()
      .removeIf(encumbrance -> !encumbrance.getPoLineId()
        .equals(polId));

    encumbrances.setTotalRecords(encumbrances.getEncumbrances().size());

    JsonObject data = JsonObject.mapFrom(encumbrances);
    addServerRqRsData(HttpMethod.GET, FINANCE_STORAGE_ENCUMBRANCES, data);
    serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
  }

  private void handleGetAcquisitionsUnits(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsUnits got: " + ctx.request().path());

    String query = StringUtils.trimToEmpty(ctx.request().getParam("query"));

    AcquisitionsUnitCollection units;

    try {
      units = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_UNITS_COLLECTION)).mapTo(AcquisitionsUnitCollection.class);
    } catch (IOException e) {
      units = new AcquisitionsUnitCollection();
    }

    if (query.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else {

      if(query.contains("name==")) {
        String name = query.replace("name==", "");
        if (StringUtils.isNotEmpty(name)) {
          units.getAcquisitionsUnits().removeIf(unit -> !unit.getName().equals(name));
        }
      }

      if(query.contains("id==")) {
        List<String> ids = extractIdsFromQuery(query);
        if (!ids.isEmpty()) {
          units.getAcquisitionsUnits().removeIf(unit -> !ids.contains(unit.getId()));
        }
      }

      JsonObject data = JsonObject.mapFrom(units.withTotalRecords(units.getAcquisitionsUnits().size()));
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_UNITS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    }
  }

  private void handleGetAcquisitionsUnit(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsUnits got: " + ctx.request().path());
    String id = ctx.request().getParam(ID);

    AcquisitionsUnitCollection units;
    try {
      units = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_UNITS_COLLECTION)).mapTo(AcquisitionsUnitCollection.class);
    } catch (IOException e) {
      units = new AcquisitionsUnitCollection();
    }

    AcquisitionsUnit acquisitionsUnit = units.getAcquisitionsUnits()
      .stream()
      .filter(unit -> unit.getId().equals(id))
      .findAny()
      .orElse(null);

    if (acquisitionsUnit != null) {
      JsonObject data = JsonObject.mapFrom(acquisitionsUnit);
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_UNITS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    } else {
      serverResponse(ctx, 404, TEXT_PLAIN, id);
    }
  }

  private void handleGetAcquisitionsMemberships(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsMemberships got: " + ctx.request().path());

    String query = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    if (query.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else {

      Matcher userIdMatcher = Pattern.compile(".*userId==(\\S+).*").matcher(query);
      final String userId = userIdMatcher.find() ? userIdMatcher.group(1) : EMPTY;

      AcquisitionsUnitMembershipCollection memberships;
      try {
        memberships = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_MEMBERSHIPS_COLLECTION)).mapTo(AcquisitionsUnitMembershipCollection.class);
      } catch (IOException e) {
        memberships = new AcquisitionsUnitMembershipCollection();
      }

      if (StringUtils.isNotEmpty(userId)) {
        memberships.getAcquisitionsUnitMemberships().removeIf(membership -> !membership.getUserId().equals(userId));
        List<String> acquisitionsUnitIds = extractIdsFromQuery("acquisitionsUnitId", query);
          if (acquisitionsUnitIds.size() > 0) {
            memberships.getAcquisitionsUnitMemberships().removeIf(membership -> !acquisitionsUnitIds.contains(membership.getAcquisitionsUnitId()));
          }
      }

      JsonObject data = JsonObject.mapFrom(memberships.withTotalRecords(memberships.getAcquisitionsUnitMemberships().size()));
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_MEMBERSHIPS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    }
  }

  private void handleGetAcquisitionsMembership(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsMembership got: " + ctx.request().path());
    String id = ctx.request().getParam(ID);

    AcquisitionsUnitMembershipCollection memberships;
    try {
      memberships = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_MEMBERSHIPS_COLLECTION)).mapTo(AcquisitionsUnitMembershipCollection.class);
    } catch (IOException e) {
      memberships = new AcquisitionsUnitMembershipCollection();
    }

    AcquisitionsUnitMembership acquisitionsUnitMembership = memberships.getAcquisitionsUnitMemberships()
      .stream()
      .filter(membership -> membership.getId().equals(id))
      .findAny()
      .orElse(null);

    if (acquisitionsUnitMembership != null) {
      JsonObject data = JsonObject.mapFrom(acquisitionsUnitMembership);
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_MEMBERSHIPS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    } else {
      serverResponse(ctx, 404, TEXT_PLAIN, id);
    }
  }

  private void handleGetAcquisitionsUnitAssignments(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsUnitAssignments got: " + ctx.request().path());

    String query = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    if (query.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else {
      String name = query.replace("recordId==", "");
      AcquisitionsUnitAssignmentCollection units;
      try {
        units = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_UNIT_ASSIGNMENTS_COLLECTION)).mapTo(AcquisitionsUnitAssignmentCollection.class);
      } catch (IOException e) {
        units = new AcquisitionsUnitAssignmentCollection();
      }

      if (StringUtils.isNotEmpty(name)) {
        units.getAcquisitionsUnitAssignments().removeIf(unit -> !unit.getRecordId().equals(name));
      }

      JsonObject data = JsonObject.mapFrom(units.withTotalRecords(units.getAcquisitionsUnitAssignments().size()));
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_UNIT_ASSIGNMENTS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    }
  }

  private void handleGetAcquisitionsUnitAssignment(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsUnitAssignment got: " + ctx.request().path());
    String id = ctx.request().getParam(ID);

    AcquisitionsUnitAssignmentCollection unitAssignments;
    try {
      unitAssignments = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_UNIT_ASSIGNMENTS_COLLECTION)).mapTo(AcquisitionsUnitAssignmentCollection.class);
    } catch (IOException e) {
      unitAssignments = new AcquisitionsUnitAssignmentCollection();
    }

    AcquisitionsUnitAssignment acquisitionsUnitAssignment = unitAssignments.getAcquisitionsUnitAssignments()
      .stream()
      .filter(unitAssignment -> unitAssignment.getId().equals(id))
      .findAny()
      .orElse(null);

    if (acquisitionsUnitAssignment != null) {
      JsonObject data = JsonObject.mapFrom(acquisitionsUnitAssignment);
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_UNIT_ASSIGNMENTS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    } else {
      serverResponse(ctx, 404, TEXT_PLAIN, id);
    }
  }
}
