package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPostResponseWithQueryParams;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_ECS;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ID;
import static org.folio.TestConstants.ID_BAD_FORMAT;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.X_ECHO_STATUS;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.TestUtils.getTitle;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.core.exceptions.ErrorCodes.REQUEST_FOUND;
import static org.folio.rest.impl.MockServer.CONSISTENT_ECS_PURCHASE_ORDER_ID_ELECTRONIC;
import static org.folio.rest.impl.MockServer.CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PIECES_JSON;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PO_LINE_JSON;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_PURCHASE_ORDER_JSON;
import static org.folio.rest.impl.MockServer.ECS_CONSORTIUM_TITLES_JSON;
import static org.folio.rest.impl.MockServer.ITEM_RECORDS;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ECS_UNIVERSITY_HOLDINGS_RECORD_JSON;
import static org.folio.rest.impl.MockServer.ECS_UNIVERSITY_INSTANCE_JSON;
import static org.folio.rest.impl.MockServer.ECS_UNIVERSITY_ITEM_JSON;
import static org.folio.rest.impl.MockServer.PO_LINES_COLLECTION;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.getCreatedItems;
import static org.folio.rest.impl.MockServer.getCreatedPieces;
import static org.folio.service.inventory.InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_STATUS_NAME;
import static org.folio.service.inventory.InventoryUtils.HOLDINGS_RECORDS;
import static org.folio.service.inventory.InventoryUtils.INSTANCES;
import static org.folio.service.inventory.InventoryUtils.ITEMS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;
import java.util.stream.Stream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.CopilotGenerated;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.orders.events.handlers.HandlersTestHelper;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

@CopilotGenerated(partiallyGenerated = true)
public class PieceApiTest {

  private static final Logger logger = LogManager.getLogger();

  public static final String PIECES_ENDPOINT = "/orders/pieces";
  public static final String PIECES_BATCH_ENDPOINT = "/orders/pieces-batch";
  private static final String PIECES_ID_PATH = PIECES_ENDPOINT + "/%s";
  static final String CONSISTENT_RECEIVED_STATUS_PIECE_UUID = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  private final JsonObject pieceJsonReqData = getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json");
  private final JsonObject piecesBatchReqData = getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieces-batch-request.json");
  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @AfterEach
  void afterEach() {
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void testPostPiece() {
    logger.info("=== Test POST Piece (Create Piece) ===");

    Piece postPieceRq = pieceJsonReqData.mapTo(Piece.class);
    // To skip unit's permission validation
    postPieceRq.setPoLineId("2bafc9e1-9dd3-4ede-9f23-c4a03f8bb2d5");

    // Positive cases
    // Piece id is null initially
    assertThat(postPieceRq.getId(), nullValue());

    Piece postPieceRs = verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Piece.class);

    // Piece id not null
    assertThat(postPieceRs.getId(), notNullValue());

    // Negative cases
    // Unable to create piece test
    int status400 = HttpStatus.HTTP_BAD_REQUEST.toInt();
    verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID,
      new Header(X_ECHO_STATUS, String.valueOf(status400))), APPLICATION_JSON, status400);

    // Internal error on mod-orders-storage test
    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID,
      new Header(X_ECHO_STATUS, String.valueOf(status500))), APPLICATION_JSON, status500);
  }

  @Test
  void testPostPhysicalPieceCancelledPurchaseOrder() {
    logger.info("=== Test POST physical piece with a cancelled purchase order ===");

    CompositePoLine compositePoLine = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(14).mapTo(CompositePoLine.class);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(compositePoLine.getPurchaseOrderId()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);
    Title tile = getTitle(compositePoLine);
    addMockEntry(PURCHASE_ORDER_STORAGE, compositePurchaseOrder);
    addMockEntry(PO_LINES_STORAGE, compositePoLine);
    addMockEntry(TITLES, tile);

    Location location = compositePoLine.getLocations().stream().findFirst().orElseThrow();
    Piece piece = new Piece().withPoLineId(compositePoLine.getId())
      .withTitleId(tile.getId())
      .withFormat(Piece.Format.PHYSICAL)
      .withLocationId(location.getLocationId())
      .withHoldingId(location.getHoldingId());

    assertThat(piece.getId(), nullValue());

    Piece createdPiece = verifyPostResponseWithQueryParams(PIECES_ENDPOINT, JsonObject.mapFrom(piece).encode(),
      Map.of("createItem", "true"),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON,
      HttpStatus.HTTP_CREATED.toInt()).as(Piece.class);

    assertThat(createdPiece.getId(), notNullValue());
    assertThat(createdPiece.getItemId(), notNullValue());
    assertThat(createdPiece.getReceivingStatus().value(), is(Piece.ReceivingStatus.EXPECTED.value()));

    List<JsonObject> piecesCreated = getCreatedPieces();
    List<JsonObject> itemsCreated = getCreatedItems();

    assertThat(piecesCreated, hasSize(1));
    assertThat(itemsCreated, hasSize(1));

    for (JsonObject itemJson : itemsCreated) {
      logger.info("Item with from a cancelled purchase order: {}", JsonObject.mapFrom(itemJson).encodePrettily());
      assertThat(itemJson.getString(ID), is(createdPiece.getItemId()));
      assertThat(itemJson.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER), is(compositePoLine.getId()));
      assertThat(itemJson.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), is(CheckInPiece.ItemStatus.ORDER_CLOSED.value()));
    }
  }

  private static Stream<Arguments> testPostPieceCancelledOrderLineArgs() {
    return Stream.of(
      Arguments.of(15, CheckInPiece.ItemStatus.ON_ORDER),
      Arguments.of(16, CheckInPiece.ItemStatus.ORDER_CLOSED)
    );
  }

  @ParameterizedTest
  @MethodSource("testPostPieceCancelledOrderLineArgs")
  void testPostPieceCancelledOrderLine(int poLineIdx, CheckInPiece.ItemStatus itemStatus) {
    logger.info("=== Test POST physical piece with a cancelled order line ===");

    CompositePoLine compositePoLine = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(poLineIdx).mapTo(CompositePoLine.class);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(compositePoLine.getPurchaseOrderId()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    Title tile = getTitle(compositePoLine);
    addMockEntry(PURCHASE_ORDER_STORAGE, compositePurchaseOrder);
    addMockEntry(PO_LINES_STORAGE, compositePoLine);
    addMockEntry(TITLES, tile);

    Location location = compositePoLine.getLocations().stream().findFirst().orElseThrow();
    Piece piece = new Piece().withPoLineId(compositePoLine.getId())
      .withTitleId(tile.getId())
      .withFormat(Piece.Format.PHYSICAL)
      .withLocationId(location.getLocationId())
      .withHoldingId(location.getHoldingId());

    assertThat(piece.getId(), nullValue());

    Piece createdPiece = verifyPostResponseWithQueryParams(PIECES_ENDPOINT, JsonObject.mapFrom(piece).encode(),
      Map.of("createItem", "true"),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON,
      HttpStatus.HTTP_CREATED.toInt()).as(Piece.class);

    assertThat(createdPiece.getId(), notNullValue());
    assertThat(createdPiece.getItemId(), notNullValue());
    assertThat(createdPiece.getReceivingStatus().value(), is(Piece.ReceivingStatus.EXPECTED.value()));

    List<JsonObject> piecesCreated = getCreatedPieces();
    List<JsonObject> itemsCreated = getCreatedItems();

    assertThat(piecesCreated, hasSize(1));
    assertThat(itemsCreated, hasSize(1));

    for (JsonObject itemJson : itemsCreated) {
      logger.info("Item with from a cancelled order line: {}", JsonObject.mapFrom(itemJson).encodePrettily());
      assertThat(itemJson.getString(ID), is(createdPiece.getItemId()));
      assertThat(itemJson.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER), is(compositePoLine.getId()));
      assertThat(itemJson.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), is(itemStatus.value()));
    }
  }

  @Test
  void testPostShouldSuccessfullyCreatePieceWithoutReceiptDate() {
    logger.info("=== Test POST Piece (Create Piece) without receiptDate===");

    Piece postPieceRq = pieceJsonReqData.mapTo(Piece.class);
    // To skip unit's permission validation
    postPieceRq.setPoLineId("2bafc9e1-9dd3-4ede-9f23-c4a03f8bb2d5");
    postPieceRq.setReceiptDate(null);

    Piece piece = verifyPostResponse(PIECES_ENDPOINT, JsonObject.mapFrom(postPieceRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(Piece.class);

    assertNull(piece.getReceiptDate());
  }

  @Test
  void testPostOrdersPiecesBatch() {
    logger.info("=== Test POST Orders Pieces Batch ===");

    var pieceCollection = piecesBatchReqData.mapTo(PieceCollection.class);

    var createdPieces = verifyPostResponse(PIECES_BATCH_ENDPOINT, JsonObject.mapFrom(pieceCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(PieceCollection.class);

    assertThat(createdPieces.getPieces(), hasSize(2));
    assertThat(createdPieces.getPieces().get(0).getId(), notNullValue());
    assertThat(createdPieces.getPieces().get(1).getId(), notNullValue());
  }

  @Test
  void testPostOrdersPiecesBatchBadRequest() {
    logger.info("=== Test POST Orders Pieces Batch - Bad Request ===");

    var pieceCollection = piecesBatchReqData.mapTo(PieceCollection.class);
    pieceCollection.getPieces().get(0).withPoLineId(ID_BAD_FORMAT);

    int status422 = HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt();
    verifyPostResponse(PIECES_BATCH_ENDPOINT, JsonObject.mapFrom(pieceCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, new Header(X_ECHO_STATUS, String.valueOf(status422))),
      APPLICATION_JSON, status422);
  }

  @Test
  void testPostOrdersPiecesBatchInternalServerError() {
    logger.info("=== Test POST Orders Pieces Batch - Internal Server Error ===");

    var pieceCollection = piecesBatchReqData.mapTo(PieceCollection.class);
    pieceCollection.getPieces().get(0).withPoLineId(ID_FOR_INTERNAL_SERVER_ERROR);

    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(PIECES_BATCH_ENDPOINT, JsonObject.mapFrom(pieceCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, new Header(X_ECHO_STATUS, String.valueOf(status500))),
      APPLICATION_JSON, status500);
  }

  @Test
  void testPutPiecesByIdTest() throws Exception {
    logger.info("=== Test update piece by id - valid Id 204 ===");

    CompositePoLine poLineForOpenOrder = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(CompositePoLine.class);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(poLineForOpenOrder.getPurchaseOrderId()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    addMockEntry(PURCHASE_ORDER_STORAGE, compositePurchaseOrder);
    addMockEntry(PO_LINES_STORAGE, poLineForOpenOrder);

    String reqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json");
    String pieceId = UUID.randomUUID().toString();
    JsonObject pieceStorage = new JsonObject((reqData));
    pieceStorage.put(ID, pieceId);
    pieceStorage.put("receivingStatus", "Expected");
    JsonObject pieceRequest = new JsonObject((reqData));
    pieceRequest.put(ID, pieceId);
    pieceRequest.put("receivingStatus", "Received");
    pieceRequest.put("poLineId", poLineForOpenOrder.getId());

    addMockEntry(PIECES_STORAGE, pieceStorage);
    verifyPut(String.format(PIECES_ID_PATH, pieceId), pieceRequest, "", 204);

    // Message sent to event bus
    HandlersTestHelper.verifyReceiptStatusUpdateEvent(1);
  }

  private static Stream<Arguments> testPutPiecesByIdEcsArgs() {
    return Stream.of(
      Arguments.of(CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL, "aaecf1f7-28dc-4940-bfd4-be0e26afde95", true),
      Arguments.of(CONSISTENT_ECS_PURCHASE_ORDER_ID_ELECTRONIC, "8cf0c835-9ad5-4cfb-8bd9-7fa78e65f7c3", true)
    );
  }

  @ParameterizedTest
  @MethodSource("testPutPiecesByIdEcsArgs")
  void testPutPiecesByIdEcs(String purchaseOrderId, String pieceId, boolean mockItem) throws Exception {
    logger.info("=== Test update piece by id ECS - valid Id 204 ===");

    var purchaseOrderMock = getMockData(String.format(ECS_CONSORTIUM_PURCHASE_ORDER_JSON, purchaseOrderId));
    var poLineMock = getMockData(String.format(ECS_CONSORTIUM_PO_LINE_JSON, purchaseOrderId));
    var titlesMock = getMockData(String.format(ECS_CONSORTIUM_TITLES_JSON, purchaseOrderId));
    var piecesMock = getMockData(String.format(ECS_CONSORTIUM_PIECES_JSON, purchaseOrderId));
    var instanceMock = getMockData(String.format(ECS_UNIVERSITY_INSTANCE_JSON, purchaseOrderId));
    var holdingsMock = getMockData(String.format(ECS_UNIVERSITY_HOLDINGS_RECORD_JSON, purchaseOrderId));

    var piecesStorage = new JsonObject(piecesMock).mapTo(Piece.class);
    var piecesRequest = new JsonObject(piecesMock).mapTo(Piece.class);
    piecesRequest.setReceivingStatus(Piece.ReceivingStatus.RECEIVED);
    piecesRequest.setReceivingTenantId("college");

    addMockEntry(PURCHASE_ORDER_STORAGE, new JsonObject(purchaseOrderMock).mapTo(PurchaseOrder.class));
    addMockEntry(TITLES, new JsonObject(titlesMock).mapTo(Title.class));
    addMockEntry(PO_LINES_STORAGE, new JsonObject(poLineMock).mapTo(PoLine.class));
    addMockEntry(PIECES_STORAGE, piecesStorage);
    addMockEntry(INSTANCES, new JsonObject(instanceMock));
    addMockEntry(HOLDINGS_RECORDS, new JsonObject(holdingsMock));

    if (mockItem) { // In case we need to test electronic resources without item in the inventory
      var itemMock = getMockData(String.format(ECS_UNIVERSITY_ITEM_JSON, purchaseOrderId));
      var itemStorage = new JsonObject(itemMock);
      addMockEntry(ITEMS, itemStorage);
    }

    var headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_ECS, X_OKAPI_USER_ID);
    var piecesRequestBody = JsonObject.mapFrom(piecesRequest).encode();

    verifyPut(String.format(PIECES_ID_PATH, pieceId), piecesRequestBody, headers, "", HttpStatus.HTTP_NO_CONTENT.toInt());

    var piecesResponseBody = verifyGet(String.format(PIECES_ID_PATH, pieceId),headers, APPLICATION_JSON, HttpStatus.HTTP_OK.toInt());
    var piecesResponse = piecesResponseBody.as(Piece.class);

    if (purchaseOrderId.equals(CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL)) {
      assertThat(piecesResponse.getItemId(), notNullValue());
    }

    // Message sent to event bus
    HandlersTestHelper.verifyReceiptStatusUpdateEvent(1);
  }

  @Test
  void testPutPiecesByIdConsistentReceiptStatusTest() throws Exception {
    logger.info("=== Test update piece by id when receipt status is consistent - valid Id 204 ===");

    CompositePoLine poLineForOpenOrder = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(CompositePoLine.class);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(poLineForOpenOrder.getPurchaseOrderId()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    addMockEntry(PURCHASE_ORDER_STORAGE, compositePurchaseOrder);
    addMockEntry(PO_LINES_STORAGE, poLineForOpenOrder);

    String reqData = getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-received-consistent-receipt-status-5b454292-6aaa-474f-9510-b59a564e0c8d2.json");

    verifyPut(String.format(PIECES_ID_PATH, CONSISTENT_RECEIVED_STATUS_PIECE_UUID), reqData, "", 204);

    // Message not sent to event bus
    HandlersTestHelper.verifyReceiptStatusUpdateEvent(0);
  }

  @Test
  void testPutPiecesByNonExistentId() {
    logger.info("=== Test update piece by id - Id does not exists 404 ===");

    Piece reqData = pieceJsonReqData.mapTo(Piece.class);
    reqData.setId(ID_DOES_NOT_EXIST);
    String jsonBody = JsonObject.mapFrom(reqData)
      .encode();

    verifyPut(String.format(PIECES_ID_PATH, ID_DOES_NOT_EXIST), jsonBody, APPLICATION_JSON, 404);
  }

  @Test
  void testPutPiecesWithError() {
    logger.info("=== Test update piece by id - internal error from storage 500 ===");

    Piece reqData = pieceJsonReqData.mapTo(Piece.class);
    reqData.setId(ID_FOR_INTERNAL_SERVER_ERROR);
    String jsonBody = JsonObject.mapFrom(reqData)
      .encode();

    verifyPut(String.format(PIECES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), jsonBody, APPLICATION_JSON, 500);
  }

  @Test
  void shouldNotInvokeItemDeletionButInvokeDeletePieceByIdWhenItemAlreadyDeletedTest() {
    logger.info("=== Test delete piece by id - item has already deleted ===");
    String lineId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(orderId);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(2);
    CompositePoLine poLine = new CompositePoLine().withId(lineId).withPurchaseOrderId(order.getId())
      .withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withLocations(List.of(loc)).withCost(cost)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM));
    order.setCompositePoLines(Collections.singletonList(poLine));
    Title title = new Title().withId(titleId).withTitle("title name");
    Piece piece = new Piece().withFormat(Piece.Format.PHYSICAL).withId(UUID.randomUUID().toString())
      .withHoldingId(holdingId)
      .withItemId(ID_DOES_NOT_EXIST).withPoLineId(poLine.getId())
      .withTitleId(titleId);

    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));
    MockServer.addMockEntry(TITLES, JsonObject.mapFrom(title));

    verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 204);
    assertNull(MockServer.getItemDeletions());
    assertThat(MockServer.getPieceDeletions(), hasSize(1));
  }

  @Test
  void shouldNotDeletePieceAndItemIfGetItemByIdTrowInternalServerErrorTest() {
    logger.info("=== Test delete piece by id - item deletion Internal Server Error ===");

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(order.getId())
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM));
    Piece piece = new Piece().withId(UUID.randomUUID().toString())
                              .withFormat(Piece.Format.PHYSICAL)
                              .withItemId(ID_FOR_INTERNAL_SERVER_ERROR).withPoLineId(poLine.getId());
    order.setCompositePoLines(Collections.singletonList(poLine));
    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));

    verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 500);
    assertNull(MockServer.getItemDeletions());
    assertNull(MockServer.getPieceDeletions());
  }

  @Test
  void deletePieceByIdWithoutItemDeletionTest() {
    logger.info("=== Test delete piece by id - piece without item id ===");
    String lineId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(orderId);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(2);
    CompositePoLine poLine = new CompositePoLine().withId(lineId).withPurchaseOrderId(order.getId())
      .withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withLocations(List.of(loc)).withCost(cost)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.NONE));
    order.setCompositePoLines(Collections.singletonList(poLine));
    Title title = new Title().withId(titleId).withTitle("title name");
    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withPoLineId(poLine.getId()).withTitleId(titleId);

    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));
    MockServer.addMockEntry(TITLES, JsonObject.mapFrom(title));

    verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 204);
    assertThat(MockServer.getItemDeletions(), nullValue());
    assertThat(MockServer.getPieceDeletions(), hasSize(1));
  }

  @Test
  void deletePieceByIdWithItemDeletionTest() {
    logger.info("=== Test delete piece by id - item deleted ===");
    String itemId = UUID.randomUUID().toString();
    JsonObject item = new JsonObject().put(ID, itemId);
    String lineId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();

    CompositePurchaseOrder order = new CompositePurchaseOrder().withId(orderId);
    Location loc = new Location().withHoldingId(holdingId).withQuantityElectronic(1).withQuantity(1);
    Cost cost = new Cost().withQuantityElectronic(2);
    CompositePoLine poLine = new CompositePoLine().withId(lineId).withPurchaseOrderId(order.getId())
      .withIsPackage(false).withPurchaseOrderId(orderId).withId(lineId)
      .withOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withLocations(List.of(loc)).withCost(cost)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM));
    order.setCompositePoLines(Collections.singletonList(poLine));
    Title title = new Title().withId(titleId).withTitle("title name");

    Piece piece = new Piece().withId(UUID.randomUUID().toString()).withFormat(Piece.Format.PHYSICAL)
      .withHoldingId(holdingId).withItemId(itemId).withPoLineId(poLine.getId())
      .withTitleId(titleId);

    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));
    MockServer.addMockEntry(TITLES, JsonObject.mapFrom(title));
    MockServer.addMockEntry(ITEM_RECORDS, item);

    verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 204);
    assertNull(MockServer.getItemDeletions());
    assertThat(MockServer.getPieceDeletions(), hasSize(1));
  }

  @Test
  void deletePieceByIdWithRequestsTest() {
    logger.info("=== Test delete piece by id without requests ===");

    PurchaseOrder order = new PurchaseOrder().withId(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(order.getId())
      .withCost(new Cost().withQuantityElectronic(2));
    Title title = new Title().withId(UUID.randomUUID().toString()).withTitle("title name");
    Piece piece = new Piece().withId(UUID.randomUUID().toString())
      .withItemId("522a501a-56b5-48d9-b28a-3a8f02482d98")
      .withPoLineId(poLine.getId())
      .withTitleId(title.getId());

    MockServer.addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));
    MockServer.addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));
    MockServer.addMockEntry(TITLES, JsonObject.mapFrom(title));

    Errors response = verifyDeleteResponse(String.format(PIECES_ID_PATH, piece.getId()), "", 422).as(Errors.class);
    List<Error> errors = response.getErrors();
    assertThat(errors, hasSize(1));
    Error error = errors.get(0);
    assertThat(error.getCode(), is(REQUEST_FOUND.getCode()));

    assertThat(MockServer.getItemDeletions(), nullValue());
    assertThat(MockServer.getPieceDeletions(), nullValue());
  }

  @Test
  void deletePiecesByIdWithInvalidFormatTest() {
    logger.info("=== Test delete piece by id - bad Id format 400 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_BAD_FORMAT), TEXT_PLAIN, 400);
  }

  @Test
  void deleteNotExistentPieceTest() {
    logger.info("=== Test delete piece by id - id does not exists 404 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_DOES_NOT_EXIST), APPLICATION_JSON, 404);
  }

  @Test
  void deletePieceInternalErrorOnStorageTest() {
    logger.info("=== Test delete piece by id - internal error from storage 500 ===");
    verifyDeleteResponse(String.format(PIECES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), APPLICATION_JSON, 500);
  }
}
