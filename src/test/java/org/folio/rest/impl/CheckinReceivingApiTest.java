package org.folio.rest.impl;

import io.restassured.response.Response;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.Collection;
import java.util.stream.Stream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.jaxrs.model.BindPiecesCollection;
import org.folio.rest.jaxrs.model.BindPiecesResult;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.ExpectCollection;
import org.folio.rest.jaxrs.model.ExpectPiece;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.ProcessingStatus;
import org.folio.rest.jaxrs.model.ReceivedItem;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.model.ReceivingItemResult;
import org.folio.rest.jaxrs.model.ReceivingResult;
import org.folio.rest.jaxrs.model.ReceivingResults;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.rest.jaxrs.model.ToBeExpected;
import org.folio.rest.jaxrs.model.ToBeReceived;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_ECS;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ORDERS_BIND_ENDPOINT;
import static org.folio.TestConstants.ORDERS_BIND_ID_ENDPOINT;
import static org.folio.TestConstants.ORDERS_CHECKIN_ENDPOINT;
import static org.folio.TestConstants.ORDERS_EXPECT_ENDPOINT;
import static org.folio.TestConstants.ORDERS_RECEIVING_ENDPOINT;
import static org.folio.TestUtils.getMinimalContentBindItem;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.TestUtils.getMinimalContentPiece;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.TestUtils.getTitle;
import static org.folio.models.ItemStatus.ORDER_CLOSED;
import static org.folio.orders.events.handlers.HandlersTestHelper.verifyCheckinOrderStatusUpdateEvent;
import static org.folio.orders.events.handlers.HandlersTestHelper.verifyOrderStatusUpdateEvent;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.RestConstants.VALIDATION_ERROR;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_NOT_RETRIEVED;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.rest.core.exceptions.ErrorCodes.LOC_NOT_PROVIDED;
import static org.folio.rest.core.exceptions.ErrorCodes.MULTIPLE_NONPACKAGE_TITLES;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECES_HAVE_DIFFERENT_RECEIVING_TENANT_IDS;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECES_MUST_HAVE_RECEIVED_STATUS;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_ALREADY_RECEIVED;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_NOT_RETRIEVED;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_POL_MISMATCH;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_UPDATE_FAILED;
import static org.folio.rest.core.exceptions.ErrorCodes.REQUESTS_ACTION_REQUIRED;
import static org.folio.rest.core.exceptions.ErrorCodes.TITLE_NOT_FOUND;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL_MULTIPLE_ITEMS;
import static org.folio.rest.impl.MockServer.CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL_SINGLE_ITEM;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.PO_LINES_COLLECTION;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.addMockTitles;
import static org.folio.rest.impl.MockServer.getCreatedHoldings;
import static org.folio.rest.impl.MockServer.getCreatedItems;
import static org.folio.rest.impl.MockServer.getItemUpdates;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPieceUpdates;
import static org.folio.rest.impl.MockServer.getPoLineBatchUpdates;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.folio.rest.impl.MockServer.getPurchaseOrderRetrievals;
import static org.folio.rest.impl.MockServer.getUpdatedTitles;
import static org.folio.rest.jaxrs.model.ProcessingStatus.Type.SUCCESS;
import static org.folio.rest.jaxrs.model.ReceivedItem.ItemStatus.ON_ORDER;
import static org.folio.service.inventory.InventoryItemManager.COPY_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ACCESSION_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_CHRONOLOGY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISCOVERY_SUPPRESS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISPLAY_SUMMARY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ENUMERATION;
import static org.folio.service.inventory.InventoryItemManager.ITEM_HOLDINGS_RECORD_ID;
import static org.folio.service.inventory.InventoryUtils.ITEMS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.in;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CheckinReceivingApiTest {
  private static final Logger logger = LogManager.getLogger();

  private static final String RECEIVING_RQ_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "receiving/";
  private static final String CHECKIN_RQ_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "checkIn/";
  private static final String ITEM_BARCODE = "barcode";
  private static final String ITEM_LEVEL_CALL_NUMBER = "itemLevelCallNumber";
  private static final String HOLDING_PERMANENT_LOCATION_ID = "permanentLocationId";
  private static final String ITEM_STATUS_NAME = "name";
  private static final String ITEM_STATUS = "status";
  private static final String COMPOSITE_POLINE_ONGOING_ID = "6e2b169a-ebeb-4c3c-a2f2-6233ff9c59ae";
  private static final String COMPOSITE_POLINE_CANCELED_ID = "1196fcd9-7607-447d-ae85-6e91883d7e4f";
  private static final String OPEN_REQUEST_ITEM_ID = "f972fa0e-5e84-4a47-a27b-137724a73fee";

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
  void testPostCheckInElectronicWithNoItems() {
    logger.info("=== Test POST Checkin - CheckIn Electronic resource");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(7).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

    CheckinCollection checkInRq = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + "checkin-pe-mix-2-electronic-resources.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkInRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkInRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(getItemsSearches(), is(nullValue()));
    assertThat(getItemUpdates(), is(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(checkInRq.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(checkInRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polUpdates, hasSize(pieceIdsByPol.size()));

    JsonArray poLinesJson = polUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    }

    // Verify message is sent via event bus
    verifyCheckinOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostCheckInPhysicalWithMissingItem() {
    logger.info("=== Test POST Checkin - CheckIn physical resource with only one item updated");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(7).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

    CheckinCollection checkInRq = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + "checkin-pe-mix-2-physical-resources.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkInRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkInRq.getTotalRecords()));
    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(is(emptyString())));
      assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
      assertThat(receivingItemResult.getProcessingStatus().getType(), is(SUCCESS));
      assertThat(receivingItemResult.getProcessingStatus().getError(), nullValue());
    }

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> itemUpdates = getItemUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    assertThat(getPieceSearches(), hasSize(2));
    assertThat(pieceUpdates, hasSize(2));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(2));
    assertThat(polUpdates, hasSize(1));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(CheckInPiece.ItemStatus.IN_PROCESS.value()));
    });

    for (int i = 0; i < itemUpdates.size(); i++) {
      JsonObject item = itemUpdates.get(i);
      CheckInPiece piece = checkInRq.getToBeCheckedIn().get(0).getCheckInPieces().get(i);
      assertEquals(piece.getDisplaySummary(), item.getString(ITEM_DISPLAY_SUMMARY));
      assertEquals(piece.getEnumeration(), item.getString(ITEM_ENUMERATION));
      assertEquals(piece.getCopyNumber(), item.getString(COPY_NUMBER));
      assertEquals(piece.getChronology(), item.getString(ITEM_CHRONOLOGY));
      assertEquals(piece.getBarcode(), item.getString(ITEM_BARCODE));
      assertEquals(piece.getAccessionNumber(), item.getString(ITEM_ACCESSION_NUMBER));
      assertEquals(piece.getCallNumber(), item.getString(ITEM_LEVEL_CALL_NUMBER));
      assertEquals(piece.getDiscoverySuppress(), item.getBoolean(ITEM_DISCOVERY_SUPPRESS));
    }

    JsonArray poLinesJson = polUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    }

    // Verify message is sent via event bus
    verifyCheckinOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostCheckinRevertPhysicalResource() {
    logger.info("=== Test POST Check-in - Revert received Physical resource");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(8).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

    CheckinCollection checkinReq = getMockAsJson(
      CHECKIN_RQ_MOCK_DATA_PATH + "revert-checkin-physical-1-resource.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkinReq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkinReq.getTotalRecords()));

    verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> itemUpdates = getItemUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polBatchUpdates, not(nullValue()));

    // The piece searches should be made 2 times: 1st time to get piece record,
    // 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(2));
    assertThat(pieceUpdates, hasSize(1));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(2));
    assertThat(polBatchUpdates, hasSize(1));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), is(nullValue()));
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ON_ORDER.value()));
    });
    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.AWAITING_RECEIPT));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    }

    // Verify message is sent via event bus
    verifyCheckinOrderStatusUpdateEvent(1);
  }

  @Test
  void testReceiveOngoingOrderWithChangedLocation() {
    logger.info("=== Test POST Receive - Ongoing PO Lines");

    PoLine originalPoLine = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(9).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(originalPoLine));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-ongoing.json").mapTo(ReceivingCollection.class);
    receivingRq.getToBeReceived().get(0).setPoLineId(COMPOSITE_POLINE_ONGOING_ID);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));

    assertThat(polSearches, not(nullValue()));

    // The piece searches should be made 2 times: 1st time to get all required piece records and second - get pieces by po line
    assertThat(pieceSearches, hasSize(2));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));

    // check no status updates were performed, but location was updated
    assertThat(polBatchUpdates, hasSize(1));
    polBatchUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLineCollection.class).getPoLines().get(0);
      assertThat(poLine.getCheckinItems(), is(false));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.ONGOING));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));

      Location originalPolineLocation = originalPoLine.getLocations().get(0);
      Location updatePolineLocation = poLine.getLocations().get(0);
      assertThat(originalPolineLocation.getLocationId(), not(updatePolineLocation.getLocationId()));
    });

    // Verify no status updated for ongoing order
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testReceiveCancelledOrderWithChangedLocation() {
    logger.info("=== Test POST Receive - Cancelled PO Lines");

    PoLine originalPoLine = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(10).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(originalPoLine));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-cancelled.json").mapTo(ReceivingCollection.class);
    receivingRq.getToBeReceived().get(0).setPoLineId(COMPOSITE_POLINE_CANCELED_ID);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));

    assertThat(polSearches, not(nullValue()));

    // The piece searches should be made 2 times: 1st time to get all required piece records and second - get pieces by po line
    assertThat(pieceSearches, hasSize(2));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));

    // check no status updates were performed and POL remained canceled
    assertThat(polBatchUpdates, hasSize(1));
    polBatchUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLineCollection.class).getPoLines().get(0);
      assertThat(poLine.getCheckinItems(), is(false));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.CANCELLED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));

      Location originalPolineLocation = originalPoLine.getLocations().get(0);
      Location updatePolineLocation = poLine.getLocations().get(0);
      assertThat(originalPolineLocation.getLocationId(), not(updatePolineLocation.getLocationId()));
    });

    // Verify no status updated for ongoing order
    verifyOrderStatusUpdateEvent(0);
  }


  @Test
  void testPostCheckInLocationId() {
    logger.info("=== Test POST Checkin - locationId checking ===");

    String poLineId = "fe47e95d-24e9-4a9a-9dc0-bcba64b51f56";
    String pieceId = UUID.randomUUID().toString();
    PoLine poLine = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine));

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(new ToBeCheckedIn()
      .withCheckedIn(1)
      .withPoLineId(poLineId)
      .withCheckInPieces(Arrays.asList(new CheckInPiece().withId(pieceId).withItemStatus(CheckInPiece.ItemStatus.ON_ORDER),
        new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.IN_PROCESS))));

    CheckinCollection request = new CheckinCollection()
      .withToBeCheckedIn(toBeCheckedInList)
      .withTotalRecords(2);

    String physicalPieceWithoutLocationId = "90894300-5285-4d83-80f4-76cf621e555e";
    String electronicPieceWithoutLocationId = "1a247602-c51a-4221-9a07-27d075d03625";

    // Positive cases:

    // 1. Both CheckInPiece with locationId
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setId(physicalPieceWithoutLocationId);
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(UUID.randomUUID().toString());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setId(electronicPieceWithoutLocationId);
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setLocationId(UUID.randomUUID().toString());

    checkResultWithErrors(request, 0);
    assertThat(getPieceSearches(), hasSize(2));
    assertThat(getPieceUpdates(), hasSize(2));
    assertThat(getPoLineSearches(), hasSize(2));
    assertThat(getPoLineBatchUpdates(), hasSize(1));
    verifyCheckinOrderStatusUpdateEvent(1);


    // Negative cases:
    // 1. One CheckInPiece and corresponding Piece without locationId
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(null);

    clearServiceInteractions();

    MockServer.addMockOrderData(Collections.singletonList(poLine));

    checkResultWithErrors(request, 1);
    assertThat(getPieceSearches(), hasSize(2));
    assertThat(getPieceUpdates(), hasSize(1));
    assertThat(getPoLineSearches(), hasSize(2));
    assertThat(getPoLineBatchUpdates(), hasSize(1));
    verifyCheckinOrderStatusUpdateEvent(1);


    // 2. All CheckInPieces and corresponding Pieces without locationId
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(null);
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setLocationId(null);

    clearServiceInteractions();
    MockServer.addMockOrderData(Collections.singletonList(poLine));
    checkResultWithErrors(request, 2);
    assertThat(getPieceSearches(), hasSize(1));
    assertThat(getPieceUpdates(), nullValue());
    assertThat(getPoLineSearches(), hasSize(2));
    assertThat(getPoLineUpdates(), nullValue());
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testPostReceivingPhysicalAll() {
    logger.info("=== Test POST Receiving - Receive physical resources ===");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(2).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-all-resources.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> itemUpdates = getItemUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polBatchUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receivingRq.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(itemsSearches, hasSize(expectedSearchRqQty));
    assertThat(itemUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      assertThat(item.getString(ITEM_BARCODE), not(is(emptyString())));
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.IN_PROCESS.value()));
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), not(is(emptyString())));
    });
    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.FULLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    }

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostCheckinElectronicPhysicalChangeLocationIdNewHoldingIsCreatedForPhysicalPiece() {

    logger.info("=== Test POST check-in - Check-in physical and electronic resource with new locationId ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    PoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setOrderFormat(PoLine.OrderFormat.P_E_MIX);
    poLine.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE)); // holding mustn't be created
    poLine.setPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING)); // holding must be created


    String locationForPhysical = UUID.randomUUID().toString();
    String locationForElectronic = UUID.randomUUID().toString();

    String titleId = UUID.randomUUID().toString();
    MockServer.addMockTitleWithId(poLine, titleId);

    Piece physicalPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.EXPECTED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.PHYSICAL)
      .withLocationId(locationForPhysical)
      .withId(UUID.randomUUID().toString())
      .withTitleId(titleId)
      .withItemId(UUID.randomUUID().toString());
    Piece electronicPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.EXPECTED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC)
      .withId(UUID.randomUUID().toString())
      .withTitleId(titleId)
      .withItemId(UUID.randomUUID().toString());

    addMockEntry(PURCHASE_ORDER_STORAGE, order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN));
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, physicalPiece);
    addMockEntry(PIECES_STORAGE, electronicPiece);

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(new ToBeCheckedIn()
      .withCheckedIn(1)
      .withPoLineId(poLine.getId())
      .withCheckInPieces(Arrays.asList(new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER), new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER))));

    CheckinCollection request = new CheckinCollection()
      .withToBeCheckedIn(toBeCheckedInList)
      .withTotalRecords(2);

    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setId(physicalPiece.getId());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(locationForPhysical);
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setId(electronicPiece.getId());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setLocationId(locationForElectronic);

    checkResultWithErrors(request, 0);
    assertThat(getCreatedHoldings(), hasSize(1));

    assertThat(getCreatedHoldings().get(0).getString(HOLDING_PERMANENT_LOCATION_ID), is(locationForPhysical));

  }

  @Test
  void testPostCheckInPhysicalFullyReceivedWithChangedLocation() {
    logger.info("=== Test POST check-in - Check-in Fully Received physical resource with changed location ===");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(11).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

    CheckinCollection checkinCollection = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + "checkin-fully-receive-physical-resource.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkinCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkinCollection.getTotalRecords()));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
    assertThat(receivingResult.getProcessedSuccessfully(), is(1));
    assertThat(receivingResult.getProcessedWithError(), is(0));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(getItemsSearches(), not(nullValue()));
    assertThat(getItemUpdates(), is(nullValue()));
    assertThat(polSearches, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(checkinCollection.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(checkinCollection.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine newPoLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      logger.info("POL location before checkIn: {}", JsonArray.of(newPoLine.getLocations()).encodePrettily());
      logger.info("POL location after checkIn: {}", JsonArray.of(newPoLine.getLocations()).encodePrettily());
      Location originalPolineLocation = poLine1.getLocations().get(0);
      Location updatePolineLocation = newPoLine.getLocations().get(0);
      assertThat(originalPolineLocation.getLocationId(), nullValue());
      assertThat(updatePolineLocation.getLocationId(), nullValue());
      assertThat(originalPolineLocation.getHoldingId(), not(updatePolineLocation.getHoldingId()));
      assertThat(newPoLine.getReceiptStatus(), is(PoLine.ReceiptStatus.FULLY_RECEIVED));
      assertThat(newPoLine.getReceiptDate(), not(nullValue()));
    }

    verifyCheckinOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostReceivePhysicalCancelledOrder() {
    logger.info("=== Test POST Receive - Check-in Fully Received physical resource with cancelled order ===");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(14).mapTo(PoLine.class);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(poLine1.getPurchaseOrderId()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);
    addMockEntry(PURCHASE_ORDER_STORAGE, compositePurchaseOrder);
    addMockEntry(PO_LINES_STORAGE, poLine1);
    MockServer.addMockTitles(Collections.singletonList(poLine1));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-resource-cancelled-order.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> itemUpdates = getItemUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polBatchUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receivingRq.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(itemsSearches, hasSize(expectedSearchRqQty));
    assertThat(itemUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      assertThat(item.getString(ITEM_BARCODE), not(is(emptyString())));
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.ORDER_CLOSED.value()));
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), not(is(emptyString())));
    });

    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getPaymentStatus(), is(PoLine.PaymentStatus.CANCELLED));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.CANCELLED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    }

    // Verify no status updated for cancelled order
    verifyOrderStatusUpdateEvent(0);
  }

  private static Stream<Arguments> testPostReceivePhysicalCancelledOrderLineArgs() {
    return Stream.of(
      Arguments.of(1, 15, PoLine.PaymentStatus.AWAITING_PAYMENT, PoLine.ReceiptStatus.AWAITING_RECEIPT),
      Arguments.of(2, 16, PoLine.PaymentStatus.CANCELLED, PoLine.ReceiptStatus.CANCELLED)
    );
  }

  @ParameterizedTest
  @MethodSource("testPostReceivePhysicalCancelledOrderLineArgs")
  void testPostReceivePhysicalCancelledOrderLine(int fileIdx, int poLineIdx, PoLine.PaymentStatus paymentStatus, PoLine.ReceiptStatus receiptStatus) {
    logger.info("=== Test POST Receive - Check-in Fully Received physical resource with cancelled order line ===");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(poLineIdx).mapTo(PoLine.class);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(poLine1.getPurchaseOrderId()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    addMockEntry(PURCHASE_ORDER_STORAGE, compositePurchaseOrder);
    addMockEntry(PO_LINES_STORAGE, poLine1);
    MockServer.addMockTitles(Collections.singletonList(poLine1));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + String.format("receive-physical-resource-cancelled-order-line-%d.json", fileIdx)).mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> itemUpdates = getItemUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polBatchUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receivingRq.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;
    boolean isPoLineCancelled = poLine1.getReceiptStatus() == PoLine.ReceiptStatus.CANCELLED;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(itemsSearches, hasSize(expectedSearchRqQty));
    assertThat(itemUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      logger.info("Item with from a cancelled purchase order: {}", item.encodePrettily());
      assertThat(item.getString(ITEM_BARCODE), not(is(emptyString())));
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(isPoLineCancelled ? ORDER_CLOSED.value() : ON_ORDER.value()));
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), not(is(emptyString())));
    });

    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(receiptStatus));
      assertThat(poLine.getPaymentStatus(), is(paymentStatus));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    }

    // Verify no status updated for cancelled order but is updated for other POL statuses
    verifyOrderStatusUpdateEvent(isPoLineCancelled ? 0 : 1);
  }

  @Test
  void testPostCheckInPhysicalFullyReceivedCancelledOrder() {
    logger.info("=== Test POST check-in - Check-in Fully Received physical resource with cancelled order ===");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(14).mapTo(PoLine.class);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(poLine1.getPurchaseOrderId()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);
    addMockEntry(PURCHASE_ORDER_STORAGE, compositePurchaseOrder);
    addMockEntry(PO_LINES_STORAGE, poLine1);
    MockServer.addMockTitles(Collections.singletonList(poLine1));

    CheckinCollection checkinCollection = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + "checkin-fully-receive-physical-resource-cancelled-order.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkinCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkinCollection.getTotalRecords()));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
    assertThat(receivingResult.getProcessedSuccessfully(), is(1));
    assertThat(receivingResult.getProcessedWithError(), is(0));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> purchaseOrderRetrievals = getPurchaseOrderRetrievals();
    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();
    List<JsonObject> itemUpdates = getItemUpdates();

    assertThat(purchaseOrderRetrievals, not(nullValue()));
    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(getItemsSearches(), not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(checkinCollection.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(purchaseOrderRetrievals, hasSize(1));
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(checkinCollection.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      Location originalPolineLocation = poLine.getLocations().get(0);
      Location updatePolineLocation = poLine.getLocations().get(0);
      assertThat(originalPolineLocation.getLocationId(), nullValue());
      assertThat(updatePolineLocation.getLocationId(), nullValue());
      assertThat(originalPolineLocation.getHoldingId(), is(updatePolineLocation.getHoldingId()));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.CANCELLED));
      assertThat(poLine.getPaymentStatus(), is(PoLine.PaymentStatus.CANCELLED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    }

    List<Piece> pieces = pieceUpdates.stream().map(json -> JsonObject.mapFrom(json).mapTo(Piece.class)).toList();
    for (Piece piece : pieces) {
      JsonObject itemJson = itemUpdates.stream().filter(json -> json.getString(ID).equals(piece.getItemId())).findFirst().orElseThrow();
      assertThat(itemJson.getString(ITEM_HOLDINGS_RECORD_ID), is(piece.getHoldingId()));
      assertThat(itemJson.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), is(CheckInPiece.ItemStatus.IN_PROCESS.value()));
    }

    // Verify no status updated for cancelled order
    verifyCheckinOrderStatusUpdateEvent(0);
  }

  private static Stream<Arguments> testPostCheckInPhysicalFullyReceivedCancelledOrderLineArgs() {
    return Stream.of(
      Arguments.of(1, 15, PoLine.PaymentStatus.AWAITING_PAYMENT, PoLine.ReceiptStatus.PARTIALLY_RECEIVED),
      Arguments.of(2, 16, PoLine.PaymentStatus.CANCELLED, PoLine.ReceiptStatus.CANCELLED)
    );
  }

  @ParameterizedTest
  @MethodSource("testPostCheckInPhysicalFullyReceivedCancelledOrderLineArgs")
  void testPostCheckInPhysicalFullyReceivedCancelledOrderLine(int fileIdx, int poLineIdx, PoLine.PaymentStatus paymentStatus, PoLine.ReceiptStatus receiptStatus) {
    logger.info("=== Test POST check-in - Check-in Fully Received physical resource with cancelled order line ===");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(poLineIdx).mapTo(PoLine.class);
    CompositePurchaseOrder compositePurchaseOrder = new CompositePurchaseOrder().withId(poLine1.getPurchaseOrderId()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    addMockEntry(PURCHASE_ORDER_STORAGE, compositePurchaseOrder);
    addMockEntry(PO_LINES_STORAGE, poLine1);
    MockServer.addMockTitles(Collections.singletonList(poLine1));

    CheckinCollection checkinCollection = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + String.format("checkin-fully-receive-physical-resource-cancelled-order-line-%d.json", fileIdx)).mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkinCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkinCollection.getTotalRecords()));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
    assertThat(receivingResult.getProcessedSuccessfully(), is(1));
    assertThat(receivingResult.getProcessedWithError(), is(0));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> purchaseOrderRetrievals = getPurchaseOrderRetrievals();
    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();
    List<JsonObject> itemUpdates = getItemUpdates();

    assertThat(purchaseOrderRetrievals, not(nullValue()));
    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(getItemsSearches(), not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(checkinCollection.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;
    boolean isPoLineCancelled = poLine1.getReceiptStatus() == PoLine.ReceiptStatus.CANCELLED;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(purchaseOrderRetrievals, hasSize(1));
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(checkinCollection.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      Location originalPolineLocation = poLine.getLocations().get(0);
      Location updatePolineLocation = poLine.getLocations().get(0);
      assertThat(originalPolineLocation.getLocationId(), nullValue());
      assertThat(updatePolineLocation.getLocationId(), nullValue());
      assertThat(originalPolineLocation.getHoldingId(), is(updatePolineLocation.getHoldingId()));
      assertThat(poLine.getReceiptStatus(), is(receiptStatus));
      assertThat(poLine.getPaymentStatus(), is(paymentStatus));
      assertThat(poLine.getReceiptDate(), isPoLineCancelled ? is(nullValue()) : not(nullValue()));
    }

    List<Piece> pieces = pieceUpdates.stream().map(json -> JsonObject.mapFrom(json).mapTo(Piece.class)).toList();
    for (Piece piece : pieces) {
      JsonObject itemJson = itemUpdates.stream().filter(json -> json.getString(ID).equals(piece.getItemId())).findFirst().orElseThrow();
      assertThat(itemJson.getString(ITEM_HOLDINGS_RECORD_ID), is(piece.getHoldingId()));
      assertThat(itemJson.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), is(CheckInPiece.ItemStatus.IN_PROCESS.value()));
    }

    // Verify no status updated for cancelled order but is updated for other POL statuses
    verifyCheckinOrderStatusUpdateEvent(isPoLineCancelled ? 0 : 1);
  }

  private static Stream<Arguments> testPostCheckInPhysicalFullyReceivedEcsArgs() {
    return Stream.of(
      Arguments.of(CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL_SINGLE_ITEM, "checkin-fully-receive-physical-resource-ecs-single.json", 1, 4, 2, 1),
      Arguments.of(CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL_MULTIPLE_ITEMS, "checkin-fully-receive-physical-resource-ecs-multiple.json", 2, 4, 2, 2)
    );
  }

  @ParameterizedTest
  @MethodSource("testPostCheckInPhysicalFullyReceivedEcsArgs")
  void testPostCheckInPhysicalFullyReceivedEcs(String orderId, String checkInCollectionPath, int processedCount, int poLineCount, int orderCount, int locationCount) {
    logger.info("=== Test POST check-in - Check-in Fully Received physical resource with changed affiliation in a ECS environment ===");

    CompositePurchaseOrder purchaseOrder = new CompositePurchaseOrder().withId(orderId).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    addMockEntry(PURCHASE_ORDER_STORAGE, purchaseOrder);

    PoLine poLine = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines")
      .stream()
      .map(json -> (JsonObject) json)
      .filter(json -> json.getString("purchaseOrderId").equals(purchaseOrder.getId()))
      .findFirst()
      .orElseThrow()
      .mapTo(PoLine.class);
    addMockEntry(PO_LINES_STORAGE, poLine);
    MockServer.addMockTitles(Collections.singletonList(poLine));

    CheckinCollection checkinCollection = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + checkInCollectionPath).mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkinCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_ECS), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkinCollection.getTotalRecords()));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
    assertThat(receivingResult.getProcessedSuccessfully(), is(processedCount));
    assertThat(receivingResult.getProcessedWithError(), is(0));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> purchaseOrderSearches = getPurchaseOrderRetrievals();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> itemUpdates = getItemUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(checkinCollection.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;
    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(checkinCollection.getTotalRecords()));
    // Should be >1 due to an extra call performed in CheckinHelper.createItemsWithPieceUpdate per CheckInPiece
    assertThat(polSearches, hasSize(poLineCount));
    assertThat(purchaseOrderSearches, hasSize(orderCount));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLineAfterReceive = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      logger.info("POL location before ECS checkIn: {}", JsonArray.of(poLine.getLocations()).encodePrettily());
      logger.info("POL location after ECS checkIn: {}", JsonArray.of(poLineAfterReceive.getLocations()).encodePrettily());

      assertThat(poLine.getLocations().size(), is(locationCount));
      assertThat(poLineAfterReceive.getLocations().size(), is(locationCount));
      for (int j = 0; j < poLine.getLocations().size(); j++) {
        Location oldLocation = poLine.getLocations().get(j);
        Location newLocation = poLineAfterReceive.getLocations().get(j);
        assertThat(oldLocation.getLocationId(), nullValue());
        assertThat(newLocation.getLocationId(), nullValue());
        assertThat(oldLocation.getHoldingId(), not(nullValue()));
        assertThat(newLocation.getHoldingId(), not(nullValue()));
        assertThat(oldLocation.getTenantId(), is("university"));
        assertThat(newLocation.getTenantId(), is("college"));
        assertThat(oldLocation.getTenantId(), not(newLocation.getTenantId()));
        assertThat(oldLocation.getHoldingId(), not(newLocation.getHoldingId()));
        assertThat(pieceUpdates.stream().anyMatch(piece -> piece.getString("holdingId").equals(newLocation.getHoldingId())), is(true));
      }

      assertThat(poLineAfterReceive.getReceiptStatus(), is(PoLine.ReceiptStatus.FULLY_RECEIVED));
      assertThat(poLineAfterReceive.getReceiptDate(), not(nullValue()));
    }

    pieceUpdates.forEach(pieceJson -> {
      logger.info("Piece: {}", JsonObject.mapFrom(pieceJson).encodePrettily());
      Piece piece = pieceJson.mapTo(Piece.class);
      assertThat(piece.getReceivingTenantId(), is("college"));
      assertThat(piece.getReceivingStatus(), is(Piece.ReceivingStatus.RECEIVED));
    });

    verifyCheckinOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostCheckinForPackagePOL() {
    logger.info("=== Test POST check-in - Package POL ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    PoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setIsPackage(true);
    poLine.setOrderFormat(PoLine.OrderFormat.P_E_MIX);
    poLine.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM));
    poLine.setPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM));

    String locationForPhysical = UUID.randomUUID().toString();
    String locationForElectronic = UUID.randomUUID().toString();

    String titleIdForPhysical = UUID.randomUUID().toString();
    MockServer.addMockTitleWithId(poLine, titleIdForPhysical);
    String titleIdForElectronic = UUID.randomUUID().toString();
    MockServer.addMockTitleWithId(poLine, titleIdForElectronic);

    Piece physicalPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.CLAIM_DELAYED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.PHYSICAL)
      .withLocationId(locationForPhysical)
      .withId(UUID.randomUUID().toString())
      .withTitleId(titleIdForPhysical)
      .withItemId(UUID.randomUUID().toString());
    Piece electronicPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.CLAIM_SENT)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC)
      .withId(UUID.randomUUID().toString())
      .withTitleId(titleIdForElectronic)
      .withItemId(UUID.randomUUID().toString());

    addMockEntry(PURCHASE_ORDER_STORAGE, order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN));
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, physicalPiece);
    addMockEntry(PIECES_STORAGE, electronicPiece);

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(new ToBeCheckedIn()
      .withCheckedIn(2)
      .withPoLineId(poLine.getId())
      .withCheckInPieces(Arrays.asList(new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER),
        new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER))));

    CheckinCollection request = new CheckinCollection()
      .withToBeCheckedIn(toBeCheckedInList)
      .withTotalRecords(2);

    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setId(physicalPiece.getId());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(locationForPhysical);
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setId(electronicPiece.getId());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setLocationId(locationForElectronic);

    checkResultWithErrors(request, 0);
  }

  @Test
  void testPostCheckinForNewTenant() {
    logger.info("=== Test POST check-in - New Tenant ===");

    String tenant = "test";

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    PoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setIsPackage(true);
    poLine.setOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE);
    poLine.setPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM));
    Location location = new Location()
      .withTenantId(tenant)
      .withQuantity(1)
      .withQuantityPhysical(1);
    poLine.setLocations(List.of(location));

    var itemId = UUID.randomUUID().toString();

    JsonObject minimalItem = new JsonObject()
      .put("id", itemId)
      .put("purchaseOrderLineIdentifier", poLine.getId());

    String locationForPhysical = UUID.randomUUID().toString();

    String titleIdForPhysical = UUID.randomUUID().toString();
    MockServer.addMockTitleWithId(poLine, titleIdForPhysical);
    String titleIdForElectronic = UUID.randomUUID().toString();
    MockServer.addMockTitleWithId(poLine, titleIdForElectronic);

    Piece physicalPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.CLAIM_DELAYED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.PHYSICAL)
      .withLocationId(locationForPhysical)
      .withId(UUID.randomUUID().toString())
      .withTitleId(titleIdForPhysical)
      .withItemId(itemId);

    addMockEntry(PURCHASE_ORDER_STORAGE, order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN));
    addMockEntry(ITEMS, minimalItem);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, physicalPiece);

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(new ToBeCheckedIn()
      .withCheckedIn(1)
      .withPoLineId(poLine.getId())
      .withCheckInPieces(List.of(
        new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER))
      ));

    CheckinCollection request = new CheckinCollection()
      .withToBeCheckedIn(toBeCheckedInList)
      .withTotalRecords(2);

    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setReceivingTenantId(tenant);
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setId(physicalPiece.getId());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(locationForPhysical);

    checkResultWithErrors(request, 0);
  }

  @Test
  void testPostCheckinMultipleTitlesError() {
    logger.info("=== Test POST check-in multiple titles error for non-packages ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    PoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setOrderFormat(PoLine.OrderFormat.P_E_MIX);
    poLine.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM));
    poLine.setPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM));

    String locationForPhysical = UUID.randomUUID().toString();
    String locationForElectronic = UUID.randomUUID().toString();

    String titleIdForPhysical = UUID.randomUUID().toString();
    MockServer.addMockTitleWithId(poLine, titleIdForPhysical);
    String titleIdForElectronic = UUID.randomUUID().toString();
    MockServer.addMockTitleWithId(poLine, titleIdForElectronic);

    Piece physicalPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.CLAIM_DELAYED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.PHYSICAL)
      .withLocationId(locationForPhysical)
      .withId(UUID.randomUUID().toString())
      .withTitleId(titleIdForPhysical)
      .withItemId(UUID.randomUUID().toString());
    Piece electronicPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.CLAIM_SENT)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC)
      .withId(UUID.randomUUID().toString())
      .withTitleId(titleIdForElectronic)
      .withItemId(UUID.randomUUID().toString());

    addMockEntry(PURCHASE_ORDER_STORAGE, order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN));
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, physicalPiece);
    addMockEntry(PIECES_STORAGE, electronicPiece);

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(new ToBeCheckedIn()
      .withCheckedIn(2)
      .withPoLineId(poLine.getId())
      .withCheckInPieces(Arrays.asList(new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER),
        new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER))));

    CheckinCollection request = new CheckinCollection()
      .withToBeCheckedIn(toBeCheckedInList)
      .withTotalRecords(2);

    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setId(physicalPiece.getId());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(locationForPhysical);
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setId(electronicPiece.getId());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setLocationId(locationForElectronic);

    Response response = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(request).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_BAD_REQUEST.toInt());
    assertThat(response.as(Errors.class).getErrors().get(0).getMessage(), is(MULTIPLE_NONPACKAGE_TITLES.getDescription()));
  }

  @Test
  void testPostCheckinTitleNotFoundError() {
    logger.info("=== Test POST check-in title not found error ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    PoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setIsPackage(true);
    poLine.setOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    poLine.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM));

    String locationForElectronic = UUID.randomUUID().toString();

    Piece electronicPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.EXPECTED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC)
      .withId(UUID.randomUUID().toString())
      .withTitleId(UUID.randomUUID().toString())
      .withItemId(UUID.randomUUID().toString());

    addMockEntry(PURCHASE_ORDER_STORAGE, order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN));
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, electronicPiece);

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(new ToBeCheckedIn()
      .withCheckedIn(1)
      .withPoLineId(poLine.getId())
      .withCheckInPieces(Collections.singletonList(new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER))));

    CheckinCollection request = new CheckinCollection()
      .withToBeCheckedIn(toBeCheckedInList)
      .withTotalRecords(1);

    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setId(electronicPiece.getId());
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(locationForElectronic);

    Response response = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(request).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_BAD_REQUEST.toInt());
    assertThat(response.as(Errors.class).getErrors().get(0).getMessage(), is(TITLE_NOT_FOUND.getDescription()));
  }

  @Test
  void testPostReceivingElectronicPartially() {
    logger.info("=== Test POST Receiving - Receive partially electronic resources");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(4).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

    // 10 pieces in total with PoLineId daa9cfd1-b330-4b65-8c2a-3663aaae5130
    // 5/10 are received, 5 are expected but not received, expected receipt status is "Partially Received"
    ReceivingCollection receiving = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-electronic-5-of-10-resources-no-items.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receiving).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receiving.getTotalRecords()));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
    assertThat(receivingResult.getProcessedSuccessfully(), is(5));
    assertThat(receivingResult.getProcessedWithError(), is(0));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(getItemsSearches(), is(nullValue()));
    assertThat(getItemUpdates(), is(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polBatchUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receiving.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(receiving.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    }

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostReceivingPhysicalWithErrors() throws IOException {
    logger.info("=== Test POST Receiving - Receive physical resources with different errors");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-resources-6-of-10-with-errors.json").mapTo(ReceivingCollection.class);
    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(3).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));
    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(receivingRq.getTotalRecords()));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
    assertThat(receivingResult.getProcessedSuccessfully(), is(5));
    assertThat(receivingResult.getProcessedWithError(), is(5));

    Set<String> errorCodes = new HashSet<>();
    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(is(emptyString())));
      assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
      if (receivingItemResult.getProcessingStatus().getType() == SUCCESS) {
        assertThat(receivingItemResult.getProcessingStatus().getError(), nullValue());
      } else {
        assertThat(receivingItemResult.getProcessingStatus().getError(), not(nullValue()));
        errorCodes.add(receivingItemResult.getProcessingStatus().getError().getCode());
      }
    }

    assertThat(errorCodes, containsInAnyOrder(PIECE_ALREADY_RECEIVED.getCode(),
      PIECE_POL_MISMATCH.getCode(), PIECE_NOT_FOUND.getCode(), ITEM_UPDATE_FAILED.getCode(),
      PIECE_UPDATE_FAILED.getCode()));

    List<JsonObject> itemUpdates = getItemUpdates();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();
    assertThat(getPoLineSearches(), hasSize(2));
    assertThat(polBatchUpdates, hasSize(1));
    assertThat(itemUpdates, hasSize(6));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.IN_PROCESS.value()));
    });

    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    }

    verifyProperQuantityOfHoldingsCreated(receivingRq);

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testMovePieceStatusFromUnreceivableToExpected() {
    logger.info("=== Test POST Expect");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    PoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setIsPackage(true);
    poLine.setOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    poLine.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM));

    Piece electronicPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.UNRECEIVABLE)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC)
      .withId(UUID.randomUUID().toString())
      .withTitleId(UUID.randomUUID().toString())
      .withItemId(UUID.randomUUID().toString());

    addMockEntry(PURCHASE_ORDER_STORAGE, order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN));
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, electronicPiece);

    List<ToBeExpected> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(new ToBeExpected()
      .withExpected(1)
      .withPoLineId(poLine.getId())
      .withExpectPieces(Collections.singletonList(new ExpectPiece().withId(electronicPiece.getId()).withComment("test"))));

    ExpectCollection request = new ExpectCollection()
      .withToBeExpected(toBeCheckedInList)
      .withTotalRecords(1);

    Response response = verifyPostResponse(ORDERS_EXPECT_ENDPOINT, JsonObject.mapFrom(request).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt());
    assertThat(response.as(ReceivingResults.class).getReceivingResults().get(0).getProcessedSuccessfully(), is(1));

    List<JsonObject> pieceUpdates = getPieceUpdates();

    assertThat(pieceUpdates, not(nullValue()));
    assertThat(pieceUpdates, hasSize(request.getTotalRecords()));

    pieceUpdates.forEach(pol -> {
      Piece piece = pol.mapTo(Piece.class);
      assertThat(piece.getId(), is(electronicPiece.getId()));
      assertThat(piece.getReceivingStatus(), is(Piece.ReceivingStatus.EXPECTED));
      assertThat(piece.getComment(), is("test"));
    });

    JsonArray poLinesJson = getPoLineBatchUpdates().get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine updatedPoLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(updatedPoLine.getReceiptStatus(), is(PoLine.ReceiptStatus.FULLY_RECEIVED));
    }
  }

  private void verifyProperQuantityOfHoldingsCreated(ReceivingCollection receivingRq) throws IOException {
    // get processed poline
    PoLineCollection poLineCollection = new JsonObject(getMockData(PO_LINES_COLLECTION)).mapTo(PoLineCollection.class);

    // get processed pieces for receiving
    PieceCollection pieces = new JsonObject(getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecordsCollection.json"))
      .mapTo(PieceCollection.class);

    List<String> pieceIds = new ArrayList<>();

    receivingRq.getToBeReceived()
      .get(0)
      .getReceivedItems()
      .forEach(recItem -> pieceIds.add(recItem.getPieceId()));

    // get processed pieces from mock collection
    pieces.getPieces()
      .removeIf(piece -> !pieceIds.contains(piece.getId()));

    long piecesRequestingNewHolding = receivingRq.getToBeReceived().stream()
        .map(ToBeReceived::getReceivedItems)
        .map(Collection::stream)
        .map(piecesList -> piecesList
          .filter(piece -> Objects.nonNull(piece.getLocationId()))
          .count())
        .findFirst()
        .get();

    assertEquals(piecesRequestingNewHolding, getCreatedHoldings().size());
  }

  @Test
  void testBindPiecesToTitleWithItem() {
    logger.info("=== Test POST Bind to Title With Item");

    var holdingId = "849241fa-4a14-4df5-b951-846dcd6cfc4d";
    var receivingStatus = Piece.ReceivingStatus.UNRECEIVABLE;
    var format = Piece.Format.ELECTRONIC;
    var tenantId = "tenantId";

    var order = getMinimalContentCompositePurchaseOrder()
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    var poLine = getMinimalContentCompositePoLine(order.getId());
    var title = getTitle(poLine);
    var bindingPiece1 = getMinimalContentPiece(poLine.getId())
      .withTitleId(title.getId())
      .withHoldingId(holdingId)
      .withReceivingStatus(receivingStatus)
      .withFormat(format);
    var bindingPiece2 = getMinimalContentPiece(poLine.getId())
      .withId(UUID.randomUUID().toString())
      .withTitleId(title.getId())
      .withHoldingId(holdingId)
      .withReceivingStatus(receivingStatus)
      .withFormat(format);

    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, bindingPiece1);
    addMockEntry(PIECES_STORAGE, bindingPiece2);
    addMockEntry(TITLES, title);

    var pieceIds = List.of(bindingPiece1.getId(), bindingPiece2.getId());
    var bindPiecesCollection = new BindPiecesCollection()
      .withPoLineId(poLine.getId())
      .withBindItem(getMinimalContentBindItem()
        .withLocationId(null)
        .withHoldingId(holdingId)
        .withTenantId(tenantId))
      .withBindPieceIds(pieceIds);

    var response = verifyPostResponse(ORDERS_BIND_ENDPOINT, JsonObject.mapFrom(bindPiecesCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt())
      .as(BindPiecesResult.class);

    assertThat(response.getPoLineId(), is(poLine.getId()));
    assertThat(response.getBoundPieceIds(), is(pieceIds));
    assertThat(response.getItemId(), notNullValue());
    var newItemId = response.getItemId();

    var pieceUpdates = getPieceUpdates();
    assertThat(pieceUpdates, notNullValue());
    assertThat(pieceUpdates, hasSize(bindPiecesCollection.getBindPieceIds().size()));

    var pieceList = pieceUpdates.stream()
      .map(json -> json.mapTo(Piece.class))
      .filter(piece -> pieceIds.contains(piece.getId()))
      .filter(piece -> piece.getBindItemId().equals(newItemId))
      .filter(piece -> piece.getBindItemTenantId().equals(tenantId))
      .toList();
    assertThat(pieceList.size(), is(2));

    var titleUpdates = getUpdatedTitles();
    assertThat(titleUpdates, notNullValue());
    assertThat(titleUpdates, hasSize(1));

    var updatedTitleOpt = titleUpdates.stream()
      .map(json -> json.mapTo(Title.class))
      .filter(updatedTitle -> updatedTitle.getId().equals(pieceList.get(0).getTitleId()))
      .filter(updatedTitle -> updatedTitle.getId().equals(pieceList.get(1).getTitleId()))
      .findFirst();

    assertThat(updatedTitleOpt.isPresent(), is(true));
    var titleBindItemIds = updatedTitleOpt.get().getBindItemIds();
    assertThat(titleBindItemIds, hasSize(1));
    assertThat(titleBindItemIds.get(0), is(newItemId));

    var createdHoldings = getCreatedHoldings();
    assertThat(createdHoldings, nullValue());

    var createdItems = getCreatedItems();
    assertThat(createdItems, notNullValue());
    createdItems.forEach(item -> {
      var id = item.getString(ID);
      if (Objects.equals(newItemId, id)) {
        assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.IN_PROCESS.value()));
      }
    });
  }

  @Test
  void testBindPiecesWithLocationIdOnly() {
    logger.info("=== Test POST Bind to Title With Item using locationId");

    var receivingStatus = Piece.ReceivingStatus.UNRECEIVABLE;
    var format = Piece.Format.ELECTRONIC;
    var tenantId = "tenantId";

    var order = getMinimalContentCompositePurchaseOrder()
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    var poLine = getMinimalContentCompositePoLine(order.getId());
    var bindingPiece = getMinimalContentPiece(poLine.getId())
      .withReceivingStatus(receivingStatus)
      .withFormat(format);

    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, bindingPiece);
    addMockEntry(TITLES, getTitle(poLine));


    var locationId = UUID.randomUUID().toString();
    var pieceIds = List.of(bindingPiece.getId());
    var bindPiecesCollection = new BindPiecesCollection()
      .withPoLineId(poLine.getId())
      .withBindItem(getMinimalContentBindItem()
        .withLocationId(locationId)
        .withTenantId(tenantId))
      .withBindPieceIds(pieceIds);

    var response = verifyPostResponse(ORDERS_BIND_ENDPOINT, JsonObject.mapFrom(bindPiecesCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt())
      .as(BindPiecesResult.class);

    assertThat(response.getPoLineId(), is(poLine.getId()));
    assertThat(response.getBoundPieceIds(), is(pieceIds));
    assertThat(response.getItemId(), notNullValue());

    var pieceUpdates = getPieceUpdates();
    assertThat(pieceUpdates, notNullValue());
    assertThat(pieceUpdates, hasSize(bindPiecesCollection.getBindPieceIds().size()));

    var pieceList = pieceUpdates.stream().filter(pol -> {
      Piece piece = pol.mapTo(Piece.class);
      String pieceId = piece.getId();
      return Objects.equals(bindingPiece.getId(), pieceId)
        && Objects.equals(piece.getBindItemTenantId(), tenantId);
    }).toList();
    assertThat(pieceList.size(), is(1));

    var createdHoldings = getCreatedHoldings();
    assertThat(createdHoldings, notNullValue());
    assertThat(createdHoldings, hasSize(1));
    assertThat(createdHoldings.get(0).getString(HOLDING_PERMANENT_LOCATION_ID), is(locationId));
  }

  @Test
  void testBindPiecesToTitleWithItemWithOutstandingRequest() {
    logger.info("=== Test POST Bind to Title with Item with Outstanding Request");

    var order = getMinimalContentCompositePurchaseOrder()
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    var poLine = getMinimalContentCompositePoLine(order.getId())
      .withLocations(List.of(new Location().withHoldingId(UUID.randomUUID().toString())
        .withQuantityPhysical(1).withQuantity(1)));
    var bindingPiece = getMinimalContentPiece(poLine.getId())
      .withItemId(OPEN_REQUEST_ITEM_ID)
      .withReceivingStatus(Piece.ReceivingStatus.UNRECEIVABLE)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC);
    var bindPiecesCollection = new BindPiecesCollection()
      .withPoLineId(poLine.getId())
      .withBindItem(getMinimalContentBindItem()
        .withLocationId(null)
        .withHoldingId(UUID.randomUUID().toString()))
      .withBindPieceIds(List.of(bindingPiece.getId()));

    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, bindingPiece);
    addMockEntry(TITLES, getTitle(poLine));

    var errors = verifyPostResponse(ORDERS_BIND_ENDPOINT, JsonObject.mapFrom(bindPiecesCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, VALIDATION_ERROR)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(REQUESTS_ACTION_REQUIRED.getDescription()));
  }

  @Test
  void testBindPiecesToTitleWithBindItemWithDifferentTenantId() {
    logger.info("=== Test POST Bind to Title with Item with Different Tenant ID");

    var order = getMinimalContentCompositePurchaseOrder()
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    var poLine = getMinimalContentCompositePoLine(order.getId())
      .withLocations(List.of(new Location().withHoldingId(UUID.randomUUID().toString())
        .withQuantityPhysical(1).withQuantity(1)));
    var bindingPiece = getMinimalContentPiece(poLine.getId())
      .withItemId(OPEN_REQUEST_ITEM_ID)
      .withReceivingStatus(Piece.ReceivingStatus.UNRECEIVABLE)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC);
    var bindPiecesCollection = new BindPiecesCollection()
      .withPoLineId(poLine.getId())
      .withBindItem(getMinimalContentBindItem()
        .withTenantId("differentTenantId")
        .withLocationId(null)
        .withHoldingId(UUID.randomUUID().toString()))
      .withBindPieceIds(List.of(bindingPiece.getId()))
      .withRequestsAction(BindPiecesCollection.RequestsAction.TRANSFER);

    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, bindingPiece);
    addMockEntry(TITLES, getTitle(poLine));

    var errors = verifyPostResponse(ORDERS_BIND_ENDPOINT, JsonObject.mapFrom(bindPiecesCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, VALIDATION_ERROR)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(PIECES_HAVE_DIFFERENT_RECEIVING_TENANT_IDS.getDescription()));
  }

  @Test
  void testBindPiecesToTitleWithTransferRequestsAction() {
    logger.info("=== Test POST Bind to Title with Item with Transfer as Requests Action");

    var order = getMinimalContentCompositePurchaseOrder()
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    var poLine = getMinimalContentCompositePoLine(order.getId())
      .withLocations(List.of(new Location().withHoldingId(UUID.randomUUID().toString())
        .withQuantityPhysical(1).withQuantity(1)));
    var bindingPiece = getMinimalContentPiece(poLine.getId())
      .withItemId("522a501a-56b5-48d9-b28a-3a8f02482d98") // Present in mockdata/itemRequests/itemRequests.json
      .withReceivingStatus(Piece.ReceivingStatus.UNRECEIVABLE)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC);
    var bindPiecesCollection = new BindPiecesCollection()
      .withPoLineId(poLine.getId())
      .withBindItem(getMinimalContentBindItem()
        .withLocationId(null)
        .withHoldingId(UUID.randomUUID().toString()))
      .withBindPieceIds(List.of(bindingPiece.getId()))
      .withRequestsAction(BindPiecesCollection.RequestsAction.TRANSFER);

    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, bindingPiece);
    addMockEntry(TITLES, getTitle(poLine));

    var response = verifyPostResponse(ORDERS_BIND_ENDPOINT, JsonObject.mapFrom(bindPiecesCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt())
      .as(BindPiecesResult.class);

    assertThat(response.getPoLineId(), is(poLine.getId()));
    assertThat(response.getBoundPieceIds(), is(List.of(bindingPiece.getId())));

    var pieceUpdates = getPieceUpdates();
    assertThat(pieceUpdates, notNullValue());
    assertThat(pieceUpdates, hasSize(bindPiecesCollection.getBindPieceIds().size()));

    var pieceList = pieceUpdates.stream()
      .map(pol -> pol.mapTo(Piece.class))
      .filter(piece -> Objects.equals(bindingPiece.getId(), piece.getId()))
      .toList();

    assertThat(pieceList.size(), is(1));
  }

  @Test
  void testBindExpectedPieces() {
    logger.info("=== Test POST Bind to Title with Expected Piece ");

    var order = getMinimalContentCompositePurchaseOrder()
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    var poLine = getMinimalContentCompositePoLine(order.getId())
      .withLocations(List.of(new Location().withHoldingId(UUID.randomUUID().toString())
        .withQuantityPhysical(1).withQuantity(1)));
    var bindingPiece = getMinimalContentPiece(poLine.getId())
      .withItemId("522a501a-56b5-48d9-b28a-3a8f02482d98") // Present in mockdata/itemRequests/itemRequests.json
      .withReceivingStatus(Piece.ReceivingStatus.EXPECTED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC);
    var bindPiecesCollection = new BindPiecesCollection()
      .withPoLineId(poLine.getId())
      .withBindItem(getMinimalContentBindItem()
        .withLocationId(null)
        .withHoldingId(UUID.randomUUID().toString()))
      .withBindPieceIds(List.of(bindingPiece.getId()))
      .withRequestsAction(BindPiecesCollection.RequestsAction.TRANSFER);

    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, bindingPiece);
    addMockEntry(TITLES, getTitle(poLine));

    var errors = verifyPostResponse(ORDERS_BIND_ENDPOINT, JsonObject.mapFrom(bindPiecesCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, VALIDATION_ERROR)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(PIECES_MUST_HAVE_RECEIVED_STATUS.getDescription()));
  }

  @Test
  void testRemovePieceBinding() {
    logger.info("=== Test DELETE Remove binding");

    var holdingId = "849241fa-4a14-4df5-b951-846dcd6cfc4d";
    var tenantId = "tenantId";
    var order = getMinimalContentCompositePurchaseOrder()
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    var poLine = getMinimalContentCompositePoLine(order.getId());
    var title = getTitle(poLine);
    var bindPiece1 = getMinimalContentPiece(poLine.getId())
      .withTitleId(title.getId())
      .withHoldingId(holdingId)
      .withReceivingStatus(Piece.ReceivingStatus.RECEIVED)
      .withFormat(Piece.Format.PHYSICAL);
    var bindPiece2 = getMinimalContentPiece(poLine.getId())
      .withId(UUID.randomUUID().toString())
      .withTitleId(title.getId())
      .withHoldingId(holdingId)
      .withReceivingStatus(Piece.ReceivingStatus.RECEIVED)
      .withFormat(Piece.Format.PHYSICAL);
    var bindPieceIds = List.of(bindPiece1.getId(), bindPiece2.getId());

    addMockEntry(PURCHASE_ORDER_STORAGE, order);
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, bindPiece1);
    addMockEntry(PIECES_STORAGE, bindPiece2);
    addMockEntry(TITLES, title);

    var bindPiecesCollection = new BindPiecesCollection()
      .withPoLineId(poLine.getId())
      .withBindItem(getMinimalContentBindItem()
        .withLocationId(null)
        .withHoldingId(holdingId)
        .withTenantId(tenantId))
      .withBindPieceIds(bindPieceIds);

    var bindResponse = verifyPostResponse(ORDERS_BIND_ENDPOINT, JsonObject.mapFrom(bindPiecesCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt())
      .as(BindPiecesResult.class);

    assertThat(bindResponse.getPoLineId(), is(poLine.getId()));
    assertThat(bindResponse.getBoundPieceIds(), hasSize(2));
    assertThat(bindResponse.getBoundPieceIds(), is(bindPieceIds));
    assertThat(bindResponse.getItemId(), notNullValue());

    var url = String.format(ORDERS_BIND_ID_ENDPOINT, bindPiece1.getId());
    verifyDeleteResponse(url, "", HttpStatus.HTTP_NO_CONTENT.toInt());

    var pieceUpdates = getPieceUpdates();
    assertThat(pieceUpdates, notNullValue());
    assertThat(pieceUpdates, hasSize(3));

    var pieceList = pieceUpdates.stream()
      .map(json -> json.mapTo(Piece.class))
      .filter(piece -> Objects.equals(bindPiece1.getId(), piece.getId()))
      .sorted(Comparator.comparing(Piece::getIsBound))
      .toList();
    assertThat(pieceList.size(), is(2));

    var pieceBefore = pieceList.get(1);
    assertThat(pieceBefore.getIsBound(), is(true));
    assertThat(pieceBefore.getBindItemId(), notNullValue());
    assertThat(pieceBefore.getBindItemTenantId(), notNullValue());

    var pieceAfter = pieceList.get(0);
    assertThat(pieceAfter.getIsBound(), is(false));
    assertThat(pieceAfter.getBindItemId(), nullValue());
    assertThat(pieceAfter.getReceivingTenantId(), nullValue());
  }

  @Test
  void testPostReceivingWithErrorSearchingForPiece() {
    logger.info("=== Test POST Receiving - Receive resources with error searching for piece");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-500-error-for-pieces-lookup.json").mapTo(ReceivingCollection.class);
    receivingRq.getToBeReceived().forEach(toBeReceived -> {
      CompositePurchaseOrder purchaseOrder = new CompositePurchaseOrder().withId(UUID.randomUUID().toString()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
      PoLine poLine = new PoLine().withId(toBeReceived.getPoLineId()).withPurchaseOrderId(purchaseOrder.getId());
      addMockEntry(PURCHASE_ORDER_STORAGE, purchaseOrder);
      addMockEntry(PO_LINES_STORAGE, poLine);
    });

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(1));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);
    assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
    assertThat(receivingResult.getProcessedSuccessfully(), is(0));
    assertThat(receivingResult.getProcessedWithError(), is(1));

    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(is(emptyString())));
      assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
      assertThat(receivingItemResult.getProcessingStatus().getType(), is(ProcessingStatus.Type.FAILURE));
      assertThat(receivingItemResult.getProcessingStatus().getError().getCode(), is(PIECE_NOT_RETRIEVED.getCode()));
    }

    assertThat(getPieceSearches(), not(nullValue()));
    assertThat(getPieceUpdates(), is(nullValue()));
    assertThat(getItemsSearches(), is(nullValue()));
    assertThat(getItemUpdates(), is(nullValue()));
    assertThat(getPoLineSearches(), not(nullValue()));
    assertThat(getPoLineUpdates(), is(nullValue()));

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testPostReceivingWithErrorSearchingForItem() {
    logger.info("=== Test POST Receiving - Receive resources with error searching for item");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-500-error-for-items-lookup.json").mapTo(ReceivingCollection.class);
    receivingRq.getToBeReceived().forEach(toBeReceived -> {
      CompositePurchaseOrder purchaseOrder = new CompositePurchaseOrder().withId(UUID.randomUUID().toString()).withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
      PoLine poLine = getMinimalContentCompositePoLine().withId(toBeReceived.getPoLineId()).withPurchaseOrderId(purchaseOrder.getId());
      addMockEntry(PURCHASE_ORDER_STORAGE, purchaseOrder);
      addMockEntry(PO_LINES_STORAGE, poLine);
      addMockTitles(Collections.singletonList(poLine));
    });
    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(1));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);
    assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
    assertThat(receivingResult.getProcessedSuccessfully(), is(0));
    assertThat(receivingResult.getProcessedWithError(), is(1));

    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(is(emptyString())));
      assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
      assertThat(receivingItemResult.getProcessingStatus().getType(), is(ProcessingStatus.Type.FAILURE));
      assertThat(receivingItemResult.getProcessingStatus().getError().getCode(), is(ITEM_NOT_RETRIEVED.getCode()));
    }

    assertThat(getPieceSearches(), not(nullValue()));
    assertThat(getItemsSearches(), not(nullValue()));
    assertThat(getPieceUpdates(), is(nullValue()));
    assertThat(getItemUpdates(), is(nullValue()));
    assertThat(getPoLineSearches(), not(nullValue()));
    assertThat(getPoLineUpdates(), is(nullValue()));

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testPostReceivingRevertMixedResources() {
    logger.info("=== Test POST Receiving - Revert received P/E Mix resources");

    PoLine poLines = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLines));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "revert-pe-mix-4-of-5-resources.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> itemUpdates = getItemUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polBatchUpdates, not(nullValue()));

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(2));
    // In total 4 pieces required update
    assertThat(pieceUpdates, hasSize(4));
    assertThat(itemsSearches, hasSize(1));
    // There are 3 piece records with item id's
    assertThat(itemUpdates, hasSize(3));
    assertThat(polSearches, hasSize(pieceIdsByPol.size() + 1));
    assertThat(polBatchUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), is(nullValue()));
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.ON_ORDER.value()));
    });
    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    }

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostReceivingRevertElectronicResource() {
    logger.info("=== Test POST Receiving - Revert received electronic resource");

    PoLine poLine1 = getMockAsJson(PO_LINES_COLLECTION).getJsonArray("poLines").getJsonObject(6).mapTo(PoLine.class);
    MockServer.addMockOrderData(Collections.singletonList(poLine1));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "revert-electronic-1-of-1-resource.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> itemsSearches = getItemsSearches();
    List<JsonObject> itemUpdates = getItemUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polBatchUpdates = getPoLineBatchUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polBatchUpdates, not(nullValue()));

    // The piece searches should be made 2 times: 1st time to get piece record, 2nd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(2));
    assertThat(pieceUpdates, hasSize(1));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(2));
    assertThat(polBatchUpdates, hasSize(1));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), is(nullValue()));
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.ON_ORDER.value()));
    });
    JsonArray poLinesJson = polBatchUpdates.get(0).getJsonArray("poLines");
    for (int i = 0; i < poLinesJson.size(); i++) {
      PoLine poLine = poLinesJson.getJsonObject(i).mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.AWAITING_RECEIPT));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    }

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  private void checkResultWithErrors(CheckinCollection request, int expectedNumOfErrors) {

    ReceivingResult response = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(request).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt())
      .as(ReceivingResults.class).getReceivingResults().get(0);

    assertThat(request.getToBeCheckedIn().get(0).getPoLineId(), equalTo(response.getPoLineId()));
    assertThat(response.getProcessedSuccessfully(), is(response.getReceivingItemResults().size() - expectedNumOfErrors));
    assertThat(response.getProcessedWithError(), is(expectedNumOfErrors));

    List<String> pieceIds
      = request.getToBeCheckedIn().stream()
      .flatMap(toBeCheckedIn -> toBeCheckedIn.getCheckInPieces().stream())
      .map(CheckInPiece::getId).collect(Collectors.toList());


    List<ReceivingItemResult> results = response.getReceivingItemResults();
    for (ReceivingItemResult r : results) {
      Error error = r.getProcessingStatus().getError();
      if (error != null) {
        assertThat(r.getProcessingStatus().getError().getCode(), equalTo(LOC_NOT_PROVIDED.getCode()));
        assertThat(r.getProcessingStatus().getError().getMessage(), equalTo(LOC_NOT_PROVIDED.getDescription()));
        assertThat(r.getPieceId(), is(in(pieceIds)));
      }
    }
  }

  private Map<String, Set<String>> verifyReceivingSuccessRs(ReceivingResults results) {
    Map<String, Set<String>> pieceIdsByPol = new HashMap<>();
    for (ReceivingResult receivingResult : results.getReceivingResults()) {
      assertThat(receivingResult.getPoLineId(), not(is(emptyString())));
      assertThat(receivingResult.getProcessedSuccessfully(), is(receivingResult.getReceivingItemResults().size()));
      assertThat(receivingResult.getProcessedWithError(), is(0));

      for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
        assertThat(receivingItemResult.getPieceId(), not(is(emptyString())));
        assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
        assertThat(receivingItemResult.getProcessingStatus().getType(), is(SUCCESS));
        assertThat(receivingItemResult.getProcessingStatus().getError(), nullValue());

        pieceIdsByPol.computeIfAbsent(receivingResult.getPoLineId(), k -> new HashSet<>())
          .add(receivingItemResult.getPieceId());
      }
    }
    return pieceIdsByPol;
  }
}
