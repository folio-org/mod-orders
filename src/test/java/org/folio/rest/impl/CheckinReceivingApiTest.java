package org.folio.rest.impl;

import io.restassured.response.Response;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.jaxrs.model.BindPiecesCollection;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.ExpectCollection;
import org.folio.rest.jaxrs.model.ExpectPiece;
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
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.rest.jaxrs.model.ToBeExpected;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ORDERS_BIND_ENDPOINT;
import static org.folio.TestConstants.ORDERS_CHECKIN_ENDPOINT;
import static org.folio.TestConstants.ORDERS_EXPECT_ENDPOINT;
import static org.folio.TestConstants.ORDERS_RECEIVING_ENDPOINT;
import static org.folio.TestUtils.getInstanceId;
import static org.folio.TestUtils.getMinimalContentBindItem;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.TestUtils.getMinimalContentPiece;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.TestUtils.getTitle;
import static org.folio.orders.events.handlers.HandlersTestHelper.verifyCheckinOrderStatusUpdateEvent;
import static org.folio.orders.events.handlers.HandlersTestHelper.verifyOrderStatusUpdateEvent;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForEresource;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForPhysical;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_NOT_RETRIEVED;
import static org.folio.rest.core.exceptions.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.rest.core.exceptions.ErrorCodes.LOC_NOT_PROVIDED;
import static org.folio.rest.core.exceptions.ErrorCodes.MULTIPLE_NONPACKAGE_TITLES;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_ALREADY_RECEIVED;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_NOT_RETRIEVED;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_POL_MISMATCH;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECE_UPDATE_FAILED;
import static org.folio.rest.core.exceptions.ErrorCodes.TITLE_NOT_FOUND;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.POLINES_COLLECTION;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.addMockTitles;
import static org.folio.rest.impl.MockServer.getCreatedHoldings;
import static org.folio.rest.impl.MockServer.getItemUpdates;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPieceUpdates;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.folio.rest.jaxrs.model.ProcessingStatus.Type.SUCCESS;
import static org.folio.rest.jaxrs.model.ReceivedItem.ItemStatus.ON_ORDER;
import static org.folio.service.inventory.InventoryItemManager.COPY_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ACCESSION_NUMBER;
import static org.folio.service.inventory.InventoryItemManager.ITEM_CHRONOLOGY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISCOVERY_SUPPRESS;
import static org.folio.service.inventory.InventoryItemManager.ITEM_DISPLAY_SUMMARY;
import static org.folio.service.inventory.InventoryItemManager.ITEM_ENUMERATION;
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

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(7).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

    CheckinCollection checkInRq = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + "checkin-pe-mix-2-electronic-resources.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkInRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkInRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(getItemsSearches(),is(nullValue()));
    assertThat(getItemUpdates(), is(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(checkInRq.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

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

    // Verify message is sent via event bus
    verifyCheckinOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostCheckInPhysicalWithMissingItem() {
    logger.info("=== Test POST Checkin - CheckIn physical resource with only one item updated");

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(7).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

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
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    assertThat(pieceSearches, hasSize(2));
    assertThat(pieceUpdates, hasSize(2));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(1));
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

    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    });

    // Verify message is sent via event bus
    verifyCheckinOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostCheckinRevertPhysicalResource() {
    logger.info("=== Test POST Check-in - Revert received Physical resource");

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(8).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

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
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    // The piece searches should be made 2 times: 1st time to get piece record,
    // 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(2));
    assertThat(pieceUpdates, hasSize(1));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(1));
    assertThat(polUpdates, hasSize(1));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), is(nullValue()));
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ON_ORDER.value()));
    });
    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.AWAITING_RECEIPT));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });

    // Verify message is sent via event bus
    verifyCheckinOrderStatusUpdateEvent(1);
  }

  @Test
  void testReceiveOngoingOrder() {
    logger.info("=== Test POST Receive - Ongoing PO Lines");

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(9).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-ongoing.json").mapTo(ReceivingCollection.class);
    receivingRq.getToBeReceived().get(0).setPoLineId(COMPOSITE_POLINE_ONGOING_ID);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));

    assertThat(polSearches, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receivingRq.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 1 time: 1st time to get all required piece records
    assertThat(pieceSearches, hasSize(expectedSearchRqQty));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));

    // check no status updates were performed and POL remained ongoing
    assertThat(polUpdates, nullValue());
    polSearches.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLineCollection.class).getPoLines().get(0);
      assertThat(poLine.getCheckinItems(), is(false));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.ONGOING));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    });

    // Verify no status updated for ongoing order
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testReceiveCanceledOrder() {
    logger.info("=== Test POST Receive - Ongoing PO Lines");

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(10).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-cancelled.json").mapTo(ReceivingCollection.class);
    receivingRq.getToBeReceived().get(0).setPoLineId(COMPOSITE_POLINE_CANCELED_ID);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receivingRq.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));

    assertThat(polSearches, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receivingRq.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 1 time: 1st time to get all required piece records
    assertThat(pieceSearches, hasSize(expectedSearchRqQty));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));

    // check no status updates were performed and POL remained canceled
    assertThat(polUpdates, nullValue());
    polSearches.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLineCollection.class).getPoLines().get(0);
      assertThat(poLine.getCheckinItems(), is(false));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.CANCELLED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    });

    // Verify no status updated for ongoing order
    verifyOrderStatusUpdateEvent(0);
  }


  @Test
  void testPostCheckInLocationId() {
   logger.info("=== Test POST Checkin - locationId checking ===");

    String poLineId = "fe47e95d-24e9-4a9a-9dc0-bcba64b51f56";
    String pieceId = UUID.randomUUID().toString();
    CompositePoLine poLine = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLine));

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
    assertThat(getPoLineSearches(), hasSize(1));
    assertThat(getPoLineUpdates(), hasSize(1));
    verifyCheckinOrderStatusUpdateEvent(1);


    // Negative cases:
    // 1. One CheckInPiece and corresponding Piece without locationId
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(null);

    clearServiceInteractions();

    MockServer.addMockTitles(Collections.singletonList(poLine));

    checkResultWithErrors(request, 1);
    assertThat(getPieceSearches(), hasSize(2));
    assertThat(getPieceUpdates(), hasSize(1));
    assertThat(getPoLineSearches(), hasSize(1));
    assertThat(getPoLineUpdates(), hasSize(1));
    verifyCheckinOrderStatusUpdateEvent(1);


    // 2. All CheckInPieces and corresponding Pieces without locationId
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(0).setLocationId(null);
    request.getToBeCheckedIn().get(0).getCheckInPieces().get(1).setLocationId(null);

    clearServiceInteractions();
    MockServer.addMockTitles(Collections.singletonList(poLine));
    checkResultWithErrors(request, 2);
    assertThat(getPieceSearches(), hasSize(1));
    assertThat(getPieceUpdates(), nullValue());
    assertThat(getPoLineSearches(), hasSize(1));
    assertThat(getPoLineUpdates(), nullValue());
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testPostReceivingPhysicalAll() {
    logger.info("=== Test POST Receiving - Receive physical resources ===");

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(2).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

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
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receivingRq.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(itemsSearches, hasSize(expectedSearchRqQty));
    assertThat(itemUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));
    assertThat(polUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      assertThat(item.getString(ITEM_BARCODE), not(is(emptyString())));
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.IN_PROCESS.value()));
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), not(is(emptyString())));
    });
    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.FULLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    });

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostCheckinElectronicPhysicalChangeLocationIdNewHoldingIsCreatedForPhysicalPiece() {

    logger.info("=== Test POST check-in - Check-in physical and electronic resource with new locationId ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setOrderFormat(CompositePoLine.OrderFormat.P_E_MIX);
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
  void testPostCheckinForPackagePOL() {
    logger.info("=== Test POST check-in - Package POL ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setIsPackage(true);
    poLine.setOrderFormat(CompositePoLine.OrderFormat.P_E_MIX);
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
  void testPostCheckinMultipleTitlesError() {
    logger.info("=== Test POST check-in multiple titles error for non-packages ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setOrderFormat(CompositePoLine.OrderFormat.P_E_MIX);
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
    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setIsPackage(true);
    poLine.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
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

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(4).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

    ReceivingCollection receiving = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-electronic-5-of-10-resources-no-items.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receiving).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(receiving.getTotalRecords()));

    Map<String, Set<String>> pieceIdsByPol = verifyReceivingSuccessRs(results);

    List<JsonObject> pieceSearches = getPieceSearches();
    List<JsonObject> pieceUpdates = getPieceUpdates();
    List<JsonObject> polSearches = getPoLineSearches();
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(getItemsSearches(), is(nullValue()));
    assertThat(getItemUpdates(), is(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    int expectedSearchRqQty = Math.floorDiv(receiving.getTotalRecords(), MAX_IDS_FOR_GET_RQ_15) + 1;

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

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostReceivingPhysicalWithErrors() throws IOException {
    logger.info("=== Test POST Receiving - Receive physical resources with different errors");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-resources-6-of-10-with-errors.json").mapTo(ReceivingCollection.class);
    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(3).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));
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
    List<JsonObject> polUpdates = getPoLineUpdates();
    assertThat(getPoLineSearches(), hasSize(1));
    assertThat(polUpdates, hasSize(1));
    assertThat(itemUpdates, hasSize(6));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.IN_PROCESS.value()));
    });

    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });

    verifyProperQuantityOfHoldingsCreated(receivingRq);

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testMovePieceStatusFromUnreceivableToExpected() {
    logger.info("=== Test POST Expect");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setIsPackage(true);
    poLine.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
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

    PoLine updatedPoLine = getPoLineUpdates().get(0).mapTo(PoLine.class);
    assertEquals(PoLine.ReceiptStatus.FULLY_RECEIVED, updatedPoLine.getReceiptStatus());
  }

  private void verifyProperQuantityOfHoldingsCreated(ReceivingCollection receivingRq) throws IOException {
    Set<String> expectedHoldings = new HashSet<>();

    // get processed poline
    PoLineCollection poLineCollection = new JsonObject(getMockData(POLINES_COLLECTION)).mapTo(PoLineCollection.class);
    PoLine poline = poLineCollection.getPoLines()
      .stream()
      .filter(poLine -> poLine.getId()
        .equals(receivingRq.getToBeReceived()
          .get(0)
          .getPoLineId()))
      .findFirst()
      .get();
    CompositePoLine compPOL = PoLineCommonUtil.convertToCompositePoLine(poline);

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

    for (org.folio.rest.acq.model.Piece piece : pieces.getPieces()) {
      for (ReceivedItem receivedItem : receivingRq.getToBeReceived()
        .get(0)
        .getReceivedItems()) {
        if (receivedItem.getPieceId()
          .equals(piece.getId())
            && !receivedItem.getLocationId()
              .equals(piece.getLocationId())
            && isHoldingsUpdateRequired(piece, compPOL)) {
          expectedHoldings.add(getInstanceId(poline) + receivedItem.getLocationId());
        }
      }
    }
    assertEquals(expectedHoldings.size(), getCreatedHoldings().size());
  }

  private boolean isHoldingsUpdateRequired(org.folio.rest.acq.model.Piece piece, CompositePoLine compPOL) {
    if (piece.getFormat() == org.folio.rest.acq.model.Piece.PieceFormat.ELECTRONIC) {
     return isHoldingUpdateRequiredForEresource(compPOL);
    } else {
      return isHoldingUpdateRequiredForPhysical(compPOL);
    }
  }


  @Test
  void testBindPiecesToTitleWithItem() {
    logger.info("=== Test POST Bind to Title With Item");

    var order = getMinimalContentCompositePurchaseOrder()
      .withId(UUID.randomUUID().toString());
    var poLine = getMinimalContentCompositePoLine(order.getId())
      .withId(UUID.randomUUID().toString());

    var bindingPiece1 = getMinimalContentPiece(poLine.getId())
      .withId(UUID.randomUUID().toString())
      .withHoldingId("849241fa-4a14-4df5-b951-846dcd6cfc4d")
      .withReceivingStatus(Piece.ReceivingStatus.UNRECEIVABLE)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC);
    var bindingPiece2 = getMinimalContentPiece(poLine.getId())
      .withId(UUID.randomUUID().toString())
      .withHoldingId("849241fa-4a14-4df5-b951-846dcd6cfc4d")
      .withReceivingStatus(Piece.ReceivingStatus.UNRECEIVABLE)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC);
    var bindItem = getMinimalContentBindItem();

    addMockEntry(PURCHASE_ORDER_STORAGE, order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN));
    addMockEntry(PO_LINES_STORAGE, poLine);
    addMockEntry(PIECES_STORAGE, bindingPiece1);
    addMockEntry(PIECES_STORAGE, bindingPiece2);
    addMockEntry(TITLES, getTitle(poLine));

    var bindPiecesCollection = new BindPiecesCollection()
      .withPoLineId(poLine.getId())
        .withBindItem(bindItem)
        .withBindPieceIds(List.of(bindingPiece1.getId(), bindingPiece2.getId()));

    var response = verifyPostResponse(ORDERS_BIND_ENDPOINT, JsonObject.mapFrom(bindPiecesCollection).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_OK.toInt());

    assertThat(response.as(ReceivingResults.class).getReceivingResults().get(0).getProcessedSuccessfully(), is(2));

    var pieceUpdates = getPieceUpdates();

    assertThat(pieceUpdates, not(nullValue()));
    assertThat(pieceUpdates, hasSize(bindPiecesCollection.getBindPieceIds().size()));

    var pieceList = pieceUpdates.stream().filter(pol -> {
      Piece piece = pol.mapTo(Piece.class);
      String pieceId = piece.getId();
      return Objects.equals(bindingPiece1.getId(), pieceId) || Objects.equals(bindingPiece2.getId(), pieceId);
    }).toList();

    assertThat(pieceList.size(), is(2));
  }

  @Test
  void testPostReceivingWithErrorSearchingForPiece() {
    logger.info("=== Test POST Receiving - Receive resources with error searching for piece");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-500-error-for-pieces-lookup.json").mapTo(ReceivingCollection.class);

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
    assertThat(getPoLineSearches(), is(nullValue()));
    assertThat(getPoLineUpdates(), is(nullValue()));

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testPostReceivingWithErrorSearchingForItem() {
    logger.info("=== Test POST Receiving - Receive resources with error searching for item");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-500-error-for-items-lookup.json").mapTo(ReceivingCollection.class);
    receivingRq.getToBeReceived().forEach(toBeReceived -> {
      CompositePoLine poLine = getMinimalContentCompositePoLine().withId(toBeReceived.getPoLineId());
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

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

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
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(2));
    // In total 4 pieces required update
    assertThat(pieceUpdates, hasSize(4));
    assertThat(itemsSearches, hasSize(1));
    // There are 3 piece records with item id's
    assertThat(itemUpdates, hasSize(3));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));
    assertThat(polUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), is(nullValue()));
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.ON_ORDER.value()));
    });
    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testPostReceivingRevertElectronicResource() {
    logger.info("=== Test POST Receiving - Revert received electronic resource");

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(6).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

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
    List<JsonObject> polUpdates = getPoLineUpdates();

    assertThat(pieceSearches, not(nullValue()));
    assertThat(pieceUpdates, not(nullValue()));
    assertThat(itemsSearches, not(nullValue()));
    assertThat(itemUpdates, not(nullValue()));
    assertThat(polSearches, not(nullValue()));
    assertThat(polUpdates, not(nullValue()));

    // The piece searches should be made 2 times: 1st time to get piece record, 2nd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(2));
    assertThat(pieceUpdates, hasSize(1));
    assertThat(itemsSearches, hasSize(1));
    assertThat(itemUpdates, hasSize(1));
    assertThat(polSearches, hasSize(1));
    assertThat(polUpdates, hasSize(1));

    itemUpdates.forEach(item -> {
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), is(nullValue()));
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.ON_ORDER.value()));
    });
    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.AWAITING_RECEIPT));
      assertThat(poLine.getReceiptDate(), is(nullValue()));
    });

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
    for(ReceivingItemResult r : results) {
      Error error = r.getProcessingStatus().getError();
      if(error != null) {
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
