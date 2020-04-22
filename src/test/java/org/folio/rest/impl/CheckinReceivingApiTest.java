package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.orders.utils.ErrorCodes.ITEM_NOT_RETRIEVED;
import static org.folio.orders.utils.ErrorCodes.ITEM_UPDATE_FAILED;
import static org.folio.orders.utils.ErrorCodes.LOC_NOT_PROVIDED;
import static org.folio.orders.utils.ErrorCodes.PIECE_ALREADY_RECEIVED;
import static org.folio.orders.utils.ErrorCodes.PIECE_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.PIECE_NOT_RETRIEVED;
import static org.folio.orders.utils.ErrorCodes.PIECE_POL_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.PIECE_UPDATE_FAILED;
import static org.folio.orders.utils.HelperUtils.isHoldingUpdateRequiredForEresource;
import static org.folio.orders.utils.HelperUtils.isHoldingUpdateRequiredForPhysical;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.rest.impl.InventoryHelper.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.rest.impl.InventoryHelper.ITEM_BARCODE;
import static org.folio.rest.impl.InventoryHelper.ITEM_LEVEL_CALL_NUMBER;
import static org.folio.rest.impl.InventoryHelper.ITEM_STATUS;
import static org.folio.rest.impl.InventoryHelper.ITEM_STATUS_NAME;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.POLINES_COLLECTION;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.addMockTitles;
import static org.folio.rest.impl.MockServer.getCreatedHoldings;
import static org.folio.rest.impl.MockServer.getHoldingsSearches;
import static org.folio.rest.impl.MockServer.getItemUpdates;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPieceUpdates;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineUpdates;
import static org.folio.rest.jaxrs.model.ProcessingStatus.Type.SUCCESS;
import static org.folio.rest.jaxrs.model.ReceivedItem.ItemStatus.ON_ORDER;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyString;
import static org.hamcrest.Matchers.isIn;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.folio.HttpStatus;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
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
import org.junit.Assert;
import org.junit.Test;

import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class CheckinReceivingApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(CheckinReceivingApiTest.class);

  private static final String RECEIVING_RQ_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "receiving/";
  private static final String CHECKIN_RQ_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "checkIn/";

  @Test
  public void testPostCheckInElectronicWithNoItems() {
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

    int expectedSearchRqQty = Math.floorDiv(checkInRq.getTotalRecords(), AbstractHelper.MAX_IDS_FOR_GET_RQ) + 1;

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
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  public void testPostCheckInPhysicalWithMissingItem() {
    logger.info("=== Test POST Checkin - CheckIn physical resource with only one item updated");

    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(7).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));

    CheckinCollection checkInRq = getMockAsJson(CHECKIN_RQ_MOCK_DATA_PATH + "checkin-pe-mix-2-physical-resources.json").mapTo(CheckinCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_CHECKIN_ENDPOINT, JsonObject.mapFrom(checkInRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), equalTo(checkInRq.getTotalRecords()));
    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
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

    polUpdates.forEach(pol -> {
      PoLine poLine = pol.mapTo(PoLine.class);
      assertThat(poLine.getCheckinItems(), is(true));
      assertThat(poLine.getReceiptStatus(), is(PoLine.ReceiptStatus.PARTIALLY_RECEIVED));
      assertThat(poLine.getReceiptDate(), is(notNullValue()));
    });

    // Verify message is sent via event bus
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  public void testPostCheckinRevertPhysicalResource() {
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
    assertThat(pieceSearches, hasSize(3));
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
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  public void testPostCheckInLocationId() {
   logger.info("=== Test POST Checkin - locationId checking ===");

    String poLineId = "fe47e95d-24e9-4a9a-9dc0-bcba64b51f56";

    CompositePoLine poLine = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(5).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLine));

    List<ToBeCheckedIn> toBeCheckedInList = new ArrayList<>();
    toBeCheckedInList.add(new ToBeCheckedIn()
      .withCheckedIn(1)
      .withPoLineId(poLineId)
      .withCheckInPieces(Arrays.asList(new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.ON_ORDER), new CheckInPiece().withItemStatus(CheckInPiece.ItemStatus.IN_PROCESS))));

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
    verifyOrderStatusUpdateEvent(1);


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
    verifyOrderStatusUpdateEvent(1);


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
  public void testPostReceivingPhysicalAll() {
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

    int expectedSearchRqQty = Math.floorDiv(receivingRq.getTotalRecords(), AbstractHelper.MAX_IDS_FOR_GET_RQ) + 1;

    // The piece searches should be made 2 times: 1st time to get all required piece records, 2nd time to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(expectedSearchRqQty + pieceIdsByPol.size()));
    assertThat(pieceUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(itemsSearches, hasSize(expectedSearchRqQty));
    assertThat(itemUpdates, hasSize(receivingRq.getTotalRecords()));
    assertThat(polSearches, hasSize(pieceIdsByPol.size()));
    assertThat(polUpdates, hasSize(pieceIdsByPol.size()));

    itemUpdates.forEach(item -> {
      assertThat(item.getString(ITEM_BARCODE), not(isEmptyString()));
      assertThat(item.getJsonObject(ITEM_STATUS), notNullValue());
      assertThat(item.getJsonObject(ITEM_STATUS).getString(ITEM_STATUS_NAME), equalTo(ReceivedItem.ItemStatus.IN_PROCESS.value()));
      assertThat(item.getString(ITEM_LEVEL_CALL_NUMBER), not(isEmptyString()));
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
  public void testPostCheckinElectronicPhysicalChangeLocationIdHoldingIsCreatedForPhysicalPiece() {

    logger.info("=== Test POST check-in - Check-in physical and electronic resource with new locationId ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId());
    poLine.setOrderFormat(CompositePoLine.OrderFormat.P_E_MIX);
    poLine.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE)); // holding mustn't be created
    poLine.setPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING)); // holding must be created


    String locationForPhysical = UUID.randomUUID().toString();
    String locationForElectronic = UUID.randomUUID().toString();

    Piece physicalPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.EXPECTED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.PHYSICAL)
      .withLocationId(locationForPhysical);
    Piece electronicPiece = getMinimalContentPiece(poLine.getId()).withReceivingStatus(Piece.ReceivingStatus.EXPECTED)
      .withFormat(org.folio.rest.jaxrs.model.Piece.Format.ELECTRONIC);

    addMockEntry(PURCHASE_ORDER, order.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN));
    addMockEntry(PO_LINES, poLine);
    addMockEntry(PIECES, physicalPiece);
    addMockEntry(PIECES, electronicPiece);

    MockServer.addMockTitles(Collections.singletonList(poLine));

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
    assertThat(getHoldingsSearches(), hasSize(1));
    assertThat(getCreatedHoldings(), hasSize(1));

    assertThat(getCreatedHoldings().get(0).getString(HOLDING_PERMANENT_LOCATION_ID), is(locationForPhysical));

  }

  @Test
  public void testPostReceivingElectronicPartially() {
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

    int expectedSearchRqQty = Math.floorDiv(receiving.getTotalRecords(), AbstractHelper.MAX_IDS_FOR_GET_RQ) + 1;

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
  public void testPostReceivingPhysicalWithErrors() throws IOException {
    logger.info("=== Test POST Receiving - Receive physical resources with different errors");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-physical-resources-6-of-10-with-errors.json").mapTo(ReceivingCollection.class);
    CompositePoLine poLines = getMockAsJson(POLINES_COLLECTION).getJsonArray("poLines").getJsonObject(3).mapTo(CompositePoLine.class);
    MockServer.addMockTitles(Collections.singletonList(poLines));
    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(receivingRq.getTotalRecords()));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);

    assertThat(receivingResult.getPoLineId(), not(isEmptyString()));
    assertThat(receivingResult.getProcessedSuccessfully(), is(5));
    assertThat(receivingResult.getProcessedWithError(), is(5));

    Set<String> errorCodes = new HashSet<>();
    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
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

  private void verifyProperQuantityOfHoldingsCreated(ReceivingCollection receivingRq) throws IOException {
    Set<String> expectedHoldings = new HashSet<>();

    // get processed poline
    PoLineCollection poLineCollection = new JsonObject(ApiTestBase.getMockData(POLINES_COLLECTION)).mapTo(PoLineCollection.class);
    PoLine poline = poLineCollection.getPoLines()
      .stream()
      .filter(poLine -> poLine.getId()
        .equals(receivingRq.getToBeReceived()
          .get(0)
          .getPoLineId()))
      .findFirst()
      .get();

    // get processed pieces for receiving
    PieceCollection pieces = new JsonObject(ApiTestBase.getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecordsCollection.json"))
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
            && isHoldingsUpdateRequired(piece, poline)) {
          expectedHoldings.add(getInstanceId(poline) + receivedItem.getLocationId());
        }
      }
    }
    Assert.assertEquals(expectedHoldings.size(), getCreatedHoldings().size());
  }

  private boolean isHoldingsUpdateRequired(org.folio.rest.acq.model.Piece piece, PoLine poLine) {
    if (piece.getFormat() == org.folio.rest.acq.model.Piece.Format.ELECTRONIC) {
     return isHoldingUpdateRequiredForEresource(poLine.getEresource());
    } else {
      return isHoldingUpdateRequiredForPhysical(poLine.getPhysical());
    }
  }


  @Test
  public void testPostReceivingWithErrorSearchingForPiece() {
    logger.info("=== Test POST Receiving - Receive resources with error searching for piece");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-500-error-for-pieces-lookup.json").mapTo(ReceivingCollection.class);

    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(1));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);
    assertThat(receivingResult.getPoLineId(), not(isEmptyString()));
    assertThat(receivingResult.getProcessedSuccessfully(), is(0));
    assertThat(receivingResult.getProcessedWithError(), is(1));

    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
      assertThat(receivingItemResult.getProcessingStatus(), not(nullValue()));
      assertThat(receivingItemResult.getProcessingStatus().getType(), is(ProcessingStatus.Type.FAILURE));
      assertThat(receivingItemResult.getProcessingStatus().getError().getCode(), is(PIECE_NOT_RETRIEVED.getCode()));
    }

    assertThat(getPieceSearches(), not(nullValue()));
    assertThat(getPieceUpdates(), is(nullValue()));
    assertThat(getItemsSearches(), is(nullValue()));
    assertThat(getItemUpdates(), is(nullValue()));
    assertThat(getPoLineSearches(), hasSize(1));
    assertThat(getPoLineUpdates(), is(nullValue()));

    // Verify messages sent via event bus
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  public void testPostReceivingWithErrorSearchingForItem() {
    logger.info("=== Test POST Receiving - Receive resources with error searching for item");

    ReceivingCollection receivingRq = getMockAsJson(RECEIVING_RQ_MOCK_DATA_PATH + "receive-500-error-for-items-lookup.json").mapTo(ReceivingCollection.class);
    receivingRq.getToBeReceived().forEach(toBeReceived -> {
      CompositePoLine poLine = getMinimalContentCompositePoLine().withId(toBeReceived.getPoLineId());
      addMockEntry(PO_LINES, poLine);
      addMockTitles(Collections.singletonList(poLine));
    });
    ReceivingResults results = verifyPostResponse(ORDERS_RECEIVING_ENDPOINT, JsonObject.mapFrom(receivingRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 200).as(ReceivingResults.class);

    assertThat(results.getTotalRecords(), is(1));
    assertThat(results.getReceivingResults(), hasSize(1));

    ReceivingResult receivingResult = results.getReceivingResults().get(0);
    assertThat(receivingResult.getPoLineId(), not(isEmptyString()));
    assertThat(receivingResult.getProcessedSuccessfully(), is(0));
    assertThat(receivingResult.getProcessedWithError(), is(1));

    for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
      assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
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
  public void testPostReceivingRevertMixedResources() {
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

    // The piece searches should be made 3 times: 1st time to get all required piece records, 2nd and 3rd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(3));
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
  public void testPostReceivingRevertElectronicResource() {
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

    // The piece searches should be made 3 times: 1st time to get piece record, 2nd and 3rd times to calculate expected PO Line status
    assertThat(pieceSearches, hasSize(3));
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
        assertThat(r.getPieceId(), isIn(pieceIds));
      }
    }
  }

  private Map<String, Set<String>> verifyReceivingSuccessRs(ReceivingResults results) {
    Map<String, Set<String>> pieceIdsByPol = new HashMap<>();
    for (ReceivingResult receivingResult : results.getReceivingResults()) {
      assertThat(receivingResult.getPoLineId(), not(isEmptyString()));
      assertThat(receivingResult.getProcessedSuccessfully(), is(receivingResult.getReceivingItemResults().size()));
      assertThat(receivingResult.getProcessedWithError(), is(0));

      for (ReceivingItemResult receivingItemResult : receivingResult.getReceivingItemResults()) {
        assertThat(receivingItemResult.getPieceId(), not(isEmptyString()));
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
