package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.ApiTestSuite.mockPort;
import static org.folio.helper.InventoryHelper.ITEMS;
import static org.folio.helper.InventoryHelper.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.MockServer.ITEMS_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.COMP_PO_LINES_MOCK_DATA_PATH;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING_ITEM;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.models.PoLineUpdateHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class InventoryHelperTest extends ApiTestBase {
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);
  public static final String HOLDING_INSTANCE_ID = "5294d737-a04b-4158-857a-3f3c555bcc60";
  public static final String OLD_LOCATION_ID = "758258bc-ecc1-41b8-abca-f7b610822fff";
  public static final String NEW_LOCATION_ID = "fcd64ce1-6995-48f0-840e-89ffa2288371";
  public static final String OLD_HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d63";
  public static final String NON_EXISTED_NEW_HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d23";
  public static final String ONLY_NEW_HOLDING_EXIST_ID = "65cb2bf0-d4c2-4822-8ad0-b76f1ba75d22";
  public static final String HOLDING_INSTANCE_ID_2_HOLDING = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d48";

  private Context ctxMock;
  private Map<String, String> okapiHeadersMock;
  private HttpClientInterface httpClient;


  @BeforeEach
  public void initMocks(){
    super.setUp();
    ctxMock = Vertx.vertx().getOrCreateContext();
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    httpClient = HttpClientFactory.getHttpClient(okapiURL, TENANT_ID);
    MockitoAnnotations.openMocks(this);
  }


  @Test
  public void testShouldUpdateAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).updateItem(any());
    //When
    JsonObject item1 = new JsonObject().put("id", UUID.randomUUID().toString());
    JsonObject item2 = new JsonObject().put("id", UUID.randomUUID().toString());
    List<JsonObject> items = Arrays.asList(item1, item2);
    inventoryHelper.updateItemRecords(items).join();
    //Then
    verify(inventoryHelper, times(1)).updateItem(item1);
    verify(inventoryHelper, times(1)).updateItem(item2);
  }

  @Test
  public void testShouldNotUpdateItemIfProvidedListEmpty() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).updateItem(any());
    //When
    inventoryHelper.updateItemRecords(Collections.emptyList()).join();
    //Then
    verify(inventoryHelper, times(0)).updateItem(any());
  }

  @Test
  public void testShouldDeleteAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    String itemId1 = UUID.randomUUID().toString();
    String itemId2 = UUID.randomUUID().toString();
    List<String> items = Arrays.asList(itemId1, itemId2);
    inventoryHelper.deleteItems(items).join();
    //Then
    verify(inventoryHelper, times(1)).deleteItem(itemId1);
    verify(inventoryHelper, times(1)).deleteItem(itemId2);
  }

  @Test
  public void testShouldNotDeleteItemIfProvidedListEmpty() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    inventoryHelper.deleteItems(Collections.emptyList()).join();
    //Then
    verify(inventoryHelper, times(0)).updateItem(any());
  }

  @Test
  public void testShouldUpdateHoldingsRecordIfOldAndNewLocationProvided() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
                                        .withOldLocationId(OLD_LOCATION_ID)
                                        .withNewLocationId(NEW_LOCATION_ID);
    inventoryHelper.updateHoldingsRecord(holder).join();
    //Then
    assertThat(holder.getOldHoldingId(), equalTo(OLD_HOLDING_ID));
  }

  @Test
  public void testShouldCreateNewHoldingsRecordIfOnlyOldLocationProvided() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
                                                        .withOldLocationId(OLD_LOCATION_ID)
                                                        .withNewLocationId(NON_EXISTED_NEW_HOLDING_ID);
    inventoryHelper.updateHoldingsRecord(holder).join();
    //Then
    verify(inventoryHelper, times(1)).getOrCreateHoldingsRecord(HOLDING_INSTANCE_ID, NON_EXISTED_NEW_HOLDING_ID);
    assertThat(holder.getNewLocationId(), equalTo(NON_EXISTED_NEW_HOLDING_ID));
  }

  @Test
  public void testShouldThrowExceptionIfHoldingWithOldLocationIsNotExist() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
      .withOldLocationId(UUID.randomUUID().toString())
      .withNewLocationId(NON_EXISTED_NEW_HOLDING_ID);
    CompletableFuture<Void> result = inventoryHelper.updateHoldingsRecord(holder);
    //Then
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(400, httpException.getCode());
  }

  @Test
  public void testShouldNotHandleItemRecordsIfCheckinItemsIsTrue() {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    reqData.setCheckinItems(true);

    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    //When

    Location line = new Location().withLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    List<Piece> pieces = inventoryHelper.handleItemRecords(reqData, OLD_HOLDING_ID, Collections.singletonList(line)).join();

    assertEquals(0, pieces.size());
  }

  @Test
  public void testShouldNotUpdateHolderIfReturnMoreThen2Record() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID_2_HOLDING)
      .withOldLocationId(OLD_LOCATION_ID);
    inventoryHelper.updateHoldingsRecord(holder).join();
    //Then
    verify(inventoryHelper, times(0)).getOrCreateHoldingsRecord(HOLDING_INSTANCE_ID_2_HOLDING, OLD_LOCATION_ID);
    assertNull(holder.getNewLocationId());
    assertThat(holder.getOldLocationId(), equalTo(OLD_LOCATION_ID));
  }

  @Test
  public void testHandleHoldingsAndItemsRecordsIsNotRequired() {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    reqData.setCheckinItems(true);

    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    //When

    Location line = new Location().withLocationId(NEW_LOCATION_ID);
    PoLine storageData = JsonObject.mapFrom(reqData).mapTo(PoLine.class);
    storageData.setLocations(Collections.singletonList(line));
    List<Piece> pieces = inventoryHelper.handleHoldingsAndItemsRecords(reqData, storageData).join();

    assertEquals(0, pieces.size());
  }


  @Test
  public void testShouldNotHandleItemRecordsIfCheckinItemsIsTrueInUpdatePoLIneTime() {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    reqData.setCheckinItems(true);

    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    //When
    PoLineUpdateHolder poLineUpdateHolder = new PoLineUpdateHolder().withNewLocationId(NEW_LOCATION_ID);
//    List<Piece> pieces = inventoryHelper.handleItemRecords(reqData, poLineUpdateHolder).join();
//
//    assertEquals(0, pieces.size());
  }

  @Test
  public void testShouldHandleItemRecordsIfPhysycAndElecPresentInUpdatePoLIneTime() throws IOException {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c0d08448-347b-418a-8c2f-5fb50248d67e").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67d";
    String itemId = "86481a22-633e-4b97-8061-0dc5fdaaeabb";
    String materialType = "1a54b431-2e4f-452d-9cae-9cee66c9a892";
    String locationId = "758258bc-ecc1-41b8-abca-f7b610822fff";

    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getPhysical().setMaterialType(materialType);
    reqData.getPhysical().setMaterialSupplier(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    reqData.getLocations().get(0).setLocationId(locationId);
    reqData.getLocations().get(0).setQuantityPhysical(1);
    reqData.getLocations().get(0).setQuantity(1);
    reqData.getCost().setQuantityPhysical(1);

    JsonObject items = new JsonObject(ApiTestBase.getMockData(ITEMS_RECORDS_MOCK_DATA_PATH + "inventoryItemsCollection.json"));
    List<JsonObject> needUpdateItems = items.getJsonArray(ITEMS).stream()
                                  .map(o -> ((JsonObject) o))
                                  .filter(item -> item.getString(ID).equals(itemId))
                                  .map(item -> item.put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, poLineId))
                                  .collect(toList());;

    String path = PIECE_RECORDS_MOCK_DATA_PATH + String.format("pieceRecords-%s.json", poLineId);
    PieceCollection existedPieces = new JsonObject(ApiTestBase.getMockData(path)).mapTo(PieceCollection.class);
    existedPieces.getPieces().get(0).setItemId(itemId);
    existedPieces.getPieces().get(0).setFormat(Piece.PieceFormat.PHYSICAL);
    existedPieces.getPieces().get(0).setPoLineId(poLineId);
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(existedPieces)).when(inventoryHelper).getExpectedPiecesByLineId(poLineId);
    doReturn(completedFuture(needUpdateItems)).when(inventoryHelper).getItemRecordsByIds(Collections.singletonList(itemId));
    doReturn(completedFuture(null)).when(inventoryHelper).updateItemRecords(any());
    //When
//    PoLineUpdateHolder poLineUpdateHolder = new PoLineUpdateHolder().withNewLocationId(locationId);
//    List<Piece> pieces = inventoryHelper.handleItemRecords(reqData, poLineUpdateHolder).join();
//
//    assertEquals(1, pieces.size());
//    assertEquals(itemId, pieces.get(0).getItemId());
//    assertEquals(locationId, pieces.get(0).getLocationId());
  }
}
