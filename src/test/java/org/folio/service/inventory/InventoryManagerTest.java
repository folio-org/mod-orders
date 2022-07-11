package org.folio.service.inventory;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.ACTIVE_ACCESS_PROVIDER_B;
import static org.folio.TestConstants.PIECE_PATH;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.orders.utils.HelperUtils.extractId;
import static org.folio.orders.utils.HelperUtils.getFirstObjectFromResponse;
import static org.folio.rest.RestConstants.NOT_FOUND;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.exceptions.ErrorCodes.HOLDINGS_BY_ID_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.PARTIALLY_RETURNED_COLLECTION;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.HOLDINGS_OLD_NEW_PATH;
import static org.folio.rest.impl.MockServer.ITEMS_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.COMP_PO_LINES_MOCK_DATA_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING;
import static org.folio.service.inventory.InventoryManager.HOLDINGS_RECORDS;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.folio.service.inventory.InventoryManager.HOLDINGS_SOURCES;
import static org.folio.service.inventory.InventoryManager.ITEMS;
import static org.folio.service.inventory.InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.TestConstants;
import org.folio.models.PoLineUpdateHolder;
import org.folio.rest.core.PostResponseType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.hamcrest.core.IsInstanceOf;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class InventoryManagerTest {
  private static final Logger logger = LogManager.getLogger();

  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String OLD_LOCATION_ID = "758258bc-ecc1-41b8-abca-f7b610822fff";
  public static final String NEW_LOCATION_ID = "fcd64ce1-6995-48f0-840e-89ffa2288371";
  public static final String NON_EXISTED_NEW_HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d23";
  public static final String ONLY_NEW_HOLDING_EXIST_ID = "65cb2bf0-d4c2-4822-8ad0-b76f1ba75d22";
  public static final String HOLDING_INSTANCE_ID_2_HOLDING = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d48";
  private static final String TILES_PATH = BASE_MOCK_DATA_PATH + "titles/";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String INSTANCE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instances/" + "instance.json";
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  public static final String HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d61";
  private static final JsonObject HOLDINGS_SOURCE_ID_RESPONSE = new JsonObject()
    .put(HOLDINGS_SOURCES, "f32d531e-df79-46b3-8932-cdd35f7a2264");

  @Autowired
  InventoryManager inventoryManager;
  @Autowired
  private RestClient restClient;
  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private ConfigurationEntriesService configurationEntriesService;


  private Map<String, String> okapiHeadersMock;
  private Context ctxMock;
  private RequestContext requestContext;

  private HttpClientInterface httpClient;
  private static boolean runningOnOwn;

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(InventoryManagerTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @BeforeEach
  void beforeEach() {
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    httpClient = HttpClientFactory.getHttpClient(okapiURL, X_OKAPI_TENANT.getValue());
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  @Test
  void shouldReturnHoldingsByIdsIfTheyExist() throws IOException, ExecutionException, InterruptedException {
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
       .map(o -> ((JsonObject) o))
       .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());

    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    List<JsonObject> actHoldings = inventoryManager.getHoldingsByIds(holdingIds, requestContext).get();
    assertThat(actHoldings.size(), equalTo(holdings.size()));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldTrowExceptionHoldingsByIdsIfNotAllOfThemExist() throws IOException, ExecutionException, InterruptedException {
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());
    holdingIds.add(UUID.randomUUID().toString());
    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    CompletableFuture<List<JsonObject>> resultFuture = inventoryManager.getHoldingsByIds(holdingIds, requestContext);

    ExecutionException executionException = assertThrows(ExecutionException.class, resultFuture::get);

    assertThat(executionException.getCause(), IsInstanceOf.instanceOf(HttpException.class));

    HttpException httpException = (HttpException) executionException.getCause();
    assertEquals(404, httpException.getCode());
    assertEquals(PARTIALLY_RETURNED_COLLECTION.toError().getCode(), httpException.getError().getCode());
  }

  @Test
  void shouldReturnHoldingsByInstanceIdAndLocationIdsIfTheyExist() throws IOException, ExecutionException, InterruptedException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());

    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    List<JsonObject> actHoldings = inventoryManager.getHoldingRecords(instanceId, locationIds, requestContext).get();
    assertThat(actHoldings.size(), equalTo(holdings.size()));
    verify(restClient, times(2)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldReturnHoldingsByInstanceIdAndLocationIdIfTheyExist() throws IOException, ExecutionException, InterruptedException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());

    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    inventoryManager.getFirstHoldingRecord(instanceId, locationIds.get(0), requestContext).get();
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void testShouldUpdateAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    doReturn(completedFuture(null)).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    JsonObject item1 = new JsonObject().put("id", UUID.randomUUID().toString());
    JsonObject item2 = new JsonObject().put("id", UUID.randomUUID().toString());
    List<JsonObject> items = Arrays.asList(item1, item2);
    //When
    inventoryManager.updateItemRecords(items, requestContext).join();
    //Then
    verify(restClient, times(1)).put(any(RequestEntry.class), eq(item1), eq(requestContext));
    verify(restClient, times(1)).put(any(RequestEntry.class), eq(item2), eq(requestContext));
  }

  @Test
  void testShouldNotUpdateItemIfProvidedListEmpty() {
    //given
    doReturn(completedFuture(null)).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    //When
    inventoryManager.updateItemRecords(Collections.emptyList(), requestContext).join();
    //Then
    verify(restClient, times(0)).put(any(RequestEntry.class),any(JsonObject.class), eq(requestContext));
  }

  @Test
  void testShouldDeleteAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    String itemId1 = UUID.randomUUID().toString();
    String itemId2 = UUID.randomUUID().toString();
    List<String> items = Arrays.asList(itemId1, itemId2);

    doReturn(completedFuture(null)).when(restClient).delete(any(RequestEntry.class), eq(false), eq(requestContext));
    //When
    inventoryManager.deleteItems(items, false, requestContext).join();
    //Then
    verify(restClient, times(2)).delete(any(RequestEntry.class), eq(false), eq(requestContext));
  }

  @Test
  void testShouldNotDeleteItemIfProvidedListEmpty() {
    //given
    doReturn(completedFuture(null)).when(restClient).delete(any(RequestEntry.class), eq(requestContext));
    //When
    inventoryManager.deleteItems(Collections.emptyList(), false, requestContext).join();
    //Then
    verify(restClient, times(0)).delete(any(RequestEntry.class), eq(requestContext));
  }


  @Test
  void testShouldNotHandleItemRecordsIfCheckinItemsIsTrue() {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    reqData.setCheckinItems(true);

    //When
    Location location = new Location().withLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    List<Piece> pieces = inventoryManager.handleItemRecords(reqData, location, requestContext).join();

    assertEquals(0, pieces.size());
  }

  @Test
  void testShouldNotHandleItemRecordsIfCheckinItemsIsTrueInUpdatePoLIneTime() {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    reqData.setCheckinItems(true);

    CompositePoLine storagePoLineCom = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c0d08448-347b-418a-8c2f-5fb50248d67e").mapTo(CompositePoLine.class);
    storagePoLineCom.setAlerts(null);
    storagePoLineCom.setReportingCodes(null);
    storagePoLineCom.getLocations().get(0).setQuantityPhysical(1);
    storagePoLineCom.getLocations().get(0).setQuantity(1);
    storagePoLineCom.getCost().setQuantityPhysical(1);
    //When
    PoLineUpdateHolder poLineUpdateHolder = new PoLineUpdateHolder().withNewLocationId(NEW_LOCATION_ID);
    List<Piece> pieces = inventoryManager.handleItemRecords(reqData, poLineUpdateHolder, requestContext).join();

    assertEquals(0, pieces.size());
  }

  @Test
  void testShouldHandleItemRecordsIfPhysycPresentInUpdatePoLineTime() throws IOException {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c0d08448-347b-418a-8c2f-5fb50248d67e").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67d";
    String itemId = "86481a22-633e-4b97-8061-0dc5fdaaeabb";
    String materialType = "1a54b431-2e4f-452d-9cae-9cee66c9a892";
    String locationId = "758258bc-ecc1-41b8-abca-f7b610822fff";
    String oldLocationId = "fcd64ce1-6995-48f0-840e-89ffa2288371";

    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getPhysical().setMaterialType(materialType);
    reqData.getPhysical().setMaterialSupplier(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    reqData.getLocations().get(0).setQuantityPhysical(1);
    reqData.getLocations().get(0).setQuantity(1);
    reqData.getCost().setQuantityPhysical(1);
    reqData.getLocations().get(0).setLocationId(locationId);

    CompositePoLine storagePoLineCom = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c0d08448-347b-418a-8c2f-5fb50248d67e").mapTo(CompositePoLine.class);
    storagePoLineCom.setAlerts(null);
    storagePoLineCom.setReportingCodes(null);
    storagePoLineCom.getLocations().get(0).setQuantityPhysical(1);
    storagePoLineCom.getLocations().get(0).setQuantity(1);
    storagePoLineCom.getCost().setQuantityPhysical(1);

    JsonObject items = new JsonObject(getMockData(ITEMS_RECORDS_MOCK_DATA_PATH + "inventoryItemsCollection.json"));
    List<JsonObject> needUpdateItems = items.getJsonArray(ITEMS).stream()
      .map(o -> ((JsonObject) o))
      .filter(item -> item.getString(TestConstants.ID).equals(itemId))
      .map(item -> item.put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, poLineId))
      .collect(toList());

    String path = PIECE_RECORDS_MOCK_DATA_PATH + String.format("pieceRecords-%s.json", poLineId);
    PieceCollection existedPieces = new JsonObject(getMockData(path)).mapTo(PieceCollection.class);
    existedPieces.getPieces().get(0).setItemId(itemId);
    existedPieces.getPieces().get(0).setFormat(Piece.Format.PHYSICAL);
    existedPieces.getPieces().get(0).setPoLineId(poLineId);
    //given
    doReturn(completedFuture(existedPieces)).when(pieceStorageService).getExpectedPiecesByLineId(poLineId, requestContext);
    doReturn(completedFuture(needUpdateItems)).when(inventoryManager).getItemRecordsByIds(Collections.singletonList(itemId), requestContext);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemRecords(any(), eq(requestContext));
    doReturn(completedFuture(null)).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    //When
    PoLineUpdateHolder poLineUpdateHolder = new PoLineUpdateHolder().withOldLocationId(oldLocationId).withNewLocationId(locationId);
    List<Piece> pieces = inventoryManager.handleItemRecords(reqData, poLineUpdateHolder, requestContext).join();

    assertEquals(1, pieces.size());
    assertEquals(itemId, pieces.get(0).getItemId());
    assertEquals(locationId, pieces.get(0).getLocationId());
  }

  @Test
  void testShouldBuildInstanceWithoutTitleFields() {
    //given
    Title title = spy(getMockAsJson(TILES_PATH,"title").mapTo(Title.class));
    title.setContributors(null);
    title.setPublishedDate(null);
    title.setPublisher(null);
    title.setProductIds(null);
    JsonObject statuseJSON = new JsonObject("{\"instanceTypes\":\"30fffe0e-e985-4144-b2e2-1e8179bdb41f\"" +
      ",\"instanceStatuses\":\"daf2681c-25af-4202-a3fa-e58fdf806183\"}");
    //When
    inventoryManager.buildInstanceRecordJsonObject(title, statuseJSON);
    //Then
    verify(title).getContributors();
    verify(title, times(1)).getPublishedDate();
    verify(title, times(1)).getPublisher();
    verify(title).getProductIds();
  }

  @Test
  void testShouldBuildInstanceWithPublishedDateFromTitle() {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Title title = spy(getMockAsJson(TILES_PATH,"title").mapTo(Title.class));
    title.setContributors(line.getContributors());
    title.setPublishedDate(line.getPublicationDate());
    title.setPublisher(null);
    title.setProductIds(line.getDetails().getProductIds());
    title.setEdition("Edition");
    JsonObject statuseJSON = new JsonObject("{\"instanceTypes\":\"30fffe0e-e985-4144-b2e2-1e8179bdb41f\"" +
      ",\"instanceStatuses\":\"daf2681c-25af-4202-a3fa-e58fdf806183\"}");
    //When
    inventoryManager.buildInstanceRecordJsonObject(title, statuseJSON);
    //Then
    verify(title).getContributors();
    verify(title, times(2)).getPublishedDate();
    verify(title, times(2)).getPublisher();
    verify(title).getProductIds();
  }

  @Test
  void testShouldBuildInstanceWithPublisherFromTitle() {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Title title = spy(getMockAsJson(TILES_PATH,"title").mapTo(Title.class));
    title.setContributors(line.getContributors());
    title.setPublishedDate(null);
    title.setPublisher(line.getPublisher());
    title.setProductIds(line.getDetails().getProductIds());
    title.setEdition("Edition");
    JsonObject statuseJSON = new JsonObject("{\"instanceTypes\":\"30fffe0e-e985-4144-b2e2-1e8179bdb41f\"" +
      ",\"instanceStatuses\":\"daf2681c-25af-4202-a3fa-e58fdf806183\"}");
    //When
    inventoryManager.buildInstanceRecordJsonObject(title, statuseJSON);
    //Then
    verify(title).getContributors();
    verify(title, times(1)).getPublishedDate();
    verify(title, times(2)).getPublisher();
    verify(title).getProductIds();
  }

  @Test
  void testShouldBuildInstanceWithFieldFromTitles() {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Title title = spy(getMockAsJson(TILES_PATH,"title").mapTo(Title.class));
    title.setContributors(line.getContributors());
    title.setPublishedDate(line.getPublicationDate());
    title.setPublisher(line.getPublisher());
    title.setProductIds(line.getDetails().getProductIds());
    title.setEdition("Edition");
    JsonObject statuseJSON = new JsonObject("{\"instanceTypes\":\"30fffe0e-e985-4144-b2e2-1e8179bdb41f\"" +
      ",\"instanceStatuses\":\"daf2681c-25af-4202-a3fa-e58fdf806183\"}");
    //When
    inventoryManager.buildInstanceRecordJsonObject(title, statuseJSON);
    //Then
    verify(title).getContributors();
    verify(title, times(1)).getPublishedDate();
    verify(title, times(2)).getPublisher();
    verify(title).getProductIds();
  }

  @Test
  void shouldCheckIfTheHoldingExistsWhenHoldingIdSpecifiedAndIfExistThenReturnHoldingIdFromLocation() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    JsonObject holdingExp = getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS);
    String holdingIdExp = extractId(holdingExp);
    Location location = new Location().withHoldingId(holdingIdExp).withQuantity(1).withQuantityPhysical(1);

    doReturn(completedFuture(holdingExp)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    String holdingIdAct = inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext).join();

    assertThat(holdingIdAct, equalTo(holdingIdExp));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingExistsWhenLocationIdAlwaysNewHoldingShouldBeCreated() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    String holdingIdExp = extractId(getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS));
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withLocationId(locationIds.get(0)).withQuantity(1).withQuantityPhysical(1);

    doReturn(completedFuture(holdingIdExp)).when(restClient).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    doReturn(completedFuture(HOLDINGS_SOURCE_ID_RESPONSE)).when(inventoryManager).getEntryId(eq(HOLDINGS_SOURCES), any(ErrorCodes.class), eq(requestContext));

    String holdingIdAct = inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext).join();

    assertThat(holdingIdAct, equalTo(holdingIdExp));
    verify(restClient, times(1)).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));

  }

  @Test
  void shouldCheckIfTheHoldingExistsWhenLocationIdSpecifiedAndIfNotExistThenCreateNewHoldingReturnHoldingId() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    String holdingIdExp = extractId(getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS));
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withLocationId(locationIds.get(0)).withQuantity(1).withQuantityPhysical(1);

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(InventoryManager.HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(InventoryManager.HOLDING_PERMANENT_LOCATION_ID, locationIds.get(0));
    JsonObject emptyHoldingCollection = new JsonObject().put(HOLDINGS_RECORDS, new JsonArray());

    doReturn(completedFuture(emptyHoldingCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(completedFuture(holdingIdExp)).when(restClient).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    doReturn(completedFuture(HOLDINGS_SOURCE_ID_RESPONSE)).when(inventoryManager).getEntryId(eq(HOLDINGS_SOURCES), any(ErrorCodes.class), eq(requestContext));

    String holdingIdAct = inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext).join();

    assertThat(holdingIdAct, equalTo(holdingIdExp));
    verify(restClient, times(1)).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
  }

  @Test
  void shouldThrowExceptionIfHoldingIsNotAlreadyExist() {
    String instanceId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    Location location = new Location().withHoldingId(holdingId).withQuantity(1).withQuantityPhysical(1);
    String msg = String.format(HOLDINGS_BY_ID_NOT_FOUND.getDescription(), holdingId);
    Error error = new Error().withCode(HOLDINGS_BY_ID_NOT_FOUND.getCode()).withMessage(msg);

    when(restClient.getAsJsonObject(any(RequestEntry.class), eq(requestContext)))
      .thenThrow(new CompletionException(new HttpException(NOT_FOUND, error)));

    CompletionException exception = assertThrows(CompletionException.class,
      () -> inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext).join());

    assertThat(exception.getCause(), IsInstanceOf.instanceOf(HttpException.class));
    HttpException cause = (HttpException) exception.getCause();
    assertEquals(NOT_FOUND, cause.getCode());
    assertEquals(error, cause.getError());
  }

  @Test
  void shouldRetrieveItemIfHoldingIdProvidedAndHoldingFound() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    doReturn(completedFuture(holding)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    JsonObject holdingIdAct = inventoryManager.getHoldingById(holdingId, true, requestContext).join();

    assertThat(holding, equalTo(holdingIdAct));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
  }

  @Test
  void shouldRetrieveItemIfHoldingIdAndOrderLineProvided() throws ExecutionException, InterruptedException {
    String itemId = UUID.randomUUID().toString();
    String poLineId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String holdingsRecordId = UUID.randomUUID().toString();

    JsonObject item = new JsonObject();
    item.put(ID, itemId);

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(ID, holdingId);
    holdingsRecJson.put(InventoryManager.ITEM_HOLDINGS_RECORD_ID, holdingsRecordId);
    holdingsRecJson.put(InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, poLineId);

    JsonObject holdingsRecJsonColl = new JsonObject();
    holdingsRecJsonColl.put(HOLDINGS_RECORDS, new JsonArray().add(holdingsRecJson));
    holdingsRecJsonColl.put(ITEMS, new JsonArray().add(item));

    doReturn(completedFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    List<JsonObject> items = inventoryManager.getItemsByHoldingIdAndOrderLineId(holdingsRecordId, poLineId, requestContext).get();

    assertThat(1, equalTo(items.size()));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldRetrieveItemIfHoldingIdProvidedAndHoldingFoundAndNotSkipNotFound() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    doReturn(completedFuture(holding)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(false), eq(requestContext));
    JsonObject holdingIdAct = inventoryManager.getHoldingById(holdingId, requestContext).join();

    assertThat(holding, equalTo(holdingIdAct));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(false), eq(requestContext));
  }

  @Test
  void shouldRetrieveEmptyJsonObjectItemIfHoldingIdIsNotProvidedAndHoldingFound() {
    JsonObject holding = new JsonObject();
    JsonObject holdingIdAct = inventoryManager.getHoldingById(null, true, requestContext).join();

    assertThat(holding, equalTo(holdingIdAct));
    verify(restClient, times(0)).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
  }

  @Test
  void testShouldSkipCreationNewInstanceIfInstanceIdIsProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    CompletableFuture<Title> result = inventoryManager.openOrderHandlePackageLineInstance(title, false, requestContext);
    //Then
    Title actTitle = result.get();
    assertEquals(title, actTitle);
  }

  @Test
  void testShouldCreateInstanceRecordIfInstanceMatchingIsDisabled() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    inventoryManager.getOrCreateInstanceRecord(title, true, requestContext).get();
    //Then
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testShouldCreateInstanceIfInstanceIdIsNotProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setInstanceId(null);
    mock(PieceService.class, CALLS_REAL_METHODS);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).getOrCreateInstanceRecord(any(Title.class), any(Boolean.class), eq(requestContext));
    //When
    inventoryManager.openOrderHandlePackageLineInstance(title, false, requestContext).get();
    //Then
    verify(inventoryManager, times(1)).getOrCreateInstanceRecord(title, false, requestContext);
  }

  @Test
  void testShouldCreateInstanceRecordIfProductIsEmpty() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setProductIds(null);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    inventoryManager.getOrCreateInstanceRecord(title, requestContext).get();
    //Then
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testShouldCreateInstanceRecordIfProductPresentAndInstancesNotFoundInDB() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    doReturn(completedFuture(new JsonObject("{\"instances\" : []}"))).when(inventoryManager).searchInstancesByProducts(any(List.class), eq(requestContext));
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    inventoryManager.getOrCreateInstanceRecord(title, requestContext).get();
    //Then
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testShouldNotCreateInstanceRecordIfInstancesFoundInDB() throws ExecutionException, InterruptedException, IOException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    JsonObject instances = new JsonObject(getMockData(INSTANCE_RECORDS_MOCK_DATA_PATH));
    doReturn(completedFuture(instances)).when(inventoryManager).searchInstancesByProducts(any(List.class), eq(requestContext));
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    inventoryManager.getOrCreateInstanceRecord(title, false, requestContext).get();
    //Then
    verify(inventoryManager, times(0)).createInstanceRecord(any(Title.class), eq(requestContext));
    verify(inventoryManager, times(1)).searchInstancesByProducts(any(List.class), eq(requestContext));
  }


  @Test
  void testShouldCreateItemRecordForEresources() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Eresource eresource = new Eresource().withMaterialType(line.getPhysical().getMaterialType())
      .withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    line.setPhysical(null);
    line.setEresource(eresource);
    line.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    String expItemId = UUID.randomUUID().toString();
    doReturn(completedFuture(Collections.singletonList(expItemId)))
      .when(inventoryManager).createMissingElectronicItems(any(CompositePoLine.class), any(Piece.class), eq(1), eq(requestContext));
    //When
    CompletableFuture<String> result = inventoryManager.openOrderCreateItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    verify(inventoryManager).createMissingElectronicItems(any(CompositePoLine.class), any(Piece.class), eq(1), eq(requestContext));
    assertEquals(expItemId, actItemId);
  }

  @Test
  void testShouldCreateItemRecordForPhysical() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    String expItemId = UUID.randomUUID().toString();

    doReturn(completedFuture(Collections.singletonList(expItemId)))
      .when(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), any(Piece.class), eq(1), eq(requestContext));
    //When
    CompletableFuture<String> result = inventoryManager.openOrderCreateItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    verify(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), any(Piece.class), eq(1), eq(requestContext));
    assertEquals(expItemId, actItemId);
  }

  @Test
  void testHoldingsItemCreationShouldBeSkippedIfEresourceOrPhysicsIsAbsent() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setEresource(null);
    line.setPhysical(null);

    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(completedFuture(HOLDING_ID)).when(inventoryManager).getOrCreateHoldingsRecord(anyString(), any(Location.class), eq(requestContext));
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    CompletableFuture<String> result = inventoryManager.handleHoldingsRecord(line, location, title.getInstanceId(), requestContext);

    //Then
    String holdingId = result.get();
    verify(inventoryManager,never()).getOrCreateHoldingsRecord(title.getInstanceId(), location, requestContext);
    assertNull(holdingId);
  }

  @Test
  void testHoldingsItemShouldNotBeCreatedIfPOLIsNull() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    CompletableFuture<String> result = inventoryManager.handleHoldingsRecord(null, location, title.getInstanceId(), requestContext);
    //Then
    assertTrue(result.isCompletedExceptionally());
  }

  @Test
  void testHoldingsRecordShouldBeCreated() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(completedFuture(HOLDING_ID)).when(inventoryManager).getOrCreateHoldingsRecord(anyString(), any(Location.class), eq(requestContext));
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    CompletableFuture<String> result = inventoryManager.handleHoldingsRecord(line, location, title.getInstanceId(), requestContext);
    String actHoldingId = result.get();
    //Then
    verify(inventoryManager).getOrCreateHoldingsRecord(eq(title.getInstanceId()), eq(location), eq(requestContext));
    assertEquals(HOLDING_ID, actHoldingId);
  }

  @Test
  void testUpdateInventoryNegativeCaseIfPOLIsNull() {
    //When
    CompletableFuture<String> result =  inventoryManager.openOrderCreateItemRecord(null, UUID.randomUUID().toString(), requestContext);
    //Then
    assertTrue(result.isCompletedExceptionally());
  }

  @Test
  void shouldCheckIfThatInstanceReferenceForListOfHoldingAreUpdated() throws IOException {
    String oldInstanceId = UUID.randomUUID().toString();
    String newInstanceId = UUID.randomUUID().toString();

    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    String holdingIdExp = extractId(getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS));
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(InventoryManager.HOLDING_INSTANCE_ID, oldInstanceId);
    holdingsRecJson.put(InventoryManager.HOLDING_PERMANENT_LOCATION_ID, locationIds.get(0));

    doReturn(completedFuture(holdingIdExp)).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));

    inventoryManager.updateInstanceForHoldingRecords(List.of(holdingsRecJson), newInstanceId, requestContext).join();

    assertThat(holdingsRecJson.getString(InventoryManager.HOLDING_INSTANCE_ID), equalTo(newInstanceId));
    verify(restClient, times(1)).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingExistsWhenHoldingIdSpecifiedAndIfExistThenHoldingWillBeRetrieved() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    String holdingIdExp = extractId(getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS));
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withHoldingId(holdingIdExp).withQuantity(1).withQuantityPhysical(1);

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(InventoryManager.HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(InventoryManager.HOLDING_PERMANENT_LOCATION_ID, locationIds.get(0));

    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    JsonObject holdingIdAct = inventoryManager.getOrCreateHoldingsJsonRecord(instanceId, location, requestContext).join();

    assertThat(holdingIdAct.size(), equalTo(holdingsCollection.size()));
    verify(restClient, times(0)).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingNotExistWhenHoldingIdSpecifiedThenErrorReturned() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    String holdingIdExp = extractId(getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS));
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withHoldingId(holdingIdExp).withQuantity(1).withQuantityPhysical(1);

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(InventoryManager.HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(InventoryManager.HOLDING_PERMANENT_LOCATION_ID, locationIds.get(0));

    doReturn(CompletableFuture.failedFuture(new CompletionException(new HttpException(NOT_FOUND, "Not_Found"))))
                  .when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    CompletionException exceptionAct = assertThrows(CompletionException.class,
                  () -> inventoryManager.getOrCreateHoldingsJsonRecord(instanceId, location, requestContext).join());

    HttpException httpException = (HttpException) exceptionAct.getCause();
    assertThat(httpException.getError().getCode(), is(HOLDINGS_BY_ID_NOT_FOUND.getCode()));

    verify(restClient, times(0)).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingWillBeRetrievedByInstanceId() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    JsonObject firstHoldingJson = getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS);
    String holdingIdExp = extractId(firstHoldingJson);
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding -> holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withHoldingId(holdingIdExp).withQuantity(1).withQuantityPhysical(1);

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(ID, holdingIdExp);
    holdingsRecJson.put(InventoryManager.HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(InventoryManager.HOLDING_PERMANENT_LOCATION_ID, locationIds.get(0));

    JsonObject holdingsRecJsonColl = new JsonObject();
    holdingsRecJsonColl.put(HOLDINGS_RECORDS, new JsonArray().add(holdingsRecJson));

    doReturn(completedFuture(holdingsRecJson)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    doReturn(completedFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    String holdingIdAct = inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(instanceId, location, requestContext).join();

    assertThat(holdingIdAct, equalTo(holdingIdExp));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    verify(restClient, times(0)).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingWillBeRetrievedByIdAndWasNotFoundByInstanceIdAndLOcationThenCreateNewHolding() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    JsonObject firstHoldingJson = getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS);
    String firstHoldingIdExp = extractId(firstHoldingJson);
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding -> holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withHoldingId(firstHoldingIdExp).withQuantity(1).withQuantityPhysical(1);

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(ID, firstHoldingIdExp);
    holdingsRecJson.put(InventoryManager.HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(InventoryManager.HOLDING_PERMANENT_LOCATION_ID, locationIds.get(0));

    JsonObject holdingsRecJsonColl = new JsonObject();
    holdingsRecJsonColl.put(HOLDINGS_RECORDS, new JsonArray());

    doReturn(completedFuture(firstHoldingIdExp)).when(restClient).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    doReturn(completedFuture(firstHoldingJson)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    doReturn(completedFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(completedFuture(HOLDINGS_SOURCE_ID_RESPONSE)).when(inventoryManager).getEntryId(eq(HOLDINGS_SOURCES), any(ErrorCodes.class), eq(requestContext));
    String holdingIdAct = inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(instanceId, location, requestContext).join();

    assertThat(holdingIdAct, equalTo(firstHoldingIdExp));
    verify(restClient, times(1)).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingWillBeRetrievedByIdAndRetrievedIfInTheLocationOnlyLocationIdPresent() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    JsonObject firstHoldingJson = getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS);
    String firstHoldingIdExp = extractId(firstHoldingJson);
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding -> holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withLocationId(locationIds.get(0)).withQuantity(1).withQuantityPhysical(1);

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(ID, firstHoldingIdExp);
    holdingsRecJson.put(InventoryManager.HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(InventoryManager.HOLDING_PERMANENT_LOCATION_ID, locationIds.get(0));

    JsonObject holdingsRecJsonColl = new JsonObject();
    holdingsRecJsonColl.put(HOLDINGS_RECORDS, new JsonArray().add(holdingsRecJson));

    doReturn(completedFuture(holdingsRecJson)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    doReturn(completedFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    String holdingIdAct = inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(instanceId, location, requestContext).join();

    assertThat(holdingIdAct, equalTo(firstHoldingIdExp));
    verify(restClient, times(0)).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldCreateNewHoldingIfHoldingIsAbsentByIdAndRetrievedIfInTheLocationOnlyLocationIdPresent() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    JsonObject firstHoldingJson = getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS);
    String firstHoldingIdExp = extractId(firstHoldingJson);
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding -> holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withLocationId(locationIds.get(0)).withQuantity(1).withQuantityPhysical(1);

    JsonObject holdingsRecJson = new JsonObject();
    holdingsRecJson.put(ID, firstHoldingIdExp);
    holdingsRecJson.put(InventoryManager.HOLDING_INSTANCE_ID, instanceId);
    holdingsRecJson.put(InventoryManager.HOLDING_PERMANENT_LOCATION_ID, locationIds.get(0));

    JsonObject holdingsRecJsonColl = new JsonObject();
    holdingsRecJsonColl.put(HOLDINGS_RECORDS, new JsonArray());

    doReturn(completedFuture(firstHoldingIdExp)).when(restClient).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    doReturn(completedFuture(holdingsRecJson)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    doReturn(completedFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(completedFuture(HOLDINGS_SOURCE_ID_RESPONSE)).when(inventoryManager).getEntryId(eq(HOLDINGS_SOURCES), any(ErrorCodes.class), eq(requestContext));

    String holdingIdAct = inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(instanceId, location, requestContext).join();

    assertThat(holdingIdAct, equalTo(firstHoldingIdExp));
    verify(restClient, times(1)).post(any(RequestEntry.class), any(JsonObject.class), eq(PostResponseType.UUID), eq(String.class), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  /**
   * Define unit test specific beans to override actual ones
   */
  static class ContextConfiguration {
    @Bean
    public ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }

    @Bean
    public PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    public RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean
    public InventoryManager inventoryManager(RestClient restClient, ConfigurationEntriesService configurationEntriesService,
                                              PieceStorageService pieceStorageService) {
      return spy(new InventoryManager(restClient, configurationEntriesService, pieceStorageService));
    }
  }
}
