package org.folio.service.inventory;

import static io.vertx.core.Future.succeededFuture;
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
import static org.folio.service.inventory.InventoryManager.HOLDINGS_SOURCES;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;
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
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.TestConstants;
import org.folio.models.PoLineUpdateHolder;
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
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.caches.InventoryCache;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.pieces.PieceService;
import org.folio.service.pieces.PieceStorageService;
import org.hamcrest.core.IsInstanceOf;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
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
  private static final String INSTANCE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instances/" + "instances.json";
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
  @Autowired
  private ConfigurationEntriesCache configurationEntriesCache;
  @Autowired
  private InventoryCache inventoryCache;
  @Autowired
  private InventoryService inventoryService;


  private Map<String, String> okapiHeadersMock;
  private Context ctxMock;
  private RequestContext requestContext;
  private static boolean runningOnOwn;
  private AutoCloseable mockitoClosable;

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
    mockitoClosable = MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() throws Exception {
    mockitoClosable.close();
    Mockito.reset(restClient);
    clearServiceInteractions();
  }

  @Test
  void shouldReturnHoldingsByIdsIfTheyExist(VertxTestContext vertxTestContext) throws IOException {
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
       .map(o -> ((JsonObject) o))
       .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());

    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    var future = inventoryManager.getHoldingsByIds(holdingIds, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertThat(result.result().size(), equalTo(holdings.size()));
        verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
        vertxTestContext.completeNow();
      });

  }

  @Test
  void shouldTrowExceptionHoldingsByIdsIfNotAllOfThemExist(VertxTestContext vertxTestContext) throws IOException {
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());
    holdingIds.add(UUID.randomUUID().toString());
    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    var future = inventoryManager.getHoldingsByIds(holdingIds, requestContext);

    vertxTestContext.assertFailure(future)
      .onComplete(executionException -> {
        assertThat(executionException.cause(), IsInstanceOf.instanceOf(HttpException.class));

        HttpException httpException = (HttpException) executionException.cause();
        assertEquals(404, httpException.getCode());
        assertEquals(PARTIALLY_RETURNED_COLLECTION.toError().getCode(), httpException.getError().getCode());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void shouldReturnHoldingsByInstanceIdAndLocationIdsIfTheyExist() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());

    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    List<JsonObject> actHoldings = inventoryManager.getHoldingRecords(instanceId, locationIds, requestContext).result();
    assertThat(actHoldings.size(), equalTo(holdings.size()));
    verify(restClient, times(2)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldReturnHoldingsByInstanceIdAndLocationIdIfTheyExist() throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());

    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    inventoryManager.getFirstHoldingRecord(instanceId, locationIds.get(0), requestContext).result();
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void testShouldUpdateAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    doReturn(succeededFuture(null)).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    JsonObject item1 = new JsonObject().put("id", UUID.randomUUID().toString());
    JsonObject item2 = new JsonObject().put("id", UUID.randomUUID().toString());
    List<JsonObject> items = Arrays.asList(item1, item2);
    //When
    inventoryManager.updateItemRecords(items, requestContext).result();
    //Then
    verify(restClient, times(1)).put(any(RequestEntry.class), eq(item1), eq(requestContext));
    verify(restClient, times(1)).put(any(RequestEntry.class), eq(item2), eq(requestContext));
  }

  @Test
  void testShouldNotUpdateItemIfProvidedListEmpty() {
    //given
    doReturn(succeededFuture(null)).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    //When
    inventoryManager.updateItemRecords(Collections.emptyList(), requestContext).result();
    //Then
    verify(restClient, times(0)).put(any(RequestEntry.class),any(JsonObject.class), eq(requestContext));
  }

  @Test
  void testShouldDeleteAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    String itemId1 = UUID.randomUUID().toString();
    String itemId2 = UUID.randomUUID().toString();
    List<String> items = Arrays.asList(itemId1, itemId2);

    doReturn(succeededFuture(null)).when(restClient).delete(any(RequestEntry.class), eq(false), eq(requestContext));
    //When
    inventoryManager.deleteItems(items, false, requestContext).result();
    //Then
    verify(restClient, times(2)).delete(any(RequestEntry.class), eq(false), eq(requestContext));
  }

  @Test
  void testShouldNotDeleteItemIfProvidedListEmpty() {
    //given
    doReturn(succeededFuture(null)).when(restClient).delete(any(RequestEntry.class), eq(requestContext));
    //When
    inventoryManager.deleteItems(Collections.emptyList(), false, requestContext).result();
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
    List<Piece> pieces = inventoryManager.handleItemRecords(reqData, location, requestContext).result();

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
    List<Piece> pieces = inventoryManager.handleItemRecords(reqData, poLineUpdateHolder, requestContext).result();

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
    doReturn(succeededFuture(existedPieces)).when(pieceStorageService).getExpectedPiecesByLineId(poLineId, requestContext);
    doReturn(succeededFuture(needUpdateItems)).when(inventoryManager).getItemRecordsByIds(Collections.singletonList(itemId), requestContext);
    doReturn(succeededFuture(null)).when(inventoryManager).updateItemRecords(any(), eq(requestContext));
    doReturn(succeededFuture(null)).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    //When
    PoLineUpdateHolder poLineUpdateHolder = new PoLineUpdateHolder().withOldLocationId(oldLocationId).withNewLocationId(locationId);
    List<Piece> pieces = inventoryManager.handleItemRecords(reqData, poLineUpdateHolder, requestContext).result();

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

    doReturn(succeededFuture(holdingExp)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    String holdingIdAct = inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext).result();

    assertThat(holdingIdAct, equalTo(holdingIdExp));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingExistsWhenLocationIdAlwaysNewHoldingShouldBeCreated(VertxTestContext vertxTestContext) throws IOException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    String holdingIdExp = extractId(getFirstObjectFromResponse(holdingsCollection, HOLDINGS_RECORDS));
    List<JsonObject> holdings = holdingsCollection.getJsonArray(HOLDINGS_RECORDS).stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());
    Location location = new Location().withLocationId(locationIds.get(0)).withQuantity(1).withQuantityPhysical(1);

    doReturn(succeededFuture(new JsonObject().put(ID, holdingIdExp))).when(restClient).postJsonObject(any(RequestEntry.class), any(), any(RequestContext.class));
    doReturn(succeededFuture(HOLDINGS_SOURCE_ID_RESPONSE)).when(inventoryManager).getEntryId(eq(HOLDINGS_SOURCES), any(ErrorCodes.class), eq(requestContext));

    var future = inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(res -> {
        assertThat(res.result(), equalTo(holdingIdExp));
        verify(restClient, times(1)).postJsonObject(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
        vertxTestContext.completeNow();
      });

  }

  @Test
  void shouldCheckIfTheHoldingExistsWhenLocationIdSpecifiedAndIfNotExistThenCreateNewHoldingReturnHoldingId(VertxTestContext vertxTestContext) throws IOException {
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

    doReturn(succeededFuture(emptyHoldingCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), any(RequestContext.class));
    doReturn(succeededFuture(new JsonObject().put(ID, holdingIdExp))).when(restClient).postJsonObject(any(RequestEntry.class), any(JsonObject.class), any(RequestContext.class));
    doReturn(succeededFuture(HOLDINGS_SOURCE_ID_RESPONSE)).when(inventoryManager).getEntryId(eq(HOLDINGS_SOURCES), any(ErrorCodes.class), any(RequestContext.class));

    var future = inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(res -> {
        assertThat(res.result(), equalTo(holdingIdExp));
        verify(restClient, times(1)).postJsonObject(any(RequestEntry.class), any(JsonObject.class), any(RequestContext.class));
        vertxTestContext.completeNow();
      });
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
      () -> inventoryManager.getOrCreateHoldingsRecord(instanceId, location, requestContext).result());

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
    doReturn(succeededFuture(holding)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    JsonObject holdingIdAct = inventoryManager.getHoldingById(holdingId, true, requestContext).result();

    assertThat(holding, equalTo(holdingIdAct));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
  }

  @Test
  void shouldRetrieveItemIfHoldingIdAndOrderLineProvided()  {
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

    doReturn(succeededFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    List<JsonObject> items = inventoryManager.getItemsByHoldingIdAndOrderLineId(holdingsRecordId, poLineId, requestContext).result();

    assertThat(1, equalTo(items.size()));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldRetrieveItemIfHoldingIdProvidedAndHoldingFoundAndNotSkipNotFound() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holdingId);
    doReturn(succeededFuture(holding)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(false), eq(requestContext));
    JsonObject holdingIdAct = inventoryManager.getHoldingById(holdingId, requestContext).result();

    assertThat(holding, equalTo(holdingIdAct));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(false), eq(requestContext));
  }

  @Test
  void shouldRetrieveEmptyJsonObjectItemIfHoldingIdIsNotProvidedAndHoldingFound() {
    JsonObject holding = new JsonObject();
    JsonObject holdingIdAct = inventoryManager.getHoldingById(null, true, requestContext).result();

    assertThat(holding, equalTo(holdingIdAct));
    verify(restClient, times(0)).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
  }

  @Test
  void testShouldSkipCreationNewInstanceIfInstanceIdIsProvided()  {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    Future<Title> result = inventoryManager.openOrderHandlePackageLineInstance(title, false, requestContext);
    //Then
    Title actTitle = result.result();
    assertEquals(title, actTitle);
  }

  @Test
  void testShouldCreateInstanceRecordIfInstanceMatchingIsDisabled()  {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    inventoryManager.getOrCreateInstanceRecord(title, true, requestContext).result();
    //Then
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testShouldCreateInstanceIfInstanceIdIsNotProvided()  {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setInstanceId(null);
    mock(PieceService.class, CALLS_REAL_METHODS);
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager).getOrCreateInstanceRecord(any(Title.class), any(Boolean.class), eq(requestContext));
    //When
    inventoryManager.openOrderHandlePackageLineInstance(title, false, requestContext).result();
    //Then
    verify(inventoryManager, times(1)).getOrCreateInstanceRecord(title, false, requestContext);
  }

  @Test
  void testShouldCreateInstanceRecordIfProductIsEmpty()  {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setProductIds(null);
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    inventoryManager.getOrCreateInstanceRecord(title, requestContext).result();
    //Then
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testShouldCreateInstanceRecordIfProductPresentAndInstancesNotFoundInDB()  {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    doReturn(succeededFuture(new JsonObject("{\"instances\" : []}"))).when(inventoryManager).searchInstancesByProducts(any(), eq(requestContext));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    inventoryManager.getOrCreateInstanceRecord(title, requestContext).result();
    //Then
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testShouldNotCreateInstanceRecordIfInstancesFoundInDB() throws IOException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    JsonObject instances = new JsonObject(getMockData(INSTANCE_RECORDS_MOCK_DATA_PATH));
    doReturn(succeededFuture(instances)).when(inventoryManager).searchInstancesByProducts(any(), eq(requestContext));
    doReturn(succeededFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    inventoryManager.getOrCreateInstanceRecord(title, false, requestContext).result();
    //Then
    verify(inventoryManager, times(0)).createInstanceRecord(any(Title.class), eq(requestContext));
    verify(inventoryManager, times(1)).searchInstancesByProducts(any(), eq(requestContext));
  }


  @Test
  void testShouldCreateItemRecordForEresources()  {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Eresource eresource = new Eresource().withMaterialType(line.getPhysical().getMaterialType())
      .withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    line.setPhysical(null);
    line.setEresource(eresource);
    line.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    String expItemId = UUID.randomUUID().toString();
    doReturn(succeededFuture(Collections.singletonList(expItemId)))
      .when(inventoryManager).createMissingElectronicItems(any(CompositePoLine.class), any(Piece.class), eq(1), eq(requestContext));
    //When
    Future<String> result = inventoryManager.openOrderCreateItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.result();
    //Then
    verify(inventoryManager).createMissingElectronicItems(any(CompositePoLine.class), any(Piece.class), eq(1), eq(requestContext));
    assertEquals(expItemId, actItemId);
  }

  @Test
  void testShouldCreateItemRecordForPhysical()  {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    String expItemId = UUID.randomUUID().toString();

    doReturn(succeededFuture(Collections.singletonList(expItemId)))
      .when(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), any(Piece.class), eq(1), eq(requestContext));
    //When
    Future<String> result = inventoryManager.openOrderCreateItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.result();
    //Then
    verify(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), any(Piece.class), eq(1), eq(requestContext));
    assertEquals(expItemId, actItemId);
  }

  @Test
  void testHoldingsItemCreationShouldBeSkippedIfEresourceOrPhysicsIsAbsent()  {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setEresource(null);
    line.setPhysical(null);

    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(succeededFuture(HOLDING_ID)).when(inventoryManager).getOrCreateHoldingsRecord(anyString(), any(Location.class), eq(requestContext));
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    Future<String> result = inventoryManager.handleHoldingsRecord(line, location, title.getInstanceId(), requestContext);

    //Then
    String holdingId = result.result();
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
    Future<String> result = inventoryManager.handleHoldingsRecord(null, location, title.getInstanceId(), requestContext);
    //Then
    assertTrue(result.failed());
  }

  @Test
  void testHoldingsRecordShouldBeCreated()  {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(succeededFuture(HOLDING_ID)).when(inventoryManager).getOrCreateHoldingsRecord(anyString(), any(Location.class), eq(requestContext));
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    Future<String> result = inventoryManager.handleHoldingsRecord(line, location, title.getInstanceId(), requestContext);
    String actHoldingId = result.result();
    //Then
    verify(inventoryManager).getOrCreateHoldingsRecord(eq(title.getInstanceId()), eq(location), eq(requestContext));
    assertEquals(HOLDING_ID, actHoldingId);
  }

  @Test
  void testUpdateInventoryNegativeCaseIfPOLIsNull() {
    //When
    Future<String> result =  inventoryManager.openOrderCreateItemRecord(null, UUID.randomUUID().toString(), requestContext);
    //Then
    assertTrue(result.failed());
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

    doReturn(succeededFuture(holdingIdExp)).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));

    inventoryManager.updateInstanceForHoldingRecords(List.of(holdingsRecJson), newInstanceId, requestContext).result();

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

    doReturn(succeededFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    JsonObject holdingIdAct = inventoryManager.getOrCreateHoldingsJsonRecord(null, instanceId, location, requestContext).result();

    assertThat(holdingIdAct.size(), equalTo(holdingsCollection.size()));
    verify(restClient, times(0)).postJsonObjectAndGetId(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingNotExistWhenHoldingIdSpecifiedThenErrorReturned(VertxTestContext vertxTestContext) throws IOException {
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

    doReturn(Future.failedFuture(new HttpException(NOT_FOUND, "Not_Found")))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    var future = inventoryManager.getOrCreateHoldingsJsonRecord(null, instanceId, location, requestContext);
    vertxTestContext.assertFailure(future)
      .onComplete(completionException -> {
        HttpException httpException = (HttpException) completionException.cause();
        assertThat(httpException.getError().getCode(), is(HOLDINGS_BY_ID_NOT_FOUND.getCode()));

        verify(restClient, times(0)).postJsonObjectAndGetId(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
        verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
        vertxTestContext.completeNow();
      });
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

    doReturn(succeededFuture(holdingsRecJson)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    doReturn(succeededFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    String holdingIdAct = inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(instanceId, location, requestContext).result();

    assertThat(holdingIdAct, equalTo(holdingIdExp));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    verify(restClient, times(0)).postJsonObjectAndGetId(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
  }

  @Test
  void shouldCheckIfTheHoldingWillBeRetrievedByIdAndWasNotFoundByInstanceIdAndLOcationThenCreateNewHolding(VertxTestContext vertxTestContext) throws IOException {
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

    doReturn(succeededFuture(firstHoldingJson)).when(restClient).postJsonObject(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    doReturn(succeededFuture(firstHoldingJson)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    doReturn(succeededFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(succeededFuture(HOLDINGS_SOURCE_ID_RESPONSE)).when(inventoryManager).getEntryId(eq(HOLDINGS_SOURCES), any(ErrorCodes.class), eq(requestContext));

    var future = inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(instanceId, location, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(res -> {
        assertThat(res.result(), equalTo(firstHoldingIdExp));
        verify(restClient, times(1)).postJsonObject(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
        verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
        vertxTestContext.completeNow();
      });

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

    doReturn(succeededFuture(holdingsRecJson)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), eq(requestContext));
    doReturn(succeededFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    String holdingIdAct = inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(instanceId, location, requestContext).result();

    assertThat(holdingIdAct, equalTo(firstHoldingIdExp));
    verify(restClient, times(0)).postJsonObjectAndGetId(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldCreateNewHoldingIfHoldingIsAbsentByIdAndRetrievedIfInTheLocationOnlyLocationIdPresent(VertxTestContext vertxTestContext) throws IOException {
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

    doReturn(succeededFuture(firstHoldingJson)).when(restClient).postJsonObject(any(RequestEntry.class), any(JsonObject.class), any(RequestContext.class));
    doReturn(succeededFuture(holdingsRecJson)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(true), any(RequestContext.class));
    doReturn(succeededFuture(holdingsRecJsonColl)).when(restClient).getAsJsonObject(any(RequestEntry.class), any(RequestContext.class));
    doReturn(succeededFuture(HOLDINGS_SOURCE_ID_RESPONSE)).when(inventoryManager).getEntryId(eq(HOLDINGS_SOURCES), any(ErrorCodes.class), any(RequestContext.class));

    var future = inventoryManager.getOrCreateHoldingRecordByInstanceAndLocation(instanceId, location, requestContext);

    vertxTestContext.assertComplete(future)
      .onComplete(res -> {
        assertThat(res.result(), equalTo(firstHoldingIdExp));
        vertxTestContext.completeNow();
      });

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
    public ConfigurationEntriesCache configurationEntriesCache() {
      return mock(ConfigurationEntriesCache.class);
    }

    @Bean
    public InventoryCache inventoryCache() {
      return mock(InventoryCache.class);
    }

    @Bean
    public InventoryService inventoryService() {
      return mock(InventoryService.class);
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
    public InventoryManager inventoryManager(RestClient restClient, ConfigurationEntriesCache configurationEntriesCache,
                                             PieceStorageService pieceStorageService, InventoryCache inventoryCache, InventoryService inventoryService) {
      return spy(new InventoryManager(restClient, configurationEntriesCache, pieceStorageService, inventoryCache, inventoryService));
    }
  }
}
