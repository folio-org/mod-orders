package org.folio.helper;

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
import static org.folio.TestConstants.ID;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.service.inventory.InventoryManager.ITEMS;
import static org.folio.service.inventory.InventoryManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.HOLDINGS_OLD_NEW_PATH;
import static org.folio.rest.impl.MockServer.ITEMS_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.COMP_PO_LINES_MOCK_DATA_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING;
import static org.folio.service.pieces.PiecesServiceTest.LINE_ID;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.util.ArrayList;
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

import org.folio.ApiTestSuite;
import org.folio.models.PoLineUpdateHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.inventory.InventoryManager;
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
  public static final String HOLDING_INSTANCE_ID = "5294d737-a04b-4158-857a-3f3c555bcc60";
  public static final String OLD_LOCATION_ID = "758258bc-ecc1-41b8-abca-f7b610822fff";
  public static final String NEW_LOCATION_ID = "fcd64ce1-6995-48f0-840e-89ffa2288371";
  public static final String OLD_HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d63";
  public static final String NON_EXISTED_NEW_HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d23";
  public static final String ONLY_NEW_HOLDING_EXIST_ID = "65cb2bf0-d4c2-4822-8ad0-b76f1ba75d22";
  public static final String HOLDING_INSTANCE_ID_2_HOLDING = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d48";
  private static final String TILES_PATH = BASE_MOCK_DATA_PATH + "titles/";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";

  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private RestClient restClient;
  @Autowired
  private ConfigurationEntriesService configurationEntriesService;

  private Context ctxMock;
  private Map<String, String> okapiHeadersMock;
  private RequestContext requestContext;
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
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
    reset(inventoryManager);
  }

  @Test
  void testShouldDeleteAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    String itemId1 = UUID.randomUUID().toString();
    String itemId2 = UUID.randomUUID().toString();
    List<String> items = Arrays.asList(itemId1, itemId2);

    doReturn(completedFuture(null)).when(restClient).delete(any(RequestEntry.class), eq(requestContext));
    //When
    inventoryManager.deleteItems(items, requestContext).join();
    //Then
    verify(restClient, times(2)).delete(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void testShouldNotDeleteItemIfProvidedListEmpty() {
    //given
    doReturn(completedFuture(null)).when(restClient).delete(any(RequestEntry.class), eq(requestContext));
    //When
    inventoryManager.deleteItems(Collections.emptyList(), requestContext).join();
    //Then
    verify(restClient, times(0)).delete(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void testShouldUpdateHoldingsRecordIfOldAndNewLocationProvided() throws IOException {
    //given
    JsonObject holdings = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    doReturn(completedFuture(holdings)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
                                        .withOldLocationId(OLD_LOCATION_ID)
                                        .withNewLocationId(NEW_LOCATION_ID);
    inventoryManager.updateHoldingsRecord(holder, requestContext).join();
    //Then
    assertThat(holder.getOldHoldingId(), equalTo(OLD_HOLDING_ID));
  }

  @Test
  void testShouldCreateNewHoldingsRecordIfOnlyOldLocationProvided() throws IOException {
    //given
    List holdingsList = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH)).getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .filter(holding -> holding.getString("permanentLocationId").equals(OLD_LOCATION_ID))
      .collect(toList());
    JsonObject holdings = new JsonObject().put("holdingsRecords", new JsonArray(holdingsList));
    doReturn(completedFuture(holdings)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
                                                        .withOldLocationId(OLD_LOCATION_ID)
                                                        .withNewLocationId(NON_EXISTED_NEW_HOLDING_ID);
    inventoryManager.updateHoldingsRecord(holder, requestContext).join();
    //Then
    assertThat(holder.getNewLocationId(), equalTo(NON_EXISTED_NEW_HOLDING_ID));
  }

  @Test
  void testShouldThrowExceptionIfHoldingWithOldLocationIsNotExist() {
    //given
    JsonObject holdings = new JsonObject().put("holdingsRecords", new JsonArray());
    doReturn(completedFuture(holdings)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
      .withOldLocationId(UUID.randomUUID().toString())
      .withNewLocationId(NON_EXISTED_NEW_HOLDING_ID);
    CompletableFuture<Void> result = inventoryManager.updateHoldingsRecord(holder, requestContext);
    //Then
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(400, httpException.getCode());
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
    Location line = new Location().withLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    List<Piece> pieces = inventoryManager.handleItemRecords(reqData, OLD_HOLDING_ID, Collections.singletonList(line), requestContext).join();

    assertEquals(0, pieces.size());
  }

  @Test
  void testShouldNotUpdateHolderIfReturnMoreThen2Record() throws IOException {
    //given
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID_2_HOLDING)
      .withOldLocationId(OLD_LOCATION_ID);
    List holdingsList = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH)).getJsonArray("holdingsRecords").stream().collect(toList());
    List doubleList = new ArrayList(holdingsList);
    doubleList.addAll(holdingsList);
    JsonObject holdings = new JsonObject().put("holdingsRecords", new JsonArray(doubleList));
    doReturn(completedFuture(holdings)).when(restClient).getAsJsonObject(any(), eq(requestContext));
    //When
    inventoryManager.updateHoldingsRecord(holder, requestContext).join();
    //Then
    verify(inventoryManager, times(0)).getOrCreateHoldingsRecord(HOLDING_INSTANCE_ID_2_HOLDING, OLD_LOCATION_ID, requestContext);
    assertNull(holder.getNewLocationId());
    assertThat(holder.getOldLocationId(), equalTo(OLD_LOCATION_ID));
  }

  @Test
  void testHandleHoldingsAndItemsRecordsIsNotRequired() {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");
    reqData.setCheckinItems(true);
    //When

    Location line = new Location().withLocationId(NEW_LOCATION_ID);
    PoLine storageData = JsonObject.mapFrom(reqData).mapTo(PoLine.class);
    storageData.setLocations(Collections.singletonList(line));
    List<Piece> pieces = inventoryManager.handleHoldingsAndItemsRecords(reqData, storageData, requestContext).join();

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
                                  .filter(item -> item.getString(ID).equals(itemId))
                                  .map(item -> item.put(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER, poLineId))
                                  .collect(toList());;

    String path = PIECE_RECORDS_MOCK_DATA_PATH + String.format("pieceRecords-%s.json", poLineId);
    PieceCollection existedPieces = new JsonObject(getMockData(path)).mapTo(PieceCollection.class);
    existedPieces.getPieces().get(0).setItemId(itemId);
    existedPieces.getPieces().get(0).setFormat(Piece.Format.PHYSICAL);
    existedPieces.getPieces().get(0).setPoLineId(poLineId);
    //given
    doReturn(completedFuture(existedPieces)).when(inventoryManager).getExpectedPiecesByLineId(poLineId, requestContext);
    doReturn(completedFuture(needUpdateItems)).when(inventoryManager).getItemRecordsByIds(Collections.singletonList(itemId), requestContext);
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

  /**
   * Define unit test specific beans to override actual ones
   */
  static class ContextConfiguration {
    @Bean
    public ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }


    @Bean
    public RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean
    public InventoryManager inventoryManager(RestClient restClient, ConfigurationEntriesService configurationEntriesService) {
      return spy(new InventoryManager(restClient, configurationEntriesService));
    }
  }
}
