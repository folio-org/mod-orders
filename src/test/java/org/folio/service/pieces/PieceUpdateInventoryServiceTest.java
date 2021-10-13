package org.folio.service.pieces;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

public class PieceUpdateInventoryServiceTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String PIECE_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String TILES_PATH = BASE_MOCK_DATA_PATH + "titles/";
  public static final String HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d61";

  @Autowired
  private PieceUpdateInventoryService pieceUpdateInventoryService;
  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private  PieceStorageService pieceStorageService;
  @Mock
  private Map<String, String> okapiHeadersMock;
  @Spy
  private Context ctxMock = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(PieceUpdateInventoryServiceTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  @Test
  void testHoldingsRecordShouldBeCreated() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    when(inventoryManager.getOrCreateHoldingsRecord(anyString(), any(Location.class), eq(requestContext))).thenReturn(completedFuture(HOLDING_ID));
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    CompletableFuture<String> result = pieceUpdateInventoryService.handleHoldingsRecord(line, location, title.getInstanceId(), requestContext);
    String actHoldingId = result.get();
    //Then
    verify(inventoryManager).getOrCreateHoldingsRecord(eq(title.getInstanceId()), eq(location), eq(requestContext));
    assertEquals(HOLDING_ID, actHoldingId);
  }

  @Test
  void testHoldingsItemShouldNotBeCreatedIfPOLIsNull() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    CompletableFuture<String> result = pieceUpdateInventoryService.handleHoldingsRecord(null, location, title.getInstanceId(), requestContext);
    //Then
    assertTrue(result.isCompletedExceptionally());
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
    CompletableFuture<String> result = pieceUpdateInventoryService.handleHoldingsRecord(line, location, title.getInstanceId(), requestContext);

    //Then
    String holdingId = result.get();
    verify(inventoryManager,never()).getOrCreateHoldingsRecord(title.getInstanceId(), location, requestContext);
    assertNull(holdingId);
  }

  @Test
  void testShouldCreateInstanceIfInstanceIdIsNotProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setInstanceId(null);
    PieceService pieceService = mock(PieceService.class, CALLS_REAL_METHODS);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(pieceUpdateInventoryService).getOrCreateInstanceRecord(any(Title.class), eq(requestContext));
    //When
    pieceUpdateInventoryService.handleInstanceRecord(title, requestContext).get();
    //Then
    verify(pieceUpdateInventoryService, times(1)).getOrCreateInstanceRecord(title, requestContext);
  }

  @Test
  void testShouldSkipCreationNewInstanceIfInstanceIdIsProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    CompletableFuture<Title> result = pieceUpdateInventoryService.handleInstanceRecord(title, requestContext);
    //Then
    Title actTitle = result.get();
    assertEquals(title, actTitle);
  }

  @Test
  void testShouldCreateItemRecordForPhysical() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    String expItemId = UUID.randomUUID().toString();
    doReturn(completedFuture(Collections.singletonList(expItemId)))
      .when(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));
    //When
    CompletableFuture<String> result = pieceUpdateInventoryService.createItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    verify(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));
    assertEquals(expItemId, actItemId);
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
      .when(inventoryManager).createMissingElectronicItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));

    //When
    CompletableFuture<String> result = pieceUpdateInventoryService.createItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    verify(inventoryManager).createMissingElectronicItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));
    assertEquals(expItemId, actItemId);
  }

  @Test
  void testShouldSkipCreationItemRecord() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setCheckinItems(true);
    //When
    CompletableFuture<String> result = pieceUpdateInventoryService.createItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    assertNull(actItemId);
  }


  @Test
  void testUpdateInventoryPositiveCaseIfPOLIsTitle() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPieceWithLocationId(line, title);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext);
    //When
    pieceUpdateInventoryService.openOrderUpdateInventory(line, piece, requestContext).get();
    //Then
    assertEquals(title.getId(), piece.getTitleId());
  }

  @Test
  void testShouldCreateInstanceRecordIfProductIsEmpty() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setProductIds(null);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    pieceUpdateInventoryService.getOrCreateInstanceRecord(title, requestContext).get();
    //Thenа
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testShouldCreateInstanceRecordIfProductPresentAndInstancesNotFoundInDB() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    doReturn(completedFuture(new JsonObject("{\"instances\" : []}"))).when(inventoryManager).searchInstancesByProducts(any(List.class), eq(requestContext));
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    pieceUpdateInventoryService.getOrCreateInstanceRecord(title, requestContext).get();
    //Thenа
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testUpdateInventoryPositiveCaseIfPOLIsPackage() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setIsPackage(true);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPieceWithLocationId(line, title);
    String itemId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    Location location = new Location().withLocationId(piece.getLocationId());

    doReturn(completedFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(completedFuture(null)).when(titlesService).saveTitle(title, requestContext);

    doReturn(completedFuture(title.withInstanceId(UUID.randomUUID().toString())))
      .when(pieceUpdateInventoryService).handleInstanceRecord(any(Title.class), eq(requestContext));
    doReturn(completedFuture(holdingId))
      .when(pieceUpdateInventoryService).handleHoldingsRecord(any(CompositePoLine.class), eq(location), eq(title.getInstanceId()), eq(requestContext));
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(any(CompositePoLine.class), eq(holdingId), eq(requestContext));

    doReturn(completedFuture(itemId)).when(inventoryManager).createInstanceRecord(eq(title), eq(requestContext));
    //When
    pieceUpdateInventoryService.openOrderUpdateInventory(line, piece, requestContext).get();
    //Then
    assertEquals(piece.getItemId(), itemId);
    assertEquals(piece.getPoLineId(), line.getId());
    assertEquals(piece.getTitleId(), title.getId());
  }

  @Test
  void testUpdateInventoryNegativeCaseIfPOLIsNull() {
    //When
    CompletableFuture<String> result = pieceUpdateInventoryService.createItemRecord(null, UUID.randomUUID().toString(), requestContext);
    //Then
    assertTrue(result.isCompletedExceptionally());
  }

  @Test
  void testShouldUpdateInventoryPositiveCaseIfLieIsPackageAndPieceContainsHoldingId() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    String holdingId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();
    line.getLocations().get(0).withLocationId(null).withHoldingId(holdingId);
    line.setIsPackage(true);

    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPieceWithHoldingId(line, title);

    doReturn(completedFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(completedFuture(null)).when(titlesService).saveTitle(title, requestContext);
    doReturn(completedFuture(itemId)).when(pieceUpdateInventoryService).createItemRecord(line, holdingId, requestContext);

    //When
    pieceUpdateInventoryService.openOrderUpdateInventory(line, piece, requestContext).get();
    //Then
    assertEquals(holdingId, piece.getHoldingId());
    assertEquals(title.getId(), piece.getTitleId());
  }

  @Test
  void shouldNotDeleteHoldingIfHoldingIdIsNull() {
    pieceUpdateInventoryService.deleteHoldingById(null, requestContext);

    verify(inventoryManager, times(0)).getHoldingById(null , true, requestContext);
    verify(inventoryManager, times(0)).deleteHoldingById(null , true, requestContext);
  }

  @Test
  void shouldNotDeleteHoldingIfHoldingIdIsNotNullButNotFoundInTheDB() {
    String holdingId = UUID.randomUUID().toString();
    doReturn(completedFuture(null)).when(inventoryManager).getHoldingById(holdingId , true, requestContext);

    pieceUpdateInventoryService.deleteHoldingById(holdingId, requestContext);

    verify(inventoryManager, times(0)).deleteHoldingById(holdingId , true, requestContext);
  }


  @Test
  void shouldDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndNoPiecesAndItems() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holding);

    doReturn(completedFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holdingId, true, requestContext);
    doReturn(completedFuture(new ArrayList<>())).when(inventoryManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingById(holdingId, requestContext);

    verify(inventoryManager, times(1)).deleteHoldingById(holdingId , true, requestContext);
  }

  @Test
  void shouldNoDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndPiecesExistAndNoItems() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holding);
    Piece piece = new Piece().withId(UUID.randomUUID().toString());
    doReturn(completedFuture(List.of(piece))).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holdingId, true, requestContext);
    doReturn(completedFuture(new ArrayList<>())).when(inventoryManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingById(holdingId, requestContext);

    verify(inventoryManager, times(0)).deleteHoldingById(holdingId , true, requestContext);
  }

  @Test
  void shouldNoDeleteHoldingIfHoldingIdIsProvidedAndFoundInDBAndNoPiecesAndItemsExist() {
    String holdingId = UUID.randomUUID().toString();
    JsonObject holding = new JsonObject();
    holding.put(ID, holding);
    JsonObject item = new JsonObject().put(ID, UUID.randomUUID().toString());
    doReturn(completedFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByHoldingId(holdingId, requestContext);
    doReturn(completedFuture(holding)).when(inventoryManager).getHoldingById(holdingId, true, requestContext);
    doReturn(completedFuture(List.of(item))).when(inventoryManager).getItemsByHoldingId(holdingId, requestContext);

    pieceUpdateInventoryService.deleteHoldingById(holdingId, requestContext);

    verify(inventoryManager, times(0)).deleteHoldingById(holdingId , true, requestContext);
  }

  private Piece createPieceWithLocationId(CompositePoLine line, Title title) {
    return new Piece().withId(UUID.randomUUID().toString())
      .withTitleId(title.getId())
      .withPoLineId(line.getId())
      .withLocationId(line.getLocations().get(0).getLocationId())
      .withFormat(Piece.Format.PHYSICAL);
  }

  private Piece createPieceWithHoldingId(CompositePoLine line, Title title) {
    return new Piece().withId(UUID.randomUUID().toString())
      .withTitleId(title.getId())
      .withPoLineId(line.getId())
      .withHoldingId(line.getLocations().get(0).getHoldingId())
      .withFormat(Piece.Format.PHYSICAL);
  }

  private static class ContextConfiguration {
    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean
    PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    PieceUpdateInventoryService pieceUpdateInventoryService(TitlesService titlesService, InventoryManager inventoryManager,
                                PieceStorageService pieceStorageService) {
      return spy(new PieceUpdateInventoryService(titlesService, inventoryManager, pieceStorageService));
    }
  }
}
