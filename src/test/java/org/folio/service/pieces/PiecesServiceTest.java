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

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.vertx.core.Context;
import org.folio.ApiTestSuite;
import org.folio.helper.InventoryManager;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.ProtectionService;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.json.JsonObject;
import org.mockito.Spy;
import org.springframework.context.annotation.Bean;

public class PiecesServiceTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String PIECE_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String TILES_PATH = BASE_MOCK_DATA_PATH + "titles/";
  public static final String HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d61";

  @InjectMocks
  private PiecesService piecesService;
  @Mock
  private ProtectionService protectionService;
  @Mock
  private InventoryManager inventoryManager;
  @Mock
  private TitlesService titlesService;
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
    initSpringContext(PiecesServiceTest.ContextConfiguration.class);
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
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord")
                .mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    when(inventoryManager.getOrCreateHoldingsRecord(anyString(), anyString(), eq(requestContext))).thenReturn(completedFuture(HOLDING_ID));
    //When
    CompletableFuture<String> result = piecesService.handleHoldingsRecord(line, piece.getLocationId(), title.getInstanceId(), requestContext);
    String actHoldingId = result.get();
    //Then
    verify(inventoryManager).getOrCreateHoldingsRecord(eq(title.getInstanceId()), eq(piece.getLocationId()), eq(requestContext));
    assertEquals(HOLDING_ID, actHoldingId);
  }

  @Test
  void testHoldingsItemShouldNotBeCreatedIfPOLIsNull() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord")
      .mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    CompletableFuture<String> result = piecesService.handleHoldingsRecord(null, piece.getLocationId(), title.getInstanceId(), requestContext);
    //Then
    assertTrue(result.isCompletedExceptionally());
  }


  @Test
  void testHoldingsItemCreationShouldBeSkippedIfEresourceOrPhysicsIsAbsent() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setEresource(null);
    line.setPhysical(null);

    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord")
      .mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(completedFuture(HOLDING_ID))
      .when(inventoryManager).getOrCreateHoldingsRecord(anyString(), anyString(), requestContext);
    //When
    CompletableFuture<String> result =
      piecesService.handleHoldingsRecord(line, piece.getLocationId(), title.getInstanceId(), requestContext);

    //Then
    String holdingId = result.get();
    verify(inventoryManager,never()).getOrCreateHoldingsRecord(title.getInstanceId(), piece.getLocationId(), requestContext);
    assertNull(holdingId);
  }

  @Test
  void testShouldCreateInstanceIfInstanceIdIsNotProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setInstanceId(null);
    PiecesService piecesService = mock(PiecesService.class, CALLS_REAL_METHODS);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(piecesService).getOrCreateInstanceRecord(any(Title.class), requestContext);
    //When
    piecesService.handleInstanceRecord(title, requestContext).get();
    //Then
    verify(piecesService, times(1)).getOrCreateInstanceRecord(title, requestContext);
  }

  @Test
  void testShouldSkipCreationNewInstanceIfInstanceIdIsProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    CompletableFuture<Title> result = piecesService.handleInstanceRecord(title, requestContext);
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
      .when(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), requestContext);

    //When
    CompletableFuture<String> result = piecesService.createItemRecord(line, HOLDING_ID, requestContext);
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
    CompletableFuture<String> result = piecesService.createItemRecord(line, HOLDING_ID, requestContext);
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
    CompletableFuture<String> result = piecesService.createItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    assertNull(actItemId);
  }


  @Test
  void testUpdateInventoryPositiveCaseIfPOLIsTitle() throws ExecutionException, InterruptedException {
    //given
    PiecesService piecesService = spy(new PiecesService());
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPiece(line, title);
    doReturn(completedFuture(null))
      .when(inventoryManager).updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext);
    //When
    Piece result = piecesService.updateInventory(line, piece, new RequestContext(ctxMock, okapiHeadersMock)).get();
    //Then
    assertEquals(piece.getId(), result.getId());
    assertNull(result.getTitleId());
  }

  @Test
  void testShouldCreateInstanceRecordIfProductIsEmpty() throws ExecutionException, InterruptedException {
    //given
    PiecesService piecesService = spy(new PiecesService());

    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setProductIds(null);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    piecesService.getOrCreateInstanceRecord(title, requestContext).get();
    //Thenа
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testShouldCreateInstanceRecordIfProductPresentAndInstancesNotFoundInDB() throws ExecutionException, InterruptedException {
    //given
    PiecesService piecesService = spy(new PiecesService());

    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    doReturn(completedFuture(new JsonObject("{\"instances\" : []}"))).when(inventoryManager).searchInstancesByProducts(any(List.class), eq(requestContext));
    doReturn(completedFuture(UUID.randomUUID().toString())).when(inventoryManager).createInstanceRecord(any(Title.class), eq(requestContext));
    //When
    piecesService.getOrCreateInstanceRecord(title, requestContext).get();
    //Thenа
    verify(inventoryManager, times(1)).createInstanceRecord(any(Title.class), eq(requestContext));
  }

  @Test
  void testUpdateInventoryPositiveCaseIfPOLIsPackage() throws ExecutionException, InterruptedException {
    //given
    PiecesService piecesService = spy(new PiecesService());
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setIsPackage(true);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPiece(line, title);

    doReturn(completedFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), new RequestContext(ctxMock, okapiHeadersMock));
    doReturn(completedFuture(title.withInstanceId(UUID.randomUUID().toString())))
      .when(piecesService).handleInstanceRecord(any(Title.class), eq(requestContext));
    doReturn(completedFuture(null)).when(titlesService).updateTitle(title, new RequestContext(ctxMock, okapiHeadersMock));
    String holdingId = UUID.randomUUID().toString();
    doReturn(completedFuture(holdingId))
      .when(piecesService).handleHoldingsRecord(any(CompositePoLine.class), eq(piece.getLocationId()), eq(title.getInstanceId()), eq(requestContext));

    String itemId = UUID.randomUUID().toString();
    doReturn(completedFuture(itemId))
      .when(piecesService).createItemRecord(any(CompositePoLine.class), eq(holdingId), eq(requestContext));
    //When
    Piece result = piecesService.updateInventory(line, piece, new RequestContext(ctxMock, okapiHeadersMock)).get();
    //Then
    assertEquals(piece.getItemId(), itemId);
    assertEquals(piece.getPoLineId(), line.getId());
    assertEquals(piece.getTitleId(), title.getId());
  }

  @Test
  void testUpdateInventoryNegativeCaseIfPOLIsNull() {
    //given
    PiecesService piecesService = spy(new PiecesService());
    //When
    CompletableFuture<String> result = piecesService.createItemRecord(null, UUID.randomUUID().toString(), eq(requestContext));
    //Then
    assertTrue(result.isCompletedExceptionally());
  }


  @Test
  void testShouldUpdatePieceByInvokingMethodWithJaxRsModelIfAcqModelProvided() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord")
      .mapTo(Piece.class);
    PiecesService piecesService = spy(new PiecesService());
    doReturn(completedFuture(null)).when(piecesService).updatePieceRecord(any(Piece.class), eq(new RequestContext(ctxMock, okapiHeadersMock)));
    //When
    piecesService.updatePieceRecord(piece, new RequestContext(ctxMock, okapiHeadersMock)).join();
    //Then
    Piece jaxRSPiece = JsonObject.mapFrom(piece).mapTo(Piece.class);
    verify(piecesService).updatePieceRecord(jaxRSPiece, eq(new RequestContext(ctxMock, okapiHeadersMock)));
  }

  private Piece createPiece(CompositePoLine line, Title title) {
    return new Piece().withId(UUID.randomUUID().toString())
      .withTitleId(title.getId())
      .withPoLineId(line.getId())
      .withLocationId(line.getLocations().get(0).getLocationId())
      .withFormat(Piece.Format.PHYSICAL);
  }

  private static class ContextConfiguration {

    @Bean
    public ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }
  }
}
