package org.folio.helper;

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
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.configuration.ConfigurationEntriesService;
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

public class PiecesHelperTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String PIECE_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String TILES_PATH = BASE_MOCK_DATA_PATH + "titles/";
  public static final String HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d61";

  @InjectMocks
  private PiecesHelper piecesHelper;
  @Mock
  private ProtectionHelper protectionHelper;
  @Mock
  private InventoryHelper inventoryHelper;
  @Mock
  private TitlesHelper titlesHelper;
  @Mock
  private Map<String, String> okapiHeadersMock;
  @Spy
  private Context ctxMock = getFirstContextFromVertx(getVertx());

  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(PiecesHelperTest.ContextConfiguration.class);
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
    org.folio.rest.acq.model.Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord")
                .mapTo(org.folio.rest.acq.model.Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    when(inventoryHelper.getOrCreateHoldingsRecord(anyString(), anyString())).thenReturn(completedFuture(HOLDING_ID));
    //When
    CompletableFuture<String> result = piecesHelper.handleHoldingsRecord(line, piece.getLocationId(), title.getInstanceId());
    String actHoldingId = result.get();
    //Then
    verify(inventoryHelper).getOrCreateHoldingsRecord(eq(title.getInstanceId()), eq(piece.getLocationId()));
    assertEquals(HOLDING_ID, actHoldingId);
  }

  @Test
  void testHoldingsItemShouldNotBeCreatedIfPOLIsNull() {
    //given
    org.folio.rest.acq.model.Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord")
      .mapTo(org.folio.rest.acq.model.Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    CompletableFuture<String> result = piecesHelper.handleHoldingsRecord(null, piece.getLocationId(), title.getInstanceId());
    //Then
    assertTrue(result.isCompletedExceptionally());
  }


  @Test
  void testHoldingsItemCreationShouldBeSkippedIfEresourceOrPhysicsIsAbsent() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setEresource(null);
    line.setPhysical(null);

    org.folio.rest.acq.model.Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord")
      .mapTo(org.folio.rest.acq.model.Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(completedFuture(HOLDING_ID))
      .when(inventoryHelper).getOrCreateHoldingsRecord(anyString(), anyString());
    //When
    CompletableFuture<String> result =
      piecesHelper.handleHoldingsRecord(line, piece.getLocationId(), title.getInstanceId());

    //Then
    String holdingId = result.get();
    verify(inventoryHelper,never()).getOrCreateHoldingsRecord(title.getInstanceId(), piece.getLocationId());
    assertNull(holdingId);
  }

  @Test
  void testShouldCreateInstanceIfInstanceIdIsNotProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setInstanceId(null);
    PiecesHelper piecesHelper = mock(PiecesHelper.class, CALLS_REAL_METHODS);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(piecesHelper).getInstanceRecord(any(Title.class));
    //When
    piecesHelper.handleInstanceRecord(title).get();
    //Then
    verify(piecesHelper, times(1)).getInstanceRecord(title);
  }

  @Test
  void testShouldSkipCreationNewInstanceIfInstanceIdIsProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    CompletableFuture<Title> result = piecesHelper.handleInstanceRecord(title);
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
      .when(inventoryHelper).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1));

    //When
    CompletableFuture<String> result = piecesHelper.createItemRecord(line, HOLDING_ID);
    String actItemId = result.get();
    //Then
    verify(inventoryHelper).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1));
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
      .when(inventoryHelper).createMissingElectronicItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1));

    //When
    CompletableFuture<String> result = piecesHelper.createItemRecord(line, HOLDING_ID);
    String actItemId = result.get();
    //Then
    verify(inventoryHelper).createMissingElectronicItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1));
    assertEquals(expItemId, actItemId);
  }

  @Test
  void testShouldSkipCreationItemRecord() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setCheckinItems(true);
    //When
    CompletableFuture<String> result = piecesHelper.createItemRecord(line, HOLDING_ID);
    String actItemId = result.get();
    //Then
    assertNull(actItemId);
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
    piecesHelper.buildInstanceRecordJsonObject(title, statuseJSON);
    //Then
    verify(title).getContributors();
    verify(title, times(1)).getPublishedDate();
    verify(title, times(2)).getPublisher();
    verify(title).getProductIds();
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
    piecesHelper.buildInstanceRecordJsonObject(title, statuseJSON);
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
    piecesHelper.buildInstanceRecordJsonObject(title, statuseJSON);
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
    piecesHelper.buildInstanceRecordJsonObject(title, statuseJSON);
    //Then
    verify(title).getContributors();
    verify(title, times(1)).getPublishedDate();
    verify(title, times(2)).getPublisher();
    verify(title).getProductIds();
  }

  @Test
  void testUpdateInventoryPositiveCaseIfPOLIsTitle() throws ExecutionException, InterruptedException {
    //given
    PiecesHelper piecesHelper = spy(new PiecesHelper(okapiHeadersMock, ctxMock, "en"
      ,protectionHelper, inventoryHelper, titlesHelper));
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPiece(line, title);
    doReturn(completedFuture(null))
      .when(inventoryHelper).updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId());
    //When
    Piece result = piecesHelper.updateInventory(line, piece).get();
    //Then
    assertEquals(piece.getId(), result.getId());
    assertNull(result.getTitleId());
  }

  @Test
  void testShouldCreateInstanceRecordIfProductIsEmpty() throws ExecutionException, InterruptedException {
    //given
    PiecesHelper piecesHelper = spy(new PiecesHelper(okapiHeadersMock, ctxMock, "en"
      ,protectionHelper, inventoryHelper, titlesHelper));

    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setProductIds(null);
    doReturn(completedFuture(UUID.randomUUID().toString())).when(piecesHelper).createInstanceRecord(any(Title.class));
    //When
    piecesHelper.getInstanceRecord(title).get();
    //Thenа
    verify(piecesHelper, times(1)).createInstanceRecord(any(Title.class));
  }

  @Test
  void testShouldCreateInstanceRecordIfProductPresentAndInstancesNotFoundInDB() throws ExecutionException, InterruptedException {
    //given
    PiecesHelper piecesHelper = spy(new PiecesHelper(okapiHeadersMock, ctxMock, "en"
      ,protectionHelper, inventoryHelper, titlesHelper));

    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    doReturn(completedFuture(new JsonObject("{\"instances\" : []}"))).when(piecesHelper).searchInstancesByProducts(any(List.class));
    doReturn(completedFuture(UUID.randomUUID().toString())).when(piecesHelper).createInstanceRecord(any(Title.class));
    //When
    piecesHelper.getInstanceRecord(title).get();
    //Thenа
    verify(piecesHelper, times(1)).createInstanceRecord(any(Title.class));
  }

  @Test
  void testUpdateInventoryPositiveCaseIfPOLIsPackage() throws ExecutionException, InterruptedException {
    //given
    PiecesHelper piecesHelper = spy(new PiecesHelper(okapiHeadersMock, ctxMock, "en"
                    ,protectionHelper, inventoryHelper, titlesHelper));
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setIsPackage(true);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPiece(line, title);

    doReturn(completedFuture(title)).when(titlesHelper).getTitle(piece.getTitleId());
    doReturn(completedFuture(title.withInstanceId(UUID.randomUUID().toString())))
      .when(piecesHelper).handleInstanceRecord(any(Title.class));
    doReturn(completedFuture(null)).when(titlesHelper).updateTitle(title);
    String holdingId = UUID.randomUUID().toString();
    doReturn(completedFuture(holdingId))
      .when(piecesHelper).handleHoldingsRecord(any(CompositePoLine.class), eq(piece.getLocationId()), eq(title.getInstanceId()));

    String itemId = UUID.randomUUID().toString();
    doReturn(completedFuture(itemId))
      .when(piecesHelper).createItemRecord(any(CompositePoLine.class), eq(holdingId));
    //When
    Piece result = piecesHelper.updateInventory(line, piece).get();
    //Then
    assertEquals(piece.getItemId(), itemId);
    assertEquals(piece.getPoLineId(), line.getId());
    assertEquals(piece.getTitleId(), title.getId());
  }

  @Test
  void testUpdateInventoryNegativeCaseIfPOLIsNull() {
    //given
    PiecesHelper piecesHelper = spy(new PiecesHelper(okapiHeadersMock, ctxMock, "en"
      ,protectionHelper, inventoryHelper, titlesHelper));
    //When
    CompletableFuture<String> result = piecesHelper.createItemRecord(null, UUID.randomUUID().toString());
    //Then
    assertTrue(result.isCompletedExceptionally());
  }


  @Test
  void testShouldUpdatePieceByInvokingMethodWithJaxRsModelIfAcqModelProvided() {
    //given
    org.folio.rest.acq.model.Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord")
      .mapTo(org.folio.rest.acq.model.Piece.class);
    PiecesHelper piecesHelper = spy(new PiecesHelper(okapiHeadersMock, ctxMock, "en"
      ,protectionHelper, inventoryHelper, titlesHelper));
    doReturn(completedFuture(null)).when(piecesHelper).updatePieceRecord(any(Piece.class));
    //When
    piecesHelper.updatePieceRecord(piece).join();
    //Then
    Piece jaxRSPiece = JsonObject.mapFrom(piece).mapTo(Piece.class);
    verify(piecesHelper).updatePieceRecord(jaxRSPiece);
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
