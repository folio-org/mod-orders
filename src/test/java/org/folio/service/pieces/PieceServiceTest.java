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
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.inventory.InventoryManager;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.ProtectionService;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.orders.CompositePurchaseOrderService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.json.JsonObject;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

public class PieceServiceTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String PIECE_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String TILES_PATH = BASE_MOCK_DATA_PATH + "titles/";
  public static final String HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d61";
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Autowired
  private PiecesService piecesService;
  @Autowired
  private ProtectionService protectionService;
  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private CompositePurchaseOrderService compositePurchaseOrderService;
  @Autowired
  private PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  @Autowired
  private RestClient restClient;

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
    initSpringContext(PieceServiceTest.ContextConfiguration.class);
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
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
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

    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(completedFuture(HOLDING_ID))
      .when(inventoryManager).getOrCreateHoldingsRecord(anyString(), anyString(), eq(requestContext));
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
    doReturn(completedFuture(UUID.randomUUID().toString())).when(piecesService).getOrCreateInstanceRecord(any(Title.class), eq(requestContext));
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
      .when(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));

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
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPiece(line, title);
    doReturn(completedFuture(null))
      .when(inventoryManager).updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext);
    //When
    Piece result = piecesService.updateInventory(line, piece, requestContext).get();
    //Then
    assertEquals(piece.getId(), result.getId());
    assertNull(result.getTitleId());
  }

  @Test
  void testShouldCreateInstanceRecordIfProductIsEmpty() throws ExecutionException, InterruptedException {
    //given
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
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setIsPackage(true);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPiece(line, title);
    String itemId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();

    doReturn(completedFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(completedFuture(null)).when(titlesService).updateTitle(title, requestContext);

    doReturn(completedFuture(title.withInstanceId(UUID.randomUUID().toString())))
      .when(piecesService).handleInstanceRecord(any(Title.class), eq(requestContext));
    doReturn(completedFuture(holdingId))
      .when(piecesService).handleHoldingsRecord(any(CompositePoLine.class), eq(piece.getLocationId()), eq(title.getInstanceId()), eq(requestContext));
    doReturn(completedFuture(itemId)).when(piecesService).createItemRecord(any(CompositePoLine.class), eq(holdingId), eq(requestContext));

    doReturn(completedFuture(itemId)).when(inventoryManager).createInstanceRecord(eq(title), eq(requestContext));
    //When
    piecesService.updateInventory(line, piece, requestContext).get();
    //Then
    assertEquals(piece.getItemId(), itemId);
    assertEquals(piece.getPoLineId(), line.getId());
    assertEquals(piece.getTitleId(), title.getId());
  }

  @Test
  void testUpdateInventoryNegativeCaseIfPOLIsNull() {
    //When
    CompletableFuture<String> result = piecesService.createItemRecord(null, UUID.randomUUID().toString(), requestContext);
    //Then
    assertTrue(result.isCompletedExceptionally());
  }


  @Test
  void testShouldUpdatePieceByInvokingMethodAndDontSentEventToUpdatePoLineIfReceivingStatusInStorageAndFromRequestTheSame() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Piece pieceFromStorage = JsonObject.mapFrom(piece).mapTo(Piece.class);
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), eq(ProtectedOperationType.UPDATE), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(eq(piece.getItemId()), eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(order)).when(piecesService).getOrderByPoLineId(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(pieceFromStorage)).when(piecesService).getPieceById(eq(piece.getId()), eq(requestContext));
    doReturn(completedFuture(null)).when(piecesService).updatePiece(eq(piece), eq(requestContext));
    //When
    piecesService.updatePieceRecord(piece, requestContext).join();
    //Then
    verify(receiptStatusPublisher, times(0)).sendEvent(eq(MessageAddress.RECEIPT_STATUS), any(JsonObject.class), eq(requestContext));
  }

  @Test
  void testShouldUpdatePieceByInvokingMethodAndSentEventToUpdatePoLineIfReceivingStatusInStorageAndFromRequestAreNotTheSame() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Piece pieceFromStorage = JsonObject.mapFrom(piece).mapTo(Piece.class);
    pieceFromStorage.setReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), eq(ProtectedOperationType.UPDATE), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(eq(piece.getItemId()), eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(order)).when(piecesService).getOrderByPoLineId(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(pieceFromStorage)).when(piecesService).getPieceById(eq(piece.getId()), eq(requestContext));
    doReturn(completedFuture(null)).when(piecesService).updatePiece(eq(piece), eq(requestContext));
    //When
    piecesService.updatePieceRecord(piece, requestContext).join();
    //Then
    verify(receiptStatusPublisher, times(1)).sendEvent(eq(MessageAddress.RECEIPT_STATUS), any(JsonObject.class), eq(requestContext));
  }

  @Test
  void testShouldDeleteItems() {
    //given
    doReturn(completedFuture(null)).when(restClient).delete(any(RequestEntry.class), eq(requestContext));
   //When
    piecesService.deletePiecesByIds(List.of(UUID.randomUUID().toString()), requestContext).join();
    //Then
    verify(restClient, times(1)).delete(any(RequestEntry.class), eq(requestContext));
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

    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean
    ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean
    CompositePurchaseOrderService compositePurchaseOrderService() {
      return mock(CompositePurchaseOrderService.class);
    }

    @Bean
    PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean
    InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean
    RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean
    PieceChangeReceiptStatusPublisher receiptStatusPublisher() {
      return mock(PieceChangeReceiptStatusPublisher.class);
    }

    @Bean
    PiecesService piecesService(RestClient restClient, TitlesService titlesService, ProtectionService protectionService,
                                CompositePurchaseOrderService compositePurchaseOrderService, PurchaseOrderLineService purchaseOrderLineService,
                                InventoryManager inventoryManager, PieceChangeReceiptStatusPublisher receiptStatusPublisher) {
      return spy(new PiecesService(restClient, titlesService, protectionService, compositePurchaseOrderService, purchaseOrderLineService,
                                    inventoryManager, receiptStatusPublisher));
    }
  }
}
