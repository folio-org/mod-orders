package org.folio.service.orders.flows.update.open;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.PIECE_PATH;
import static org.folio.TestConstants.TILES_PATH;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.service.orders.flows.update.unopen.UnOpenCompositeOrderManagerTest.ORDER_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.folio.ApiTestSuite;
import org.folio.models.consortium.SharingInstance;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryInstanceManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

@ExtendWith(VertxExtension.class)
public class OpenCompositeOrderPieceServiceTest {

  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";

  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private ProtectionService protectionService;
  @Autowired
  private InventoryItemManager inventoryItemManager;
  @Autowired
  private InventoryHoldingManager inventoryHoldingManager;
  @Autowired
  private InventoryInstanceManager inventoryInstanceManager;
  @Autowired
  private PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  @Autowired
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired
  private OpenCompositeOrderPieceService openCompositeOrderPieceService;
  @Autowired
  private TitlesService titlesService;

  @Mock
  private Map<String, String> okapiHeadersMock;
  private final Context ctx = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;
  private AutoCloseable openMocks;

  @BeforeEach
  void initMocks(){
    openMocks = MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(OpenCompositeOrderPieceServiceTest.ContextConfiguration.class);
  }

  @AfterAll
  static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @AfterEach
  void resetMocks() throws Exception {
    if (openMocks != null) {
      openMocks.close();
    }
    clearServiceInteractions();
  }

  @Test
  void testShouldUpdatePieceByInvokingMethodAndDontSentEventToUpdatePoLineIfReceivingStatusInStorageAndFromRequestTheSame() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Piece pieceFromStorage = JsonObject.mapFrom(piece).mapTo(Piece.class);
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    Title title = new Title().withId(piece.getTitleId()).withTitle("test title");

    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), eq(ProtectedOperationType.UPDATE), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(eq(pieceFromStorage), eq(piece), eq(requestContext));
    doReturn(succeededFuture(order)).when(purchaseOrderStorageService).getCompositeOrderByPoLineId(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(succeededFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(eq(piece.getId()), eq(requestContext));
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(eq(title.getId()), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceStorageService).updatePiece(eq(piece), eq(requestContext));
    //When
    openCompositeOrderPieceService.updatePiece(piece, requestContext).result();
    //Then
    verify(receiptStatusPublisher, times(0)).sendEvent(eq(MessageAddress.RECEIPT_STATUS), any(JsonObject.class), eq(requestContext));
  }

  @Test
  void testShouldUpdatePieceByInvokingMethodAndSentEventToUpdatePoLineIfReceivingStatusInStorageAndFromRequestAreNotTheSame() {
    //given
    String titleId = UUID.randomUUID().toString();
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Piece pieceFromStorage = JsonObject.mapFrom(piece).mapTo(Piece.class);
    pieceFromStorage.setReceivingStatus(Piece.ReceivingStatus.EXPECTED);
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    Title title = new Title().withId(titleId).withTitle("test title");

    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), eq(ProtectedOperationType.UPDATE), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(eq(pieceFromStorage), eq(piece), eq(requestContext));
    doReturn(succeededFuture(order)).when(purchaseOrderStorageService).getCompositeOrderByPoLineId(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(succeededFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(eq(piece.getId()), eq(requestContext));
    doReturn(succeededFuture(null)).when(pieceStorageService).updatePiece(eq(piece), eq(requestContext));
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(anyString(), eq(requestContext));

    //When
    openCompositeOrderPieceService.updatePiece(piece, requestContext).result();
    //Then
    verify(receiptStatusPublisher, times(1)).sendEvent(eq(MessageAddress.RECEIPT_STATUS), any(JsonObject.class), eq(requestContext));
  }

  @Test
  void testUpdateInventoryPositiveCaseIfPOLIsTitle() {
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    PoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(PoLine.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPieceWithLocationId(line, title);

    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(null, piece, requestContext);

    openCompositeOrderPieceService.openOrderUpdateInventory(order, line, piece, title, false, requestContext).result();

    assertEquals(title.getId(), piece.getTitleId());
  }

  @Test
  void testUpdateInventoryPositiveCaseIfPOLIsPackage() {
    //given
    String itemId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String instanceId = UUID.randomUUID().toString();

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    PoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(PoLine.class);
    line.setIsPackage(true);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class).withInstanceId(instanceId);
    Piece piece = createPieceWithLocationId(line, title);
    SharingInstance sharingInstance = new SharingInstance(UUID.randomUUID(), UUID.randomUUID().toString(), UUID.randomUUID().toString());

    doReturn(succeededFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).updateTitleWithInstance(eq(title), anyBoolean(), anyBoolean(), eq(requestContext), eq(requestContext));

    doReturn(succeededFuture(sharingInstance))
      .when(inventoryInstanceManager).createShadowInstanceIfNeeded(eq(instanceId), eq(requestContext));
    doReturn(succeededFuture(holdingId))
      .when(inventoryHoldingManager).createHoldingAndReturnId(eq(title.getInstanceId()), eq(piece.getLocationId()), eq(requestContext));
    doReturn(succeededFuture(itemId)).when(inventoryItemManager).openOrderCreateItemRecord(any(CompositePurchaseOrder.class),
      any(PoLine.class), eq(holdingId), eq(requestContext));
    doReturn(succeededFuture(itemId)).when(inventoryInstanceManager).createInstanceRecord(eq(title), anyBoolean(), eq(requestContext));

    //When
    openCompositeOrderPieceService.openOrderUpdateInventory(order, line, piece, title, false, requestContext).result();

    //Then
    assertEquals(piece.getItemId(), itemId);
    assertEquals(piece.getPoLineId(), line.getId());
    assertEquals(piece.getTitleId(), title.getId());
  }

  @Test
  void testShouldUpdateInventoryPositiveCaseIfLineIsPackageAndPieceContainsHoldingId() {
    //given
    String instanceId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String itemId = UUID.randomUUID().toString();

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    PoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(PoLine.class);
    line.getLocations().getFirst().withLocationId(null).withHoldingId(holdingId);
    line.setIsPackage(true);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class).withInstanceId(instanceId);
    Piece piece = createPieceWithHoldingId(line, title);

    doReturn(succeededFuture(title)).when(titlesService).getTitleById(piece.getTitleId(), requestContext);
    doReturn(succeededFuture(itemId)).when(inventoryItemManager).openOrderCreateItemRecord(order, line, holdingId, requestContext);
    doReturn(succeededFuture(instanceId)).when(titlesService).updateTitleWithInstance(eq(title), anyBoolean(), anyBoolean(), eq(requestContext), eq(requestContext));

    //When
    openCompositeOrderPieceService.openOrderUpdateInventory(order, line, piece, title, false, requestContext).result();

    //Then
    assertEquals(holdingId, piece.getHoldingId());
    assertEquals(title.getId(), piece.getTitleId());
  }

  @ParameterizedTest
  @SuppressWarnings("unchecked")
  @CsvSource(value = {"Physical Resource:Instance:2:3:Physical", "Physical Resource:None:2:3:Physical",
                      "Other:Instance:2:3:Other", "Other:None:2:3:Other"}, delimiter = ':')
  void shouldCreatePieceWithLocationReferenceIfLineContainsLocationAndInventoryIsInstanceOrNoneAndNoCreatedPieces(
        String lineType, String createInventory, int qty1, int qty2, String pieceFormat) {
    //given
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String locationId1 = UUID.randomUUID().toString();
    String locationId2 = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    Date expectedReceiptDate = new Date();
    Location location1 = new Location().withLocationId(locationId1).withQuantityPhysical(qty1).withQuantity(qty1).withTenantId("tenant1");
    Location location2 = new Location().withLocationId(locationId2).withQuantityPhysical(qty2).withQuantity(qty2).withTenantId("tenant2");
    Cost cost = new Cost().withQuantityPhysical(qty1 + qty2).withQuantityElectronic(null);
    Physical physical = new Physical()
      .withCreateInventory(Physical.CreateInventory.fromValue(createInventory))
      .withExpectedReceiptDate(expectedReceiptDate);
    PoLine line = new PoLine().withId(lineId).withPurchaseOrderId(orderId).withCost(cost)
      .withLocations(List.of(location1, location2)).withIsPackage(false)
      .withPhysical(physical).withOrderFormat(PoLine.OrderFormat.fromValue(lineType));
    CompositePurchaseOrder compOrder = new CompositePurchaseOrder().withId(orderId).withPoLines(List.of(line));
    Title title = new Title().withId(titleId).withTitle("test title");

    doReturn(succeededFuture(null)).when(openCompositeOrderPieceService).openOrderUpdateInventory(any(CompositePurchaseOrder.class),
      any(PoLine.class), any(Piece.class), any(Title.class), any(Boolean.class), eq(requestContext));
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(line, requestContext);
    doReturn(succeededFuture(new PieceCollection())).when(pieceStorageService).insertPiecesBatch(any(), eq(requestContext));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(compOrder)).when(purchaseOrderStorageService).getCompositeOrderById(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(titleId, requestContext);

    final ArgumentCaptor<List<Piece>> pieceArgumentCaptor = ArgumentCaptor.forClass(List.class);
    doAnswer((Answer<Future<PieceCollection>>) invocation -> {
      List<Piece> pieces = invocation.getArgument(0);
      var pieceCollection = new PieceCollection().withPieces(pieces);
      return succeededFuture(pieceCollection);
    }).when(pieceStorageService).insertPiecesBatch(pieceArgumentCaptor.capture(), eq(requestContext));

    //When
    List<Piece> createdPieces = openCompositeOrderPieceService.handlePieces(line, titleId, Collections.emptyList(), false, requestContext).result();
    //Then
    List<Piece> piecesLoc1 = createdPieces.stream().filter(piece -> piece.getLocationId().equals(locationId1)).toList();
    assertEquals(qty1, piecesLoc1.size());
    piecesLoc1.forEach(piece -> {
      assertNull(piece.getHoldingId());
      assertNull(piece.getItemId());
      assertEquals(location1.getTenantId(), piece.getReceivingTenantId());
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(Piece.Format.fromValue(pieceFormat), piece.getFormat());
      assertEquals(expectedReceiptDate, piece.getReceiptDate());
    });
    List<Piece> piecesLoc2 =  createdPieces.stream().filter(piece -> piece.getLocationId().equals(locationId2))
                                          .toList();
    assertEquals(qty2, piecesLoc2.size());
    piecesLoc2.forEach(piece -> {
      assertNull(piece.getHoldingId());
      assertNull(piece.getItemId());
      assertEquals(location2.getTenantId(), piece.getReceivingTenantId());
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(Piece.Format.fromValue(pieceFormat), piece.getFormat());
      assertEquals(expectedReceiptDate, piece.getReceiptDate());
    });
  }

  @ParameterizedTest
  @SuppressWarnings("unchecked")
  @CsvSource(value = {"Electronic Resource:Instance:2:3:Electronic",
                      "Electronic Resource:None:2:3:Electronic"}, delimiter = ':')
  void shouldCreatePieceWithLocationReferenceIfElectronicLineContainsLocationAndInventoryIsInstanceOrNoneAndNoCreatedPieces(
        String lineType, String createInventory, int qty1, int qty2, String pieceFormat) {
    //given
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String locationId1 = UUID.randomUUID().toString();
    String locationId2 = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    Location location1 = new Location().withLocationId(locationId1).withQuantityElectronic(qty1).withQuantity(qty1);
    Location location2 = new Location().withLocationId(locationId2).withQuantityElectronic(qty2).withQuantity(qty2);
    Cost cost = new Cost().withQuantityElectronic(qty1 + qty2);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(createInventory));
    PoLine line = new PoLine().withId(lineId).withPurchaseOrderId(orderId)
      .withCost(cost).withLocations(List.of(location1, location2))
      .withIsPackage(false).withEresource(eresource)
      .withOrderFormat(PoLine.OrderFormat.fromValue(lineType));
    CompositePurchaseOrder compOrder = new CompositePurchaseOrder().withId(orderId).withPoLines(List.of(line));
    Title title = new Title().withId(titleId).withTitle("test title");


    doReturn(succeededFuture(null)).when(openCompositeOrderPieceService).openOrderUpdateInventory(any(CompositePurchaseOrder.class),
      any(PoLine.class), any(Piece.class), any(Title.class), any(Boolean.class), eq(requestContext));
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(line, requestContext);
    doReturn(succeededFuture(new PieceCollection())).when(pieceStorageService).insertPiecesBatch(any(), eq(requestContext));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(compOrder)).when(purchaseOrderStorageService).getCompositeOrderById(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(titleId, requestContext);

    final ArgumentCaptor<List<Piece>> pieceArgumentCaptor = ArgumentCaptor.forClass(List.class);
    doAnswer((Answer<Future<PieceCollection>>) invocation -> {
      List<Piece> pieces = invocation.getArgument(0);
      var pieceCollection = new PieceCollection().withPieces(pieces);
      return succeededFuture(pieceCollection);
    }).when(pieceStorageService).insertPiecesBatch(pieceArgumentCaptor.capture(), eq(requestContext));

    //When
    List<Piece> createdPieces = openCompositeOrderPieceService.handlePieces(line, titleId, Collections.emptyList(), false, requestContext).result();
    //Then
    List<Piece> piecesLoc1 = createdPieces.stream().filter(piece -> piece.getLocationId().equals(locationId1))
      .toList();
    assertEquals(qty1, piecesLoc1.size());
    piecesLoc1.forEach(piece -> {
      assertNull(piece.getHoldingId());
      assertNull(piece.getItemId());
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(Piece.Format.fromValue(pieceFormat), piece.getFormat());
      assertNull(piece.getReceiptDate());
    });
    List<Piece> piecesLoc2 =  createdPieces.stream().filter(piece -> piece.getLocationId().equals(locationId2))
      .toList();
    assertEquals(qty2, piecesLoc2.size());
    piecesLoc2.forEach(piece -> {
      assertNull(piece.getHoldingId());
      assertNull(piece.getItemId());
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(Piece.Format.fromValue(pieceFormat), piece.getFormat());
      assertNull(piece.getReceiptDate());
    });
  }

  @ParameterizedTest
  @SuppressWarnings("unchecked")
  @CsvSource(value = {"P/E Mix:Instance:None:2:3", "P/E Mix:None:Instance:2:3",
                      "P/E Mix:None:None:2:3", "P/E Mix:Instance:Instance:2:3"}, delimiter = ':')
  void shouldCreatePieceWithLocationReferenceIfMixedLineContainsLocationAndInventoryIsInstanceOrNoneAndNoCreatedPieces(
    String lineType, String elecCreateInventory, String physCreateInventory, int elecQty1, int physQty2) {
    //given
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String locationId1 = UUID.randomUUID().toString();
    String locationId2 = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    Date expectedReceiptDate = new Date();
    Location location1 = new Location().withLocationId(locationId1).withQuantityElectronic(elecQty1).withQuantity(elecQty1);
    Location location2 = new Location().withLocationId(locationId2).withQuantityPhysical(physQty2).withQuantity(physQty2);
    Cost cost = new Cost().withQuantityElectronic(elecQty1 + physQty2);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(elecCreateInventory));
    Physical physical = new Physical()
      .withCreateInventory(Physical.CreateInventory.fromValue(physCreateInventory))
      .withExpectedReceiptDate(expectedReceiptDate);

    PoLine line = new PoLine().withId(lineId).withPurchaseOrderId(orderId)
      .withCost(cost).withLocations(List.of(location1, location2))
      .withIsPackage(false).withEresource(eresource).withPhysical(physical)
      .withOrderFormat(PoLine.OrderFormat.fromValue(lineType));
    CompositePurchaseOrder compOrder = new CompositePurchaseOrder().withId(orderId).withPoLines(List.of(line));
    Title title = new Title().withId(titleId).withTitle("test title");

    doReturn(succeededFuture(null)).when(openCompositeOrderPieceService).openOrderUpdateInventory(any(CompositePurchaseOrder.class),
      any(PoLine.class), any(Piece.class), any(Title.class), any(Boolean.class), eq(requestContext));
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(line, requestContext);
    doReturn(succeededFuture(new PieceCollection())).when(pieceStorageService).insertPiecesBatch(any(), eq(requestContext));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(compOrder)).when(purchaseOrderStorageService).getCompositeOrderById(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(titleId, requestContext);

    final ArgumentCaptor<List<Piece>> pieceArgumentCaptor = ArgumentCaptor.forClass(List.class);
    doAnswer((Answer<Future<PieceCollection>>) invocation -> {
      List<Piece> pieces = invocation.getArgument(0);
      var pieceCollection = new PieceCollection().withPieces(pieces);
      return succeededFuture(pieceCollection);
    }).when(pieceStorageService).insertPiecesBatch(pieceArgumentCaptor.capture(), eq(requestContext));

    //When
    List<Piece> createdPieces = openCompositeOrderPieceService.handlePieces(line, titleId, Collections.emptyList(), false, requestContext).result();
    //Then
    List<Piece> piecesLoc1 = createdPieces.stream().filter(piece -> piece.getLocationId().equals(locationId1))
      .toList();
    assertEquals(elecQty1, piecesLoc1.size());
    piecesLoc1.forEach(piece -> {
      assertNull(piece.getHoldingId());
      assertNull(piece.getItemId());
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(Piece.Format.ELECTRONIC, piece.getFormat());
      assertNull(piece.getReceiptDate());
    });
    List<Piece> piecesLoc2 =  createdPieces.stream().filter(piece -> piece.getLocationId().equals(locationId2))
      .toList();
    assertEquals(physQty2, piecesLoc2.size());
    piecesLoc2.forEach(piece -> {
      assertNull(piece.getHoldingId());
      assertNull(piece.getItemId());
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(Piece.Format.PHYSICAL, piece.getFormat());
      assertEquals(expectedReceiptDate, piece.getReceiptDate());
    });
  }

  @ParameterizedTest
  @SuppressWarnings("unchecked")
  @CsvSource(value = {"P/E Mix:Instance:Instance, Holding:2:3", "P/E Mix:None:Instance, Holding:2:3"}, delimiter = ':')
  void shouldCreatePieceWithLocationAndHoldingReferenceIfMixedLineContainsLocationAndInventoryIsInstanceOrNoneAndNoCreatedPieces(
    String lineType, String elecCreateInventory, String physCreateInventory, int elecQty1, int physQty2) {
    //given
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String locationId1 = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    Date expectedReceiptDate = new Date();
    Location location1 = new Location().withLocationId(locationId1).withQuantityElectronic(elecQty1).withQuantity(elecQty1);
    Location location2 = new Location().withHoldingId(holdingId).withQuantityPhysical(physQty2).withQuantity(physQty2);
    Cost cost = new Cost().withQuantityElectronic(elecQty1 + physQty2);
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(elecCreateInventory));
    Physical physical = new Physical()
      .withCreateInventory(Physical.CreateInventory.fromValue(physCreateInventory))
      .withExpectedReceiptDate(expectedReceiptDate);

    PoLine line = new PoLine().withId(lineId).withPurchaseOrderId(orderId)
      .withCost(cost).withLocations(List.of(location1, location2))
      .withIsPackage(false).withEresource(eresource).withPhysical(physical)
      .withOrderFormat(PoLine.OrderFormat.fromValue(lineType));
    CompositePurchaseOrder compOrder = new CompositePurchaseOrder().withId(orderId).withPoLines(List.of(line));
    Title title = new Title().withId(titleId).withTitle("test title");

    doReturn(succeededFuture(null)).when(openCompositeOrderPieceService).openOrderUpdateInventory(any(CompositePurchaseOrder.class),
      any(PoLine.class), any(Piece.class), any(Title.class), any(Boolean.class), eq(requestContext));
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(line, requestContext);
    doReturn(succeededFuture(new PieceCollection())).when(pieceStorageService).insertPiecesBatch(any(), eq(requestContext));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(compOrder)).when(purchaseOrderStorageService).getCompositeOrderById(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(titleId, requestContext);

    final ArgumentCaptor<List<Piece>> pieceArgumentCaptor = ArgumentCaptor.forClass(List.class);
    doAnswer((Answer<Future<PieceCollection>>) invocation -> {
      List<Piece> pieces = invocation.getArgument(0);
      var pieceCollection = new PieceCollection().withPieces(pieces);
      return succeededFuture(pieceCollection);
    }).when(pieceStorageService).insertPiecesBatch(pieceArgumentCaptor.capture(), eq(requestContext));

    //When
    List<Piece> createdPieces = openCompositeOrderPieceService.handlePieces(line, titleId, Collections.emptyList(), false, requestContext).result();
    //Then
    List<Piece> piecesLoc1 = createdPieces.stream().filter(piece -> locationId1.equals(piece.getLocationId()))
      .toList();
    assertEquals(elecQty1, piecesLoc1.size());
    piecesLoc1.forEach(piece -> {
      assertNull(piece.getHoldingId());
      assertNull(piece.getItemId());
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(Piece.Format.ELECTRONIC, piece.getFormat());
      assertNull(piece.getReceiptDate());
    });
    List<Piece> piecesLoc2 =  createdPieces.stream().filter(piece -> holdingId.equals(piece.getHoldingId()))
      .toList();
    assertEquals(physQty2, piecesLoc2.size());
    piecesLoc2.forEach(piece -> {
      assertNull(piece.getLocationId());
      assertNull(piece.getItemId());
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(Piece.Format.PHYSICAL, piece.getFormat());
      assertEquals(expectedReceiptDate, piece.getReceiptDate());
    });
  }

  @ParameterizedTest
  @SuppressWarnings("unchecked")
  @CsvSource(value = {"P/E Mix:Instance:Instance, Holding, Item:2:3", "P/E Mix:None:Instance, Holding, Item:2:3"}, delimiter = ':')
  void shouldCreatePieceWithLocationAndHoldingReferenceIfMixedLineContainsLocationAndInventoryIsInstanceOrNoneAndExistPiecesWithItems(
                  String lineType, String elecCreateInventory, String physCreateInventory, int elecQty1, int physQty2) {
    //given
    String orderId = UUID.randomUUID().toString();
    String lineId = UUID.randomUUID().toString();
    String locationId = UUID.randomUUID().toString();
    String holdingId = UUID.randomUUID().toString();
    String titleId = UUID.randomUUID().toString();
    Date expectedReceiptDate = new Date();
    List<Piece> expectedPiecesWithItem = new ArrayList<>();
    Location elecLocation = new Location().withQuantityElectronic(elecQty1).withQuantity(elecQty1);

    if (Eresource.CreateInventory.INSTANCE_HOLDING_ITEM == Eresource.CreateInventory.fromValue(elecCreateInventory)) {
      elecLocation.withHoldingId(holdingId);
      expectedPiecesWithItem.addAll(createElecPiecesWithHoldingId(lineId, titleId, elecLocation, expectedReceiptDate));
    } else {
      elecLocation.withLocationId(locationId);
    }
     Location physLocation = new Location().withQuantityPhysical(physQty2).withQuantity(physQty2);
    if (Physical.CreateInventory.INSTANCE_HOLDING_ITEM == Physical.CreateInventory.fromValue(physCreateInventory)) {
      physLocation.withHoldingId(holdingId);
      expectedPiecesWithItem.addAll(createPhysPiecesWithHoldingId(lineId, titleId, physLocation, expectedReceiptDate));
    } else {
      physLocation.withLocationId(locationId);
    }

    Cost cost = new Cost().withQuantityElectronic(elecQty1 + physQty2);

    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(elecCreateInventory));
    Physical physical = new Physical()
      .withCreateInventory(Physical.CreateInventory.fromValue(physCreateInventory))
      .withExpectedReceiptDate(expectedReceiptDate);

    PoLine line = new PoLine().withId(lineId).withPurchaseOrderId(orderId)
      .withCost(cost).withLocations(List.of(elecLocation, physLocation))
      .withIsPackage(false).withEresource(eresource).withPhysical(physical)
      .withOrderFormat(PoLine.OrderFormat.fromValue(lineType));
    CompositePurchaseOrder compOrder = new CompositePurchaseOrder().withId(orderId).withPoLines(List.of(line));
    Title title = new Title().withId(titleId).withTitle("test title");

    doReturn(succeededFuture(null)).when(openCompositeOrderPieceService).openOrderUpdateInventory(any(CompositePurchaseOrder.class),
      any(PoLine.class), any(Piece.class), any(Title.class), any(Boolean.class), eq(requestContext));
    doReturn(succeededFuture(Collections.emptyList())).when(pieceStorageService).getPiecesByPoLineId(line, requestContext);
    doReturn(succeededFuture(new PieceCollection())).when(pieceStorageService).insertPiecesBatch(any(), eq(requestContext));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(compOrder)).when(purchaseOrderStorageService).getCompositeOrderById(eq(orderId), eq(requestContext));
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(titleId, requestContext);

    final ArgumentCaptor<List<Piece>> pieceArgumentCaptor = ArgumentCaptor.forClass(List.class);
    doAnswer((Answer<Future<PieceCollection>>) invocation -> {
      List<Piece> pieces = invocation.getArgument(0);
      var pieceCollection = new PieceCollection().withPieces(pieces);
      return succeededFuture(pieceCollection);
    }).when(pieceStorageService).insertPiecesBatch(pieceArgumentCaptor.capture(), eq(requestContext));

    //When
    List<Piece> createdPieces = openCompositeOrderPieceService.handlePieces(line, titleId, expectedPiecesWithItem, false, requestContext).result();
    //Then
    List<Piece> piecesLocElec = createdPieces.stream().filter(piece -> Piece.Format.ELECTRONIC.equals(piece.getFormat()))
      .toList();
    assertEquals(elecQty1, piecesLocElec.size());
    piecesLocElec.forEach(piece -> {
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertNull(piece.getReceiptDate());
      if (Eresource.CreateInventory.INSTANCE_HOLDING_ITEM == Eresource.CreateInventory.fromValue(elecCreateInventory)) {
        assertEquals(holdingId, piece.getHoldingId());
        assertNotNull(piece.getItemId());
      } else {
        assertEquals(locationId, piece.getLocationId());
        assertNull(piece.getItemId());
      }
    });
    List<Piece> piecesLocPhys =  createdPieces.stream().filter(piece -> Piece.Format.PHYSICAL.equals(piece.getFormat()))
      .toList();
    assertEquals(physQty2, piecesLocPhys.size());
    piecesLocPhys.forEach(piece -> {
      assertEquals(lineId, piece.getPoLineId());
      assertEquals(titleId, piece.getTitleId());
      assertEquals(expectedReceiptDate, piece.getReceiptDate());
      if (Physical.CreateInventory.INSTANCE_HOLDING_ITEM == Physical.CreateInventory.fromValue(physCreateInventory)) {
        assertEquals(holdingId, piece.getHoldingId());
        assertNotNull(piece.getItemId());
      } else {
        assertEquals(locationId, piece.getLocationId());
        assertNull(piece.getItemId());
      }
    });
  }

  @Test
  void shouldUpdatePieceWithLocationOfDifferentTenantAndUpdateReceivingTenantId() {
    //given
    var tenantId1 = "tenantId1";
    var tenantId2 = "tenantId2";
    var lineId = UUID.randomUUID().toString();
    var titleId = UUID.randomUUID().toString();
    Date expectedReceiptDate = new Date();
    var location1 = new Location().withLocationId(UUID.randomUUID().toString()).withTenantId(tenantId1).withQuantityPhysical(1).withQuantity(1);
    var location2 = new Location().withLocationId(UUID.randomUUID().toString()).withTenantId(tenantId2).withQuantityPhysical(1).withQuantity(1);
    var cost = new Cost().withQuantityPhysical(1).withQuantityElectronic(null);
    var physical = new Physical()
      .withCreateInventory(Physical.CreateInventory.INSTANCE)
      .withExpectedReceiptDate(expectedReceiptDate);
    var line = new PoLine().withId(lineId).withCost(cost).withLocations(List.of(location2))
      .withIsPackage(false).withPhysical(physical).withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE);
    var title = new Title().withId(titleId);
    var pieceId = UUID.randomUUID().toString();
    var pieceBefore = new Piece().withId(pieceId).withLocationId(location1.getLocationId()).withReceivingTenantId(tenantId1)
      .withPoLineId(lineId).withTitleId(titleId).withFormat(Piece.Format.PHYSICAL)
      .withReceiptDate(expectedReceiptDate);

    doReturn(succeededFuture(null)).when(openCompositeOrderPieceService).openOrderUpdateInventory(any(CompositePurchaseOrder.class),
      any(PoLine.class), any(Piece.class), any(Title.class), any(Boolean.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(protectionService).isOperationRestricted(any(), any(ProtectedOperationType.class), eq(requestContext));
    doReturn(succeededFuture(null)).when(inventoryItemManager).updateItemWithPieceFields(eq(pieceBefore), any(Piece.class), eq(requestContext));
    doReturn(succeededFuture(title)).when(titlesService).getTitleById(titleId, requestContext);
    doReturn(succeededFuture(List.of(pieceBefore))).when(pieceStorageService).getPiecesByPoLineId(line, requestContext);
    doReturn(succeededFuture(pieceBefore)).when(pieceStorageService).getPieceById(eq(pieceId), eq(requestContext));
    doReturn(succeededFuture()).when(pieceStorageService).updatePiece(eq(pieceBefore), eq(requestContext));

    // When
    var updatedPieces = openCompositeOrderPieceService.handlePieces(line, titleId, List.of(), false, requestContext).result();
    // Then
    var piecesLoc2 = updatedPieces.stream().filter(updatedPiece -> updatedPiece.getLocationId().equals(location2.getLocationId())).toList();
    assertEquals(1, piecesLoc2.size());
    piecesLoc2.forEach(updatedPiece -> {
      assertNull(updatedPiece.getHoldingId());
      assertNull(updatedPiece.getItemId());
      assertEquals(pieceId, updatedPiece.getId());
      assertEquals(lineId, updatedPiece.getPoLineId());
      assertEquals(titleId, updatedPiece.getTitleId());
      assertEquals(tenantId2, updatedPiece.getReceivingTenantId());
      assertEquals(Piece.Format.PHYSICAL, updatedPiece.getFormat());
      assertEquals(expectedReceiptDate, updatedPiece.getReceiptDate());
    });
  }

  private List<Piece> createPhysPiecesWithHoldingId(String lineId, String titleId, Location location, Date expectedReceiptDate) {
    List<Piece> pieces = new ArrayList<>();
    for (int i = 0; i < location.getQuantityPhysical(); i++) {
      Piece piece = new Piece().withId(UUID.randomUUID().toString())
        .withTitleId(titleId)
        .withPoLineId(lineId)
        .withHoldingId(location.getHoldingId())
        .withFormat(Piece.Format.PHYSICAL)
        .withReceiptDate(expectedReceiptDate);
      piece.withItemId(UUID.randomUUID().toString());
      pieces.add(piece);
    }
    return pieces;
  }

  private List<Piece> createElecPiecesWithHoldingId(String lineId, String titleId, Location location, Date expectedReceiptDate) {
    List<Piece> pieces = new ArrayList<>();
    for (int i = 0; i < location.getQuantityPhysical(); i++) {
      Piece piece = new Piece().withId(UUID.randomUUID().toString())
        .withTitleId(titleId)
        .withPoLineId(lineId)
        .withHoldingId(location.getHoldingId())
        .withFormat(Piece.Format.ELECTRONIC);
      piece.withItemId(UUID.randomUUID().toString());
      pieces.add(piece);
    }
    return pieces;
  }

  private Piece createPieceWithLocationId(PoLine line, Title title) {
    return new Piece().withId(UUID.randomUUID().toString())
      .withTitleId(title.getId())
      .withPoLineId(line.getId())
      .withLocationId(line.getLocations().getFirst().getLocationId())
      .withFormat(Piece.Format.PHYSICAL);
  }

  private Piece createPieceWithHoldingId(PoLine line, Title title) {
    return new Piece().withId(UUID.randomUUID().toString())
      .withTitleId(title.getId())
      .withPoLineId(line.getId())
      .withHoldingId(line.getLocations().getFirst().getHoldingId())
      .withFormat(Piece.Format.PHYSICAL);
  }

  private static class ContextConfiguration {

    @Bean PieceChangeReceiptStatusPublisher receiptStatusPublisher() {
      return mock(PieceChangeReceiptStatusPublisher.class);
    }

    @Bean
    PurchaseOrderStorageService purchaseOrderService() {
      return mock(PurchaseOrderStorageService.class);
    }

    @Bean
    PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean
    InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean
    InventoryHoldingManager inventoryHoldingManager() {
      return mock(InventoryHoldingManager.class);
    }

    @Bean
    InventoryInstanceManager inventoryInstanceManager() {
      return mock(InventoryInstanceManager.class);
    }

    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder(PieceStorageService pieceStorageService, TitlesService titlesService) {
      return spy(new OpenCompositeOrderHolderBuilder(pieceStorageService, titlesService));
    }

    @Bean
    OpenCompositeOrderPieceService openCompositeOrderPieceService(PurchaseOrderStorageService purchaseOrderStorageService,
                                                                  PieceStorageService pieceStorageService,
                                                                  ProtectionService protectionService,
                                                                  PieceChangeReceiptStatusPublisher receiptStatusPublisher,
                                                                  InventoryItemManager inventoryItemManager,
                                                                  InventoryHoldingManager inventoryHoldingManager,
                                                                  TitlesService titlesService,
                                                                  OpenCompositeOrderHolderBuilder openCompositeOrderHolderBuilder) {
      return spy(new OpenCompositeOrderPieceService(purchaseOrderStorageService, pieceStorageService, protectionService,
        receiptStatusPublisher, inventoryItemManager, inventoryHoldingManager, titlesService, openCompositeOrderHolderBuilder));
    }
  }
}
