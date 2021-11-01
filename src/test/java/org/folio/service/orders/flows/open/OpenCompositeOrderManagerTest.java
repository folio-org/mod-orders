package org.folio.service.orders.flows.open;

import static java.util.concurrent.CompletableFuture.completedFuture;
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
import static org.folio.service.orders.flows.unopen.UnOpenCompositeOrderManagerTest.ORDER_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.helper.PurchaseOrderLineHelper;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.ReceivingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.CompositeOrderDynamicDataPopulateService;
import org.folio.service.orders.OrderInvoiceRelationService;
import org.folio.service.orders.OrderLinesSummaryPopulateService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderFlowValidator;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderInventoryService;
import org.folio.service.orders.flows.update.open.OpenCompositeOrderManager;
import org.folio.service.pieces.PieceChangeReceiptStatusPublisher;
import org.folio.service.pieces.PieceStorageService;
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

public class OpenCompositeOrderManagerTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";

  @Autowired
  private OpenCompositeOrderManager openCompositeOrderManager;
  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private ProtectionService protectionService;
  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  @Autowired
  private OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  @Autowired
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Autowired
  private OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;

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
    initSpringContext(OpenCompositeOrderManagerTest.ContextConfiguration.class);
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
  void testUpdateInventoryPositiveCaseIfPOLIsTitle() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    Piece piece = createPieceWithLocationId(line, title);
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId(), requestContext);
    doReturn(completedFuture(title)).when(inventoryManager).handleInstanceRecord(title, requestContext);
    //When
    openCompositeOrderManager.openOrderUpdateInventory(line, piece, requestContext).get();
    //Then
    assertEquals(title.getId(), piece.getTitleId());
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
      .when(inventoryManager).handleInstanceRecord(any(Title.class), eq(requestContext));
    doReturn(completedFuture(holdingId))
      .when(openCompositeOrderInventoryService).handleHoldingsRecord(any(CompositePoLine.class), eq(location), eq(title.getInstanceId()), eq(requestContext));
    doReturn(completedFuture(itemId)).when(openCompositeOrderInventoryService).createItemRecord(any(CompositePoLine.class), eq(holdingId), eq(requestContext));

    doReturn(completedFuture(itemId)).when(inventoryManager).createInstanceRecord(eq(title), eq(requestContext));
    //When
    openCompositeOrderManager.openOrderUpdateInventory(line, piece, requestContext).get();
    //Then
    assertEquals(piece.getItemId(), itemId);
    assertEquals(piece.getPoLineId(), line.getId());
    assertEquals(piece.getTitleId(), title.getId());
  }

  @Test
  void testShouldUpdateInventoryPositiveCaseIfLineIsPackageAndPieceContainsHoldingId() throws ExecutionException, InterruptedException {
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
    doReturn(completedFuture(title)).when(inventoryManager).handleInstanceRecord(title, requestContext);
    doReturn(completedFuture(itemId)).when(openCompositeOrderInventoryService).createItemRecord(line, holdingId, requestContext);

    //When
    openCompositeOrderManager.openOrderUpdateInventory(line, piece, requestContext).get();
    //Then
    assertEquals(holdingId, piece.getHoldingId());
    assertEquals(title.getId(), piece.getTitleId());
  }


  @Test
  void testShouldUpdatePieceByInvokingMethodAndDontSentEventToUpdatePoLineIfReceivingStatusInStorageAndFromRequestTheSame() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Piece pieceFromStorage = JsonObject.mapFrom(piece).mapTo(Piece.class);
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    doReturn(completedFuture(null)).when(protectionService).isOperationRestricted(any(List.class), eq(ProtectedOperationType.UPDATE), eq(requestContext));
    doReturn(completedFuture(null)).when(inventoryManager).updateItemWithPoLineId(eq(piece.getItemId()), eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(order)).when(purchaseOrderStorageService).getCompositeOrderByPoLineId(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(eq(piece.getId()), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(eq(piece), eq(requestContext));
    //When
    openCompositeOrderManager.openOrderUpdatePieceRecord(piece, requestContext).join();
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
    doReturn(completedFuture(order)).when(purchaseOrderStorageService).getCompositeOrderByPoLineId(eq(piece.getPoLineId()), eq(requestContext));
    doReturn(completedFuture(pieceFromStorage)).when(pieceStorageService).getPieceById(eq(piece.getId()), eq(requestContext));
    doReturn(completedFuture(null)).when(pieceStorageService).updatePiece(eq(piece), eq(requestContext));
    //When
    openCompositeOrderManager.openOrderUpdatePieceRecord(piece, requestContext).join();
    //Then
    verify(receiptStatusPublisher, times(1)).sendEvent(eq(MessageAddress.RECEIPT_STATUS), any(JsonObject.class), eq(requestContext));
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
    public ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }

    @Bean TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

    @Bean InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean PieceChangeReceiptStatusPublisher receiptStatusPublisher() {
      return mock(PieceChangeReceiptStatusPublisher.class);
    }

    @Bean ReceivingEncumbranceStrategy receivingEncumbranceStrategy() {
      return mock(ReceivingEncumbranceStrategy.class);
    }

    @Bean PurchaseOrderStorageService purchaseOrderService() {
      return mock(PurchaseOrderStorageService.class);
    }

    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory() {
      return mock(EncumbranceWorkflowStrategyFactory.class);
    }

    @Bean OpenCompositeOrderInventoryService openCompositeOrderInventoryService() {
      return mock(OpenCompositeOrderInventoryService.class);
    }

    @Bean OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator() {
      return mock(OpenCompositeOrderFlowValidator.class);
    }

    @Bean PurchaseOrderLineHelper purchaseOrderLineHelper() {
      return mock(PurchaseOrderLineHelper.class);
    }

    @Bean  EncumbranceService encumbranceService() {
      return mock(EncumbranceService.class);
    }

    @Bean ExpenseClassValidationService expenseClassValidationService() {
      return mock(ExpenseClassValidationService.class);
    }

    @Bean OrderInvoiceRelationService orderInvoiceRelationService() {
      return mock(OrderInvoiceRelationService.class);
    }

    @Bean AcquisitionsUnitsService acquisitionsUnitsService() {
      return mock(AcquisitionsUnitsService.class);
    }

    @Bean CompositeOrderDynamicDataPopulateService orderLinesSummaryPopulateService() {
      return mock(OrderLinesSummaryPopulateService.class);
    }

    @Bean RestClient restClient() {
      return mock(RestClient.class);
    }


    @Bean OpenCompositeOrderManager openCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory, InventoryManager inventoryManager,
      PieceStorageService pieceStorageService, PurchaseOrderStorageService purchaseOrderStorageService, ProtectionService protectionService,
      PieceChangeReceiptStatusPublisher receiptStatusPublisher, TitlesService titlesService,
      OpenCompositeOrderInventoryService openCompositeOrderInventoryService,
      OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator, PurchaseOrderLineHelper purchaseOrderLineHelper){

      return new OpenCompositeOrderManager(purchaseOrderLineService, encumbranceWorkflowStrategyFactory, inventoryManager,
        pieceStorageService, purchaseOrderStorageService, protectionService, receiptStatusPublisher,
        titlesService, openCompositeOrderInventoryService, openCompositeOrderFlowValidator, purchaseOrderLineHelper);
    }
  }
}
