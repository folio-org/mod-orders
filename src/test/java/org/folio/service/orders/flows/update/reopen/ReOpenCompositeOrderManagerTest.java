package org.folio.service.orders.flows.update.reopen;

import static io.vertx.core.Future.succeededFuture;
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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.spy;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.ClosedToOpenEncumbranceStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.pieces.PieceStorageService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;


@ExtendWith(VertxExtension.class)
public class ReOpenCompositeOrderManagerTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Autowired
  private ReOpenCompositeOrderManager reOpenCompositeOrderManager;
  @Autowired
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private InvoiceLineService invoiceLineService;
  @Autowired
  private InvoiceService invoiceService;
  @Mock
  private ClosedToOpenEncumbranceStrategy closedToOpenEncumbranceStrategy;

  @Mock
  private Map<String, String> okapiHeadersMock;
  private final Context ctx = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ReOpenCompositeOrderManagerTest.ContextConfiguration.class);
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
    reset(encumbranceWorkflowStrategyFactory, pieceStorageService, invoiceLineService, invoiceService);
  }


  @ParameterizedTest
  @CsvSource(value = {"Open:Expected:Awaiting Receipt:Awaiting Payment",
                      "Approved:Received:Partially Received:Awaiting Payment",
                      "Paid:Received:Partially Received:Fully Paid"}, delimiter = ':')
  void shouldCheckPaymentAndReceiptStatusesIfInvoicesAndPiecesHaveSameStatuses(
      String invoiceStatus, String pieceStatus, String expReceiptStatus, String expPaymentStatus) {
    CompositePurchaseOrder oldOrder = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine1 = oldOrder.getCompositePoLines().get(0);
    String poLineId1 = poLine1.getId();
    String encumbrance1Id = poLine1.getFundDistribution().get(0).getEncumbrance();
    CompositePoLine poLine2 = JsonObject.mapFrom(poLine1).mapTo(CompositePoLine.class);
    String poLineId2 = UUID.randomUUID().toString();
    String encumbrance2Id = UUID.randomUUID().toString();
    String oldInstance2 = UUID.randomUUID().toString();
    poLine2.setId(poLineId2);
    poLine2.getFundDistribution().get(0).setEncumbrance(encumbrance2Id);
    poLine2.setInstanceId(oldInstance2);
    oldOrder.setCompositePoLines(List.of(poLine1, poLine2));
    CompositePurchaseOrder newOrder = JsonObject.mapFrom(oldOrder).mapTo(CompositePurchaseOrder.class);
    newOrder.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    String invoiceId1 = UUID.randomUUID().toString();
    Invoice invoice1 = new Invoice().withId(invoiceId1).withStatus(Invoice.Status.fromValue(invoiceStatus));
    String invoiceId2 = UUID.randomUUID().toString();
    Invoice invoice2 = new Invoice().withId(invoiceId2).withStatus(Invoice.Status.fromValue(invoiceStatus));

    InvoiceLine invoiceLine1 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withInvoiceId(invoice1.getId())
      .withPoLineId(poLineId1)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance1Id)));
    InvoiceLine invoiceLine2 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withInvoiceId(invoice2.getId())
      .withPoLineId(poLineId2)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance2Id)));
    List<InvoiceLine> invoiceLines = List.of(invoiceLine1, invoiceLine2);
    List<Invoice> invoices = List.of(invoice1, invoice2);

    Piece piece1 = new Piece().withFormat(Piece.Format.PHYSICAL).withReceivingStatus(Piece.ReceivingStatus.fromValue(pieceStatus))
      .withPoLineId(poLineId1).withLocationId(UUID.randomUUID().toString())
      .withHoldingId(UUID.randomUUID().toString());
    Piece piece2 = new Piece().withFormat(Piece.Format.PHYSICAL).withReceivingStatus(Piece.ReceivingStatus.fromValue(pieceStatus))
      .withPoLineId(poLineId2).withLocationId(UUID.randomUUID().toString())
      .withHoldingId(UUID.randomUUID().toString());

    doReturn(closedToOpenEncumbranceStrategy).when(encumbranceWorkflowStrategyFactory).getStrategy(eq(OrderWorkflowType.CLOSED_TO_OPEN));
    doReturn(succeededFuture(null)).when(closedToOpenEncumbranceStrategy).processEncumbrances(eq(newOrder), eq(oldOrder), eq(requestContext));
    doReturn(succeededFuture(invoiceLines)).when(invoiceLineService).getInvoiceLinesByOrderLineIds(List.of(poLineId1, poLineId2), requestContext);
    doReturn(succeededFuture(invoices)).when(invoiceService).getInvoicesByOrderId(oldOrder.getId(), requestContext);
    doReturn(succeededFuture(List.of(piece1, piece2))).when(pieceStorageService).getPiecesByLineIdsByChunks(List.of(poLineId1, poLineId2), requestContext);


    reOpenCompositeOrderManager.process(newOrder, oldOrder, requestContext).result();

    assertEquals(CompositePoLine.ReceiptStatus.fromValue(expReceiptStatus), newOrder.getCompositePoLines().get(0).getReceiptStatus());
    assertEquals(CompositePoLine.PaymentStatus.fromValue(expPaymentStatus), newOrder.getCompositePoLines().get(0).getPaymentStatus());
    assertEquals(CompositePoLine.ReceiptStatus.fromValue(expReceiptStatus), newOrder.getCompositePoLines().get(1).getReceiptStatus());
    assertEquals(CompositePoLine.PaymentStatus.fromValue(expPaymentStatus), newOrder.getCompositePoLines().get(1).getPaymentStatus());
  }

  @ParameterizedTest
  @CsvSource(value = {"Open:Approved:Expected:Received:Awaiting Receipt:Partially Received:Awaiting Payment:Awaiting Payment:true:false",
                      "Open:Approved:Expected:Received:Awaiting Receipt:Partially Received:Awaiting Payment:Awaiting Payment:true:true",
                      "Approved:Open:Received:Expected:Partially Received:Awaiting Receipt:Awaiting Payment:Awaiting Payment:true:true",
                      "Approved:Open:Received:Expected:Partially Received:Awaiting Receipt:Awaiting Payment:Awaiting Payment:true:false",
                      "Paid:Open:Expected:Received:Awaiting Receipt:Partially Received:Fully Paid:Awaiting Payment:true:true",
                      "Paid:Open:Expected:Received:Awaiting Receipt:Partially Received:Partially Paid:Awaiting Payment:false:true"}, delimiter = ':')
  void shouldCheckPaymentAndReceiptStatusesIfInvoicesAndPiecesHaveDifferentStatuses(
      String invoiceStatus1, String invoiceStatus2, String pieceStatus1, String pieceStatus2,
      String expReceiptStatus1,  String expReceiptStatus2, String expPaymentStatus1, String expPaymentStatus2,
      boolean releaseEncumbrances1, boolean releaseEncumbrances2) {
    CompositePurchaseOrder oldOrder = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine1 = oldOrder.getCompositePoLines().get(0);
    String poLineId1 = poLine1.getId();
    String encumbrance1Id = poLine1.getFundDistribution().get(0).getEncumbrance();
    CompositePoLine poLine2 = JsonObject.mapFrom(poLine1).mapTo(CompositePoLine.class);
    String poLineId2 = UUID.randomUUID().toString();
    String encumbrance2Id = UUID.randomUUID().toString();
    String oldInstance2 = UUID.randomUUID().toString();
    poLine2.setId(poLineId2);
    poLine2.getFundDistribution().get(0).setEncumbrance(encumbrance2Id);
    poLine2.setInstanceId(oldInstance2);
    oldOrder.setCompositePoLines(List.of(poLine1, poLine2));
    CompositePurchaseOrder newOrder = JsonObject.mapFrom(oldOrder).mapTo(CompositePurchaseOrder.class);
    newOrder.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    String invoiceId1 = UUID.randomUUID().toString();
    Invoice invoice1 = new Invoice().withId(invoiceId1).withStatus(Invoice.Status.fromValue(invoiceStatus1));
    String invoiceId2 = UUID.randomUUID().toString();
    Invoice invoice2 = new Invoice().withId(invoiceId2).withStatus(Invoice.Status.fromValue(invoiceStatus2));

    InvoiceLine invoiceLine1 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withInvoiceId(invoice1.getId())
      .withPoLineId(poLineId1)
      .withReleaseEncumbrance(releaseEncumbrances1)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance1Id)));
    InvoiceLine invoiceLine2 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withInvoiceId(invoice2.getId())
      .withPoLineId(poLineId2)
      .withReleaseEncumbrance(releaseEncumbrances2)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance2Id)));
    List<InvoiceLine> invoiceLines = List.of(invoiceLine1, invoiceLine2);
    List<Invoice> invoices = List.of(invoice1, invoice2);

    Piece piece1 = new Piece().withFormat(Piece.Format.PHYSICAL).withReceivingStatus(Piece.ReceivingStatus.fromValue(pieceStatus1))
      .withPoLineId(poLineId1).withLocationId(UUID.randomUUID().toString())
      .withHoldingId(UUID.randomUUID().toString());
    Piece piece2 = new Piece().withFormat(Piece.Format.PHYSICAL).withReceivingStatus(Piece.ReceivingStatus.fromValue(pieceStatus2))
      .withPoLineId(poLineId2).withLocationId(UUID.randomUUID().toString())
      .withHoldingId(UUID.randomUUID().toString());

    doReturn(closedToOpenEncumbranceStrategy).when(encumbranceWorkflowStrategyFactory).getStrategy(eq(OrderWorkflowType.CLOSED_TO_OPEN));
    doReturn(succeededFuture(null)).when(closedToOpenEncumbranceStrategy).processEncumbrances(eq(newOrder), eq(oldOrder), eq(requestContext));
    doReturn(succeededFuture(invoiceLines)).when(invoiceLineService).getInvoiceLinesByOrderLineIds(List.of(poLineId1, poLineId2), requestContext);
    doReturn(succeededFuture(invoices)).when(invoiceService).getInvoicesByOrderId(oldOrder.getId(), requestContext);
    doReturn(succeededFuture(List.of(piece1, piece2))).when(pieceStorageService).getPiecesByLineIdsByChunks(List.of(poLineId1, poLineId2), requestContext);


    reOpenCompositeOrderManager.process(newOrder, oldOrder, requestContext).result();

    assertEquals(CompositePoLine.ReceiptStatus.fromValue(expReceiptStatus1), newOrder.getCompositePoLines().get(0).getReceiptStatus());
    assertEquals(CompositePoLine.PaymentStatus.fromValue(expPaymentStatus1), newOrder.getCompositePoLines().get(0).getPaymentStatus());
    assertEquals(CompositePoLine.ReceiptStatus.fromValue(expReceiptStatus2), newOrder.getCompositePoLines().get(1).getReceiptStatus());
    assertEquals(CompositePoLine.PaymentStatus.fromValue(expPaymentStatus2), newOrder.getCompositePoLines().get(1).getPaymentStatus());
  }

  @ParameterizedTest
  @CsvSource(value = {
    "Open:Approved:Expected:Received:Awaiting Receipt:Partially Received:Awaiting Payment:Awaiting Payment",
    "Approved:Open:Received:Expected:Partially Received:Awaiting Receipt:Awaiting Payment:Awaiting Payment",
    "Paid:Open:Expected:Received:Awaiting Receipt:Partially Received:Fully Paid:Awaiting Payment"
  }, delimiter = ':')
  void shouldCheckPaymentAndReceiptStatusesIfInvoicesAndPiecesHaveDifferentStatusesAndPolCoveredMoreThenOneInvoice(
      String invoiceStatus1, String invoiceStatus2, String pieceStatus1, String pieceStatus2,
      String expReceiptStatus1,  String expReceiptStatus2, String expPaymentStatus1, String expPaymentStatus2) {
    CompositePurchaseOrder oldOrder = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine1 = oldOrder.getCompositePoLines().get(0);
    String poLineId1 = poLine1.getId();
    String encumbrance1Id = poLine1.getFundDistribution().get(0).getEncumbrance();
    CompositePoLine poLine2 = JsonObject.mapFrom(poLine1).mapTo(CompositePoLine.class);
    String poLineId2 = UUID.randomUUID().toString();
    String encumbrance2Id = UUID.randomUUID().toString();
    String oldInstance2 = UUID.randomUUID().toString();
    poLine2.setId(poLineId2);
    poLine2.getFundDistribution().get(0).setEncumbrance(encumbrance2Id);
    poLine2.setInstanceId(oldInstance2);
    oldOrder.setCompositePoLines(List.of(poLine1, poLine2));
    CompositePurchaseOrder newOrder = JsonObject.mapFrom(oldOrder).mapTo(CompositePurchaseOrder.class);
    newOrder.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    String invoiceId1 = UUID.randomUUID().toString();
    Invoice invoice1 = new Invoice().withId(invoiceId1).withStatus(Invoice.Status.fromValue(invoiceStatus1));
    String invoiceId2 = UUID.randomUUID().toString();
    Invoice invoice2 = new Invoice().withId(invoiceId2).withStatus(Invoice.Status.fromValue(invoiceStatus2));
    String invoiceId3 = UUID.randomUUID().toString();
    Invoice invoice3 = new Invoice().withId(invoiceId3).withStatus(Invoice.Status.fromValue(invoiceStatus2));

    InvoiceLine invoiceLine1 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withInvoiceId(invoice1.getId())
      .withPoLineId(poLineId1)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance1Id)));
    InvoiceLine invoiceLine2 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withInvoiceId(invoice2.getId())
      .withPoLineId(poLineId2)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance2Id)));
    InvoiceLine invoiceLine3 = new InvoiceLine()
      .withId(UUID.randomUUID().toString())
      .withInvoiceId(invoice3.getId())
      .withPoLineId(poLineId2)
      .withFundDistributions(List.of(new org.folio.rest.acq.model.invoice.FundDistribution()
        .withEncumbrance(encumbrance2Id)));
    List<InvoiceLine> invoiceLines = List.of(invoiceLine1, invoiceLine2, invoiceLine3);
    List<Invoice> invoices = List.of(invoice1, invoice2, invoice3);

    Piece piece1 = new Piece().withFormat(Piece.Format.PHYSICAL).withReceivingStatus(Piece.ReceivingStatus.fromValue(pieceStatus1))
      .withPoLineId(poLineId1).withLocationId(UUID.randomUUID().toString())
      .withHoldingId(UUID.randomUUID().toString());
    Piece piece2 = new Piece().withFormat(Piece.Format.PHYSICAL).withReceivingStatus(Piece.ReceivingStatus.fromValue(pieceStatus2))
      .withPoLineId(poLineId2).withLocationId(UUID.randomUUID().toString())
      .withHoldingId(UUID.randomUUID().toString());

    doReturn(closedToOpenEncumbranceStrategy).when(encumbranceWorkflowStrategyFactory).getStrategy(eq(OrderWorkflowType.CLOSED_TO_OPEN));
    doReturn(succeededFuture(null)).when(closedToOpenEncumbranceStrategy).processEncumbrances(eq(newOrder), eq(oldOrder), eq(requestContext));
    doReturn(succeededFuture(invoiceLines)).when(invoiceLineService).getInvoiceLinesByOrderLineIds(List.of(poLineId1, poLineId2), requestContext);
    doReturn(succeededFuture(invoices)).when(invoiceService).getInvoicesByOrderId(oldOrder.getId(), requestContext);
    doReturn(succeededFuture(List.of(piece1, piece2))).when(pieceStorageService).getPiecesByLineIdsByChunks(List.of(poLineId1, poLineId2), requestContext);


    reOpenCompositeOrderManager.process(newOrder, oldOrder, requestContext).result();

    assertEquals(CompositePoLine.ReceiptStatus.fromValue(expReceiptStatus1), newOrder.getCompositePoLines().get(0).getReceiptStatus());
    assertEquals(CompositePoLine.PaymentStatus.fromValue(expPaymentStatus1), newOrder.getCompositePoLines().get(0).getPaymentStatus());
    assertEquals(CompositePoLine.ReceiptStatus.fromValue(expReceiptStatus2), newOrder.getCompositePoLines().get(1).getReceiptStatus());
    assertEquals(CompositePoLine.PaymentStatus.fromValue(expPaymentStatus2), newOrder.getCompositePoLines().get(1).getPaymentStatus());
  }

  private static class ContextConfiguration {
    @Bean EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory() {
      return mock(EncumbranceWorkflowStrategyFactory.class);
    }
    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }
    @Bean InvoiceLineService invoiceLineService() {
      return mock(InvoiceLineService.class);
    }
    @Bean InvoiceService invoiceService() {
      return mock(InvoiceService.class);
    }


    @Bean ReOpenCompositeOrderManager reOpenCompositeOrderManager(EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                PieceStorageService pieceStorageService, InvoiceLineService invoiceLineService, InvoiceService invoiceService) {
      return spy(new ReOpenCompositeOrderManager(encumbranceWorkflowStrategyFactory, pieceStorageService, invoiceLineService, invoiceService));
    }
  }
}
