package org.folio.service.finance.transaction;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Vertx;

public class EncumbranceServiceTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);

  @InjectMocks
  private EncumbranceService encumbranceService;

  private RequestContext requestContextMock;

  @Mock
  private TransactionService transactionService;
  @Mock
  private TransactionSummariesService transactionSummariesService;

  @BeforeEach
  public void initMocks(){
    Context ctxMock = Vertx.vertx().getOrCreateContext();
    Map<String, String> okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContextMock = new RequestContext(ctxMock, okapiHeadersMock);
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldInvokeUpdateTransactionTimesEqualToTransactionQuantity() {
    //given

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    doReturn(completedFuture(null)).when(transactionSummariesService).updateOrderTransactionSummary(anyString(), anyInt(), eq(requestContextMock));
    //When
    encumbranceService.updateEncumbrances(Arrays.asList(encumbrance, encumbrance), requestContextMock);
    //Then
    verify(transactionService, times(1)).updateTransactions(any(), eq(requestContextMock));
  }

  @Test
  void testShouldUpdateEncumbranceWithNewAmountFromLineAndFundFromLine() {
    //Given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine line = order.getCompositePoLines().get(0);
    FundDistribution fundDistribution = order.getCompositePoLines().get(0).getFundDistribution().get(0);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    //When
    encumbranceService.updateEncumbrance(fundDistribution, line, encumbrance);
    //Then
    assertEquals(BigDecimal.valueOf(line.getCost().getListUnitPrice()), BigDecimal.valueOf(encumbrance.getAmount()));
    assertEquals(BigDecimal.valueOf(encumbrance.getAmount()), BigDecimal.valueOf(encumbrance.getEncumbrance().getInitialAmountEncumbered()));
  }

  @Test
  void shouldSimplyDeleteWhenDeleteOrderEncumbrances() {
    //Given

    String orderId = UUID.randomUUID().toString();
    Transaction encumbrance = new Transaction()
            .withId(UUID.randomUUID().toString())
            .withEncumbrance(new Encumbrance().withSourcePurchaseOrderId(orderId));
    Transaction encumbrance2 = new Transaction()
            .withId(UUID.randomUUID().toString())
            .withEncumbrance(new Encumbrance().withSourcePurchaseOrderId(orderId));
    List<Transaction> transactions = new ArrayList<>();
    transactions.add(encumbrance);
    transactions.add(encumbrance2);
    TransactionCollection transactionCollection = new TransactionCollection().withTransactions(transactions).withTotalRecords(1);
    doReturn(CompletableFuture.completedFuture(transactionCollection)).when(transactionService).getTransactions(anyString(), anyInt(), anyInt(), eq(requestContextMock));
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).deleteTransactions(any(), eq(requestContextMock));
    //When
    encumbranceService.deleteOrderEncumbrances(orderId, requestContextMock).join();

    //Then
    InOrder inOrder = inOrder(transactionService);
    inOrder.verify(transactionService).deleteTransactions(any(), eq(requestContextMock));

  }

  @Test
  void shouldSimplyDeleteWhenDeletePoLineEncumbrances() {
    //Given

    String lineId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    Transaction encumbrance = new Transaction()
            .withId(UUID.randomUUID().toString())
            .withEncumbrance(new Encumbrance()
                    .withSourcePurchaseOrderId(orderId)
                    .withSourcePoLineId(lineId));
    Transaction encumbrance2 = new Transaction()
            .withId(UUID.randomUUID().toString())
            .withEncumbrance(new Encumbrance()
                    .withSourcePurchaseOrderId(orderId)
                    .withSourcePoLineId(lineId));
    List<Transaction> transactions = new ArrayList<>();
    transactions.add(encumbrance);
    transactions.add(encumbrance2);
    TransactionCollection transactionCollection = new TransactionCollection().withTransactions(transactions).withTotalRecords(1);
    doReturn(CompletableFuture.completedFuture(transactionCollection)).when(transactionService).getTransactions(anyString(), anyInt(), anyInt(), any());
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).deleteTransactions(any(), any());
    //When
    encumbranceService.deletePoLineEncumbrances(lineId, requestContextMock).join();

    //Then
    InOrder inOrder = inOrder(transactionService);
    inOrder.verify(transactionService).deleteTransactions(any(), eq(requestContextMock));

  }


}
