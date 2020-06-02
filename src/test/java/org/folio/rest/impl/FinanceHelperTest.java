package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.folio.model.TransactionPoLineFundRelationshipHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.TransactionService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.impl.EventLoopContext;
import io.vertx.core.json.JsonObject;

public class FinanceHelperTest extends ApiTestBase{
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Mock
  private Map<String, String> okapiHeadersMock;
  @Mock
  private EventLoopContext ctxMock;
  @Mock
  private HttpClientInterface httpClient;
  @Mock
  private TransactionService transactionService;
  @Mock
  private PurchaseOrderLineHelper purchaseOrderLineHelper;

  @Before
  public void initMocks(){
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testShouldMakeEncumbrancesPending() {
    //given
    FinanceHelper financeHelper = mock(FinanceHelper.class, CALLS_REAL_METHODS);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    //When
    financeHelper.makeEncumbrancesPending(Collections.singletonList(encumbrance));
    //Then
    assertEquals(0d, encumbrance.getAmount(), 0.0);
    assertEquals(0d, encumbrance.getEncumbrance().getInitialAmountEncumbered(), 0.0);
    assertEquals(Encumbrance.Status.PENDING, encumbrance.getEncumbrance().getStatus());
  }

  @Test
  public void testShouldInvokeUpdateTransactionTimesEqualToTransactionQuantity() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
                        , purchaseOrderLineHelper, transactionService));
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    financeHelper.updateTransactions(Arrays.asList(encumbrance, encumbrance));
    //Then
    verify(transactionService, times(1)).updateTransactions(any());
  }

  @Test
  public void testShouldReturnAllPoLinesIfAllFundsInPOLDoesNotExistInStorageTransactions() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , purchaseOrderLineHelper, transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<CompositePoLine> actLines = financeHelper.findNeedEncumbrancePoLines(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(3, actLines.size());
  }

  @Test
  public void testShouldReturnAllPoLinesIfAllPOLIdsDoesNotExistInStorageTransactions() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , purchaseOrderLineHelper, transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<CompositePoLine> actLines = financeHelper.findNeedEncumbrancePoLines(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(3, actLines.size());
  }

  @Test
  public void testShouldReturnAllPoLinesIfAllFundsInPOLDoesNotExistInStorageTransactionsAndOnePOLHasMultiplyFunds() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , purchaseOrderLineHelper, transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).getFundDistribution().add(new FundDistribution().withFundId(UUID.randomUUID().toString()));

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<CompositePoLine> actLines = financeHelper.findNeedEncumbrancePoLines(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(3, actLines.size());
  }

  @Test
  public void testShouldReturnOnlyPoLinesWhichIsNotMatchesWithEncumbranceByIdAndFundId() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , purchaseOrderLineHelper, transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<CompositePoLine> actLines = financeHelper.findNeedEncumbrancePoLines(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(2, actLines.size());
  }

  @Test
  public void testShouldReturnEncumbrancesWhichIsNotMatchesWithAnyPoLinesOrFundsInPoLInes() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , purchaseOrderLineHelper, transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    matchedEncumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    matchedEncumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());

    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<Transaction> actEncumbranceForRelease =
      financeHelper.findNeedReleaseEncumbrances(order.getCompositePoLines(), Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(1, actEncumbranceForRelease.size());
    assertEquals(notMatchesEncumbrance.getEncumbrance().getSourcePoLineId(), actEncumbranceForRelease.get(0).getEncumbrance().getSourcePoLineId());
    assertEquals(notMatchesEncumbrance.getFromFundId(), actEncumbranceForRelease.get(0).getFromFundId());
  }

  @Test
  public void testShouldReturnEmptyListWhenAllEncumbrancesMatchesWithAllPoLinesByIdAndFundIdInPoLines() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , purchaseOrderLineHelper, transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<Transaction> actEncumbranceForRelease =
      financeHelper.findNeedReleaseEncumbrances(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(0, actEncumbranceForRelease.size());
  }

  @Test
  public void testShouldBuildHolderOnlyForEncumbrancesMatchesWithAnyPoLinesByIdOrFundsInPoLInes() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , purchaseOrderLineHelper, transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    matchedEncumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    matchedEncumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());

    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<TransactionPoLineFundRelationshipHolder> holders =
      financeHelper.buildNeedUpdateEncumbranceHolder(order.getCompositePoLines(), Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(1, holders.size());
    assertEquals(matchedEncumbrance.getEncumbrance().getSourcePoLineId(), holders.get(0).getTransaction().getEncumbrance().getSourcePoLineId());
    assertEquals(matchedEncumbrance.getFromFundId(), holders.get(0).getTransaction().getFromFundId());
    assertEquals(order.getCompositePoLines().get(0).getId(), holders.get(0).getPoLine().getId());
    assertEquals(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId(), holders.get(0).getFundDistribution().getFundId());
  }

  @Test
  public void testShouldUpdateEncumbranceWithNewAmountFromLineAndFundFromLine() {
    //Given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"
      , purchaseOrderLineHelper, transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine line = order.getCompositePoLines().get(0);
    FundDistribution fundDistribution = order.getCompositePoLines().get(0).getFundDistribution().get(0);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    //When
    financeHelper.updateEncumbrance(fundDistribution, line, encumbrance);
    //Then
    assertEquals(BigDecimal.valueOf(line.getCost().getListUnitPrice()), BigDecimal.valueOf(encumbrance.getAmount()));
    assertEquals(BigDecimal.valueOf(encumbrance.getAmount()), BigDecimal.valueOf(encumbrance.getEncumbrance().getInitialAmountEncumbered()));
  }
}
