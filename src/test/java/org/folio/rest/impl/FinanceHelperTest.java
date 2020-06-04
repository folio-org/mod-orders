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

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.PoLineFundHolder;
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
                        , transactionService));
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    financeHelper.updateTransactions(Arrays.asList(encumbrance, encumbrance));
    //Then
    verify(transactionService, times(1)).updateTransactions(any());
  }

  @Test
  public void testShouldReturnHoldersIfAllFundsInPOLDoesNotExistInStorageTransactions() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<PoLineFundHolder> actLines = financeHelper.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(3, actLines.size());
  }


  @Test
  public void testShouldReturnHoldersForAllPoLinesByFundsFromOrderIfNoEncumbrancesInStorage() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).getFundDistribution().add(new FundDistribution().withFundId(UUID.randomUUID().toString()));
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<PoLineFundHolder> actLines = financeHelper.buildNewEncumbrancesHolders(order.getCompositePoLines(), null);
    //Then
    assertEquals(4, actLines.size());
  }

  @Test
  public void testShouldReturnHoldersForAllPoLinesByFundsFromOrderExceptPoLineWithEmptyFundsAndNoEncumbrancesInStorage() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).setFundDistribution(null);
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<PoLineFundHolder> actLines = financeHelper.buildNewEncumbrancesHolders(order.getCompositePoLines(), null);
    //Then
    assertEquals(2, actLines.size());
  }

  @Test
  public void testShouldReturnAllPoLinesIfAllPOLIdsDoesNotExistInStorageTransactions() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<PoLineFundHolder> actLines = financeHelper.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(3, actLines.size());
  }

  @Test
  public void testShouldReturnAllPoLinesIfAllFundsInPOLDoesNotExistInStorageTransactionsAndOnePOLHasMultiplyFunds() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).getFundDistribution().add(new FundDistribution().withFundId(UUID.randomUUID().toString()));

    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    matchedEncumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    matchedEncumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    order.getCompositePoLines().get(0).getFundDistribution().get(0).setEncumbrance(matchedEncumbrance.getId());

    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<PoLineFundHolder> actLines = financeHelper.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(4, actLines.size());
  }

  @Test
  public void testShouldReturnAllPoLinesIfOnePOLHasMultiplyFundsAndOneFundForExistingEncumbrance() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).getFundDistribution().add(new FundDistribution().withFundId(UUID.randomUUID().toString()));

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<PoLineFundHolder> actLines = financeHelper.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(4, actLines.size());
  }

  @Test
  public void testShouldReturnOnlyPoLinesWhichIsNotMatchesWithEncumbranceByIdAndFundId() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<PoLineFundHolder> actLines = financeHelper.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(2, actLines.size());
  }

  @Test
  public void testShouldReturnAllEncumbranceHoldersWhichIsNotMatchesWithAnyPoLinesOrFundsInPoLInes() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    matchedEncumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    matchedEncumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());

    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<Transaction> actEncumbranceForRelease =
      financeHelper.findNeedReleaseEncumbrances(order, Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(1, actEncumbranceForRelease.size());
    Transaction encumbrance = actEncumbranceForRelease.get(0);
    assertEquals(notMatchesEncumbrance.getEncumbrance().getSourcePoLineId(), encumbrance.getEncumbrance().getSourcePoLineId());
    assertEquals(notMatchesEncumbrance.getFromFundId(), encumbrance.getFromFundId());
  }

  @Test
  public void testShouldReturnEmptyListWhenAllEncumbrancesMatchesWithAllPoLinesByIdAndFundIdInPoLines() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().remove(1);
    order.getCompositePoLines().remove(1);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<Transaction> actEncumbranceForRelease =
      financeHelper.findNeedReleaseEncumbrances(order, Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(0, actEncumbranceForRelease.size());
  }

  @Test
  public void testShouldReturnOneTransactionWhenOneEncumbrancesMatchesWithOnePoLinesByIdAndFundIdInPoLines() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().remove(1);
    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    matchedEncumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    matchedEncumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<Transaction> actEncumbranceForRelease =
      financeHelper.findNeedReleaseEncumbrances(order, Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(1, actEncumbranceForRelease.size());
  }

  @Test
  public void testShouldBuildHolderOnlyForEncumbrancesMatchesWithAnyPoLinesByIdAndFundsInPoLinesAndEncumbranceInPoLinesFunds() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    CompositePoLine firstPoLine = order.getCompositePoLines().get(0);
    firstPoLine.getFundDistribution().get(0).setEncumbrance(matchedEncumbrance.getId());

    matchedEncumbrance.getEncumbrance().setSourcePoLineId(firstPoLine.getId());
    matchedEncumbrance.setFromFundId(firstPoLine.getFundDistribution().get(0).getFundId());

    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any());
    //When
    List<EncumbranceRelationsHolder> holders =
      financeHelper.buildUpdateEncumbranceHolders(order.getCompositePoLines(), Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(1, holders.size());
    assertEquals(matchedEncumbrance.getEncumbrance().getSourcePoLineId(), holders.get(0).getTransaction().getEncumbrance().getSourcePoLineId());
    assertEquals(firstPoLine.getFundDistribution().get(0).getEncumbrance(), holders.get(0).getTransaction().getId());
    assertEquals(matchedEncumbrance.getFromFundId(), holders.get(0).getTransaction().getFromFundId());
    assertEquals(firstPoLine.getId(), holders.get(0).getPoLineFundHolder().getPoLine().getId());
    assertEquals(firstPoLine.getFundDistribution().get(0).getFundId(), holders.get(0).getPoLineFundHolder().getFundDistribution().getFundId());
  }

  @Test
  public void testShouldUpdateEncumbranceWithNewAmountFromLineAndFundFromLine() {
    //Given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en", transactionService));
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
