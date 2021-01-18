package org.folio.service.finance;

import static java.util.Collections.emptyList;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.PoLineFundHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

public class EncumbranceServiceTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);
  public static final String ACTIVE_BUDGET = "68872d8a-bf16-420b-829f-206da38f6c10";

  @InjectMocks
  private EncumbranceService encumbranceService;

  private Context ctxMock;
  private Map<String, String> okapiHeadersMock;

  private RequestContext requestContextMock;

  @Mock
  private TransactionService transactionService;
  @Mock
  private TransactionSummariesService transactionSummariesService;
  @Mock
  private ConfigurationEntriesService configurationEntriesService;
  @Mock
  private FundService fundService;
  @Mock
  private LedgerService ledgerService;
  @Mock
  private FiscalYearService fiscalYearService;
  @Mock
  private BudgetService budgetService;


  @BeforeEach
  public void initMocks(){
    ctxMock = Vertx.vertx().getOrCreateContext();
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
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
  void testShouldReturnHoldersIfAllFundsInPOLDoesNotExistInStorageTransactions() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<PoLineFundHolder> actLines = encumbranceService.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(3, actLines.size());
  }


  @Test
  void testShouldReturnHoldersForAllPoLinesByFundsFromOrderIfNoEncumbrancesInStorage() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).getFundDistribution().add(new FundDistribution().withFundId(UUID.randomUUID().toString()));
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<PoLineFundHolder> actLines = encumbranceService.buildNewEncumbrancesHolders(order.getCompositePoLines(), null);
    //Then
    assertEquals(4, actLines.size());
  }

  @Test
  void testShouldReturnHoldersForAllPoLinesByFundsFromOrderExceptPoLineWithEmptyFundsAndNoEncumbrancesInStorage() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).setFundDistribution(null);
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<PoLineFundHolder> actLines = encumbranceService.buildNewEncumbrancesHolders(order.getCompositePoLines(), null);
    //Then
    assertEquals(2, actLines.size());
  }

  @Test
  void testShouldReturnAllPoLinesIfAllPOLIdsDoesNotExistInStorageTransactions() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<PoLineFundHolder> actLines = encumbranceService.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(3, actLines.size());
  }

  @Test
  void testShouldReturnAllPoLinesIfAllFundsInPOLDoesNotExistInStorageTransactionsAndOnePOLHasMultiplyFunds() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).getFundDistribution().add(new FundDistribution().withFundId(UUID.randomUUID().toString()));

    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    matchedEncumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    matchedEncumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    order.getCompositePoLines().get(0).getFundDistribution().get(0).setEncumbrance(matchedEncumbrance.getId());

    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<PoLineFundHolder> actLines = encumbranceService.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(3, actLines.size());
  }

  @Test
  void testShouldReturnAllPoLinesIfOnePOLHasMultiplyFundsAndOneFundForExistingEncumbrance() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().get(0).getFundDistribution().add(new FundDistribution().withFundId(UUID.randomUUID().toString()));

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<PoLineFundHolder> actLines = encumbranceService.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(4, actLines.size());
  }

  @Test
  void testShouldReturnOnlyPoLinesWhichIsNotMatchesWithEncumbranceByIdAndFundId() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<PoLineFundHolder> actLines = encumbranceService.buildNewEncumbrancesHolders(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(2, actLines.size());
  }

  @Test
  void testShouldReturnAllEncumbranceHoldersWhichIsNotMatchesWithAnyPoLinesOrFundsInPoLInes() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    matchedEncumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    matchedEncumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());

    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<Transaction> actEncumbranceForRelease =
      encumbranceService.findNeedReleaseEncumbrances(order.getCompositePoLines(), Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(1, actEncumbranceForRelease.size());
    Transaction encumbrance = actEncumbranceForRelease.get(0);
    assertEquals(notMatchesEncumbrance.getEncumbrance().getSourcePoLineId(), encumbrance.getEncumbrance().getSourcePoLineId());
    assertEquals(notMatchesEncumbrance.getFromFundId(), encumbrance.getFromFundId());
  }

  @Test
  void testShouldReturnEmptyListWhenAllEncumbrancesMatchesWithAllPoLinesByIdAndFundIdInPoLines() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().remove(1);
    order.getCompositePoLines().remove(1);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    encumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    encumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<Transaction> actEncumbranceForRelease =
      encumbranceService.findNeedReleaseEncumbrances(order.getCompositePoLines(), Arrays.asList(encumbrance, encumbrance));
    //Then
    assertEquals(0, actEncumbranceForRelease.size());
  }

  @Test
  void testShouldReturnOneTransactionWhenOneEncumbrancesMatchesWithOnePoLinesByIdAndFundIdInPoLines() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().remove(1);
    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    matchedEncumbrance.getEncumbrance().setSourcePoLineId(order.getCompositePoLines().get(0).getId());
    matchedEncumbrance.setFromFundId(order.getCompositePoLines().get(0).getFundDistribution().get(0).getFundId());
    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<Transaction> actEncumbranceForRelease =
      encumbranceService.findNeedReleaseEncumbrances(order.getCompositePoLines(), Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(1, actEncumbranceForRelease.size());
  }

  @Test
  void testShouldBuildHolderOnlyForEncumbrancesMatchesWithAnyPoLinesByIdAndFundsInPoLinesAndEncumbranceInPoLinesFunds() {
    //given

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Transaction notMatchesEncumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    Transaction matchedEncumbrance = JsonObject.mapFrom(notMatchesEncumbrance).mapTo(Transaction.class);
    CompositePoLine firstPoLine = order.getCompositePoLines().get(0);
    firstPoLine.getFundDistribution().get(0).setEncumbrance(matchedEncumbrance.getId());

    matchedEncumbrance.getEncumbrance().setSourcePoLineId(firstPoLine.getId());
    matchedEncumbrance.setFromFundId(firstPoLine.getFundDistribution().get(0).getFundId());

    doReturn(completedFuture(null)).when(transactionService).updateTransaction(any(), eq(requestContextMock));
    //When
    List<EncumbranceRelationsHolder> holders =
      encumbranceService.buildUpdateEncumbranceHolders(order.getCompositePoLines(), Arrays.asList(notMatchesEncumbrance, matchedEncumbrance));
    //Then
    assertEquals(1, holders.size());
    assertEquals(matchedEncumbrance.getEncumbrance().getSourcePoLineId(), holders.get(0).getTransaction().getEncumbrance().getSourcePoLineId());
    assertEquals(firstPoLine.getFundDistribution().get(0).getEncumbrance(), holders.get(0).getTransaction().getId());
    assertEquals(matchedEncumbrance.getFromFundId(), holders.get(0).getTransaction().getFromFundId());
    assertEquals(firstPoLine.getId(), holders.get(0).getPoLineFundHolder().getPoLine().getId());
    assertEquals(firstPoLine.getFundDistribution().get(0).getFundId(), holders.get(0).getPoLineFundHolder().getFundDistribution().getFundId());
  }

  @Test
  void shouldBuildEncumbrancesForFundDistributionsRelatedToDifferentLedgers() {

    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    String fund1Id = UUID.randomUUID().toString();
    String fund2Id = UUID.randomUUID().toString();

    Ledger ledger1 = new Ledger().withId(UUID.randomUUID().toString())
            .withRestrictEncumbrance(false);

    Ledger ledger2 = new Ledger().withId(UUID.randomUUID().toString())
            .withRestrictEncumbrance(true);

    Fund fund1 = new Fund().withId(fund1Id)
            .withLedgerId(ledger1.getId());

    Fund fund2 = new Fund().withId(fund2Id)
            .withLedgerId(ledger2.getId());


    Budget budget1 = new Budget()
            .withFundId(fund1Id)
            .withAllocated(0d)
            .withAllowableEncumbrance(100d)
            .withBudgetStatus(Budget.BudgetStatus.ACTIVE);

    Budget budget2 = new Budget()
            .withFundId(fund2Id)
            .withAllocated(1000d)
            .withAllowableEncumbrance(100d)
            .withBudgetStatus(Budget.BudgetStatus.ACTIVE);

    doReturn(completedFuture(Arrays.asList(fund1, fund2))).when(fundService).getAllFunds(anyCollection(), any());
    doReturn(completedFuture(Arrays.asList(budget1, budget2))).when(budgetService).getBudgets(anyCollection(), any());
    doReturn(completedFuture(Arrays.asList(ledger1, ledger2))).when(ledgerService).getLedgersByIds(anyCollection(), any());
    doReturn(completedFuture(new FiscalYear().withId(UUID.randomUUID().toString()).withCurrency("USD")))
            .when(fiscalYearService).getCurrentFiscalYear(anyString(), any());

    FundDistribution fundDistribution1 = new FundDistribution()
            .withFundId(fund1Id)
            .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
            .withValue(50d);

    FundDistribution fundDistribution2 = new FundDistribution()
            .withFundId(fund2Id)
            .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
            .withValue(50d);

    CompositePoLine line = new CompositePoLine()
            .withId(UUID.randomUUID().toString())
            .withPurchaseOrderId(order.getId())
            .withCost(new Cost().withCurrency("USD").withListUnitPrice(10d).withQuantityPhysical(1)
              .withPoLineEstimatedPrice(10d))
            .withFundDistribution(Arrays.asList(fundDistribution1, fundDistribution2));

    doReturn(completedFuture("USD")).when(configurationEntriesService).getSystemCurrency(any());

    CompletableFuture<List<EncumbranceRelationsHolder>> future = encumbranceService.buildNewEncumbrances(order, Collections.singletonList(line), emptyList(), requestContextMock);
    List<EncumbranceRelationsHolder> holders = future.join();

    assertThat(holders, hasSize(2));
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
  void testBudgetShouldBeActive() {

    String budgetId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    List<Budget> budgets = Collections.singletonList(new Budget().withId(budgetId).withFundId(fundId).withBudgetStatus(Budget.BudgetStatus.ACTIVE));
    encumbranceService.verifyBudgetsForEncumbrancesAreActive(budgets);
  }

  @Test
  void testShouldThrowExceptionIfBudgetIsNotActive() {

    String budgetId = UUID.randomUUID().toString();
    String fundId = UUID.randomUUID().toString();
    List<Budget> budgets = Collections.singletonList(new Budget().withId(budgetId)
      .withFundId(fundId)
      .withBudgetStatus(Budget.BudgetStatus.INACTIVE));
    assertThrows(HttpException.class, () -> encumbranceService.verifyBudgetsForEncumbrancesAreActive(budgets));
  }

  @Test
  void shouldReleaseEncumbrancesBeforeDeletionWhenDeleteOrderEncumbrances() {
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
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).updateTransactions(any(), eq(requestContextMock));
    doReturn(CompletableFuture.completedFuture(null)).when(transactionSummariesService).updateOrderTransactionSummary(anyString(), anyInt(), any());
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).deleteTransactions(any(), eq(requestContextMock));
    //When
    encumbranceService.deleteOrderEncumbrances(orderId, requestContextMock).join();

    //Then
    InOrder inOrder = inOrder(transactionSummariesService, transactionService);
    inOrder.verify(transactionSummariesService).updateOrderTransactionSummary(eq(orderId), eq(2), eq(requestContextMock));
    inOrder.verify(transactionService).updateTransactions(any(), eq(requestContextMock));
    inOrder.verify(transactionService).deleteTransactions(any(), eq(requestContextMock));

  }

  @Test
  void shouldReleaseEncumbrancesBeforeDeletionWhenDeletePoLineEncumbrances() {
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
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).updateTransactions(any(), any());
    doReturn(CompletableFuture.completedFuture(null)).when(transactionSummariesService).updateOrderTransactionSummary(anyString(), anyInt(), any());
    doReturn(CompletableFuture.completedFuture(null)).when(transactionService).deleteTransactions(any(), any());
    //When
    encumbranceService.deletePoLineEncumbrances(lineId, requestContextMock).join();

    //Then
    InOrder inOrder = inOrder(transactionSummariesService, transactionService);
    inOrder.verify(transactionSummariesService).updateOrderTransactionSummary(eq(orderId), eq(2), eq(requestContextMock));
    inOrder.verify(transactionService).updateTransactions(any(), eq(requestContextMock));
    inOrder.verify(transactionService).deleteTransactions(any(), eq(requestContextMock));

  }


}
