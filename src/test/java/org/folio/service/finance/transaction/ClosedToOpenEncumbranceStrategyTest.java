package org.folio.service.finance.transaction;

import io.vertx.core.json.JsonObject;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class ClosedToOpenEncumbranceStrategyTest {
  @InjectMocks
  private ClosedToOpenEncumbranceStrategy closedToOpenEncumbranceStrategy;
  @Mock
  private EncumbranceService encumbranceService;
  @Mock
  EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  @Mock
  FundsDistributionService fundsDistributionService;
  @Mock
  BudgetRestrictionService budgetRestrictionService;
  @Mock
  private RequestContext requestContext;


  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldCreateMissingEncumbrances() {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Map<String, List<CompositePoLine>> mapFiscalYearsWithCompPOLines = new HashMap<>();
    String fiscalYearId = UUID.randomUUID().toString();
    mapFiscalYearsWithCompPOLines.put(fiscalYearId, singletonList(new CompositePoLine().withId(UUID.randomUUID().toString())));
    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    orderFromStorage.setWorkflowStatus(WorkflowStatus.CLOSED);
    doReturn(completedFuture(mapFiscalYearsWithCompPOLines)).when(encumbranceRelationsHoldersBuilder).retrieveMapFiscalYearsWithCompPOLines(eq(order), eq(orderFromStorage), eq(requestContext));

    doReturn(completedFuture(emptyList())).when(encumbranceService).getOrderEncumbrancesToUnrelease(any(), any(), any());

    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = new ArrayList<>();
    encumbranceRelationsHolders.add(new EncumbranceRelationsHolder()
      .withFundDistribution(new FundDistribution()));
    doReturn(encumbranceRelationsHolders).when(encumbranceRelationsHoldersBuilder).buildBaseHolders(any());
    doReturn(completedFuture(encumbranceRelationsHolders)).when(encumbranceRelationsHoldersBuilder).withBudgets(any(), any());
    doReturn(completedFuture(encumbranceRelationsHolders)).when(encumbranceRelationsHoldersBuilder).withLedgersData(any(),any());
    doReturn(completedFuture(encumbranceRelationsHolders)).when(encumbranceRelationsHoldersBuilder).withFiscalYearData(any(), any());
    doReturn(completedFuture(encumbranceRelationsHolders)).when(encumbranceRelationsHoldersBuilder).withConversion(any(), any());

    doReturn(encumbranceRelationsHolders).when(encumbranceRelationsHoldersBuilder).withKnownTransactions(any(), any());
    doReturn(encumbranceRelationsHolders).when(fundsDistributionService).distributeFunds(any());
    doReturn(completedFuture(null)).when(encumbranceService).createOrUpdateEncumbrances(any(), any());

    // When
    CompletableFuture<Void> result = closedToOpenEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    // Then
    verify(encumbranceService, times(1)).createOrUpdateEncumbrances(
      argThat(h -> h.getEncumbrancesForCreate().size() == 1), eq(requestContext));
  }

  @Test
  void stopIfNothingNeedsToBeDone() {
    // Given
    String orderId = UUID.randomUUID().toString();
    String poLineId = UUID.randomUUID().toString();
    CompositePurchaseOrder order = new CompositePurchaseOrder()
      .withId(orderId)
      .withWorkflowStatus(WorkflowStatus.OPEN);
    FundDistribution fd = new FundDistribution()
      .withEncumbrance(UUID.randomUUID().toString());
    CompositePoLine poLine = new CompositePoLine()
      .withId(poLineId)
      .withPurchaseOrderId(order.getId())
      .withFundDistribution(singletonList(fd));
    order.setCompositePoLines(Collections.singletonList(poLine));

    Map<String, List<CompositePoLine>> mapFiscalYearsWithCompPOLines = new HashMap<>();
    String fiscalYearId = UUID.randomUUID().toString();
    mapFiscalYearsWithCompPOLines.put(fiscalYearId, singletonList(new CompositePoLine().withId(UUID.randomUUID().toString())));
    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    orderFromStorage.setWorkflowStatus(WorkflowStatus.CLOSED);
    doReturn(completedFuture(mapFiscalYearsWithCompPOLines)).when(encumbranceRelationsHoldersBuilder).retrieveMapFiscalYearsWithCompPOLines(eq(order), eq(orderFromStorage), eq(requestContext));

    doReturn(completedFuture(emptyList())).when(encumbranceService).getOrderEncumbrancesToUnrelease(any(), any(), any());

    // When
    CompletableFuture<Void> result = closedToOpenEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);
    assertFalse(result.isCompletedExceptionally());
    result.join();

    // Then
    verify(encumbranceRelationsHoldersBuilder, never()).buildBaseHolders(any());
    verify(encumbranceService, never()).createOrUpdateEncumbrances(any(), any());
  }

}
