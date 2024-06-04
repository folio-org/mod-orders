package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import io.vertx.junit5.VertxExtension;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
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
  private AutoCloseable mockitoMocks;

  @BeforeEach
  public void initMocks() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void resetMocks() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void testShouldCreateMissingEncumbrances(VertxTestContext vertxTestContext) {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

    Map<String, List<CompositePoLine>> mapFiscalYearsWithCompPOLines = new HashMap<>();
    String fiscalYearId = UUID.randomUUID().toString();
    mapFiscalYearsWithCompPOLines.put(fiscalYearId, singletonList(new CompositePoLine().withId(UUID.randomUUID().toString())));
    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    orderFromStorage.setWorkflowStatus(WorkflowStatus.CLOSED);
    doReturn(succeededFuture(mapFiscalYearsWithCompPOLines)).when(encumbranceRelationsHoldersBuilder).retrieveMapFiscalYearsWithCompPOLines(eq(order), eq(orderFromStorage), eq(requestContext));

    doReturn(succeededFuture(emptyList())).when(encumbranceService).getOrderEncumbrancesToUnrelease(any(), any(), any());

    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = new ArrayList<>();
    encumbranceRelationsHolders.add(new EncumbranceRelationsHolder()
      .withFundDistribution(new FundDistribution()));
    doReturn(encumbranceRelationsHolders).when(encumbranceRelationsHoldersBuilder).buildBaseHolders(any());
    doReturn(succeededFuture()).when(encumbranceRelationsHoldersBuilder).withFinances(any(), any());

    doReturn(encumbranceRelationsHolders).when(fundsDistributionService).distributeFunds(any());
    doReturn(succeededFuture(null)).when(encumbranceService).createOrUpdateEncumbrances(any(), any());

    // When
    Future<Void> future = closedToOpenEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        // Then
        verify(encumbranceService, times(1)).createOrUpdateEncumbrances(
          argThat(h -> h.getEncumbrancesForCreate().size() == 1), eq(requestContext));
        vertxTestContext.completeNow();
      });
  }

  @Test
  void stopIfNothingNeedsToBeDone(VertxTestContext vertxTestContext) {
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
    doReturn(succeededFuture(mapFiscalYearsWithCompPOLines)).when(encumbranceRelationsHoldersBuilder).retrieveMapFiscalYearsWithCompPOLines(eq(order), eq(orderFromStorage), eq(requestContext));

    doReturn(succeededFuture(emptyList())).when(encumbranceService).getOrderEncumbrancesToUnrelease(any(), any(), any());

    // When
    Future<Void> future = closedToOpenEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        // Then
        verify(encumbranceRelationsHoldersBuilder, never()).buildBaseHolders(any());
        verify(encumbranceService, never()).createOrUpdateEncumbrances(any(), any());
        vertxTestContext.completeNow();
      });

  }

}
