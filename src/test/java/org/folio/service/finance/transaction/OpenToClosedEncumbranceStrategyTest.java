package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static java.util.Collections.singletonList;
import static org.folio.TestConstants.COMP_ORDER_MOCK_DATA_PATH;
import static org.folio.TestConstants.PO_WFD_ID_OPEN_STATUS;
import static org.folio.TestUtils.getMockAsJson;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

@ExtendWith(MockitoExtension.class)
public class OpenToClosedEncumbranceStrategyTest {
  public static final String ORDER_PATH = COMP_ORDER_MOCK_DATA_PATH + PO_WFD_ID_OPEN_STATUS + ".json";

  @InjectMocks
  private OpenToClosedEncumbranceStrategy openToClosedEncumbranceStrategy;
  @Mock
  private EncumbranceService encumbranceService;
  @Mock
  EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  @Mock
  private RequestContext requestContext;

  @Test
  void testShouldReleaseAndSetOrderStatusToEncumbrances() {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine poLine = order.getCompositePoLines().get(0);
    FundDistribution fundDistribution = poLine.getFundDistribution().get(0);

    Map<String, List<CompositePoLine>> mapFiscalYearsWithCompPOLines = new HashMap<>();
    String fiscalYearId = UUID.randomUUID().toString();
    mapFiscalYearsWithCompPOLines.put(fiscalYearId, singletonList(new CompositePoLine().withId(UUID.randomUUID().toString())));
    CompositePurchaseOrder orderFromStorage = JsonObject.mapFrom(order).mapTo(CompositePurchaseOrder.class);
    order.setWorkflowStatus(WorkflowStatus.CLOSED);
    doReturn(succeededFuture(mapFiscalYearsWithCompPOLines)).when(encumbranceRelationsHoldersBuilder)
      .retrieveMapFiscalYearsWithCompPOLines(eq(order), eq(orderFromStorage), eq(requestContext));

    Encumbrance encumbrance = new Encumbrance()
      .withSourcePurchaseOrderId(order.getId())
      .withSourcePoLineId(poLine.getId())
      .withOrderType(Encumbrance.OrderType.fromValue(order.getOrderType().value()))
      .withInitialAmountEncumbered(10d)
      .withOrderStatus(Encumbrance.OrderStatus.OPEN)
      .withStatus(Encumbrance.Status.UNRELEASED);
    Transaction transaction = new Transaction()
      .withId(fundDistribution.getEncumbrance())
      .withFromFundId(fundDistribution.getFundId())
      .withEncumbrance(encumbrance);
    List<Transaction> encumbrances = singletonList(transaction);
    doReturn(succeededFuture(encumbrances)).when(encumbranceService).getEncumbrancesByPoLinesFromCurrentFy(any(), any());
    doReturn(succeededFuture(null)).when(encumbranceService).updateEncumbrances(any(), any());

    // When
    Future<Void> result = openToClosedEncumbranceStrategy.processEncumbrances(order, orderFromStorage, requestContext);
    assertTrue(result.succeeded());
    result.result();

    // Then
    verify(encumbranceService, times(1)).updateEncumbrances(
      argThat(transactions -> transactions.size() == 1 &&
        transactions.get(0).getEncumbrance().getOrderStatus() == Encumbrance.OrderStatus.CLOSED &&
        transactions.get(0).getEncumbrance().getStatus() == Encumbrance.Status.RELEASED), eq(requestContext));
  }
}
