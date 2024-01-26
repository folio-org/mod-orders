package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.transaction.summary.OrderTransactionSummariesService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(VertxExtension.class)
public class TransactionSummariesServiceTest {

  @InjectMocks
  private OrderTransactionSummariesService orderTransactionSummariesService;

  @Mock
  private RestClient restClient;

  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldNotUpdateTransactionsSummariesWhenNoEncumbrances() {
    // When
    orderTransactionSummariesService.updateTransactionSummary(UUID.randomUUID()
      .toString(), 0, requestContext);
    // Then
    verify(restClient, never()).put(anyString(), any(), any());
  }

  @Test
  void testShouldTransactionsCreatedForEncumbrances() {
    // Given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    CompositePoLine line = order.getCompositePoLines()
      .get(0);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions")
      .getJsonObject(0)
      .mapTo(Transaction.class);
    FundDistribution fundDistribution = order.getCompositePoLines()
      .get(0)
      .getFundDistribution()
      .get(0);
    // When
    orderTransactionSummariesService.updateTransactionSummary(order.getId(), 1, requestContext);
    // Then
    assertNull(orderTransactionSummariesService.getTransactionSummary(order.getId(), requestContext));
  }

  @Test
  void testShouldCreateTransactionSummaryInStorageTransactions() {
    // Given
    String uuid = UUID.randomUUID().toString();
    JsonObject response = new JsonObject().put("id", uuid);
    when(restClient.post(any(RequestEntry.class), any(), any(), any(RequestContext.class)))
      .thenReturn(succeededFuture(response));
    OrderTransactionSummary expectedSummary = new OrderTransactionSummary().withId(uuid).withNumTransactions(2);
    // When
    Future<OrderTransactionSummary> result = orderTransactionSummariesService.createTransactionSummary(expectedSummary, requestContext);
    // Then
    verify(restClient).post(any(RequestEntry.class), any(), any(), any(RequestContext.class));
    JsonObject res = JsonObject.mapFrom(result.result());
    String resultId = res.getString("id");
    assertEquals(uuid, resultId);
  }
}

