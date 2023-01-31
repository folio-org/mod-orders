package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.UUID;

import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.transaction.summary.OrderTransactionSummariesService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.json.JsonObject;

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
    // given

    String uuid = UUID.randomUUID().toString();
    var response = new JsonObject("{\"id\": \"" + uuid + "\"}");
    doReturn(succeededFuture(response)).when(restClient).post(anyString(), any(), any(), any());
    // When
    OrderTransactionSummary summary = orderTransactionSummariesService.createTransactionSummary(new OrderTransactionSummary().withId(uuid).withNumTransactions(2), requestContext)
      .result();
    // Then
    assertEquals(uuid, summary.getId());
    verify(restClient).post(anyString(), any(), any(), any());
  }
}
