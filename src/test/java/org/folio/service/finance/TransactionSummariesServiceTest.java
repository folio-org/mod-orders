package org.folio.service.finance;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
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
import org.folio.rest.tools.client.Response;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.json.JsonObject;

public class TransactionSummariesServiceTest {

  @InjectMocks
  private TransactionSummariesService transactionSummariesService;

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
    transactionSummariesService.updateOrderTransactionSummary(UUID.randomUUID()
      .toString(), 0, requestContext);
    // Then
    verify(restClient, never()).put(any(), any(), any());
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
    transactionSummariesService.updateOrderTransactionSummary(order.getId(), 1, requestContext);
    // Then
    assertNull(transactionSummariesService.getOrderTransactionSummary(order.getId(), requestContext));
  }

  @Test
  void testShouldCreateTransactionSummaryInStorageTransactions() {
    // given

    String uuid = UUID.randomUUID()
      .toString();
    Response response = new Response();
    response.setBody(new JsonObject("{\"id\": \"" + uuid + "\"}"));
    response.setCode(201);
    doReturn(completedFuture(response)).when(restClient)
      .post(any(), any(), any(), any());
    // When
    OrderTransactionSummary summary = transactionSummariesService.createOrderTransactionSummary(uuid, 2, requestContext)
      .join();
    // Then
    assertEquals(uuid, summary.getId());
    verify(restClient).post(any(), any(), any(), any());
  }
}
