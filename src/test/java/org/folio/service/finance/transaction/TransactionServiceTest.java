package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import org.folio.rest.acq.model.finance.Batch;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.junit5.VertxExtension;

import java.util.List;


@ExtendWith(VertxExtension.class)
public class TransactionServiceTest {
  @InjectMocks
  private TransactionService transactionService;
  @Mock
  private RestClient restClient;
  @Mock
  private RequestContext requestContext;

  @BeforeEach
  public void initMocks(){
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testShouldInvokeUpdateTransaction() {
    //Given
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0)
      .mapTo(Transaction.class);
    Batch batch = new Batch()
      .withTransactionsToUpdate(List.of(encumbrance));
    doReturn(succeededFuture())
      .when(restClient).postEmptyResponse(anyString(), eq(batch), eq(requestContext));

    //When
    transactionService.batchUpdate(List.of(encumbrance), requestContext).result();

    //Then
    ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);
    verify(restClient).postEmptyResponse(argumentCaptor.capture(), eq(batch), eq(requestContext));
    String endPoint = argumentCaptor.getValue();
    assertEquals("/finance/transactions/batch-all-or-nothing", endPoint);
  }

}
