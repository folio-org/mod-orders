package org.folio.service.finance.transaction;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import io.vertx.junit5.VertxExtension;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.client.Response;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


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
  void testShouldInvokeUpdateTransaction() throws Exception {
    //given
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    doReturn(succeededFuture()).when(restClient).put(any(RequestEntry.class), eq(encumbrance), eq(requestContext));
    //When
    transactionService.updateTransaction(encumbrance, requestContext).result();
    //Then
    ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
    verify(restClient).put(argumentCaptor.capture(), eq(encumbrance), eq(requestContext));
    RequestEntry requestEntry = argumentCaptor.getValue();

    assertEquals(encumbrance.getId(), requestEntry.getPathParams().get("id"));
    assertEquals("/finance/encumbrances/{id}", requestEntry.getBaseEndpoint());
  }




}
