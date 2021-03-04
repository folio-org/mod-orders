package org.folio.service.finance.transaction;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.client.Response;
import org.folio.service.finance.transaction.TransactionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

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

    Response response = new Response();
    response.setCode(204);
    doReturn(completedFuture(response)).when(restClient).put(any(), any(), any());
    //When
    transactionService.updateTransaction(encumbrance, requestContext).get();
    //Then
    ArgumentCaptor<RequestEntry> argumentCaptor = ArgumentCaptor.forClass(RequestEntry.class);
    verify(restClient).put(argumentCaptor.capture(), any(), any());
    RequestEntry requestEntry = argumentCaptor.getValue();

    assertEquals(encumbrance.getId(), requestEntry.getPathParams().get("id"));
    assertEquals("/finance/encumbrances/{id}", requestEntry.getBaseEndpoint());
  }




}
