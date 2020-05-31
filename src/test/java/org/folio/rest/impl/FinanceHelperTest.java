package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import io.vertx.core.impl.EventLoopContext;

public class FinanceHelperTest extends ApiTestBase{
  @Mock
  private Map<String, String> okapiHeadersMock;
  @Mock
  private EventLoopContext ctxMock;
  @Mock
  private HttpClientInterface httpClient;

  @Before
  public void initMocks(){
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testShouldMakeEncumbrancesPending() {
    //given
    FinanceHelper financeHelper = mock(FinanceHelper.class, CALLS_REAL_METHODS);
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    //When
    financeHelper.makeEncumbrancesPending(Collections.singletonList(encumbrance));
    //Then
    assertEquals(0d, encumbrance.getAmount(), 0.0);
    assertEquals(0d, encumbrance.getEncumbrance().getInitialAmountEncumbered(), 0.0);
    assertEquals(Encumbrance.Status.PENDING, encumbrance.getEncumbrance().getStatus());
  }


  @Test
  public void testShouldInvokeUpdateTransactionTimesEqualToTransactionQuantity() {
    //given
    FinanceHelper financeHelper = spy(new FinanceHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);
    doReturn(completedFuture(null)).when(financeHelper).updateTransaction(any());
    //When
    financeHelper.updateTransactions(Arrays.asList(encumbrance, encumbrance));
    //Then
    verify(financeHelper, times(2)).updateTransaction(any());
  }
}
