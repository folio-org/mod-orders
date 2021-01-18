package org.folio.service.finance;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;

import java.util.Collections;
import java.util.HashMap;

import io.vertx.core.Vertx;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.tools.client.HttpClientFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class OpenToPendingEncumbranceStrategyTest {

    @InjectMocks
    private OpenToPendingEncumbranceStrategy openToPendingEncumbranceStrategy;

    @Mock
    private EncumbranceService encumbranceService;
    @Mock
    private TransactionSummariesService transactionSummariesService;

    @Mock
    private RequestContext requestContext;

    @BeforeEach
    public void initMocks() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testShouldSetEncumbrancesToPending() {
        //given
        CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

        Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray("transactions").getJsonObject(0).mapTo(Transaction.class);

        doReturn(completedFuture(Collections.singletonList(encumbrance))).when(encumbranceService).getOrderEncumbrances(any(), any());
        doReturn(completedFuture(null)).when(encumbranceService).updateEncumbrances(any(), any());
        doReturn(completedFuture(null)).when(transactionSummariesService).updateOrderTransactionSummary(anyString(), anyInt(), any());
        //When
        openToPendingEncumbranceStrategy.processEncumbrances(order, requestContext).join();
        //Then
        assertEquals(0d, encumbrance.getAmount(), 0.0);
        assertEquals(0d, encumbrance.getEncumbrance().getInitialAmountEncumbered(), 0.0);
        assertEquals(Encumbrance.Status.PENDING, encumbrance.getEncumbrance().getStatus());
        assertEquals(Encumbrance.OrderStatus.PENDING, encumbrance.getEncumbrance().getOrderStatus());
    }


}
