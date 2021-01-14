package org.folio.service.finance;

import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.tools.client.Response;
import org.folio.service.finance.EncumbranceService;
import org.folio.service.finance.TransactionService;
import org.folio.service.finance.TransactionSummariesService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import static io.restassured.RestAssured.when;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.OKAPI_URL;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TRANSACTION_SUMMARIES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.impl.MockServer.ENCUMBRANCE_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

public class TransactionSummariesServiceTest {

    @InjectMocks
    private TransactionSummariesService transactionSummariesService;

    @Mock
    private RestClient transactionSummaryRestClient;

    @Mock
    private RequestContext requestContext;

    @BeforeEach
    public void initMocks(){
        MockitoAnnotations.openMocks(this);
    }


    @Test
    void testShouldNotUpdateTransactionsSummariesWhenNoEncumbrances() {
        //When
        transactionSummariesService.updateOrderTransactionSummary(UUID.randomUUID().toString(), 0, requestContext);
        //Then
       verify(transactionSummaryRestClient, never()).put(any(), any(), any());
    }

    @Test
    void testShouldTransactionsCreatedForEncumbrances() {
        //Given
        EncumbranceService encumbranceService = spy(new EncumbranceService());
        CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
        CompositePoLine line = order.getCompositePoLines().get(0);
        Transaction encumbrance = getMockAsJson(ENCUMBRANCE_PATH).getJsonArray( "transactions").getJsonObject(0).mapTo(Transaction.class);
        FundDistribution fundDistribution = order.getCompositePoLines().get(0).getFundDistribution().get(0);
        encumbranceService.updateEncumbrance(fundDistribution, line, encumbrance);
        //When
        encumbranceService.updateOrderTransactionSummary(order.getId(), 1);
        //Then
        assertNull(transactionService.getOrderTransactionSummary(order.getId()));
    }

    @Test
    void testShouldCreateTransactionSummaryInStorageTransactions() throws Exception {
        //given
        TransactionService service = spy(new TransactionService(httpClient, okapiHeaders, ctxMock, "en"));
        String uuid = UUID.randomUUID().toString();
        Response response = new Response();
        response.setBody(new JsonObject("{\"id\": \""+uuid+"\"}"));
        response.setCode(201);
        doReturn(completedFuture(response)).when(httpClient).request(eq(HttpMethod.POST), any(), eq(resourcesPath(ORDER_TRANSACTION_SUMMARIES)), any());
        //When
        String summaryId= service.createOrderTransactionSummary(uuid, 2).join();
        //Then
        assertEquals(uuid, summaryId);
        verify(httpClient).request(eq(HttpMethod.POST), any(), eq(resourcesPath(ORDER_TRANSACTION_SUMMARIES)), any());
    }
}
