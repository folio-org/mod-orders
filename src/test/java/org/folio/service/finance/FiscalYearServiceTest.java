package org.folio.service.finance;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Vertx;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.finance.EncumbranceService;
import org.folio.service.finance.FiscalYearService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import static org.folio.ApiTestSuite.mockPort;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.ApiTestBase.ID_DOES_NOT_EXIST;
import static org.folio.rest.impl.ApiTestBase.X_OKAPI_TOKEN;
import static org.folio.rest.impl.ApiTestBase.X_OKAPI_USER_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class FiscalYearServiceTest {

    public static final String TENANT_ID = "ordertest";
    public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);


    private FiscalYearService fiscalYearService;

    private Context ctxMock;
    private Map<String, String> okapiHeadersMock;
    private HttpClientInterface httpClient;
    private RequestContext requestContextMock;

    @BeforeEach
    public void initMocks(){
        ctxMock = Vertx.vertx().getOrCreateContext();
        okapiHeadersMock = new HashMap<>();
        okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
        okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
        okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
        okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
        String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
        requestContextMock = new RequestContext(ctxMock, okapiHeadersMock);
        httpClient = HttpClientFactory.getHttpClient(okapiURL, TENANT_ID);
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testShouldReturnCurrentFiscalYearForLedger() {

        String ledgerId = UUID.randomUUID().toString();
        FiscalYear fy = fiscalYearService.getCurrentFiscalYear(ledgerId, requestContextMock).join();
        assertNotNull(fy);
    }

    @Test
    public void testShouldThrowHttpException() {

        CompletableFuture<FiscalYear> result =  fiscalYearService.getCurrentFiscalYear(ID_DOES_NOT_EXIST, requestContextMock);
        CompletionException expectedException = assertThrows(CompletionException.class, result::join);

        HttpException httpException = (HttpException) expectedException.getCause();
        assertEquals(404, httpException.getCode());
    }
}
