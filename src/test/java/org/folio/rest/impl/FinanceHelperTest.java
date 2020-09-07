package org.folio.rest.impl;

import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.ApiTestSuite.mockPort;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;

import org.folio.helper.FinanceHelper;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.RestConstants;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.junit.Before;
import org.junit.Test;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

public class FinanceHelperTest extends ApiTestBase {
  private static final String VALID_BATCH_VOUCHER_EXPORTS_ID ="566c9156-e52f-4597-9fee-5ddac91d14f2";
  public static final String BATCH_VOUCHER_EXPORTS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "batchVoucherExports/";
  private static final String BATCH_VOUCHER_EXPORT_SAMPLE_PATH = BATCH_VOUCHER_EXPORTS_MOCK_DATA_PATH
    + VALID_BATCH_VOUCHER_EXPORTS_ID + ".json";
  public static final String TENANT_ID = "oredertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);

  private Context context;
  private Map<String, String> okapiHeaders;
  private HttpClientInterface httpClient;
  private String okapiURL;

  @Before
  public void setUp()  {
    super.setUp();
    context = Vertx.vertx().getOrCreateContext();
    okapiHeaders = new HashMap<>();
    okapiHeaders.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeaders.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeaders.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeaders.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    okapiURL = okapiHeaders.getOrDefault(OKAPI_URL, "");
    httpClient = HttpClientFactory.getHttpClient(okapiURL, TENANT_ID);
  }

  @Test
  public void testShouldReturnCurrentFiscalYearForLedger() {
    FinanceHelper financeHelper = new FinanceHelper(httpClient, okapiHeaders, context, "en");
    String ledgerId = UUID.randomUUID().toString();
    FiscalYear fy = financeHelper.getCurrentFiscalYear(ledgerId).join();
    assertNotNull(fy);
  }

  @Test(expected = CompletionException.class)
  public void testShouldThrowHttpException() {
    FinanceHelper financeHelper = new FinanceHelper(httpClient, okapiHeaders, context, "en");
    financeHelper.getCurrentFiscalYear(ID_DOES_NOT_EXIST).join();
  }

}
