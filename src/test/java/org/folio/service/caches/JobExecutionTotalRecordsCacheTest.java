package org.folio.service.caches;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.common.ConsoleNotifier;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import com.github.tomakehurst.wiremock.matching.RegexPattern;
import com.github.tomakehurst.wiremock.matching.UrlPathPattern;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.RunTestOnContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.folio.dataimport.util.RestUtil;
import org.folio.rest.util.OkapiConnectionParams;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static com.github.tomakehurst.wiremock.client.WireMock.get;

@RunWith(VertxUnitRunner.class)
public class JobExecutionTotalRecordsCacheTest {

  private static final String TENANT_ID = "diku";
  private static final String JOB_ID_FIELD = "id";
  private static final String JOB_PROGRESS_FIELD = "progress";
  public static final String JOB_TOTAL_RECORDS_FIELD = "total";
  private static final String JOB_EXECUTION_PATH = "/change-manager/jobExecutions/";

  @Rule
  public RunTestOnContext rule = new RunTestOnContext();
  @Rule
  public WireMockRule mockServer = new WireMockRule(WireMockConfiguration.wireMockConfig()
    .dynamicPort()
    .notifier(new ConsoleNotifier(false)));

  private JobExecutionTotalRecordsCache jobExecutionTotalRecordsCache;
  private OkapiConnectionParams okapiConnectionParams;

  @Before
  public void setUp() {
    Vertx vertx = Vertx.vertx();
    jobExecutionTotalRecordsCache = new JobExecutionTotalRecordsCache(vertx, 1L);
    this.okapiConnectionParams = new OkapiConnectionParams(new HashMap<>(Map.of(
      RestUtil.OKAPI_URL_HEADER, mockServer.baseUrl(),
      RestUtil.OKAPI_TENANT_HEADER, TENANT_ID,
      RestUtil.OKAPI_TOKEN_HEADER, "token"
    )), vertx);
  }

  @Test
  public void shouldReturnTotalRecordsAmountByJobExecutionId(TestContext context) {
    Async async = context.async();

    int expectedTotalAmount = 42;
    JsonObject jobExecutionJson = new JsonObject()
      .put(JOB_ID_FIELD, UUID.randomUUID().toString())
      .put(JOB_PROGRESS_FIELD, new JsonObject()
        .put(JOB_TOTAL_RECORDS_FIELD, expectedTotalAmount));

    WireMock.stubFor(get(new UrlPathPattern(new RegexPattern(JOB_EXECUTION_PATH + ".*"), true))
      .willReturn(WireMock.ok().withBody(jobExecutionJson.encodePrettily())));

    jobExecutionTotalRecordsCache.get(jobExecutionJson.getString(JOB_ID_FIELD), okapiConnectionParams).onComplete(ar -> {
      context.assertTrue(ar.succeeded());
      context.assertEquals(expectedTotalAmount, ar.result());
      async.complete();
    });
  }

  @Test
  public void shouldReturnFailedFutureWhenGetErrorResponseOnTotalRecordsAmountLoading(TestContext context) {
    WireMock.stubFor(get(new UrlPathPattern(new RegexPattern(JOB_EXECUTION_PATH + ".*"), true))
      .willReturn(WireMock.serverError()));

    jobExecutionTotalRecordsCache.get(UUID.randomUUID().toString(), okapiConnectionParams)
      .onComplete(context.asyncAssertFailure());
  }

}
