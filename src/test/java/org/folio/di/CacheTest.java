package org.folio.di;

import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.okJson;
import static com.github.tomakehurst.wiremock.client.WireMock.serverError;
import static org.folio.dataimport.util.RestUtil.OKAPI_URL_HEADER;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.rest.jaxrs.model.ProfileType.JOB_PROFILE;
import static org.folio.rest.util.OkapiConnectionParams.OKAPI_TENANT_HEADER;
import static org.folio.rest.util.OkapiConnectionParams.OKAPI_TOKEN_HEADER;

import java.util.HashMap;
import java.util.Optional;
import java.util.UUID;

import org.folio.Organization;
import org.folio.processing.mapping.defaultmapper.processor.parameters.MappingParameters;
import org.folio.rest.core.RestClient;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.service.caches.CacheLoadingException;
import org.folio.service.caches.JobProfileSnapshotCache;
import org.folio.service.caches.MappingParametersCache;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.common.ConsoleNotifier;
import com.github.tomakehurst.wiremock.core.WireMockConfiguration;
import com.github.tomakehurst.wiremock.junit.WireMockRule;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.RunTestOnContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

@RunWith(VertxUnitRunner.class)
public class CacheTest {

  @Rule
  public RunTestOnContext rule = new RunTestOnContext();

  private JobProfileSnapshotCache jobProfileSnapshotCache;
  private MappingParametersCache mappingParametersCache;

  private OkapiConnectionParams okapiConnectionParams;

  @Rule
  public WireMockRule snapshotMockServer =
    new WireMockRule(WireMockConfiguration.wireMockConfig().dynamicPort().notifier(new ConsoleNotifier(false)));

  @Before
  public void setUp() {
    Vertx vertx = rule.vertx();
    jobProfileSnapshotCache = new JobProfileSnapshotCache(vertx);
    mappingParametersCache = new MappingParametersCache(vertx, new RestClient());

    HashMap<String, String> headers = new HashMap<>();
    headers.put(OKAPI_URL_HEADER, "http://localhost:" + snapshotMockServer.port());
    headers.put(OKAPI_TENANT_HEADER, "diku");
    headers.put(OKAPI_TOKEN_HEADER, "token");
    okapiConnectionParams = new OkapiConnectionParams(headers, vertx);
  }

  @Test
  public void getJobProfileSnapshotFromCache(TestContext context) {
    Async async = context.async();
    String jobProfileSnapshotId = UUID.randomUUID().toString();

    WireMock.stubFor(
      get("/data-import-profiles/jobProfileSnapshots/" + jobProfileSnapshotId)
        .willReturn(okJson(new JsonObject()
          .put("childSnapshotWrappers", new JsonArray())
          .put("contentType", "JOB_PROFILE")
          .toString())));

    jobProfileSnapshotCache
      .get(jobProfileSnapshotId, okapiConnectionParams)
      .onComplete(
        ar -> {
          context.assertTrue(ar.succeeded());
          Optional<ProfileSnapshotWrapper> result = ar.result();
          context.assertNotNull(result.orElse(null));
          context.assertEquals(result.orElse(new ProfileSnapshotWrapper()).getContentType(), JOB_PROFILE);
          async.complete();
        });
  }

  @Test
  public void getMappingParametersFromCache(TestContext context) {
    Async async = context.async();
    ReflectionTestUtils.setField(mappingParametersCache, "settingsLimit", 5000);

    String queryParam = "&query=" + encodeQuery("(cql.allRecords=1) sortBy id");
    String organizationId = UUID.randomUUID().toString();
    Organization organization = new Organization().withId(organizationId);

    WireMock.stubFor(
      get("/organizations/organizations?limit=5000" + queryParam)
        .willReturn(okJson(new JsonObject()
          .put("organizations", JsonArray.of(organization))
          .put("totalRecords", 5001)
          .toString())));

    WireMock.stubFor(
      get("/organizations/organizations?offset=5000" + queryParam + "&limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("organizations", JsonArray.of(organization))
          .put("totalRecords", 5001)
          .toString())));

    mappingParametersCache
      .get(okapiConnectionParams)
      .onComplete(
        ar -> {
          context.assertTrue(ar.succeeded());
          MappingParameters result = ar.result();
          context.assertNotNull(result);
          result.getOrganizations().forEach(org -> context.assertEquals(organizationId, org.getId()));
          async.complete();
        });
  }

  @Test
  public void getMappingParametersFromCacheWithError(TestContext context) {
    Async async = context.async();
    String queryParam = "&query=" + encodeQuery("(cql.allRecords=1) sortBy id");
    WireMock.stubFor(
      get("/organizations/organizations?limit=0" + queryParam)
        .willReturn(serverError()));

    mappingParametersCache
      .get(okapiConnectionParams)
      .onComplete(
        ar -> {
          context.assertTrue(ar.failed());
          context.assertEquals(ar.cause().getCause().getClass(), CacheLoadingException.class);
          async.complete();
        });
  }
}
