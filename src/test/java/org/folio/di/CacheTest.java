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

import org.folio.AcquisitionMethod;
import org.folio.AcquisitionsUnit;
import org.folio.ContributorNameType;
import org.folio.ExpenseClass;
import org.folio.Fund;
import org.folio.IdentifierType;
import org.folio.Location;
import org.folio.Mtype;
import org.folio.Organization;
import org.folio.processing.mapping.defaultmapper.processor.parameters.MappingParameters;
import org.folio.rest.core.RestClient;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.util.OkapiConnectionParams;
import org.folio.service.AcquisitionMethodsService;
import org.folio.service.AcquisitionsUnitsService;
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
  private final AcquisitionsUnitsService acquisitionsUnitsService = new AcquisitionsUnitsService(new RestClient());
  private final AcquisitionMethodsService acquisitionMethodsService = new AcquisitionMethodsService(new RestClient(), null);

  @Rule
  public WireMockRule snapshotMockServer =
    new WireMockRule(WireMockConfiguration.wireMockConfig().dynamicPort().notifier(new ConsoleNotifier(false)));

  @Before
  public void setUp() {
    Vertx vertx = rule.vertx();
    jobProfileSnapshotCache = new JobProfileSnapshotCache(vertx);
    mappingParametersCache = new MappingParametersCache(vertx, new RestClient(), acquisitionsUnitsService, acquisitionMethodsService);

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

    String orgQueryParam = "&query=" + encodeQuery("(cql.allRecords=1) sortBy id");
    String addressQueryParam = "query=" + encodeQuery("(module==TENANT and configName==tenant.addresses)");
    String acqUnitsQueryParam = "&query=" + encodeQuery("isDeleted==false");
    String uuid = UUID.randomUUID().toString();
    Organization organization = new Organization().withId(uuid);
    Location location = new Location().withId(uuid);
    Mtype materialType = new Mtype().withId(uuid);
    IdentifierType identifierType = new IdentifierType().withId(uuid);
    ContributorNameType contributorNameType = new ContributorNameType().withId(uuid);
    Fund fund = new Fund().withId(uuid);
    ExpenseClass expenseClass = new ExpenseClass().withId(uuid);
    AcquisitionsUnit acquisitionsUnit = new AcquisitionsUnit().withId(uuid);
    AcquisitionMethod acquisitionMethod = new AcquisitionMethod().withId(uuid);
    String addressName = "test";
    String addressStreet = "test st. 1";
    JsonObject address = new JsonObject().put("name", addressName).put("address", addressStreet);

    WireMock.stubFor(
      get("/organizations/organizations?limit=5000" + orgQueryParam)
        .willReturn(okJson(new JsonObject()
          .put("organizations", JsonArray.of(organization))
          .put("totalRecords", 5001)
          .toString())));

    WireMock.stubFor(
      get("/organizations/organizations?offset=5000" + orgQueryParam + "&limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("organizations", JsonArray.of(organization))
          .put("totalRecords", 5001)
          .toString())));

    WireMock.stubFor(
      get("/locations?limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("locations", JsonArray.of(location))
          .put("totalRecords", 5001)
          .toString())));

    WireMock.stubFor(
      get("/material-types?limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("mtypes", JsonArray.of(materialType))
          .put("totalRecords", 10)
          .toString())));

    WireMock.stubFor(
      get("/identifier-types?limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("identifierTypes", JsonArray.of(identifierType))
          .put("totalRecords", 10)
          .toString())));

    WireMock.stubFor(
      get("/contributor-name-types?limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("contributorNameTypes", JsonArray.of(contributorNameType))
          .put("totalRecords", 10)
          .toString())));

    WireMock.stubFor(
      get("/finance/expense-classes?limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("expenseClasses", JsonArray.of(expenseClass))
          .put("totalRecords", 10)
          .toString())));

    WireMock.stubFor(
      get("/finance/funds?limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("funds", JsonArray.of(fund))
          .put("totalRecords", 10)
          .toString())));

    WireMock.stubFor(
      get("/orders-storage/acquisition-methods?offset=0&limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("acquisitionMethods", JsonArray.of(acquisitionMethod))
          .put("totalRecords", 10)
          .toString())));

    WireMock.stubFor(
      get("/acquisitions-units-storage/units?offset=0" + acqUnitsQueryParam + "&limit=5000")
        .willReturn(okJson(new JsonObject()
          .put("acquisitionsUnits", JsonArray.of(acquisitionsUnit))
          .put("totalRecords", 10)
          .toString())));

    WireMock.stubFor(
      get("/configurations/entries?" + addressQueryParam)
        .willReturn(okJson(new JsonObject()
          .put("configs", JsonArray.of(new JsonObject().put("id", uuid).put("value", address.encode())))
          .put("totalRecords", 10)
          .toString())));

    mappingParametersCache
      .get(okapiConnectionParams)
      .onComplete(
        ar -> {
          context.assertTrue(ar.succeeded());
          MappingParameters result = ar.result();
          context.assertNotNull(result);
          context.assertEquals(2, result.getOrganizations().size());
          context.assertEquals(1, result.getLocations().size());
          context.assertEquals(1, result.getMaterialTypes().size());
          context.assertEquals(1, result.getIdentifierTypes().size());
          context.assertEquals(1, result.getContributorNameTypes().size());
          context.assertEquals(1, result.getFunds().size());
          context.assertEquals(1, result.getExpenseClasses().size());
          context.assertEquals(1, result.getAcquisitionsUnits().size());
          context.assertEquals(1, result.getAcquisitionMethods().size());
          context.assertEquals(1, result.getTenantConfigurationAddresses().size());

          result.getOrganizations().forEach(org -> context.assertEquals(uuid, org.getId()));
          result.getLocations().forEach(loc -> context.assertEquals(uuid, loc.getId()));
          result.getMaterialTypes().forEach(mt -> context.assertEquals(uuid, mt.getId()));
          result.getIdentifierTypes().forEach(it -> context.assertEquals(uuid, it.getId()));
          result.getContributorNameTypes().forEach(cnt -> context.assertEquals(uuid, cnt.getId()));
          result.getFunds().forEach(fnd -> context.assertEquals(uuid, fnd.getId()));
          result.getExpenseClasses().forEach(exp -> context.assertEquals(uuid, exp.getId()));
          result.getAcquisitionsUnits().forEach(units -> context.assertEquals(uuid, units.getId()));
          result.getAcquisitionMethods().forEach(method -> context.assertEquals(uuid, method.getId()));
          result.getTenantConfigurationAddresses().forEach(a -> {
            JsonObject addr = new JsonObject(a);
            context.assertEquals(uuid, addr.getString("id"));
            context.assertEquals(addressName, addr.getString("name"));
            context.assertEquals(addressStreet, addr.getString("address"));
          });
          async.complete();
        });
  }

  @Test
  public void getMappingParametersFromCacheWithError(TestContext context) {
    Async async = context.async();
    String queryParam = "&query=" + encodeQuery("(cql.allRecords=1) sortBy id");
    String addressQueryParam = "query=" + encodeQuery("(module==TENANT and configName==tenant.addresses)");
    String acqUnitsQueryParam = "&query=" + encodeQuery("isDeleted==false");
    WireMock.stubFor(
      get("/organizations/organizations?limit=0" + queryParam)
        .willReturn(serverError()));
    WireMock.stubFor(
      get("/locations?limit=0")
        .willReturn(serverError()));

    WireMock.stubFor(
      get("/material-types?limit=0")
        .willReturn(serverError()));

    WireMock.stubFor(
      get("/identifier-types?limit=0")
        .willReturn(serverError()));

    WireMock.stubFor(
      get("/contributor-name-types?limit=0")
        .willReturn(serverError()));

    WireMock.stubFor(
      get("/finance/expense-classes?limit=0")
        .willReturn(serverError()));

    WireMock.stubFor(
      get("/finance/funds?limit=0")
        .willReturn(serverError()));

    WireMock.stubFor(
      get("/orders-storage/acquisition-methods?offset=0&limit=0")
        .willReturn(serverError()));

    WireMock.stubFor(
      get("/acquisitions-units-storage/units?offset=0" + acqUnitsQueryParam + "&limit=0")
        .willReturn(serverError()));

    WireMock.stubFor(
      get("/configurations/entries?" + addressQueryParam)
        .willReturn(serverError()));
    mappingParametersCache
      .get(okapiConnectionParams)
      .onComplete(
        ar -> {
          context.assertTrue(ar.failed());
          context.assertEquals(ar.cause().getClass(), CacheLoadingException.class);
          async.complete();
        });
  }
}
