package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.RestTestUtils.verifySuccessGet;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.BAD_QUERY;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.X_ECHO_STATUS;
import static org.folio.TestUtils.getMockData;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.rest.impl.MockServer.ACQUISITIONS_UNITS_COLLECTION;
import static org.folio.rest.impl.MockServer.getAcqUnitsRetrievals;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.folio.rest.impl.MockServer.getRqRsEntries;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import javax.ws.rs.core.HttpHeaders;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.Errors;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;

public class AcquisitionsUnitsTest {
  private static final Logger logger = LogManager.getLogger();

  private static final String ACQ_UNITS_UNITS_ENDPOINT = "/acquisitions-units/units";
  static final String IS_DELETED_PROP = "isDeleted";
  static final String ACTIVE_UNITS_CQL = IS_DELETED_PROP + "==false";
  static final String ALL_UNITS_CQL = IS_DELETED_PROP + "=*";

  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @AfterEach
  void afterEach() {
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void testGetAcqUnitsNoQuery() throws IOException {
    logger.info("=== Test Get Acquisitions Units - With empty query ===");
    AcquisitionsUnitCollection expected = new JsonObject(getMockData(ACQUISITIONS_UNITS_COLLECTION)).mapTo(AcquisitionsUnitCollection.class);
    final AcquisitionsUnitCollection units = verifySuccessGet(ACQ_UNITS_UNITS_ENDPOINT, AcquisitionsUnitCollection.class);
    assertThat(expected.getAcquisitionsUnits(), hasSize(expected.getTotalRecords()));
    assertThat(units.getAcquisitionsUnits(), hasSize(expected.getTotalRecords()));

    List<String> queryParams = getQueryParams(ACQUISITIONS_UNITS);
    assertThat(queryParams, hasSize(1));
    assertThat(queryParams.get(0), containsString(ACTIVE_UNITS_CQL));
  }

  @Test
  void testGetAcqUnitsWithQuery() {
    logger.info("=== Test GET Acquisitions Units - search by query ===");
    String cql = ALL_UNITS_CQL + " and name==Read only";
    String url = ACQ_UNITS_UNITS_ENDPOINT + "?query=" + cql;

    final AcquisitionsUnitCollection units = verifySuccessGet(url, AcquisitionsUnitCollection.class);
    assertThat(units.getAcquisitionsUnits(), hasSize(1));

    List<String> queryParams = getQueryParams(ACQUISITIONS_UNITS);
    assertThat(queryParams, hasSize(1));
    assertThat(queryParams.get(0), equalTo(cql));
  }

  @Test
  void testGetAcqUnitsWithUnprocessableQuery() {
    logger.info("=== Test GET Acquisitions Units - unprocessable query ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "?query=" + BAD_QUERY;

    verifyGet(url, APPLICATION_JSON, 400);
  }

  @Test
  void testGetAcqUnitSuccess() {
    logger.info("=== Test GET Acquisitions Unit - success case ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/0e9525aa-d123-4e4d-9f7e-1b302a97eb90";

    final AcquisitionsUnit unit = verifySuccessGet(url, AcquisitionsUnit.class);
    assertThat(unit, notNullValue());
  }

  @Test
  void testGetAcqUnitNotFound() {
    logger.info("=== Test GET Acquisitions Unit - not found ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyGet(url, APPLICATION_JSON, 404);
  }

  @Test
  void testPutAcqUnitSuccess() {
    logger.info("=== Test PUT acquisitions unit - success case ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/0e9525aa-d123-4e4d-9f7e-1b302a97eb90";

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnit().withName("Some name")), "", 204);
  }

  @Test
  void testValidationOnPutUnitWithoutBody() {
    logger.info("=== Test validation on PUT acquisitions unit with no body ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + UUID.randomUUID().toString();

    verifyPut(url, "", TEXT_PLAIN, 400);
  }

  @Test
  void testPutUnitNotFound() {
    logger.info("=== Test PUT acquisitions unit - not found ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnit().withName("Some name")), APPLICATION_JSON, 404);
  }

  @Test
  void testPutUnitIdMismatch() {
    logger.info("=== Test PUT acquisitions unit - different ids in path and body ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + UUID.randomUUID().toString();

    AcquisitionsUnit unit = new AcquisitionsUnit().withName("Some name").withId(UUID.randomUUID().toString());
    Errors errors = verifyPut(url, JsonObject.mapFrom(unit), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
  }

  @Test
  void testDeleteAcqUnitSuccess() {
    logger.info("=== Test DELETE acquisitions unit - success case ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/0e9525aa-d123-4e4d-9f7e-1b302a97eb90";

    verifyDeleteResponse(url, "", 204);
    assertThat(getAcqUnitsRetrievals(), hasSize(1));

    List<AcquisitionsUnit> updates = getRqRsEntries(HttpMethod.PUT, ACQUISITIONS_UNITS).stream()
      .map(json -> json.mapTo(AcquisitionsUnit.class))
      .collect(Collectors.toList());
    assertThat(updates, hasSize(1));
    assertThat(updates.get(0).getIsDeleted(), is(true));
  }

  @Test
  void testDeleteUnitNotFound() {
    logger.info("=== Test DELETE acquisitions unit - not found ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyDeleteResponse(url, APPLICATION_JSON, 404);
    assertThat(getRqRsEntries(HttpMethod.PUT, ACQUISITIONS_UNITS), empty());
  }

  @Test
  void testPostAcqUnitSuccess() {
    logger.info("=== Test POST acquisitions unit - success case ===");

    String body = JsonObject.mapFrom(new AcquisitionsUnit().withName("Some name")).encode();
    Response response = verifyPostResponse(ACQ_UNITS_UNITS_ENDPOINT, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10),
        APPLICATION_JSON, 201);
    AcquisitionsUnit unit = response.as(AcquisitionsUnit.class);

    assertThat(unit.getId(), not(isEmptyOrNullString()));
    assertThat(response.header(HttpHeaders.LOCATION), containsString(unit.getId()));
  }

  @Test
  void testPostAcqUnitServerError() {
    logger.info("=== Test POST acquisitions unit - Server Error ===");

    String body = JsonObject.mapFrom(new AcquisitionsUnit().withName("Some name")).encode();
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, new Header(X_ECHO_STATUS, String.valueOf(500)));
    verifyPostResponse(ACQ_UNITS_UNITS_ENDPOINT, body, headers, APPLICATION_JSON, 500);
  }
}
