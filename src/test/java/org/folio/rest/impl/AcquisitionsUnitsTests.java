package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.rest.impl.MockServer.ACQUISITIONS_UNITS_COLLECTION;
import static org.folio.rest.impl.MockServer.getAcqUnitsRetrievals;
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
import java.util.stream.Collectors;

import javax.ws.rs.core.HttpHeaders;

import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.Errors;
import org.junit.Test;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class AcquisitionsUnitsTests extends ApiTestBase {
  private static final Logger logger = LoggerFactory.getLogger(AcquisitionsUnitsTests.class);

  private static final String ACQ_UNITS_UNITS_ENDPOINT = "/acquisitions-units/units";

  @Test
  public void testGetAcqUnitsNoQuery() throws IOException {
    logger.info("=== Test Get Acquisitions Units - With empty query ===");
    AcquisitionsUnitCollection expected = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_UNITS_COLLECTION)).mapTo(AcquisitionsUnitCollection.class);
    final AcquisitionsUnitCollection units = verifySuccessGet(ACQ_UNITS_UNITS_ENDPOINT, AcquisitionsUnitCollection.class);
    assertThat(expected.getAcquisitionsUnits(), hasSize(expected.getTotalRecords()));
    assertThat(units.getAcquisitionsUnits(), hasSize(expected.getTotalRecords()));
  }

  @Test
  public void testGetAcqUnitsWithQuery() {
    logger.info("=== Test GET Acquisitions Units - search by query ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "?query=name==Read only";

    final AcquisitionsUnitCollection units = verifySuccessGet(url, AcquisitionsUnitCollection.class);
    assertThat(units.getAcquisitionsUnits(), hasSize(1));
  }

  @Test
  public void testGetAcqUnitsWithUnprocessableQuery() {
    logger.info("=== Test GET Acquisitions Units - unprocessable query ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "?query=" + BAD_QUERY;

    verifyGet(url, APPLICATION_JSON, 400);
  }

  @Test
  public void testGetAcqUnitSuccess() {
    logger.info("=== Test GET Acquisitions Unit - success case ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/0e9525aa-d123-4e4d-9f7e-1b302a97eb90";

    final AcquisitionsUnit unit = verifySuccessGet(url, AcquisitionsUnit.class);
    assertThat(unit, notNullValue());
  }

  @Test
  public void testGetAcqUnitNotFound() {
    logger.info("=== Test GET Acquisitions Unit - not found ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyGet(url, APPLICATION_JSON, 404);
  }

  @Test
  public void testPutAcqUnitSuccess() {
    logger.info("=== Test PUT acquisitions unit - success case ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/0e9525aa-d123-4e4d-9f7e-1b302a97eb90";

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnit().withName("Some name")), "", 204);
  }

  @Test
  public void testValidationOnPutUnitWithoutBody() {
    logger.info("=== Test validation on PUT acquisitions unit with no body ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + UUID.randomUUID().toString();

    verifyPut(url, "", TEXT_PLAIN, 400);
  }

  @Test
  public void testPutUnitNotFound() {
    logger.info("=== Test PUT acquisitions unit - not found ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnit().withName("Some name")), APPLICATION_JSON, 404);
  }

  @Test
  public void testPutUnitIdMismatch() {
    logger.info("=== Test PUT acquisitions unit - different ids in path and body ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + UUID.randomUUID().toString();

    AcquisitionsUnit unit = new AcquisitionsUnit().withName("Some name").withId(UUID.randomUUID().toString());
    Errors errors = verifyPut(url, JsonObject.mapFrom(unit), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
  }

  @Test
  public void testDeleteAcqUnitSuccess() {
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
  public void testDeleteUnitNotFound() {
    logger.info("=== Test DELETE acquisitions unit - not found ===");
    String url = ACQ_UNITS_UNITS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyDeleteResponse(url, APPLICATION_JSON, 404);
    assertThat(getRqRsEntries(HttpMethod.PUT, ACQUISITIONS_UNITS), empty());
  }

  @Test
  public void testPostAcqUnitSuccess() {
    logger.info("=== Test POST acquisitions unit - success case ===");

    String body = JsonObject.mapFrom(new AcquisitionsUnit().withName("Some name")).encode();
    Response response = verifyPostResponse(ACQ_UNITS_UNITS_ENDPOINT, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10),
        APPLICATION_JSON, 201);
    AcquisitionsUnit unit = response.as(AcquisitionsUnit.class);

    assertThat(unit.getId(), not(isEmptyOrNullString()));
    assertThat(response.header(HttpHeaders.LOCATION), containsString(unit.getId()));
  }

  @Test
  public void testPostAcqUnitServerError() {
    logger.info("=== Test POST acquisitions unit - Server Error ===");

    String body = JsonObject.mapFrom(new AcquisitionsUnit().withName("Some name")).encode();
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, new Header(X_ECHO_STATUS, String.valueOf(500)));
    verifyPostResponse(ACQ_UNITS_UNITS_ENDPOINT, body, headers, APPLICATION_JSON, 500);
  }
}
