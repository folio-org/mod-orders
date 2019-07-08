package org.folio.rest.impl;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembership;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembershipCollection;
import org.folio.rest.jaxrs.model.Errors;
import org.junit.Test;

import javax.ws.rs.core.HttpHeaders;
import java.io.IOException;
import java.util.UUID;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.impl.MockServer.ACQUISITIONS_MEMBERSHIPS_COLLECTION;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

public class AcquisitionsMembershipsTests extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(AcquisitionsMembershipsTests.class);

  private static final String ACQ_UNITS_MEMBERSHIPS_ENDPOINT = "/acquisitions-units/memberships";

  @Test
  public void testGetAcqMembershipsNoQuery() throws IOException {
    logger.info("=== Test GET acquisitions units - with empty query ===");
    AcquisitionsUnitMembershipCollection expected = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_MEMBERSHIPS_COLLECTION)).mapTo(AcquisitionsUnitMembershipCollection.class);
    final AcquisitionsUnitMembershipCollection memberships = verifySuccessGet(ACQ_UNITS_MEMBERSHIPS_ENDPOINT, AcquisitionsUnitMembershipCollection.class);
    assertThat(expected.getAcquisitionsUnitMemberships(), hasSize(expected.getTotalRecords()));
    assertThat(memberships.getAcquisitionsUnitMemberships(), hasSize(expected.getTotalRecords()));
  }

  @Test
  public void testGetAcqUnitsMembershipsWithQuery() {
    logger.info("=== Test GET acquisitions units memberships - search by query ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "?query=userId==480dba68-ee84-4b9c-a374-7e824fc49227";

    final AcquisitionsUnitMembershipCollection units = verifySuccessGet(url, AcquisitionsUnitMembershipCollection.class);
    assertThat(units.getAcquisitionsUnitMemberships(), hasSize(2));
  }

  @Test
  public void testGetAcqUnitsMembershipsWithUnprocessableQuery() {
    logger.info("=== Test GET acquisitions units memberships - unprocessable query ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "?query=" + BAD_QUERY;

    verifyGet(url, APPLICATION_JSON, 400);
  }

  @Test
  public void testGetAcqUnitsMembershipSuccess() {
    logger.info("=== Test GET acquisitions units membership - success case ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "/567a6d67-460f-43ab-af28-8c730c9aa4da";

    final AcquisitionsUnitMembership unit = verifySuccessGet(url, AcquisitionsUnitMembership.class);
    assertThat(unit, notNullValue());
  }

  @Test
  public void testGetAcqUnitsMembershipNotFound() {
    logger.info("=== Test GET acquisitions units membership - not found ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyGet(url, APPLICATION_JSON, 404);
  }

  @Test
  public void testPutAcqUnitsMembershipSuccess() {
    logger.info("=== Test PUT acquisitions units membership - success case ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "/0e9525aa-d123-4e4d-9f7e-1b302a97eb90";

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnitMembership().withUserId(UUID.randomUUID().toString()).withAcquisitionsUnitId(UUID.randomUUID().toString())), "", 204);
  }

  @Test
  public void testValidationOnPutUnitsMembershipWithoutBody() {
    logger.info("=== Test validation on PUT acquisitions units membership with no body ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "/" + UUID.randomUUID().toString();

    verifyPut(url, "", TEXT_PLAIN, 400);
  }

  @Test
  public void testPutUnitsMembershipNotFound() {
    logger.info("=== Test PUT acquisitions units membership - not found ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnitMembership().withUserId(UUID.randomUUID().toString()).withAcquisitionsUnitId(UUID.randomUUID().toString())), APPLICATION_JSON, 404);
  }

  @Test
  public void testPutUnitsMembershipIdMismatch() {
    logger.info("=== Test PUT acquisitions units membership - different ids in path and body ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "/" + UUID.randomUUID().toString();

    AcquisitionsUnitMembership unit = new AcquisitionsUnitMembership().withUserId(UUID.randomUUID().toString()).withAcquisitionsUnitId(UUID.randomUUID().toString()).withId(UUID.randomUUID().toString());
    Errors errors = verifyPut(url, JsonObject.mapFrom(unit), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
  }

  @Test
  public void testDeleteAcqUnitsMembershipSuccess() {
    logger.info("=== Test DELETE acquisitions units membership - success case ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "/0e9525aa-d123-4e4d-9f7e-1b302a97eb90";

    verifyDeleteResponse(url, "", 204);
  }

  @Test
  public void testDeletePutUnitsMembershipNotFound() {
    logger.info("=== Test DELETE acquisitions units membership - not found ===");
    String url = ACQ_UNITS_MEMBERSHIPS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnitMembership().withUserId(UUID.randomUUID().toString()).withAcquisitionsUnitId(UUID.randomUUID().toString())), APPLICATION_JSON, 404);
  }

  @Test
  public void testPostAcqUnitsMembershipSuccess() {
    logger.info("=== Test POST acquisitions unit - success case ===");

    String body = JsonObject.mapFrom(new AcquisitionsUnitMembership().withUserId(UUID.randomUUID().toString()).withAcquisitionsUnitId(UUID.randomUUID().toString())).encode();

    Response response = verifyPostResponse(ACQ_UNITS_MEMBERSHIPS_ENDPOINT, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10),
        APPLICATION_JSON, 201);
    AcquisitionsUnitMembership unit = response.as(AcquisitionsUnitMembership.class);

    assertThat(unit.getId(), not(isEmptyOrNullString()));
    assertThat(response.header(HttpHeaders.LOCATION), containsString(unit.getId()));
  }

  @Test
  public void testPostAcqUnitsMembershipServerError() {
    logger.info("=== Test POST acquisitions unit - Server Error ===");

    String body = JsonObject.mapFrom(new AcquisitionsUnitMembership().withUserId(UUID.randomUUID().toString()).withAcquisitionsUnitId(UUID.randomUUID().toString())).encode();
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, new Header(X_ECHO_STATUS, String.valueOf(500)));
    verifyPostResponse(ACQ_UNITS_MEMBERSHIPS_ENDPOINT, body, headers, APPLICATION_JSON, 500);
  }
}
