package org.folio.rest.impl;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitAssignment;
import org.folio.rest.jaxrs.model.AcquisitionsUnitAssignmentCollection;
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
import static org.folio.rest.impl.MockServer.ACQUISITIONS_UNIT_ASSIGNMENTS_COLLECTION;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

public class AcquisitionsUnitAssignmentsTests extends ApiTestBase {
  private static final Logger logger = LoggerFactory.getLogger(AcquisitionsUnitAssignmentsTests.class);

  private static final String ACQ_UNIT_ASSIGNMENTS_ENDPOINT = "/orders/acquisitions-unit-assignments";

  @Test
  public void testGetAcqUnitAssignmentsNoQuery() throws IOException {
    logger.info("=== Test Get Acquisitions Unit Assignments - With empty query ===");
    AcquisitionsUnitAssignmentCollection expected = new JsonObject(ApiTestBase.getMockData(ACQUISITIONS_UNIT_ASSIGNMENTS_COLLECTION)).mapTo(AcquisitionsUnitAssignmentCollection.class);
    final AcquisitionsUnitAssignmentCollection assignments = verifySuccessGet(ACQ_UNIT_ASSIGNMENTS_ENDPOINT, AcquisitionsUnitAssignmentCollection.class);
    assertThat(expected.getAcquisitionsUnitAssignments(), hasSize(expected.getTotalRecords()));
    assertThat(assignments.getAcquisitionsUnitAssignments(), hasSize(expected.getTotalRecords()));
  }

  @Test
  public void testGetAcqUnitAssignmentsWithQuery() {
    logger.info("=== Test GET Acquisitions Unit Assignments - search by query ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "?query=recordId==57257531-c6bc-4b6e-b91a-06bc0034b768";

    final AcquisitionsUnitAssignmentCollection units = verifySuccessGet(url, AcquisitionsUnitAssignmentCollection.class);
    assertThat(units.getAcquisitionsUnitAssignments(), hasSize(1));
  }

  @Test
  public void testGetAcqUnitAssignmentsWithUnprocessableQuery() {
    logger.info("=== Test GET Acquisitions Unit Assignments - unprocessable query ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "?query=" + BAD_QUERY;

    verifyGet(url, APPLICATION_JSON, 400);
  }

  @Test
  public void testGetAcqUnitAssignmentsSuccess() {
    logger.info("=== Test GET Acquisitions Unit Assignment - success case ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "/8ad580ee-d3f1-4271-bff4-c7c80ccb22e5";

    final AcquisitionsUnitAssignment unit = verifySuccessGet(url, AcquisitionsUnitAssignment.class);
    assertThat(unit, notNullValue());
  }

  @Test
  public void testGetAcqUnitAssignmentNotFound() {
    logger.info("=== Test GET Acquisitions Unit Assignment - not found ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyGet(url, APPLICATION_JSON, 404);
  }

  @Test
  public void testPutAcqUnitAssignmentSuccess() {
    logger.info("=== Test PUT Acquisitions Unit Assignment - success case ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "/8ad510ee-d3f1-4271-bff4-c7c80ccb22e5";

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnitAssignment()
      .withRecordId("47Ac60b4-159D-4e1c-9aCB-8293Df67D16d")
      .withAcquisitionsUnitId("c2d6608f-6d1f-45f7-8817-5d32c2416116")), "", 204);
  }

  @Test
  public void testValidationOnPutUnitAssignmentWithoutBody() {
    logger.info("=== Test validation on PUT Acquisitions Unit Assignment with no body ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "/" + UUID.randomUUID().toString();

    verifyPut(url, "", TEXT_PLAIN, 400);
  }

  @Test
  public void testPutUnitAssignmentNotFound() {
    logger.info("=== Test PUT Acquisitions Unit Assignment - not found ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnitAssignment()
      .withRecordId("57257531-c6bc-4b6e-b91a-06bc0034b768")
      .withAcquisitionsUnitId("8ad510ee-d3f1-4271-bff4-c7c80ccb22e5")), APPLICATION_JSON, 404);
  }

  @Test
  public void testPutUnitAssignmentIdMismatch() {
    logger.info("=== Test PUT Acquisitions Unit Assignment - different ids in path and body ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "/" + UUID.randomUUID().toString();

    AcquisitionsUnitAssignment unitAssignment = new AcquisitionsUnitAssignment().withRecordId("57257531-c6bc-4b6e-b91a-06bc0034b768")
      .withAcquisitionsUnitId("8ad510ee-d3f1-4271-bff4-c7c80ccb22e5").withId(UUID.randomUUID().toString());
    Errors errors = verifyPut(url, JsonObject.mapFrom(unitAssignment), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
  }

  @Test
  public void testDeleteAcqUnitAssignmentSuccess() {
    logger.info("=== Test DELETE Acquisitions Unit Assignment - success case ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "/8ad510ee-d3f1-4271-bff4-c7c80ccb22e5";

    verifyDeleteResponse(url, "", 204);
  }

  @Test
  public void testDeletePutUnitAssignmentNotFound() {
    logger.info("=== Test DELETE Acquisitions Unit Assignment - not found ===");
    String url = ACQ_UNIT_ASSIGNMENTS_ENDPOINT + "/" + ID_DOES_NOT_EXIST;

    verifyPut(url, JsonObject.mapFrom(new AcquisitionsUnitAssignment().withRecordId("57257531-c6bc-4b6e-b91a-06bc0034b768")
      .withAcquisitionsUnitId("c2d6608f-6d1f-45f7-8817-5d32c2416116")), APPLICATION_JSON, 404);
  }

  @Test
  public void testPostAcqUnitAssignmentSuccess() {
    logger.info("=== Test POST Acquisitions Unit Assignment - success case ===");

    String body = JsonObject.mapFrom(new AcquisitionsUnitAssignment().withRecordId("c4d6608f-6d1f-45f7-8717-5d32c2416116")
      .withAcquisitionsUnitId("6b982ffe-8efd-4690-8168-0c773b49cde1")).encode();
    Response response = verifyPostResponse(ACQ_UNIT_ASSIGNMENTS_ENDPOINT, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10),
        APPLICATION_JSON, 201);
    AcquisitionsUnitAssignment unitAssignment = response.as(AcquisitionsUnitAssignment.class);

    assertThat(unitAssignment.getId(), not(isEmptyOrNullString()));
    assertThat(response.header(HttpHeaders.LOCATION), containsString(unitAssignment.getId()));
  }

  @Test
  public void testPostAcqUnitAssignmentServerError() {
    logger.info("=== Test POST Acquisitions Unit Assignment - Server Error ===");

    String body = JsonObject.mapFrom(new AcquisitionsUnitAssignment().withRecordId("57257531-c6bc-4b6e-b91a-06bc0034b768")
      .withAcquisitionsUnitId("c2d6608f-6d1f-45f7-8817-5d32c2416116")).encode();
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, new Header(X_ECHO_STATUS, String.valueOf(500)));
    verifyPostResponse(ACQ_UNIT_ASSIGNMENTS_ENDPOINT, body, headers, APPLICATION_JSON, 500);
  }
}
