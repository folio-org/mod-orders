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
import static org.folio.rest.impl.MockServer.ACQUISITION_METHODS_COLLECTION;
import static org.folio.rest.impl.MockServer.getRqRsEntries;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.emptyOrNullString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import javax.ws.rs.core.HttpHeaders;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.orders.utils.ResourcePathResolver;
import org.folio.rest.jaxrs.model.AcquisitionMethod;
import org.folio.rest.jaxrs.model.AcquisitionMethodCollection;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;

public class AcquisitionMethodAPITest {

  private static final Logger logger = LogManager.getLogger();

  public static final String ACQUISITION_METHODS = "/orders/acquisition-methods";
  static final String ACQUISITION_METHOD_UUID = "e69a29f8-f4b2-472e-8b6b-bfca1679dd38";
  static final String SYSTEM_SOURCE = "System";
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
  void testGetAcqMethodsNoQuery() throws IOException {
    logger.info("=== Test GET acquisition methods - with empty query ===");
    AcquisitionMethodCollection expected = new JsonObject(getMockData(ACQUISITION_METHODS_COLLECTION))
      .mapTo(AcquisitionMethodCollection.class);
    final AcquisitionMethodCollection acquisitionMethods = verifySuccessGet(ACQUISITION_METHODS, AcquisitionMethodCollection.class);
    assertThat(expected.getAcquisitionMethods(), hasSize(expected.getTotalRecords()));
    assertThat(acquisitionMethods.getAcquisitionMethods(), hasSize(expected.getTotalRecords()));
  }

  @Test
  void testGetAcqMethodsWithQuery() {
    logger.info("=== Test GET acquisitions methods - search by query ===");
    String url = ACQUISITION_METHODS + "?query=source==" + SYSTEM_SOURCE;

    final AcquisitionMethodCollection methods = verifySuccessGet(url, AcquisitionMethodCollection.class);
    assertThat(methods.getAcquisitionMethods(), hasSize(3));
  }

  @Test
  void testGetAcqMethodsWithUnprocessableQuery() {
    logger.info("=== Test GET acquisitions methods - unprocessable query ===");
    String url = ACQUISITION_METHODS + "?query=" + BAD_QUERY;

    verifyGet(url, APPLICATION_JSON, 400);
  }

  @Test
  void testGetAcqMethodSuccess() {
    logger.info("=== Test GET acquisitions method - success case ===");
    String url = ACQUISITION_METHODS + "/" + ACQUISITION_METHOD_UUID;

    final AcquisitionMethod method = verifySuccessGet(url, AcquisitionMethod.class);
    assertThat(method, notNullValue());
  }

  @Test
  void testGetAcqMethodNotFound() {
    logger.info("=== Test GET acquisitions method - not found ===");
    String url = ACQUISITION_METHODS + "/" + ID_DOES_NOT_EXIST;

    verifyGet(url, APPLICATION_JSON, 404);
  }

  @Test
  void testPutAcqMethodSuccess() {
    logger.info("=== Test PUT acquisitions method - success case ===");
    String url = ACQUISITION_METHODS + "/bc6e6baf-673f-4c1c-8a98-cda988e5dccb";

    verifyPut(url, JsonObject.mapFrom(new AcquisitionMethod().withValue("New value")), "", 204);
  }

  @Test
  void testValidationOnPutAcqMethodWithoutBody() {
    logger.info("=== Test validation on PUT acquisitions method with no body ===");
    String url = ACQUISITION_METHODS + "/" + UUID.randomUUID();

    verifyPut(url, "", TEXT_PLAIN, 400);
  }

  @Test
  void testPutAcqMethodNotFound() {
    logger.info("=== Test PUT acquisitions method - not found ===");
    String url = ACQUISITION_METHODS + "/" + ID_DOES_NOT_EXIST;

    verifyPut(url, JsonObject.mapFrom(new AcquisitionMethod().withValue("New value")), APPLICATION_JSON, 404);
  }

  @Test
  void testDeleteAcqMethodForbidden() {
    logger.info("=== Test DELETE acquisitions method - forbidden case ===");
    String url = ACQUISITION_METHODS + "/bc6e6baf-673f-4c1c-8a98-cda988e5dccb";

    verifyDeleteResponse(url, "", 400);
  }

  @Test
  void testDeleteAcqMethodSuccess() {
    logger.info("=== Test DELETE acquisitions method - success case ===");
    String url = ACQUISITION_METHODS + "/15c5e2f6-7a1d-4938-a929-d8b321252ee2";

    verifyDeleteResponse(url, "", 204);
  }

  @Test
  void testDeleteAcqMethodNotFound() {
    logger.info("=== Test DELETE acquisitions method - not found ===");
    String url = ACQUISITION_METHODS + "/" + ID_DOES_NOT_EXIST;

    verifyDeleteResponse(url, APPLICATION_JSON, 404);
    assertThat(getRqRsEntries(HttpMethod.PUT, ResourcePathResolver.ACQUISITION_METHODS), empty());
  }

  @Test
  void testPostAcqMethodSuccess() {
    logger.info("=== Test POST acquisitions method - success case ===");

    String body = JsonObject.mapFrom(new AcquisitionMethod().withValue("Some value")
      .withSource(AcquisitionMethod.Source.USER))
      .encode();
    Response response = verifyPostResponse(ACQUISITION_METHODS, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10),
        APPLICATION_JSON, 201);
    AcquisitionMethod method = response.as(AcquisitionMethod.class);

    assertThat(method.getId(), not(is(emptyOrNullString())));
    assertThat(response.header(HttpHeaders.LOCATION), containsString(method.getId()));
  }

  @Test
  void testPostAcqMethodServerError() {
    logger.info("=== Test POST acquisitions method - Server Error ===");

    String body = JsonObject.mapFrom(new AcquisitionMethod().withValue("Some value")
      .withSource(AcquisitionMethod.Source.USER))
      .encode();
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, new Header(X_ECHO_STATUS, String.valueOf(500)));
    verifyPostResponse(ACQUISITION_METHODS, body, headers, APPLICATION_JSON, 500);
  }

}
