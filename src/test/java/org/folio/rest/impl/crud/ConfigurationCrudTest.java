package org.folio.rest.impl.crud;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXISTED_ID;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ID;
import static org.folio.TestConstants.ID_BAD_FORMAT;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.X_ECHO_STATUS;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import io.restassured.http.Header;
import io.restassured.response.Response;
import io.vertx.core.json.JsonObject;

public class ConfigurationCrudTest {

  private final Logger logger = LogManager.getLogger();
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

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testPostCrud(CrudTestEntities entity) {
    logger.info(String.format("=== Test POST : %s ===", entity.name()));
    JsonObject json = entity.getTestSample();
    assertThat(json.remove(ID), notNullValue());
    Response response = verifyPostResponse(entity.getEndpoint(), json.encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
    assertThat(new JsonObject(response.getBody()
      .print()).getString(ID), notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testPostCrudBadRequest(CrudTestEntities entity) {
    logger.info(String.format("=== Test POST : %s (bad request) ===", entity.name()));
    JsonObject json = entity.getTestSample();
    int status400 = HttpStatus.HTTP_BAD_REQUEST.toInt();
    verifyPostResponse(entity.getEndpoint(), JsonObject.mapFrom(json)
      .encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, new Header(X_ECHO_STATUS, String.valueOf(status400))),
        APPLICATION_JSON, status400);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testPostCrudInternalServerError(CrudTestEntities entity) {
    logger.info(String.format("=== Test POST : %s (internal server error) ===", entity.name()));
    JsonObject json = entity.getTestSample();
    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(entity.getEndpoint(), JsonObject.mapFrom(json)
      .encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, new Header(X_ECHO_STATUS, String.valueOf(status500))),
        APPLICATION_JSON, status500);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testGetCollectionCrud(CrudTestEntities entity) {
    logger.info(String.format("=== Test GET : %s ===", entity.name()));
    verifyGet(entity.getEndpoint(), APPLICATION_JSON, 200).then()
      .body("totalRecords", notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testGetCollectionCrudInternalServerError(CrudTestEntities entity) {
    logger.info(String.format("=== Test GET : %s ===", entity.name()));
    verifyGet(entity.getEndpoint() + "?query=id==" + ID_FOR_INTERNAL_SERVER_ERROR, APPLICATION_JSON, 500).then()
      .body("errors", notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testGetByIdCrud(CrudTestEntities entity) {
    logger.info(String.format("=== Test GET by id : %s ===", entity.name()));
    verifyGet(entity.getEndpoint() + "/" + EXISTED_ID, APPLICATION_JSON, 200).then()
      .body(ID, notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testGetByIdCrudBadRequest(CrudTestEntities entity) {
    logger.info(String.format("=== Test GET by id : %s (bad request) ===", entity.name()));
    verifyGet(entity.getEndpoint() + "/" + ID_BAD_FORMAT, TEXT_PLAIN, 400);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testGetByIdCrudNotFound(CrudTestEntities entity) {
    logger.info(String.format("=== Test GET by id : %s (not found) ===", entity.name()));
    verifyGet(entity.getEndpoint() + "/" + ID_DOES_NOT_EXIST, APPLICATION_JSON, 404).then()
      .body("errors", notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testGetByIdCrudInternalServerError(CrudTestEntities entity) {
    logger.info(String.format("=== Test GET by id : %s (internal server error) ===", entity.name()));
    verifyGet(entity.getEndpoint() + "/" + ID_FOR_INTERNAL_SERVER_ERROR, APPLICATION_JSON, 500).then()
      .body("errors", notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testPutCrudTest(CrudTestEntities entity) {
    logger.info(String.format("=== Test PUT : %s ===", entity.name()));
    JsonObject json = entity.getTestSample();
    json.put(ID, EXISTED_ID);
    verifyPut(entity.getEndpoint() + "/" + EXISTED_ID, json.encode(), "", 204);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testPutCrudIdMismatchTest(CrudTestEntities entity) {
    logger.info(String.format("=== Test PUT (id mismatch) : %s ===", entity.name()));
    JsonObject json = entity.getTestSample();
    json.put(ID, UUID.randomUUID().toString());
    verifyPut(entity.getEndpoint() + "/" + EXISTED_ID, json.encode(), "", 422).then()
      .body("errors", notNullValue());
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testPutCrudObjectWithoutIdTest(CrudTestEntities entity) {
    logger.info(String.format("=== Test PUT (object without id) : %s ===", entity.name()));
    JsonObject json = entity.getTestSample();
    json.remove(ID);
    verifyPut(entity.getEndpoint() + "/" + EXISTED_ID, json.encode(), "", 204);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testPutCrudNotFound(CrudTestEntities entity) {
    logger.info(String.format("=== Test PUT : %s (not found) ===", entity.name()));
    JsonObject json = entity.getTestSample();
    json.put(ID, ID_DOES_NOT_EXIST);
    verifyPut(entity.getEndpoint() + "/" + ID_DOES_NOT_EXIST, json.encode(), APPLICATION_JSON, 404);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testPutCrudInternalServerError(CrudTestEntities entity) {
    logger.info(String.format("=== Test PUT : %s (internal server error) ===", entity.name()));
    JsonObject json = entity.getTestSample();
    json.put(ID, ID_FOR_INTERNAL_SERVER_ERROR);
    verifyPut(entity.getEndpoint() + "/" + ID_FOR_INTERNAL_SERVER_ERROR, json.encode(), APPLICATION_JSON, 500);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testDeleteCrud(CrudTestEntities entity) {
    logger.info(String.format("=== Test DELETE : %s ===", entity.name()));
    verifyDeleteResponse(entity.getEndpoint() + "/" + UUID.randomUUID()
      .toString(), "", 204);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testDeleteCrudBadId(CrudTestEntities entity) {
    logger.info(String.format("=== Test DELETE : %s (bad id) ===", entity.name()));
    verifyDeleteResponse(entity.getEndpoint() + "/" + ID_BAD_FORMAT, TEXT_PLAIN, 400);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testDeleteCrudNotFound(CrudTestEntities entity) {
    logger.info(String.format("=== Test DELETE : %s (not found) ===", entity.name()));
    verifyDeleteResponse(entity.getEndpoint() + "/" + ID_DOES_NOT_EXIST, APPLICATION_JSON, 404);
  }

  @ParameterizedTest
  @EnumSource(value = CrudTestEntities.class)
  void testDeleteCrudInternalServerError(CrudTestEntities entity) {
    logger.info(String.format("=== Test DELETE : %s (internal server error) ===", entity.name()));
    verifyDeleteResponse(entity.getEndpoint() + "/" + ID_FOR_INTERNAL_SERVER_ERROR, APPLICATION_JSON, 500);
  }

}
