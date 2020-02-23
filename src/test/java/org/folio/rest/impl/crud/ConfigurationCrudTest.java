package org.folio.rest.impl.crud;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;

import com.google.common.collect.Lists;
import io.restassured.http.Header;
import io.restassured.response.Response;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.junit.VertxUnitRunnerWithParametersFactory;
import org.folio.HttpStatus;
import org.folio.rest.impl.ApiTestBase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.UUID;

@RunWith(Parameterized.class)
@Parameterized.UseParametersRunnerFactory(VertxUnitRunnerWithParametersFactory.class)
public class ConfigurationCrudTest extends ApiTestBase {

  private final Logger logger = LoggerFactory.getLogger(ConfigurationCrudTest.class);

  @Parameterized.Parameters
  public static Iterable<CrudTestEntities> entities() {
    return Lists.newArrayList(CrudTestEntities.values());
  }

  private final CrudTestEntities entity;

  public ConfigurationCrudTest(CrudTestEntities entity) {
    this.entity = entity;
  }

  @Test
  public void testPostCrud() {
    logger.info(String.format("=== Test POST : %s ===", entity.name()));
    JsonObject json = entity.getTestSample();
    assertThat(json.remove(ID), notNullValue());
    Response response = verifyPostResponse(entity.getEndpoint(), json.encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
    assertThat(new JsonObject(response.getBody()
      .print()).getString(ID), notNullValue());
  }

  @Test
  public void testPostCrudBadRequest() {
    logger.info(String.format("=== Test POST : %s (bad request) ===", entity.name()));
    JsonObject json = entity.getTestSample();
    int status400 = HttpStatus.HTTP_BAD_REQUEST.toInt();
    verifyPostResponse(entity.getEndpoint(), JsonObject.mapFrom(json)
      .encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, new Header(X_ECHO_STATUS, String.valueOf(status400))),
        APPLICATION_JSON, status400);
  }

  @Test
  public void testPostCrudInternalServerError() {
    logger.info(String.format("=== Test POST : %s (internal server error) ===", entity.name()));
    JsonObject json = entity.getTestSample();
    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(entity.getEndpoint(), JsonObject.mapFrom(json)
      .encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, new Header(X_ECHO_STATUS, String.valueOf(status500))),
        APPLICATION_JSON, status500);
  }

  @Test
  public void testGetCollectionCrud() {
    logger.info(String.format("=== Test GET : %s ===", entity.name()));
    verifyGet(entity.getEndpoint(), APPLICATION_JSON, 200).then()
      .log()
      .all()
      .body("totalRecords", notNullValue());
  }

  @Test
  public void testGetCollectionCrudInternalServerError() {
    logger.info(String.format("=== Test GET : %s ===", entity.name()));
    verifyGet(entity.getEndpoint() + "?query=id==" + ID_FOR_INTERNAL_SERVER_ERROR, APPLICATION_JSON, 500).then()
      .log()
      .all()
      .body("errors", notNullValue());
  }

  @Test
  public void testGetByIdCrud() {
    logger.info(String.format("=== Test GET by id : %s ===", entity.name()));
    verifyGet(entity.getEndpoint() + "/" + EXISTED_ID, APPLICATION_JSON, 200).then()
      .log()
      .all()
      .body(ID, notNullValue());
  }

  @Test
  public void testGetByIdCrudBadRequest() {
    logger.info(String.format("=== Test GET by id : %s (bad request) ===", entity.name()));
    verifyGet(entity.getEndpoint() + "/" + ID_BAD_FORMAT, TEXT_PLAIN, 400);
  }

  @Test
  public void testGetByIdCrudNotFound() {
    logger.info(String.format("=== Test GET by id : %s (not found) ===", entity.name()));
    verifyGet(entity.getEndpoint() + "/" + ID_DOES_NOT_EXIST, APPLICATION_JSON, 404).then()
      .log()
      .all()
      .body("errors", notNullValue());
  }

  @Test
  public void testGetByIdCrudInternalServerError() {
    logger.info(String.format("=== Test GET by id : %s (internal server error) ===", entity.name()));
    verifyGet(entity.getEndpoint() + "/" + ID_FOR_INTERNAL_SERVER_ERROR, APPLICATION_JSON, 500).then()
      .log()
      .all()
      .body("errors", notNullValue());
  }

  @Test
  public void testPutCrudTest() {
    logger.info(String.format("=== Test PUT : %s ===", entity.name()));
    JsonObject json = entity.getTestSample();
    json.put(ID, EXISTED_ID);
    verifyPut(entity.getEndpoint() + "/" + EXISTED_ID, json.encode(), "", 204);
  }

  @Test
  public void testPutCrudNotFound() {
    logger.info(String.format("=== Test PUT : %s (not found) ===", entity.name()));
    JsonObject json = entity.getTestSample();
    json.put(ID, ID_DOES_NOT_EXIST);
    verifyPut(entity.getEndpoint() + "/" + ID_DOES_NOT_EXIST, json.encode(), APPLICATION_JSON, 404);
  }

  @Test
  public void testPutCrudInternalServerError() {
    logger.info(String.format("=== Test PUT : %s (internal server error) ===", entity.name()));
    JsonObject json = entity.getTestSample();
    json.put(ID, ID_FOR_INTERNAL_SERVER_ERROR);
    verifyPut(entity.getEndpoint() + "/" + ID_FOR_INTERNAL_SERVER_ERROR, json.encode(), APPLICATION_JSON, 500);
  }

  @Test
  public void testDeleteCrud() {
    logger.info(String.format("=== Test DELETE : %s ===", entity.name()));
    verifyDeleteResponse(entity.getEndpoint() + "/" + UUID.randomUUID()
      .toString(), "", 204);
  }

  @Test
  public void testDeleteCrudBadId() {
    logger.info(String.format("=== Test DELETE : %s (bad id) ===", entity.name()));
    verifyDeleteResponse(entity.getEndpoint() + "/" + ID_BAD_FORMAT, TEXT_PLAIN, 400);
  }

  @Test
  public void testDeleteCrudNotFound() {
    logger.info(String.format("=== Test DELETE : %s (not found) ===", entity.name()));
    verifyDeleteResponse(entity.getEndpoint() + "/" + ID_DOES_NOT_EXIST, APPLICATION_JSON, 404);
  }

  @Test
  public void testDeleteCrudInternalServerError() {
    logger.info(String.format("=== Test DELETE : %s (internal server error) ===", entity.name()));
    verifyDeleteResponse(entity.getEndpoint() + "/" + ID_FOR_INTERNAL_SERVER_ERROR, APPLICATION_JSON, 500);
  }

}
