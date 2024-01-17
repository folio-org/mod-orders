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
import static org.folio.TestConstants.ID_FOR_TEMPLATE_NAME_ALREADY_EXISTS;
import static org.folio.TestConstants.X_ECHO_STATUS;
import static org.folio.TestUtils.getMockData;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TEMPLATES;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.core.exceptions.ErrorCodes.TEMPLATE_NAME_ALREADY_EXISTS;
import static org.folio.rest.impl.MockServer.ORDER_TEMPLATES_COLLECTION;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.folio.rest.impl.MockServer.getRqRsEntries;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.emptyOrNullString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import javax.ws.rs.core.HttpHeaders;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.OrderTemplate;
import org.folio.rest.jaxrs.model.OrderTemplateCollection;
import org.hamcrest.collection.IsCollectionWithSize;
import org.hamcrest.core.IsEqual;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;

public class OrderTemplateTest {

  private static final Logger logger = LogManager.getLogger();

  private static final String ORDER_TEMPLATES_ENDPOINT = "/orders/order-templates";
  private static final String ORDER_TEMPLATE_ID = "0e9525aa-d123-4e4d-9f7e-1b302a97eb90";
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

  // Positive cases

  @Test
  void testPostOrderTemplateSuccess() {
    logger.info("=== Test POST Order Template - success case ===");
    OrderTemplate entity = new OrderTemplate()
      .withId(UUID.randomUUID().toString())
      .withTemplateName("Testing order template");
    String body = JsonObject.mapFrom(entity).encode();
    Response response = verifyPostResponse(ORDER_TEMPLATES_ENDPOINT, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10),
      APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
    final OrderTemplate template = response.as(OrderTemplate.class);
    assertThat(template.getId(), not(is(emptyOrNullString())));
    assertThat(response.header(HttpHeaders.LOCATION), containsString(template.getId()));
  }

  @Test
  void testPostOrderTemplateAlreadyExists() {
    logger.info("=== Test POST Order Template - failed case ===");
    OrderTemplate entity = new OrderTemplate()
      .withId(ID_FOR_TEMPLATE_NAME_ALREADY_EXISTS)
      .withTemplateName("Testing order template");
    String body = JsonObject.mapFrom(entity).encode();

    Errors errors = verifyPostResponse(ORDER_TEMPLATES_ENDPOINT, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10),
      APPLICATION_JSON, HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), IsCollectionWithSize.hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), IsEqual.equalTo(TEMPLATE_NAME_ALREADY_EXISTS.getCode()));
    assertThat(errors.getErrors().get(0).getMessage(), IsEqual.equalTo(TEMPLATE_NAME_ALREADY_EXISTS.getDescription()));
  }

  @Test
  void testGetOrderTemplateSuccess() {
    logger.info("=== Test GET Order Template - success case ===");
    String url = String.format("%s/%s", ORDER_TEMPLATES_ENDPOINT, ORDER_TEMPLATE_ID);
    final OrderTemplate template = verifySuccessGet(url, OrderTemplate.class);
    assertThat(template, notNullValue());
    assertThat(template.getId(), notNullValue());
  }

  @Test
  void testPutOrderTemplateSuccess() {
    logger.info("=== Test PUT Order Template - success case ===");
    String url = String.format("%s/%s", ORDER_TEMPLATES_ENDPOINT, ORDER_TEMPLATE_ID);
    verifyPut(url, JsonObject.mapFrom(new OrderTemplate().withTemplateName("Some name")), "", 204);
  }

  @Test
  void testDeleteOrderTemplateSuccess() {
    logger.info("=== Test DELETE Order Template - success case ===");
    String url = String.format("%s/%s", ORDER_TEMPLATES_ENDPOINT, ORDER_TEMPLATE_ID);
    verifyDeleteResponse(url, "", 204);
  }

  @Test
  void testGetOrderTemplatesNoQuery() throws IOException {
    logger.info("=== Test Get Order Templates - With empty query ===");
    OrderTemplateCollection expected = new JsonObject(getMockData(ORDER_TEMPLATES_COLLECTION)).mapTo(OrderTemplateCollection.class);
    final OrderTemplateCollection templates = verifySuccessGet(ORDER_TEMPLATES_ENDPOINT, OrderTemplateCollection.class);
    assertThat(templates.getOrderTemplates(), hasSize(expected.getTotalRecords()));
    assertThat(templates.getTotalRecords(), is(expected.getOrderTemplates().size()));
  }

  @Test
  void testGetOrderTemplatesWithQuery() {
    logger.info("=== Test GET Order Templates - search by query ===");
    String cql = "templateCode==Amazon-B";
    String url = String.format("%s?query=%s", ORDER_TEMPLATES_ENDPOINT, cql);
    final OrderTemplateCollection templates = verifySuccessGet(url, OrderTemplateCollection.class);
    assertThat(templates.getOrderTemplates(), hasSize(1));
    List<String> queryParams = getQueryParams(ORDER_TEMPLATES);
    assertThat(queryParams, hasSize(1));
    assertThat(queryParams.get(0), equalTo(cql));
  }

  // Negative cases

  @Test
  void testGetOrderTemplatesWithUnprocessableQuery() {
    logger.info("=== Test GET Order Templates - unprocessable query ===");
    String url = String.format("%s?query=%s", ORDER_TEMPLATES_ENDPOINT, BAD_QUERY);
    verifyGet(url, APPLICATION_JSON, HttpStatus.HTTP_BAD_REQUEST.toInt());
  }

  @Test
  void testGetOrderTemplateNotFound() {
    logger.info("=== Test GET Order Template - not found ===");
    String url = String.format("%s/%s", ORDER_TEMPLATES_ENDPOINT, ID_DOES_NOT_EXIST);
    verifyGet(url, APPLICATION_JSON, HttpStatus.HTTP_NOT_FOUND.toInt());
  }

  @Test
  void testValidationOnPutUnitWithoutBody() {
    logger.info("=== Test validation on PUT Order Template with no body ===");
    String url = String.format("%s/%s", ORDER_TEMPLATES_ENDPOINT, UUID.randomUUID().toString());
    verifyPut(url, "", TEXT_PLAIN, HttpStatus.HTTP_BAD_REQUEST.toInt());
  }

  @Test
  void testPutOrderTemplateNotFound() {
    logger.info("=== Test PUT Order Template - not found ===");
    String url = String.format("%s/%s", ORDER_TEMPLATES_ENDPOINT, ID_DOES_NOT_EXIST);
    verifyPut(url, JsonObject.mapFrom(new OrderTemplate().withTemplateName("Some template name")), APPLICATION_JSON, HttpStatus.HTTP_NOT_FOUND.toInt());
  }

  @Test
  void testPutOrderTemplateIdMismatch() {
    logger.info("=== Test PUT Order Template - different ids in path and body ===");
    String url = String.format("%s/%s", ORDER_TEMPLATES_ENDPOINT, UUID.randomUUID().toString());
    OrderTemplate template = new OrderTemplate().withTemplateName("Some name").withId(UUID.randomUUID().toString());
    Errors errors = verifyPut(url, JsonObject.mapFrom(template), APPLICATION_JSON, HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt()).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
  }

  @Test
  void testDeleteOrderTemplateNotFound() {
    logger.info("=== Test DELETE Order Template - not found ===");
    String url = String.format("%s/%s", ORDER_TEMPLATES_ENDPOINT, ID_DOES_NOT_EXIST);
    verifyDeleteResponse(url, APPLICATION_JSON, HttpStatus.HTTP_NOT_FOUND.toInt());
    assertThat(getRqRsEntries(HttpMethod.PUT, ORDER_TEMPLATES), empty());
  }

  @Test
  void testPostOrderTemplateServerError() {
    logger.info("=== Test POST Order Template - Server Error ===");
    String body = JsonObject.mapFrom(new OrderTemplate().withTemplateName("Some name")).encode();
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, new Header(X_ECHO_STATUS, String.valueOf(HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt())));
    verifyPostResponse(ORDER_TEMPLATES_ENDPOINT, body, headers, APPLICATION_JSON, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt());
  }
}
