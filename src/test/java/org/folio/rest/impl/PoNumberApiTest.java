package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifySuccessGet;
import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.rest.impl.MockServer.PO_NUMBER_ERROR_X_OKAPI_TENANT;
import static org.folio.rest.impl.MockServer.PO_NUMBER_VALUE;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.PoNumber;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import io.vertx.core.json.JsonObject;

public class PoNumberApiTest {

  private static final Logger logger = LogManager.getLogger();

  private static final String PONUMBER_VALIDATE_PATH = "/orders/po-number/validate";
  private static final String GET_PO_NUMBER_PATH = "/orders/po-number";

  static final String NONEXISTING_PO_NUMBER = "newPoNumber";
  static final String EXISTING_PO_NUMBER = "oldPoNumber";

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
  @Disabled
  void testPoNumberValidateWithExistingPONumber() {
    JsonObject poNumber = new JsonObject();
    poNumber.put(PO_NUMBER, EXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 400);
  }

  @Test
  void testPoNumberValidateWithUniquePONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put(PO_NUMBER, NONEXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), "", 204);
  }

  @Test
  void testPoNumberValidateWithInvalidPattern() {
    JsonObject poNumber=new JsonObject();
    poNumber.put(PO_NUMBER, "12345678901234567890123"); // 23 characters - exceeds limit of 22
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422);
  }

  @Test
  void testGetPoNumber() {
    logger.info("=== Test Get PO Number (generate poNumber) ===");

    final PoNumber poNumber = verifySuccessGet(GET_PO_NUMBER_PATH, PoNumber.class);

    String actualPoNumberValue = poNumber.getPoNumber();
    assertEquals(PO_NUMBER_VALUE, actualPoNumberValue);
  }

  @Test
  void testGetPoNumberError() {
    logger.info("=== Test Get PO Number (generate poNumber) - fail ===");

    verifyGet(GET_PO_NUMBER_PATH, prepareHeaders(X_OKAPI_URL, PO_NUMBER_ERROR_X_OKAPI_TENANT), APPLICATION_JSON, 500);
  }

}
