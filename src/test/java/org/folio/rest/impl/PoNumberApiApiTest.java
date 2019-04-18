package org.folio.rest.impl;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.rest.jaxrs.model.PoNumber;
import org.junit.Test;

import static org.folio.rest.impl.PurchaseOrdersApiApiTest.APPLICATION_JSON;
import static org.folio.rest.impl.PurchaseOrdersApiApiTest.EXISTING_PO_NUMBER;
import static org.folio.rest.impl.PurchaseOrdersApiApiTest.NONEXISTING_PO_NUMBER;
import static org.folio.rest.impl.MockServer.PO_NUMBER_VALUE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class PoNumberApiApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PoNumberApiApiTest.class);

  private static final String PONUMBER_VALIDATE_PATH = "/orders/po-number/validate";

  @Test
  public void testPoNumberValidateWithExistingPONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", EXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 400);
  }

  @Test
  public void testPoNumberValidateWithUniquePONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", NONEXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), "", 204);
  }

  @Test
  public void testPoNumberValidateWithInvalidPattern()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put("poNumber", "11");
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422);
  }

  @Test
  public void testGetPoNumber() {
    logger.info("=== Test Get PO Number (generate poNumber) ===");

    final Response response = RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10)
      .get("/orders/po-number")
        .then()
          .statusCode(200)
          .extract()
            .response();

    String actualResponse = response.getBody().asString();
    logger.info(actualResponse);

    PoNumber poNumber = response.as(PoNumber.class);
    String actualPoNumberValue = poNumber.getPoNumber();

    assertEquals(PO_NUMBER_VALUE, actualPoNumberValue);
    assertNotNull(actualResponse);
  }

  @Test
  public void testGetPoNumberError() {
    logger.info("=== Test Get PO Number (generate poNumber) - fail ===");

    RestAssured
      .with()
        .header(X_OKAPI_URL)
        .header(MockServer.PO_NUMBER_ERROR_X_OKAPI_TENANT)
      .get("/orders/po-number")
        .then()
          .statusCode(500)
          .extract()
            .response();
  }
}
