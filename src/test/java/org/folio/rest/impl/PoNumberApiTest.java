package org.folio.rest.impl;

import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.rest.jaxrs.model.PoNumber;
import org.junit.Test;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.rest.impl.MockServer.PO_NUMBER_ERROR_X_OKAPI_TENANT;
import static org.folio.rest.impl.MockServer.PO_NUMBER_VALUE;
import static org.junit.Assert.assertEquals;

public class PoNumberApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PoNumberApiTest.class);

  private static final String PONUMBER_VALIDATE_PATH = "/orders/po-number/validate";
  private static final String GET_PO_NUMBER_PATH = "/orders/po-number";

  static final String NONEXISTING_PO_NUMBER = "newPoNumber";
  static final String EXISTING_PO_NUMBER = "oldPoNumber";

  @Test
  public void testPoNumberValidateWithExistingPONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put(PO_NUMBER, EXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 400);
  }

  @Test
  public void testPoNumberValidateWithUniquePONumber()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put(PO_NUMBER, NONEXISTING_PO_NUMBER);
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), "", 204);
  }

  @Test
  public void testPoNumberValidateWithInvalidPattern()
  {
    JsonObject poNumber=new JsonObject();
    poNumber.put(PO_NUMBER, "11");
    verifyPostResponse(PONUMBER_VALIDATE_PATH, poNumber.encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422);
  }

  @Test
  public void testGetPoNumber() {
    logger.info("=== Test Get PO Number (generate poNumber) ===");

    final PoNumber poNumber = verifySuccessGet(GET_PO_NUMBER_PATH, PoNumber.class);

    String actualPoNumberValue = poNumber.getPoNumber();
    assertEquals(PO_NUMBER_VALUE, actualPoNumberValue);
  }

  @Test
  public void testGetPoNumberError() {
    logger.info("=== Test Get PO Number (generate poNumber) - fail ===");

    verifyGet(GET_PO_NUMBER_PATH, prepareHeaders(X_OKAPI_URL, PO_NUMBER_ERROR_X_OKAPI_TENANT), APPLICATION_JSON, 500);
  }
}
