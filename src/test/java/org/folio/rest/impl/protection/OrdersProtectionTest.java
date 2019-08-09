package org.folio.rest.impl.protection;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.orders.utils.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_ACQ_PERMISSIONS;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ALL_DESIRED_PERMISSIONS_HEADER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.COMPOSITE_ORDERS_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsEqual.equalTo;

import org.folio.HttpStatus;
import org.folio.rest.jaxrs.model.Errors;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.restassured.http.Headers;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import junitparams.JUnitParamsRunner;
import junitparams.Parameters;

@RunWith(JUnitParamsRunner.class)
public class OrdersProtectionTest extends ProtectedEntityTestBase {

  private static final Logger logger = LoggerFactory.getLogger(OrdersProtectionTest.class);


  @Test
  @Parameters({
    "CREATE",
    "READ"
  })
  public void testOperationWithNonExistedUnits(ProtectedOperations operation) {
    logger.info("=== Test order contains non-existent unit ids - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(NON_EXISTENT_UNITS)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_VALIDATION_ERROR.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(ORDER_UNITS_NOT_FOUND.getCode()));
    // Verify number of sub-requests
    validateNumberOfRequests(1, 0);
  }

  @Test
  @Parameters({
    "CREATE",
    "READ"
  })
  public void testOperationWithAllowedUnits(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units allowed operation - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(NOT_PROTECTED_UNITS)), headers, APPLICATION_JSON, operation.getCode());

    validateNumberOfRequests(1, 0);
  }

  @Test
  @Parameters({
    "CREATE",
    "READ"
  })
  public void testWithRestrictedUnitsAndAllowedUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user is member of order's units - expecting of calls to Units, Memberships APIs and allowance of operation ===");

    Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(PROTECTED_UNITS)), headers, APPLICATION_JSON, operation.getCode());

    validateNumberOfRequests(1, 1);
  }

  @Test
  @Parameters({
    "CREATE",
    "READ"
  })
  public void testWithProtectedUnitsAndForbiddenUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(PROTECTED_UNITS)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_HAS_NO_PERMISSIONS.getCode()));

    validateNumberOfRequests(1, 1);
  }

  @Test
  @Parameters({
    "CREATE"
  })
  public void testModifyUnitsList(ProtectedOperations operation) {
    logger.info("=== Test user without desired permissions modifying acqUnitsIds ===");

    Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(PROTECTED_UNITS)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_HAS_NO_ACQ_PERMISSIONS.getCode()));

    validateNumberOfRequests(0, 0);
  }

}
