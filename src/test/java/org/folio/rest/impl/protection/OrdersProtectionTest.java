package org.folio.rest.impl.protection;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.orders.utils.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_ACQ_PERMISSIONS;
import static org.folio.orders.utils.ErrorCodes.USER_HAS_NO_PERMISSIONS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.rest.impl.AcquisitionsUnitsHelper.ACQUISITIONS_UNIT_IDS;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ALL_DESIRED_PERMISSIONS_HEADER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.COMPOSITE_ORDERS_PATH;
import static org.folio.rest.impl.protection.ProtectedOperations.UPDATE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsEqual.equalTo;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.folio.HttpStatus;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.restassured.http.Headers;
import io.vertx.core.json.JsonObject;
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
    "UPDATE",
    "DELETE",
    "READ"
  })
  public void testOperationWithNonExistedUnits(ProtectedOperations operation) {
    logger.info("=== Test order contains non-existent unit ids - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(NON_EXISTENT_UNITS)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(ORDER_UNITS_NOT_FOUND.getCode()));
    // Verify number of sub-requests
    validateNumberOfRequests(1, 0);
  }

  @Test
  @Parameters({
    "CREATE",
    "UPDATE",
    "DELETE",
    "READ"
  })
  public void testOperationWithAllowedUnits(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units allowed operation - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(NOT_PROTECTED_UNITS)), headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(1, 0);
  }

  @Test
  @Parameters({
    "CREATE",
    "UPDATE",
    "DELETE",
    "READ"
  })
  public void testWithRestrictedUnitsAndAllowedUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user is member of order's units - expecting of calls to Units, Memberships APIs and allowance of operation ===");

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(PROTECTED_UNITS)), headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(1, 1);
  }

  @Test
  @Parameters({
    "CREATE",
    "UPDATE",
    "DELETE",
    "READ"
  })
  public void testWithProtectedUnitsAndForbiddenUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(PROTECTED_UNITS)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_HAS_NO_PERMISSIONS.getCode()));

    validateNumberOfRequests(1, 1);
  }

  @Test
  @Parameters({
    "CREATE",
    "UPDATE"
  })
  public void testModifyUnitsList(ProtectedOperations operation) {
    logger.info("=== Test user without desired permissions modifying acqUnitsIds ===");

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    CompositePurchaseOrder order = prepareOrder(Collections.emptyList());
    order.setAcqUnitIds(PROTECTED_UNITS);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(order),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_HAS_NO_ACQ_PERMISSIONS.getCode()));

    validateNumberOfRequests(0, 0);
  }

  @Test
  @Parameters({
    "CREATE",
    "UPDATE"
  })
  public void testAssigningSoftDeletedUnit(ProtectedOperations operation) {
    logger.info("=== Test order contains \"soft deleted\" unit id - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);

    // Acq to order in storage for update case and
    AcquisitionsUnit unit1 = prepareTestUnit(false);
    AcquisitionsUnit unit2 = prepareTestUnit(true);
    AcquisitionsUnit unit3 = prepareTestUnit(true);
    // Add all acq units as mock data
    addMockEntry(ACQUISITIONS_UNITS, unit1);
    addMockEntry(ACQUISITIONS_UNITS, unit2);
    addMockEntry(ACQUISITIONS_UNITS, unit3);

    // Prepare order with 2 acq units (one is "soft deleted") and add it as mock data for update case
    CompositePurchaseOrder order = prepareOrder(Arrays.asList(unit1.getId(), unit2.getId()));

    // Add the third unit to request
    order.getAcqUnitIds().add(unit3.getId());

    Errors errors = operation
      .process(COMPOSITE_ORDERS_PATH, encodePrettily(order), headers, APPLICATION_JSON,
          HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt())
      .as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    Error error = errors.getErrors().get(0);
    assertThat(error.getCode(), equalTo(ORDER_UNITS_NOT_FOUND.getCode()));
    assertThat(error.getAdditionalProperties().get(ACQUISITIONS_UNIT_IDS), instanceOf(List.class));

    // Verify number of sub-requests
    validateNumberOfRequests(1, 0);

    List<String> ids = (List<String>) error.getAdditionalProperties().get(ACQUISITIONS_UNIT_IDS);
    if (operation == UPDATE) {
      assertThat(ids, contains(unit3.getId()));
    } else {
      assertThat(ids, containsInAnyOrder(unit2.getId(), unit3.getId()));
    }
  }

  @Test
  public void testUpdateOrderAssignedToCreateProtectedUnitAddingPoLineUserNotAssigned() {
    logger.info("=== Test corresponding order has units, poLine is going to be added to order, units protect only create operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    CompositePurchaseOrder order = prepareOrder(CREATE_PROTECTED_UNITS);
    CompositePoLine line = getMinimalContentCompositePoLine(order.getId());
    line.setId(UUID.randomUUID().toString());
    order.getCompositePoLines().add(line);
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = UPDATE.process(COMPOSITE_ORDERS_PATH, encodePrettily(order),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_HAS_NO_PERMISSIONS.getCode()));

    validateNumberOfRequests(1, 1);
  }

  @Test
  public void testUpdateOrderAssignedToDeleteProtectedUnitDeletingPoLineUserNotAssigned() {
    logger.info("=== Test corresponding order has units, poLine is going to be deleted from order, units protect only delete operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    order.setAcqUnitIds(DELETE_PROTECTED_UNITS);
    CompositePoLine line1 = getMinimalContentCompositePoLine(order.getId());
    line1.setId(UUID.randomUUID().toString());
    CompositePoLine line2 = getMinimalContentCompositePoLine(order.getId());
    line2.setId(UUID.randomUUID().toString());
    order.getCompositePoLines().add(line1);
    order.getCompositePoLines().add(line2);
    addMockEntry(PO_LINES, JsonObject.mapFrom(line1));
    addMockEntry(PO_LINES, JsonObject.mapFrom(line2));
    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));
    order.getCompositePoLines().remove(1);
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = UPDATE.process(COMPOSITE_ORDERS_PATH, encodePrettily(order),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_HAS_NO_PERMISSIONS.getCode()));

    validateNumberOfRequests(1, 1);
  }

  @Test
  public void testUpdateOrderAssignedToCreateProtectedUnitAddingAndDeletingPoLinesUserAssigned() {
    logger.info("=== Test corresponding order has units, poLine is going to be deleted, new line will be added ===");
    logger.info("=== Units protect all operations, user is member of order's units - expecting of calls to Units, Memberships APIs and allowance of operation ===");

    CompositePurchaseOrder order = prepareOrder(PROTECTED_UNITS);
    CompositePoLine line = getMinimalContentCompositePoLine(order.getId());
    line.setId(UUID.randomUUID().toString());
    order.getCompositePoLines().add(line);
    addMockEntry(PO_LINES, JsonObject.mapFrom(line));
    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));
    order.getCompositePoLines().remove(0);
    order.getCompositePoLines().add(getMinimalContentCompositePoLine(order.getId()));
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    UPDATE.process(COMPOSITE_ORDERS_PATH, encodePrettily(order),
      headers, UPDATE.getContentType(), UPDATE.getCode());


    validateNumberOfRequests(1, 1);
  }

}
