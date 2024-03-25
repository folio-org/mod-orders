package org.folio.rest.impl.protection;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.encodePrettily;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.orders.utils.AcqDesiredPermissions.BYPASS_ACQ_UNITS;
import static org.folio.orders.utils.PermissionsUtil.OKAPI_HEADER_PERMISSIONS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_HAS_NO_ACQ_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_NOT_A_MEMBER_OF_THE_ACQ;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ALL_DESIRED_PERMISSIONS_HEADER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.COMPOSITE_ORDERS_PATH;
import static org.folio.rest.impl.protection.ProtectedOperations.CREATE;
import static org.folio.rest.impl.protection.ProtectedOperations.UPDATE;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.folio.service.AcquisitionsUnitsService.ACQUISITIONS_UNIT_IDS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsEqual.equalTo;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.restassured.http.Header;
import io.vertx.core.json.JsonArray;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import io.restassured.http.Headers;
import io.vertx.core.json.JsonObject;


public class OrdersProtectionTest extends ProtectedEntityTestBase {

  private static final Logger logger = LogManager.getLogger();

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
  @ValueSource(strings = {
    "CREATE",
    "UPDATE",
    "DELETE",
    "READ"
  })
  void testOperationWithNonExistedUnits(ProtectedOperations operation) {
    logger.info("=== Test order contains non-existent unit ids - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(NON_EXISTENT_UNITS, PENDING)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(ORDER_UNITS_NOT_FOUND.getCode()));
    // Verify number of sub-requests
    validateNumberOfRequests(1, 0);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "CREATE",
  })
  void testCreateOperationWithAllowedUnits(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units allowed operation - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);
    CompositePurchaseOrder order = operation == CREATE ? getMinimalContentCompositePurchaseOrder().withAcqUnitIds(new ArrayList<>(NOT_PROTECTED_UNITS)) : prepareOrder(NOT_PROTECTED_UNITS, PENDING);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(order), headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(2, 0);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "UPDATE",
    "DELETE",
    "READ"
  })
  void testUpdateOperationsWithAllowedUnits(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units allowed operation - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);
    CompositePurchaseOrder order = operation == CREATE ? getMinimalContentCompositePurchaseOrder().withAcqUnitIds(new ArrayList<>(NOT_PROTECTED_UNITS)) : prepareOrder(NOT_PROTECTED_UNITS, PENDING);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(order), headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(1, 0);
  }


  @ParameterizedTest
  @ValueSource(strings = {
    "CREATE"
  })
  void testCreateWithRestrictedUnitsAndAllowedUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user is member of order's units - expecting of calls to Units, Memberships APIs and allowance of operation ===");

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    CompositePurchaseOrder order = operation == CREATE ? getMinimalContentCompositePurchaseOrder().withAcqUnitIds(new ArrayList<>(PROTECTED_UNITS)) : prepareOrder(PROTECTED_UNITS, PENDING);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(order), headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(2, 1);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "UPDATE",
    "DELETE",
    "READ"
  })
  void testUpdateWithRestrictedUnitsAndAllowedUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user is member of order's units - expecting of calls to Units, Memberships APIs and allowance of operation ===");

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    CompositePurchaseOrder order = operation == CREATE ? getMinimalContentCompositePurchaseOrder().withAcqUnitIds(new ArrayList<>(PROTECTED_UNITS)) : prepareOrder(PROTECTED_UNITS, PENDING);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(order), headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(1, 1);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "CREATE",
  })
  void testCreateWithProtectedUnitsAndForbiddenUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(PROTECTED_UNITS, PENDING)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_NOT_A_MEMBER_OF_THE_ACQ.getCode()));

    validateNumberOfRequests(2, 1);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "UPDATE",
    "DELETE",
    "READ"
  })
  void testUpdateWithProtectedUnitsAndForbiddenUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(prepareOrder(PROTECTED_UNITS, PENDING)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_NOT_A_MEMBER_OF_THE_ACQ.getCode()));

    validateNumberOfRequests(1, 1);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "CREATE",
    "UPDATE"
  })
  void testModifyUnitsList(ProtectedOperations operation) {
    logger.info("=== Test user without desired permissions modifying acqUnitsIds ===");

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    CompositePurchaseOrder order = prepareOrder(Collections.emptyList(), PENDING);
    order.setAcqUnitIds(PROTECTED_UNITS);
    Errors errors = operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(order),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_HAS_NO_ACQ_PERMISSIONS.getCode()));

    validateNumberOfRequests(0, 0);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "CREATE",
    "UPDATE"
  })
  void testAssigningSoftDeletedUnit(ProtectedOperations operation) {
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
    CompositePurchaseOrder order = prepareOrder(Arrays.asList(unit1.getId(), unit2.getId()), PENDING);

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
  void testGetOrderWithAssignedSoftDeletedUnit() {
    logger.info("=== Test order contains only \"soft deleted\" unit id - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_ID);

    // Acq to order in storage for update case and
    AcquisitionsUnit unit1 = prepareTestUnit(true);
    // Add all acq units as mock data
    addMockEntry(ACQUISITIONS_UNITS, unit1);

    // Prepare order with one acq unit ("soft deleted")
    CompositePurchaseOrder order = prepareOrder(Collections.singletonList(unit1.getId()), PENDING);

    ProtectedOperations.READ.process(COMPOSITE_ORDERS_PATH, encodePrettily(order), headers, APPLICATION_JSON,
        HttpStatus.HTTP_OK.toInt());

    // Verify number of sub-requests
    validateNumberOfRequests(1, 0);

    // Verify that unit was deleted so logic skipped membership check
    List<AcquisitionsUnit> acquisitionsUnits = MockServer.getAcqUnitsSearches()
      .get(0)
      .mapTo(AcquisitionsUnitCollection.class)
      .getAcquisitionsUnits();
    assertThat(acquisitionsUnits, hasSize(1));
    assertThat(acquisitionsUnits.get(0).getIsDeleted(), is(true));
  }

  @Test
  void testUpdateOrderAssignedToCreateProtectedUnitAddingPoLineUserNotAssigned() {
    logger.info("=== Test corresponding order has units, poLine is going to be added to order, units protect only create operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    CompositePurchaseOrder order = prepareOrder(CREATE_PROTECTED_UNITS, PENDING);
    CompositePoLine line = getMinimalContentCompositePoLine(order.getId());
    line.setId(UUID.randomUUID().toString());
    order.getCompositePoLines().add(line);
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = UPDATE.process(COMPOSITE_ORDERS_PATH, encodePrettily(order),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_NOT_A_MEMBER_OF_THE_ACQ.getCode()));

    validateNumberOfRequests(1, 1);
  }

  @Test
  void testUpdateOrderAssignedToDeleteProtectedUnitDeletingPoLineUserNotAssigned() {
    logger.info("=== Test corresponding order has units, poLine is going to be deleted from order, units protect only delete operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    order.setAcqUnitIds(DELETE_PROTECTED_UNITS);
    CompositePoLine line1 = getMinimalContentCompositePoLine(order.getId());
    line1.setId(UUID.randomUUID().toString());
    CompositePoLine line2 = getMinimalContentCompositePoLine(order.getId());
    line2.setId(UUID.randomUUID().toString());
    order.getCompositePoLines().add(line1);
    order.getCompositePoLines().add(line2);
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(line1));
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(line2));
    addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));
    order.getCompositePoLines().remove(1);
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = UPDATE.process(COMPOSITE_ORDERS_PATH, encodePrettily(order),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_NOT_A_MEMBER_OF_THE_ACQ.getCode()));

    validateNumberOfRequests(1, 1);
  }

  @Test
  void testUpdateOrderAssignedToCreateProtectedUnitAddingAndDeletingPoLinesUserAssigned() {
    logger.info("=== Test corresponding order has units, poLine is going to be deleted, new line will be added ===");
    logger.info("=== Units protect all operations, user is member of order's units - expecting of calls to Units, Memberships APIs and allowance of operation ===");

    CompositePurchaseOrder order = prepareOrder(PROTECTED_UNITS, PENDING);
    CompositePoLine line = getMinimalContentCompositePoLine(order.getId());
    line.setId(UUID.randomUUID().toString());
    order.getCompositePoLines().add(line);
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(line));
    addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(order));
    order.getCompositePoLines().remove(0);
    order.getCompositePoLines().add(getMinimalContentCompositePoLine(order.getId()));
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_PERMISSIONS_HEADER, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER);
    UPDATE.process(COMPOSITE_ORDERS_PATH, encodePrettily(order),
      headers, UPDATE.getContentType(), UPDATE.getCode());


    validateNumberOfRequests(1, 1);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "UPDATE",
    "READ"
  })
  void testBypassAcqUnitChecks(ProtectedOperations operation) {
    Header permissionHeader = new Header(OKAPI_HEADER_PERMISSIONS,
      new JsonArray(List.of(BYPASS_ACQ_UNITS.getPermission())).encode());
    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, permissionHeader, X_OKAPI_USER_ID);
    CompositePurchaseOrder order = operation == CREATE ? getMinimalContentCompositePurchaseOrder().withAcqUnitIds(new ArrayList<>(PROTECTED_UNITS)) : prepareOrder(PROTECTED_UNITS, PENDING);
    operation.process(COMPOSITE_ORDERS_PATH, encodePrettily(order), headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(0, 0);
  }

}
