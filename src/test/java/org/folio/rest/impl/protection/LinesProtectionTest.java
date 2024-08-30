package org.folio.rest.impl.protection;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.encodePrettily;
import static org.folio.orders.utils.AcqDesiredPermissions.BYPASS_ACQ_UNITS;
import static org.folio.orders.utils.PermissionsUtil.OKAPI_HEADER_PERMISSIONS;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_UNITS_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.USER_NOT_A_MEMBER_OF_THE_ACQ;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.LINES_PATH;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsEqual.equalTo;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.restassured.http.Header;
import io.vertx.core.json.JsonArray;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.Errors;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import io.restassured.http.Headers;

public class LinesProtectionTest extends ProtectedEntityTestBase {

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
  @ValueSource(strings = { "CREATE", "UPDATE", "DELETE", "READ" })
  void testOperationWithNonExistedUnits(ProtectedOperations operation) {
    logger.info("=== Test corresponding order contains non-existent units - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID);
    Errors errors = operation.process(LINES_PATH, encodePrettily(preparePoLine(NON_EXISTENT_UNITS, PENDING)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(ORDER_UNITS_NOT_FOUND.getCode()));
    // Verify number of sub-requests
    validateNumberOfRequests(1, 0);
  }

  @ParameterizedTest
  @CsvSource({
    "CREATE,1,0",
    "UPDATE,1,0",
    "DELETE,1,0",
    "READ,2,1"
  })
  void testOperationWithAllowedUnits(ProtectedOperations operation, int numOfUnitRqs, int numOfMembershipRqs) {
    logger.info("=== Test corresponding order has units allowed operation - expecting of call only to Units API ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID);
    operation.process(LINES_PATH, encodePrettily(preparePoLine(NOT_PROTECTED_UNITS, PENDING)), headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(numOfUnitRqs, numOfMembershipRqs );
  }

  @ParameterizedTest
  @CsvSource({
    "CREATE,1,0",
    "UPDATE,1,0",
    "DELETE,1,0",
    "READ,2,1"
  })
  void testWithRestrictedUnitsAndAllowedUser(ProtectedOperations operation, int numOfUnitRqs) {
    logger.info("=== Test corresponding order has units, units protect operation, user is member of order's units - expecting of calls to Units, Memberships APIs and allowance of operation ===");

    operation.process(LINES_PATH, encodePrettily(preparePoLine(PROTECTED_UNITS, PENDING)),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER), operation.getContentType(), operation.getCode());

    validateNumberOfRequests(numOfUnitRqs, numOfUnitRqs);
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "CREATE",
    "UPDATE",
    "DELETE",
    "READ"
  })
  void testWithProtectedUnitsAndForbiddenUser(ProtectedOperations operation) {
    logger.info("=== Test corresponding order has units, units protect operation, user isn't member of order's units - expecting of calls to Units, Memberships APIs and restriction of operation ===");

    final Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER);
    Errors errors = operation.process(LINES_PATH, encodePrettily(preparePoLine(PROTECTED_UNITS, PENDING)),
      headers, APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()).as(Errors.class);
    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(USER_NOT_A_MEMBER_OF_THE_ACQ.getCode()));

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
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, permissionHeader, X_OKAPI_USER_ID);
    operation.process(LINES_PATH, encodePrettily(preparePoLine(PROTECTED_UNITS, PENDING)),
      headers, operation.getContentType(), operation.getCode());

    validateNumberOfRequests(0, 0);
  }

}
