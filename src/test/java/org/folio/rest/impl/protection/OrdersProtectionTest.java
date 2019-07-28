package org.folio.rest.impl.protection;

import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.HttpStatus;
import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.junit.Test;

import java.util.Arrays;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.rest.impl.PurchaseOrdersApiTest.COMPOSITE_ORDERS_PATH;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.nullValue;

public class OrdersProtectionTest extends ProtectedEntityTestBase {

  private static final Logger logger = LoggerFactory.getLogger(OrdersProtectionTest.class);

  @Override
  public ProtectedOperations[] geProtectedOperations() {
    ProtectedOperations[] operations = {ProtectedOperations.CREATE, ProtectedOperations.READ} ;
    return operations;
  }

  @Override
  public String getSampleForFlowWithNonExistedUnits() {
    CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
    compPo.setId(ORDER_WITHOUT_UNITS_ID);
    compPo.setAcqUnitIds(Arrays.asList(NON_EXISTED_UNIT_ID, "0f2bb7a2-728f-4e07-9268-082577a7bedb"));
    return JsonObject.mapFrom(compPo).encode();
  }

  @Override
  public String getSampleForFlowWithAllowedUnits() {
    CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
    compPo.setId(ORDER_WITH_NON_PROTECTED_UNITS_ID);
    compPo.setAcqUnitIds(Arrays.asList(SUPER_USER_UNIT_ID, FULL_PROTECTED_USERS_UNIT_ID));
    return JsonObject.mapFrom(compPo).encode();
  }

  @Override
  public String getSampleForProtectedUnitsAndAllowedUserFlow() {
    CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
    compPo.setId(ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID);
    compPo.setAcqUnitIds(Arrays.asList(FULL_PROTECTED_USERS_UNIT_ID, UPDATE_ONLY_UNIT_ID));
    return JsonObject.mapFrom(compPo).encode();
  }

  @Override
  public String getSampleForRestrictedFlow() {
    CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
    compPo.setId(ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID);
    compPo.setAcqUnitIds(Arrays.asList(FULL_PROTECTED_USERS_UNIT_ID, UPDATE_ONLY_UNIT_ID));
    return JsonObject.mapFrom(compPo).encode();
  }

  @Test
  public void testAssignmentCreation() {
    logger.info("=== Test assignment creation during POST order ===");

    // Assignment should be created
    ProtectedOperations.CREATE.process(COMPOSITE_ORDERS_PATH, getSampleForFlowWithAllowedUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
    assertThat(MockServer.getAcqAssignmentCreations(), hasSize(2));
    assertThat(MockServer.getPurchaseOrderCreations(), hasSize(1));
    MockServer.release();

    ProtectedOperations.CREATE.process(COMPOSITE_ORDERS_PATH, getSampleForProtectedUnitsAndAllowedUserFlow(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALLOWED_CREATION_HEADERS[0]), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
    assertThat(MockServer.getAcqAssignmentCreations(), hasSize(2));
    assertThat(MockServer.getPurchaseOrderCreations(), hasSize(1));
    MockServer.release();

    // Assignment shouldn't be created
    ProtectedOperations.CREATE.process(COMPOSITE_ORDERS_PATH, getSampleForFlowWithNonExistedUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_VALIDATION_ERROR.toInt());
    assertThat(MockServer.getAcqAssignmentCreations(), nullValue());
    assertThat(MockServer.getPurchaseOrderCreations(), nullValue());
    MockServer.release();

    ProtectedOperations.CREATE.process(COMPOSITE_ORDERS_PATH, getSampleForRestrictedFlow(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
    assertThat(MockServer.getAcqAssignmentCreations(), nullValue());
    assertThat(MockServer.getPurchaseOrderCreations(), nullValue());
    MockServer.release();
  }

  @Test
  public void testOperationWithNonExistedUnits() {
    logger.info("=== Test corresponding order hasn't units - expecting of call only to Assignments API ===");

      // Composite PO already contains acquisition unit IDs
    ProtectedOperations create = ProtectedOperations.CREATE;
      create.process(COMPOSITE_ORDERS_PATH, getSampleForFlowWithNonExistedUnits(),
        prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_VALIDATION_ERROR.toInt());
      // Verify number of sub-requests
      validateNumberOfRequests(1, 0, getNumOfAssignmentsGetRqs(create));

    // Composite PO already contains acquisition unit IDs
    ProtectedOperations read = ProtectedOperations.READ;
    read.process(COMPOSITE_ORDERS_PATH, getSampleForFlowWithNonExistedUnits(),
      prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, read.getCode());
    // Verify number of sub-requests
    validateNumberOfRequests(0, 0, getNumOfAssignmentsGetRqs(read));
  }

  @Test
  public void testOperationWithAllowedUnits() {
    logger.info("=== Test corresponding order has units allowed operation - expecting of call only to Assignments API and Units API ===");

    for(ProtectedOperations operation : geProtectedOperations()) {
      operation.process(COMPOSITE_ORDERS_PATH, getSampleForFlowWithAllowedUnits(),
        prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, operation.getCode());
      // Verify number of sub-requests
      validateNumberOfRequests(1, 0, getNumOfAssignmentsGetRqs(operation));
    }
  }

  @Test
  public void testWithRestrictedUnitsAndAllowedUser() {
    logger.info("=== Test corresponding order has units, units protect operation, user is member of order's units - expecting of calls to Units, Memberships, Assignments API and allowance of operation ===");

    Arrays.stream(ALLOWED_CREATION_HEADERS).forEach(header -> {
      for(ProtectedOperations operation : geProtectedOperations()) {
        operation.process(COMPOSITE_ORDERS_PATH, getSampleForProtectedUnitsAndAllowedUserFlow(),
          prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, operation.getCode());
        // Verify number of sub-requests
        validateNumberOfRequests(1, 1, getNumOfAssignmentsGetRqs(operation));
      }
    });
  }

  @Test
  public void testWithProtectedUnitsAndForbiddenUser() {
    logger.info("=== Test corresponding order has units, units protect operation, user isn't member of order's units - expecting of calls to Units, Memberships, Assignments API and restriction of operation ===");

    Arrays.stream(FORBIDDEN_CREATION_HEADERS).forEach(header -> {
      for(ProtectedOperations operation : geProtectedOperations()) {
        operation.process(COMPOSITE_ORDERS_PATH, getSampleForRestrictedFlow(),
          prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
        // Verify number of sub-requests
        validateNumberOfRequests(1, 1, getNumOfAssignmentsGetRqs(operation));
      }
    });
  }

  private int getNumOfAssignmentsGetRqs(ProtectedOperations operation) {
    return operation == ProtectedOperations.CREATE ? 0 : 1;
  }
}
