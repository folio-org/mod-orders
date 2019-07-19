package org.folio.rest.impl;


import io.restassured.http.Header;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import org.folio.HttpStatus;
import org.hamcrest.Matcher;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Arrays;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.ProtectedEntities.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

public class ProtectionHelperTest extends ApiTestBase {


  private static final String USER_IS_NOT_MEMBER_OF_ORDERS_UNITS = "7007ed1b-85ab-46e8-9524-fada8521dfd5";
  private static final Header X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER = new Header(OKAPI_USERID_HEADER, USER_IS_NOT_MEMBER_OF_ORDERS_UNITS);

  private static final String USER_IS_MEMBER_OF_ORDER_UNITS = "6b4be232-5ad9-47a6-80b1-8c1acabd6212";
  private static final Header X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER = new Header(OKAPI_USERID_HEADER, USER_IS_MEMBER_OF_ORDER_UNITS);

  private static final Header[] FORBIDDEN_CREATION_HEADERS = {X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER};
  private static final Header[] ALLOWED_CREATION_HEADERS = {X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER};

  private static final String ACQUISITIONS_UNITS = "acquisitionsUnits";
  private static final String ACQUISITIONS_MEMBERSHIPS = "acquisitionsMemberships";
  private static final String ACQUISITIONS_UNIT_ASSIGNMENTS = "acquisitionsUnitAssignments";

  @Test
  public void staticValidationTest403Test() {
    // 1. Request without user id header - expecting no calls to Units, Memberships, Assignments API.
    for(ProtectedOperations operation : ProtectedOperations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleForRestrictedFlow(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
        validateNumberOfRequests(0, 0, 0);
      }
      // Specific order case order haven't unit IDs
      operation.process(ProtectedEntities.ORDERS.getEndpoint(), JsonObject.mapFrom(ProtectedEntities.getMinimalContentCompositePurchaseOrder()).encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
      // Verify number of sub-requests
      validateNumberOfRequests(0, 0, 0);
    }
  }

  @Test
  public void flowOrderWithoutUnitsTest() {
    // 2. Order without associated units - expecting of call only to Assignments API.
    for(ProtectedOperations operation : ProtectedOperations.values()) {
      for(ProtectedEntities holder : Arrays.asList(PIECES, ORDER_LINES)) {
        operation.process(holder.getEndpoint(), holder.getSampleForFlowWithoutUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
        validateNumberOfRequests(0, 0, 1);
      }
      // Composite PO already contains acquisition unit IDs
      ProtectedEntities order = ProtectedEntities.ORDERS;
      operation.process(order.getEndpoint(), order.getSampleForFlowWithoutUnits(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_VALIDATION_ERROR.toInt());
      // Verify number of sub-requests
      validateNumberOfRequests(1, 0, 0);
    }
  }

  @Test
  public void expectedFlowWithUnitsWithStatus201Test() {
    // 3. Order with associated units (units allow operation) and skipping of memberships verification
    // - expecting of call only to Assignments API and Units API.
    for(ProtectedOperations operation : ProtectedOperations.values()) {
      for(ProtectedEntities holder : Arrays.asList(PIECES, ORDER_LINES)) {
        operation.process(holder.getEndpoint(), holder.getSampleForFlowWithAllowedUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
        validateNumberOfRequests(1, 0, 1);
      }
      // Composite PO already contains acquisition unit IDs
      ProtectedEntities order = ProtectedEntities.ORDERS;
      operation.process(order.getEndpoint(), order.getSampleForFlowWithAllowedUnits(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
      // Verify number of sub-requests
      validateNumberOfRequests(1, 0, 0);
    }
  }

  @Test
  public void expectedFullFlowStatus201Test() {
    // 5. Order with units (units protect operation), but there are units related to order - expecting of calls
    // to Units, Memberships, Assignments API and allowance of operation.

    Arrays.stream(ALLOWED_CREATION_HEADERS).forEach(header -> {
      for(ProtectedOperations operation : ProtectedOperations.values()) {
        for(ProtectedEntities entities : Arrays.asList(PIECES, ORDER_LINES)) {
          // Verify request
          operation.process(entities.getEndpoint(), entities.getSampleForProtectedUnitsAndAllowedUserFlow(),
            prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
          // Verify number of sub-requests
          validateNumberOfRequests(1, 1, 1);
        }
        ProtectedEntities order = ProtectedEntities.ORDERS;
        operation.process(order.getEndpoint(), order.getSampleForProtectedUnitsAndAllowedUserFlow(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
        // Verify number of sub-requests
        validateNumberOfRequests(1, 1, 0);
      }
    });
  }



  @Test
  public void expectedStatus403Test() {
    // 4. Order with units (units protect operation), but there are no units related to order - expecting of calls
    // to Units, Memberships, Assignments API and restriction of operation.
    Arrays.stream(FORBIDDEN_CREATION_HEADERS).forEach(header -> {
      for(ProtectedOperations operation : ProtectedOperations.values()) {
        for(ProtectedEntities entities : Arrays.asList(PIECES, ORDER_LINES)) {
          // Verify request
          operation.process(entities.getEndpoint(), entities.getSampleForRestrictedFlow(),
            prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
          // Verify number of sub-requests
          validateNumberOfRequests(1, 1, 1);
        }
        ProtectedEntities order = ProtectedEntities.ORDERS;
        operation.process(order.getEndpoint(), order.getSampleForRestrictedFlow(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
        // Verify number of sub-requests
        validateNumberOfRequests(1, 1, 0);
      }
    });
  }

  private static void validateNumberOfRequests(int numOfUnitRqs, int numOfMembershipRqs, int numOfAssignmentRqs) {
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), getMatcher(numOfUnitRqs));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), getMatcher(numOfMembershipRqs));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), getMatcher(numOfAssignmentRqs));
    MockServer.serverRqRs.clear();
  }

  private static Matcher getMatcher(int value) {
    return value > 0 ? hasSize(value) : nullValue();
  }
}


