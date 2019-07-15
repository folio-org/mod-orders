package org.folio.rest.impl;


import io.restassured.http.Header;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import org.folio.HttpStatus;
import org.junit.Test;

import java.util.Arrays;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.ProtectedEntities.ORDER_LINES;
import static org.folio.rest.impl.ProtectedEntities.PIECES;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

public class ProtectionHelperTest extends ApiTestBase {

  private static final String USER_WITHOUT_ASSIGNED_UNITS_ID = "147c36f9-35d1-43bd-ad62-aaa490d0d42c";
  private static final Header X_OKAPI_USER_WITHOUT_ASSIGNED_UNITS = new Header(OKAPI_USERID_HEADER, USER_WITHOUT_ASSIGNED_UNITS_ID);

  private static final String USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER_ID = "6e076ac5-371e-4462-af79-187c54fe70de";
  private static final Header X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER = new Header(OKAPI_USERID_HEADER, USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER_ID);

  private static final String USER_WITH_UNITS_ASSIGNED_TO_ORDER_ID = "c4785326-c687-416b-9d52-7ac88b442d17";
  private static final Header X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER = new Header(OKAPI_USERID_HEADER, USER_WITH_UNITS_ASSIGNED_TO_ORDER_ID);

  private static final Header[] FORBIDDEN_CREATION_HEADERS = {X_OKAPI_USER_WITHOUT_ASSIGNED_UNITS, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER};
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
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), nullValue());
        MockServer.serverRqRs.clear();
      }

      // 2. Specific order case order haven't unit IDs
      operation.process(ProtectedEntities.ORDERS.getEndpoint(), JsonObject.mapFrom(ProtectedEntities.getMinimalContentCompositePurchaseOrder()).encode(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
      // Verify number of sub-requests
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), nullValue());
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), nullValue());
      MockServer.serverRqRs.clear();
    }
  }

  @Test
  public void flowOrderWithoutUnitsTest() {
    // 2. Order without associated units - expecting of call only to Assignments API.
    for(ProtectedOperations operation : ProtectedOperations.values()) {
      for(ProtectedEntities holder : Arrays.asList(PIECES, ORDER_LINES)) {
        operation.process(holder.getEndpoint(), holder.getSampleForFlowWithoutUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
      }

      // Composite PO already contains acquisition unit IDs
      ProtectedEntities e = ProtectedEntities.ORDERS;
      operation.process(e.getEndpoint(), e.getSampleForFlowWithoutUnits(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
      // Verify number of sub-requests
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), nullValue());
      MockServer.serverRqRs.clear();
    }
  }

  @Test
  public void expectedFlowWithUnitsWithStatus201Test() {
    // 3. Order with associated units (units allow operation) and skipping of memberships verification
    // - expecting of call only to Assignments API and Units API.
    for(ProtectedOperations operation : ProtectedOperations.values()) {
      for(ProtectedEntities holder : Arrays.asList(PIECES, ORDER_LINES)) {
        operation.process(holder.getEndpoint(), holder.getSampleForFlow201WithNonProtectedUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
      }

      // Composite PO already contains acquisition unit IDs
      ProtectedEntities e = ProtectedEntities.ORDERS;
      operation.process(e.getEndpoint(), e.getSampleForFlow201WithNonProtectedUnits(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
      // Verify number of sub-requests
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
      assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), nullValue());
      MockServer.serverRqRs.clear();
    }
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
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), hasSize(1));
          MockServer.serverRqRs.clear();
        }

        ProtectedEntities e = ProtectedEntities.ORDERS;
        operation.process(e.getEndpoint(), e.getSampleForRestrictedFlow(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
        // Verify number of sub-requests
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), nullValue());
        MockServer.serverRqRs.clear();
      }
    });
  }

  @Test
  public void expectedFullFlowStatus201Test() {
    // 5. Order with units (units protect operation), but there are units related to order - expecting of calls
    // to Units, Memberships, Assignments API and allowance of operation.

    Arrays.stream(ALLOWED_CREATION_HEADERS).forEach(header -> {
      for(ProtectedOperations operation : ProtectedOperations.values()) {
        for(ProtectedEntities entities : Arrays.asList(PIECES, ORDER_LINES)) {
          // Verify request
          operation.process(entities.getEndpoint(), entities.getSampleForFlow201(),
            prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
          // Verify number of sub-requests
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), hasSize(1));
          MockServer.serverRqRs.clear();
        }

        ProtectedEntities order = ProtectedEntities.ORDERS;
        operation.process(order.getEndpoint(), order.getSampleForFlow201(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
        // Verify number of sub-requests
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), nullValue());
      }
    });
  }
}


