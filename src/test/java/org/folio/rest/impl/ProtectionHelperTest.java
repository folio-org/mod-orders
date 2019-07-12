package org.folio.rest.impl;


import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import org.folio.HttpStatus;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece;
import org.junit.Test;

import java.util.Arrays;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.ApiTestBase.getMockAsJson;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.COMP_PO_LINES_MOCK_DATA_PATH;
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

  public static final String ACQUISITIONS_UNITS = "acquisitionsUnits";
  public static final String ACQUISITIONS_MEMBERSHIPS = "acquisitionsMemberships";
  public static final String ACQUISITIONS_UNIT_ASSIGNMENTS = "acquisitionsUnitAssignments";


  @Test
  public void expectedFlowWithoutUserIdHeaderWithStatus403Test() {
    // 1. Request without user id header - expecting no calls to Units, Memberships, Assignments API.
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleForFlow403(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 403);
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), nullValue());
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void expectedFlowWithoutUnitsWithStatus201Test() {
    // 2. Order without associated units - expecting of call only to Assignments API.
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleForFlow201WithoutUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201);
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void expectedFlowWithUnitsWithStatus201Test() {
    // 3. Order with associated units (units allow operation) and skipping of memberships verification
    // - expecting of call only to Assignments API and Units API.
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleForFlow201WithNonProtectedUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201);
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), nullValue());
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void expectedStatus403Test() {
    // 4. Order with units (units protect operation), but there are no units related to order - expecting of calls
    // to Units, Memberships, Assignments API and restriction of operation.
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities entities : ProtectedEntities.values()) {
        Arrays.stream(FORBIDDEN_CREATION_HEADERS).forEach(header -> {
          // Verify request
          operation.process(entities.getEndpoint(), entities.getSampleForFlow403(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
          // Verify number of sub-requests
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), hasSize(1));
          MockServer.serverRqRs.clear();
        });
      }
    }
  }

  @Test
  public void expectedFullFlowStatus201Test() {
    // 5. Order with units (units protect operation), but there are units related to order - expecting of calls
    // to Units, Memberships, Assignments API and allowance of operation.
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities entities : ProtectedEntities.values()) {
        Arrays.stream(ALLOWED_CREATION_HEADERS).forEach(header -> {
          // Verify request
          operation.process(entities.getEndpoint(), entities.getSampleForFlow201(),
            prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
          // Verify number of sub-requests
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
          assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNIT_ASSIGNMENTS, HttpMethod.GET), hasSize(1));
          MockServer.serverRqRs.clear();
        });
      }
    }
  }
}

enum Operations {

  CREATE {
    @Override
    Response process(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
      return apiTestBase.verifyPostResponse(url, body, headers, expectedContentType, expectedCode);
    }
  };

  private static ApiTestBase apiTestBase = new ApiTestBase();

  abstract Response process(String url, String body, Headers headers, String expectedContentType, int expectedCode);
}

enum ProtectedEntities {

  PIECES("/orders/pieces", getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json").encode()),
  ORDER_LINES("/orders/order-lines", getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).encode());

  private String endpoint;
  private String sample;

  ProtectedEntities(String endpoint, String sample) {
    this.endpoint = endpoint;
    this.sample = sample;
  }

  public String getEndpoint() {
    return endpoint;
  }

  public String getSampleForFlow201() {
    switch (this) {
      case PIECES:
        Piece piece = new JsonObject(sample).mapTo(Piece.class);
        piece.setPoLineId("c2755a78-2f8d-47d0-a218-059a9b7391b4");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = new JsonObject(sample).mapTo(CompositePoLine.class);
        poLine.setId("c2755a78-2f8d-47d0-a218-059a9b7391b4");
        poLine.setPurchaseOrderId("1ab7ef6a-d1d4-4a4f-90a2-882aed18af14");
        return JsonObject.mapFrom(poLine).encode();
      default:
        return null;
    }
  }

  public String getSampleForFlow403() {
    return sample;
  }

  // This method should return sample associated with order that isn't associated to any unit.
  public String getSampleForFlow201WithoutUnits() {
    switch (this) {
      case PIECES:
        Piece piece = new JsonObject(sample).mapTo(Piece.class);
        piece.setPoLineId("0009662b-8b80-4001-b704-ca10971f175d");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = new JsonObject(sample).mapTo(CompositePoLine.class);
        poLine.setId("0009662b-8b80-4001-b704-ca10971f175d");
        poLine.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");
        return JsonObject.mapFrom(poLine).encode();
      default:
        return null;
    }
  }

  // This method should return sample associated with order that is associated with units allowed operation.
  public String getSampleForFlow201WithNonProtectedUnits() {
    switch (this) {
      case PIECES:
        Piece piece = new JsonObject(sample).mapTo(Piece.class);
        piece.setPoLineId("8ca5aa90-bfbd-44b3-8ad2-f6f1b7e05337");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = new JsonObject(sample).mapTo(CompositePoLine.class);
        poLine.setId("8ca5aa90-bfbd-44b3-8ad2-f6f1b7e05337");
        poLine.setPurchaseOrderId("e5ae4afd-3fa9-494e-a972-f541df9b877e");
        return JsonObject.mapFrom(poLine).encode();
      default:
        return null;
    }
  }
}
