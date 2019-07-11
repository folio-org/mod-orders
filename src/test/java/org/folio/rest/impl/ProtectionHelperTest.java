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
import java.util.UUID;
import java.util.stream.Stream;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.ApiTestBase.getMockAsJson;
import static org.folio.rest.impl.MockServer.PIECE_RECORDS_MOCK_DATA_PATH;
import static org.folio.rest.impl.ProtectionHelperTest.ALLOWED_CREATION_HEADERS;
import static org.folio.rest.impl.ProtectionHelperTest.FORBIDDEN_CREATION_HEADERS;
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

  static final Header[] FORBIDDEN_CREATION_HEADERS = {X_OKAPI_USER_WITHOUT_ASSIGNED_UNITS, X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER};
  static final Header[] ALLOWED_CREATION_HEADERS = {X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER};


  @Test
  public void testWithoutUserIdHeader() {
    // Request without user id header - expecting of call only to Assignments API
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleForNormalFlowWith403(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 403);
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), nullValue());
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void testNumberOfRequestsForOrderWithoutUnits() {
    // 1. Order without associated units - expecting of call only to Assignments API
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleWithOrderWithoutUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201);
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void testNumberOfRequestsForOrderWithoutUnits2() {
    // 2. Order with associated units (units allow operation) - expecting of call only to Assignments API and Units API.
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleWithOrderWithNonProtectiveUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201);
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), nullValue());
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void testNumberOfRequestsForOrderWithUnits() {
    // Order with units (units restrict operation) - expecting of calls to Units, Memberships, Assignments API
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleForNormalFlowWith403(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt());
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void neagtiveTest() {
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities entities : ProtectedEntities.values()) {
        entities.getForbiddenUserIdHeadersStream().forEach(header -> operation.process(entities.getEndpoint(), entities.getSampleForNormalFlowWith403(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()));
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), hasSize(2));
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), hasSize(2));
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), hasSize(2));
        MockServer.serverRqRs.clear();
      }
    }
  }



  @Test
  public void testOperationsProtection() {
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities entities : ProtectedEntities.values()) {
        entities.getAllowedUserIdHeadersStream().forEach(header -> operation.process(entities.getEndpoint(), entities.getSampleForNormalFlowWith201(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()));
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
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

  PIECES("/orders/pieces", getMockAsJson(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord.json").encode(), ALLOWED_CREATION_HEADERS, FORBIDDEN_CREATION_HEADERS),
  ORDER_LINES("/orders/order-lines", getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).encode(), ALLOWED_CREATION_HEADERS, FORBIDDEN_CREATION_HEADERS);

  private String endpoint;
  private String sample;
  private Header[] allowedUserIdHeaders, forbiddenUserIdHeaders;

  ProtectedEntities(String endpoint, String sample, Header[] allowedUserIdHeaders, Header[] forbiddenUserIdHeaders) {
    this.endpoint = endpoint;
    this.sample = sample;
    this.allowedUserIdHeaders = allowedUserIdHeaders;
    this.forbiddenUserIdHeaders = forbiddenUserIdHeaders;
  }

  public String getEndpoint() {
    return endpoint;
  }

  public String getSampleForNormalFlowWith403() {
    return sample;
  }

  public String getSampleForNormalFlowWith201() {
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

  // This method should return sample associated with order that isn't associated to any unit.
  public String getSampleWithOrderWithoutUnits() {
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
  public String getSampleWithOrderWithNonProtectiveUnits() {
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

  public Stream<Header> getAllowedUserIdHeadersStream() {
    return Arrays.stream(allowedUserIdHeaders);
  }

  public Stream<Header> getForbiddenUserIdHeadersStream() {
    return Arrays.stream(forbiddenUserIdHeaders);
  }
}
