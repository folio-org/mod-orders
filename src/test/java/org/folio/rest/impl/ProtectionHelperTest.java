package org.folio.rest.impl;


import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import org.folio.HttpStatus;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.junit.Test;


import java.util.Arrays;
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

  private static final String SUPER_AND_FULL_PROTECTED_USER_ID = "5526f491-c836-4ed3-9427-785f6513c623";
  private static final String SUPER_AND_CREATE_ONLY_USER_ID = "8661f687-46f1-41d1-ac48-5034f3251c7f";
  private static final String FULL_PROTECTED_USER_ID = "147c36f9-35d1-43bd-ad62-aaa490d0d42c";
  private static final String FULL_PROTECTED_AND_DELETE_ONLY_USER_ID = "71e62768-d7c4-498f-bbc6-c2ac9e1c9a11";

  private static final Header X_OKAPI_SUPER_AND_FULL_PROTECTED_USER_ID = new Header(OKAPI_USERID_HEADER, SUPER_AND_FULL_PROTECTED_USER_ID);
  private static final Header X_OKAPI_SUPER_AND_CREATE_ONLY_USER_ID = new Header(OKAPI_USERID_HEADER, SUPER_AND_CREATE_ONLY_USER_ID);
  private static final Header X_OKAPI_FULL_PROTECTED_USER_ID = new Header(OKAPI_USERID_HEADER, FULL_PROTECTED_USER_ID);
  private static final Header X_OKAPI_FULL_PROTECTED_AND_DELETE_ONLY_USER_ID = new Header(OKAPI_USERID_HEADER, FULL_PROTECTED_AND_DELETE_ONLY_USER_ID);

  static final Header[] FORBIDDEN_CREATION_HEADERS = {X_OKAPI_FULL_PROTECTED_USER_ID, X_OKAPI_FULL_PROTECTED_AND_DELETE_ONLY_USER_ID};
  static final Header[] ALLOWED_CREATION_HEADERS = {X_OKAPI_SUPER_AND_CREATE_ONLY_USER_ID, X_OKAPI_SUPER_AND_FULL_PROTECTED_USER_ID};


  @Test
  public void testWithoutUserIdHeader() {
    // Request without user id header - expecting of call only to Assignments API
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities holder : ProtectedEntities.values()) {
        operation.process(holder.getEndpoint(), holder.getSampleForNormalFlow(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 403);
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void testNumberOfRequestsForOrderWithUnits() {
    // Order with units - expecting of calls to Units, Memberships, Assignments API
    for(Operations operation : Arrays.asList(Operations.CREATE)) {
      for(ProtectedEntities holder : Arrays.asList(ProtectedEntities.PIECES, ProtectedEntities.ORDER_LINES)) {
        operation.process(holder.getEndpoint(), holder.getSampleForNormalFlow(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201);
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), hasSize(1));
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void testNumberOfRequestsForOrderWithoutUnits() {
    // Order without units - expecting of call only to Assignments API
    for(Operations operation : Arrays.asList(Operations.CREATE)) {
      for(ProtectedEntities holder : Arrays.asList(ProtectedEntities.ORDER_LINES)) {
        operation.process(holder.getEndpoint(), holder.getSampleWithOrderWithoutUnits(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201);
        assertThat(MockServer.serverRqRs.get("acquisitionsUnits", HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get("acquisitionsMemberships", HttpMethod.GET), nullValue());
        assertThat(MockServer.serverRqRs.get("acquisitionsUnitAssignments", HttpMethod.GET), hasSize(1));
        MockServer.serverRqRs.clear();
      }
    }
  }

  @Test
  public void testOperationsProtection() {
    for(Operations operation : Operations.values()) {
      for(ProtectedEntities entities : ProtectedEntities.values()) {
        entities.getAllowedUserIdHeadersStream().forEach(header -> operation.process(entities.getEndpoint(), entities.getSampleForNormalFlow(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()));
        entities.getForbiddenUserIdHeadersStream().forEach(header -> operation.process(entities.getEndpoint(), entities.getSampleForNormalFlow(),
          prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, header), APPLICATION_JSON, HttpStatus.HTTP_FORBIDDEN.toInt()));
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

  public String getSampleForNormalFlow() {
    return sample;
  }

  public String getSampleWithOrderWithoutUnits() {
    switch (this) {
      case ORDER_LINES:
        CompositePoLine poLine = new JsonObject(getSampleForNormalFlow()).mapTo(CompositePoLine.class);
        poLine.setPurchaseOrderId("1ab7ef6a-d1d4-4a4f-90a2-882aed18af14");
        return JsonObject.mapFrom(poLine).encode();
      default: return null;
    }
  }

  public Stream<Header> getAllowedUserIdHeadersStream() {
    return Arrays.stream(allowedUserIdHeaders);
  }

  public Stream<Header> getForbiddenUserIdHeadersStream() {
    return Arrays.stream(forbiddenUserIdHeaders);
  }
}
