package org.folio;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConstants.COMPOSITE_PO_LINES_PREFIX;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.NON_EXIST_CONFIG_X_OKAPI_TENANT;
import static org.folio.TestConstants.ORDERS_CHECKIN_ENDPOINT;
import static org.folio.TestConstants.ORDERS_RECEIVING_ENDPOINT;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestUtils.getModifiedProtectedFields;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.core.exceptions.ErrorCodes.PROHIBITED_FIELD_CHANGING;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.folio.orders.events.handlers.HandlersTestHelper;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.tools.parser.JsonPathParser;
import org.hamcrest.Matchers;

import io.restassured.RestAssured;
import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.json.JsonObject;

public class RestTestUtils {

  public static Response verifyPostResponse(String url, String body, Headers headers, String
          expectedContentType, int expectedCode) {
    Response response = RestAssured
            .with()
            .header(X_OKAPI_URL)
            .header(X_OKAPI_TOKEN)
            .headers(headers)
            .contentType(APPLICATION_JSON)
            .body(body)
            .post(url)
            .then()
            .log()
            .all()
            .statusCode(expectedCode)
            .contentType(expectedContentType)
            .extract()
            .response();

    // Verify no messages sent via event bus on POST (except receiving/check-in)
    if (!(url.startsWith(ORDERS_RECEIVING_ENDPOINT) || url.startsWith(ORDERS_CHECKIN_ENDPOINT))) {
      HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
    }

    return response;
  }

  public static Response verifyDeleteResponse(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
    return  RestAssured
      .with()
      .header(X_OKAPI_URL)
      .header(X_OKAPI_TOKEN)
      .headers(headers)
      .contentType(APPLICATION_JSON)
      .body(body)
      .when()
      .delete(url)
      .then()
      .statusCode(expectedCode)
      .contentType(expectedContentType)
      .extract()
      .response();
  }

  public static Response verifyPut(String url, JsonObject body, String expectedContentType, int expectedCode) {
    return verifyPut(url, body.encodePrettily(), expectedContentType, expectedCode);
  }

  public static Response verifyPut(String url, String body, String expectedContentType, int expectedCode) {
    return verifyPut(url, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), expectedContentType, expectedCode);
  }

  public static Response verifyPut(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
    Response response = RestAssured
            .with()
            .header(X_OKAPI_TOKEN)
            .header(X_OKAPI_URL)
            .headers(headers)
            .body(body)
            .contentType(APPLICATION_JSON)
            .put(url)
            .then()
            .statusCode(expectedCode)
            .contentType(expectedContentType)
            .extract()
            .response();

    // Verify no messages sent via event bus on PUT if there is an error
    if (expectedCode != 204) {
      HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
    }

    return response;
  }

  public static Response verifyPatch(String url, String body, String expectedContentType, int expectedCode) {
    return verifyPatch(url, body, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), expectedContentType, expectedCode);
  }
  public static Response verifyPatch(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
    Response response = RestAssured
        .with()
        .header(X_OKAPI_TOKEN)
        .header(X_OKAPI_URL)
        .headers(headers)
        .body(body)
        .contentType(APPLICATION_JSON)
        .patch(url)
        .then()
        .statusCode(expectedCode)
        .contentType(expectedContentType)
        .extract()
        .response();

    // Verify no messages sent via event bus
    if (expectedCode != 204) {
      HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
    }

    return response;
  }

  public static Response verifyGet(String url, String expectedContentType, int expectedCode) {
    Headers headers = prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT);
    return verifyGet(url, headers, expectedContentType, expectedCode);
  }

  public static Response verifyGet(String url, String expectedContentType, int expectedCode, String tenant) {
    Headers headers = prepareHeaders(new Header(OKAPI_HEADER_TENANT, tenant));
    return verifyGet(url, headers, expectedContentType, expectedCode);
  }

  public static Response verifyGet(String url, Headers headers, String expectedContentType, int expectedCode) {
    return RestAssured
            .with()
            .header(X_OKAPI_URL)
            .headers(headers)
            .get(url)
            .then()
            .statusCode(expectedCode)
            .contentType(expectedContentType)
            .extract()
            .response();
  }

  public static <T> T verifySuccessGet(String url, Class<T> clazz) {
    return verifyGet(url, APPLICATION_JSON, 200).as(clazz);
  }

  public static <T> T verifySuccessGet(String url, Class<T> clazz, String tenant) {
    return verifyGet(url, APPLICATION_JSON, 200, tenant).as(clazz);
  }

  public static Response verifyDeleteResponse(String url, String expectedContentType, int expectedCode) {
    Headers headers =  prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT);
    return verifyDeleteResponse(url, headers, expectedContentType, expectedCode);
  }

  public static Response verifyDeleteResponse(String url, Headers headers, String expectedContentType, int expectedCode) {
    Response response = RestAssured
            .with()
            .header(X_OKAPI_URL)
            .headers(headers)
            .delete(url)
            .then()
            .statusCode(expectedCode)
            .contentType(expectedContentType)
            .extract()
            .response();

    // Verify no messages sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(0);

    return response;
  }

  public static void checkPreventProtectedFieldsModificationRule(String path, JsonObject compPO, Map<String, Object> updatedFields) {
    JsonObject compPOJson = JsonObject.mapFrom(compPO);
    JsonPathParser compPOParser = new JsonPathParser(compPOJson);
    for (Map.Entry<String, Object> m : updatedFields.entrySet()) {
      compPOParser.setValueAt(m.getKey(), m.getValue());
    }
    Errors errors = verifyPut(String.format(path, compPO.getString("id")), compPOJson, "", HttpStatus.HTTP_BAD_REQUEST.toInt())
            .as(Errors.class);

    // Only one error expected
    assertThat(errors.getErrors(), hasSize(1));

    Error error = errors.getErrors()
            .get(0);
    assertThat(error.getCode(), equalTo(PROHIBITED_FIELD_CHANGING.getCode()));

    Object[] failedFieldNames = getModifiedProtectedFields(error);
    Object[] expected = updatedFields.keySet()
            .stream()
            .map(fieldName -> fieldName.replace(COMPOSITE_PO_LINES_PREFIX, StringUtils.EMPTY))
            .toArray();
    assertThat(failedFieldNames.length, is(expected.length));
    assertThat(expected, Matchers.arrayContainingInAnyOrder(failedFieldNames));
  }

  public static Headers prepareHeaders(Header... headers) {
    return new Headers(headers);
  }

  public static String buildQueryParam(String query) {
    return "?query=" + query;
  }
}
