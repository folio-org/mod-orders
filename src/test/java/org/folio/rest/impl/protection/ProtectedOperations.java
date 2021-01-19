package org.folio.rest.impl.protection;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;


import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.json.JsonObject;


enum ProtectedOperations {

  CREATE(201,  APPLICATION_JSON) {
    @Override
    Response process(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
      return verifyPostResponse(url, body, headers, expectedContentType, expectedCode);
    }
  },
  READ(200, APPLICATION_JSON) {
    @Override
    Response process(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
      JsonObject obj = new JsonObject(body);
      String id = obj.getString("id");
      return verifyGet(String.format("%s/%s", url, id), headers, expectedContentType, expectedCode);
      }
    },
  UPDATE(204) {
    @Override
    Response process(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
      JsonObject obj = new JsonObject(body);
      String id = obj.getString("id");
      return verifyPut(String.format("%s/%s", url, id), body, headers, expectedContentType, expectedCode);
    }
  },
  DELETE(204) {
    @Override
    Response process(String url, String body, Headers headers, String expectedContentType, int expectedCode) {
      JsonObject obj = new JsonObject(body);
      String id = obj.getString("id");
      return verifyDeleteResponse(String.format("%s/%s", url, id), headers, expectedContentType, expectedCode);
    }
  };

  private int code;
  private String contentType;

  ProtectedOperations(int code, String contentType) {
    this.code = code;
    this.contentType = contentType;
  }
  ProtectedOperations(int code) {
    this(code, "");
  }

  public int getCode() {
    return code;
  }

  public String getContentType() {
    return contentType;
  }

  abstract Response process(String url, String body, Headers headers, String expectedContentType, int expectedCode);


}
