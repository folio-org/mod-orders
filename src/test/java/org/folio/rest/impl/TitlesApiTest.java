package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.impl.MockServer.TITLES_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Date;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.rest.acq.model.Title;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;

public class TitlesApiTest extends ApiTestBase {
  private static final Logger logger = LogManager.getLogger();

  public static final String TITLES_ENDPOINT = "/orders/titles";
  private static final String TITLES_ID_PATH = TITLES_ENDPOINT + "/%s";
  static final String VALID_UUID = "c3e26c0e-d6a6-46fb-9309-d494cd0c82de";
  static final String CONSISTENT_RECEIVED_STATUS_TITLE_UUID = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  static final String SAMPLE_TITLE_ID = "9a665b22-9fe5-4c95-b4ee-837a5433c95d";
  private JsonObject titleJsonReqData = getMockAsJson(TITLES_MOCK_DATA_PATH + "title.json");

  @Test
  public void testPostTitle() {
    logger.info("=== Test POST Title (Create Title) ===");

    String poLineId = UUID.randomUUID().toString();
    CompositePoLine poLine = getMinimalContentCompositePoLine()
      .withId(poLineId);

    addMockEntry(PO_LINES, JsonObject.mapFrom(poLine));

    Title postTitleRq = titleJsonReqData.mapTo(Title.class).withPoLineId(poLineId);

    // Positive cases
    // Title id is null initially
    assertThat(postTitleRq.getId(), nullValue());

    Title postTitleRs = verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(postTitleRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Title.class);

    // Title id not null
    assertThat(postTitleRs.getId(), Matchers.notNullValue());

    // Negative cases
    // Unable to create title test
    int status400 = HttpStatus.HTTP_BAD_REQUEST.toInt();
    verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(postTitleRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID,
      new Header(X_ECHO_STATUS, String.valueOf(status400))), APPLICATION_JSON, status400);

    // Internal error on mod-orders-storage test
    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(postTitleRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID,
      new Header(X_ECHO_STATUS, String.valueOf(status500))), APPLICATION_JSON, status500);

    addMockEntry(TITLES, JsonObject.mapFrom(postTitleRs));

    Errors errors = verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(postTitleRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON,
      HttpStatus.HTTP_UNPROCESSABLE_ENTITY.toInt()).as(
      Errors.class);

    assertEquals("titleExist", errors.getErrors().get(0).getCode());
  }

  @Test
  public void titleShouldBePopulatedFromPackagePoLine() {

    String packagePoLineId = UUID.randomUUID().toString();
    String packageTitleName = "test title name";
    String packageTestNumber = "test number";
    Date packageExpectedDate = new Date();
    String packageNote = "test note";

    CompositePoLine packagePoLine = getMinimalContentCompositePoLine()
      .withId(packagePoLineId)
      .withTitleOrPackage(packageTitleName)
      .withPoLineNumber(packageTestNumber)
      .withPhysical(new Physical().withExpectedReceiptDate(packageExpectedDate))
      .withDetails(new Details().withReceivingNote(packageNote))
      .withIsPackage(true);

    Title titleWithPackagePoLineRQ = titleJsonReqData.mapTo(Title.class)
      .withPoLineId(packagePoLineId);

    addMockEntry(PO_LINES, JsonObject.mapFrom(packagePoLine));

    Title titleWithPackagePoLineRS = verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(titleWithPackagePoLineRQ).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Title.class);

    assertEquals(titleWithPackagePoLineRS.getPackageName(), packageTitleName);
    assertEquals(titleWithPackagePoLineRS.getExpectedReceiptDate(), packageExpectedDate);
    assertEquals(titleWithPackagePoLineRS.getPoLineNumber(), packageTestNumber);
    assertEquals(titleWithPackagePoLineRS.getReceivingNote(), packageNote);
  }

  @Test
  public void testGetTitles() {
    logger.info("=== Test Get Titles  ===");

    final TitleCollection resp = verifySuccessGet(TITLES_ENDPOINT, TitleCollection.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(1, resp.getTitles().size());
  }

  @Test
  public void testGetTitleById() {
    logger.info("=== Test Get Title By Id ===");

    final Title resp = verifySuccessGet(String.format(TITLES_ID_PATH, SAMPLE_TITLE_ID), Title.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(SAMPLE_TITLE_ID, resp.getId());
  }

  @Test
  public void testPutTitlesByIdTest() throws Exception {
    logger.info("=== Test update title by id - valid Id 204 ===");

    String reqData = getMockData(TITLES_MOCK_DATA_PATH + "title.json");

    verifyPut(String.format(TITLES_ID_PATH, VALID_UUID), reqData, "", 204);
  }

  @Test
  public void testPutTitleWithoutPoLineTest() throws Exception {
    logger.info("=== Test update title without specified poline - 422 ===");

    String reqData = getMockData(TITLES_MOCK_DATA_PATH + "title_without_poLine.json");

    verifyPut(String.format(TITLES_ID_PATH, CONSISTENT_RECEIVED_STATUS_TITLE_UUID), reqData, "", 422);
  }

  @Test
  public void testPutTitlesByNonExistentId() {
    logger.info("=== Test update title by id - Id does not exists 404 ===");

    Title reqData = titleJsonReqData.mapTo(Title.class);
    reqData.setId(ID_DOES_NOT_EXIST);
    String jsonBody = JsonObject.mapFrom(reqData).encode();

    verifyPut(String.format(TITLES_ID_PATH, ID_DOES_NOT_EXIST), jsonBody, APPLICATION_JSON, 404);
  }

  @Test
  public void testPutTitlesWithError() {
    logger.info("=== Test update title by id - internal error from storage 500 ===");

    Title reqData = titleJsonReqData.mapTo(Title.class);
    reqData.setId(ID_FOR_INTERNAL_SERVER_ERROR);
    String jsonBody = JsonObject.mapFrom(reqData).encode();

    verifyPut(String.format(TITLES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), jsonBody, APPLICATION_JSON, 500);
  }

  @Test
  public void deleteTitleByIdTest() {
    logger.info("=== Test delete title by id ===");

    verifyDeleteResponse(String.format(TITLES_ID_PATH, VALID_UUID), "", 204);
  }

  @Test
  public void deleteTitlesByIdWithInvalidFormatTest() {
    logger.info("=== Test delete title by id - bad Id format 400 ===");
    verifyDeleteResponse(String.format(TITLES_ID_PATH, ID_BAD_FORMAT), TEXT_PLAIN, 400);
  }

  @Test
  public void deleteNotExistentTitleTest() {
    logger.info("=== Test delete title by id - id does not exists 404 ===");
    verifyDeleteResponse(String.format(TITLES_ID_PATH, ID_DOES_NOT_EXIST), APPLICATION_JSON, 404);
  }

  @Test
  public void deleteTitleInternalErrorOnStorageTest() {
    logger.info("=== Test delete title by id - internal error from storage 500 ===");
    verifyDeleteResponse(String.format(TITLES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), APPLICATION_JSON, 500);
  }
}
