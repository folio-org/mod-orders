package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.RestTestUtils.verifySuccessGet;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ID_BAD_FORMAT;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.X_ECHO_STATUS;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.rest.core.exceptions.ErrorCodes.CLAIMING_CONFIG_INVALID;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.rest.impl.MockServer.TITLES_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.config.ApplicationConfig;
import org.folio.rest.acq.model.Title;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

public class TitlesApiTest {
  private static final Logger logger = LogManager.getLogger();

  public static final String TITLES_ENDPOINT = "/orders/titles";
  private static final String TITLES_ID_PATH = TITLES_ENDPOINT + "/%s";
  static final String VALID_UUID = "c3e26c0e-d6a6-46fb-9309-d494cd0c82de";
  static final String CONSISTENT_RECEIVED_STATUS_TITLE_UUID = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  public static final String SAMPLE_TITLE_ID = "9a665b22-9fe5-4c95-b4ee-837a5433c95d";
  private final JsonObject titleJsonReqData = getMockAsJson(TITLES_MOCK_DATA_PATH + "title.json");
  private final JsonObject packageTitleJsonReqData = getMockAsJson(TITLES_MOCK_DATA_PATH + "package_title.json");

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

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void testPostTitle() {
    logger.info("=== Test POST Title (Create Title) ===");

    String poLineId = UUID.randomUUID().toString();
    CompositePoLine poLine = getMinimalContentCompositePoLine()
      .withId(poLineId);

    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));

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
  }

  @Test
  void postTitleWithInvalidClaimingConfig() throws Exception {
    logger.info("=== Test update title by id - claiming config is not valid 422 ===");

    String reqData = getMockData(TITLES_MOCK_DATA_PATH + "title_invalid_claiming_config.json");

    List<Error> errors = verifyPostResponse(TITLES_ENDPOINT, reqData, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(CLAIMING_CONFIG_INVALID.getDescription()));
  }

  @Test
  void titleShouldBePopulatedFromPackagePoLine() {

    String packagePoLineId = UUID.randomUUID().toString();
    String packageTitleName = "test title name";
    String polNumbber = "1000-01";
    String packageNote = "test note";

    CompositePoLine packagePoLine = getMinimalContentCompositePoLine()
      .withId(packagePoLineId)
      .withTitleOrPackage(packageTitleName)
      .withPoLineNumber(polNumbber)
      .withPhysical(new Physical().withExpectedReceiptDate(new Date()))
      .withDetails(new Details().withReceivingNote(packageNote))
      .withIsPackage(true);

    Title titleWithPackagePoLineRQ = packageTitleJsonReqData.mapTo(Title.class)
      .withPoLineId(packagePoLineId);

    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(packagePoLine));

    Title titleWithPackagePoLineRS = verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(titleWithPackagePoLineRQ).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Title.class);

    assertEquals(titleWithPackagePoLineRS.getPackageName(), packageTitleName);
    assertNotNull(titleWithPackagePoLineRS.getExpectedReceiptDate());
    assertEquals(titleWithPackagePoLineRS.getPoLineNumber(), polNumbber);
    assertEquals(titleWithPackagePoLineRS.getReceivingNote(), packageNote);
  }

  @Test
  void testGetTitles() {
    logger.info("=== Test Get Titles  ===");

    final TitleCollection resp = verifySuccessGet(TITLES_ENDPOINT, TitleCollection.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(1, resp.getTitles().size());
  }

  @Test
  void testGetTitleById() {
    logger.info("=== Test Get Title By Id ===");

    final Title resp = verifySuccessGet(String.format(TITLES_ID_PATH, SAMPLE_TITLE_ID), Title.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(SAMPLE_TITLE_ID, resp.getId());
  }

  @Test
  void testPutTitlesByIdTest() throws Exception {
    logger.info("=== Test update title by id - valid Id 204 ===");

    String reqData = getMockData(TITLES_MOCK_DATA_PATH + "title.json");

    verifyPut(String.format(TITLES_ID_PATH, VALID_UUID), reqData, "", 204);
  }

  @Test
  void testPutTitleWithoutPoLineTest() throws Exception {
    logger.info("=== Test update title without specified poline - 422 ===");

    String reqData = getMockData(TITLES_MOCK_DATA_PATH + "title_without_poLine.json");

    verifyPut(String.format(TITLES_ID_PATH, CONSISTENT_RECEIVED_STATUS_TITLE_UUID), reqData, "", 422);
  }

  @Test
  void testPutTitlesByNonExistentId() {
    logger.info("=== Test update title by id - Id does not exists 404 ===");

    Title reqData = titleJsonReqData.mapTo(Title.class);
    reqData.setId(ID_DOES_NOT_EXIST);
    String jsonBody = JsonObject.mapFrom(reqData).encode();

    verifyPut(String.format(TITLES_ID_PATH, ID_DOES_NOT_EXIST), jsonBody, APPLICATION_JSON, 404);
  }

  @Test
  void testPutTitlesWithError() {
    logger.info("=== Test update title by id - internal error from storage 500 ===");

    Title reqData = titleJsonReqData.mapTo(Title.class);
    reqData.setId(ID_FOR_INTERNAL_SERVER_ERROR);
    String jsonBody = JsonObject.mapFrom(reqData).encode();

    verifyPut(String.format(TITLES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), jsonBody, APPLICATION_JSON, 500);
  }

  @Test
  void putTitleWithInvalidClaimingConfig() throws Exception {
    logger.info("=== Test update title by id - claiming config is not valid 422 ===");

    String reqData = getMockData(TITLES_MOCK_DATA_PATH + "title_invalid_claiming_config.json");

    List<Error> errors = verifyPut(String.format(TITLES_ID_PATH, VALID_UUID), reqData, APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(CLAIMING_CONFIG_INVALID.getDescription()));
  }

  @Test
  void putTitleWithMismatchIdBetweenParamAndBody() throws Exception {
    logger.info("=== Test update title by id - id is mismatched between param and body 422 ===");

    String reqData = getMockData(TITLES_MOCK_DATA_PATH + "title_invalid_claiming_config.json");
    String randomId = UUID.randomUUID().toString();

    List<Error> errors = verifyPut(String.format(TITLES_ID_PATH, randomId), reqData, APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getDescription()));
  }

  @Test
  void deleteTitleByIdTest() {
    logger.info("=== Test delete title by id ===");

    verifyDeleteResponse(String.format(TITLES_ID_PATH, VALID_UUID), "", 204);
  }

  @Test
  void deleteTitlesByIdWithInvalidFormatTest() {
    logger.info("=== Test delete title by id - bad Id format 400 ===");
    verifyDeleteResponse(String.format(TITLES_ID_PATH, ID_BAD_FORMAT), TEXT_PLAIN, 400);
  }

  @Test
  void deleteNotExistentTitleTest() {
    logger.info("=== Test delete title by id - id does not exists 404 ===");
    verifyDeleteResponse(String.format(TITLES_ID_PATH, ID_DOES_NOT_EXIST), APPLICATION_JSON, 404);
  }

  @Test
  void deleteTitleInternalErrorOnStorageTest() {
    logger.info("=== Test delete title by id - internal error from storage 500 ===");
    verifyDeleteResponse(String.format(TITLES_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), APPLICATION_JSON, 500);
  }
}
