package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.RestTestUtils.verifySuccessGet;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.ALL_DESIRED_ACQ_PERMISSIONS_HEADER;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ID_BAD_FORMAT;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.X_ECHO_STATUS;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestConstants.X_OKAPI_USER_ID_WITH_ACQ_UNITS;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.core.exceptions.ErrorCodes.*;
import static org.folio.rest.impl.MockServer.TITLES_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

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
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.model.TitleCollection;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.service.ProtectionService;
import org.folio.service.caches.InventoryCache;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.inventory.InventoryHoldingManager;
import org.folio.service.inventory.InventoryItemManager;
import org.folio.service.inventory.InventoryService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitleInstanceService;
import org.folio.service.titles.TitleValidationService;
import org.folio.service.titles.TitlesService;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

public class TitlesApiTest {
  private static final Logger logger = LogManager.getLogger();

  public static final String TITLES_ENDPOINT = "/orders/titles";
  private static final String TITLES_ID_PATH = TITLES_ENDPOINT + "/%s";
  private static final String VALID_UUID = "c3e26c0e-d6a6-46fb-9309-d494cd0c82de";
  private static final String CONSISTENT_RECEIVED_STATUS_TITLE_UUID = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  private static final String ACQ_UNIT_ID = "f6d2cc9d-82ca-437c-a4e6-e5c30323df00";
  public static final String SAMPLE_TITLE_ID = "9a665b22-9fe5-4c95-b4ee-837a5433c95d";
  public static final String SAMPLE_INSTANCE_ID = "8d1bc213-82ca-56fd-b4ec-238c5421a15c";
  private final JsonObject titleJsonReqData = getMockAsJson(TITLES_MOCK_DATA_PATH + "title.json");
  private final JsonObject packageTitleJsonReqData = getMockAsJson(TITLES_MOCK_DATA_PATH + "package_title.json");

  private static boolean runningOnOwn;

  @Autowired
  private TitleInstanceService titleInstanceService;
  @Autowired
  private ConsortiumConfigurationService consortiumConfigurationService;
  @Autowired
  private PieceStorageService pieceStorageService;
  private AutoCloseable mockitoMocks;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(TitlesApiTest.ContextConfiguration.class);
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
    mockitoMocks = MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
  }

  @Test
  void testPostTitle() {
    logger.info("=== Test POST Title (Create Title) ===");

    String poLineId = UUID.randomUUID().toString();
    CompositePoLine poLine = getMinimalContentCompositePoLine()
      .withId(poLineId);

    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));

    Title postTitleRq = titleJsonReqData.mapTo(Title.class)
      .withPoLineId(poLineId)
      .withAcqUnitIds(List.of(ACQ_UNIT_ID));

    doReturn(succeededFuture(SAMPLE_INSTANCE_ID)).when(titleInstanceService).getOrCreateInstance(any(Title.class), any(RequestContext.class));

    // Positive cases
    // Title id is null initially
    assertThat(postTitleRq.getId(), nullValue());

    Title postTitleRs = verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(postTitleRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID_WITH_ACQ_UNITS, ALL_DESIRED_ACQ_PERMISSIONS_HEADER), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Title.class);

    // Title id not null
    assertThat(postTitleRs.getId(), Matchers.notNullValue());

    // Negative cases
    // Unable to create title test
    int status400 = HttpStatus.HTTP_BAD_REQUEST.toInt();
    verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(postTitleRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID_WITH_ACQ_UNITS, ALL_DESIRED_ACQ_PERMISSIONS_HEADER,
      new Header(X_ECHO_STATUS, String.valueOf(status400))), APPLICATION_JSON, status400);

    // Internal error on mod-orders-storage test
    int status500 = HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt();
    verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(postTitleRq).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID_WITH_ACQ_UNITS, ALL_DESIRED_ACQ_PERMISSIONS_HEADER,
      new Header(X_ECHO_STATUS, String.valueOf(status500))), APPLICATION_JSON, status500);
  }

  @Test
  void testPostTitleShouldFail() {
    logger.info("=== Test POST Title should fail because it does not have permission ===");

    Title postTitleRq = titleJsonReqData.mapTo(Title.class);
    postTitleRq.setAcqUnitIds(List.of(ACQ_UNIT_ID));

    // Positive cases
    // Title id is null initially
    assertThat(postTitleRq.getId(), nullValue());

    List<Error> errors = verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(postTitleRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 403)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(USER_HAS_NO_ACQ_PERMISSIONS.getDescription()));
  }

  @Test
  void postTitleWithInvalidClaimingConfig() throws Exception {
    logger.info("=== Test update title by id - claiming config is not valid 422 ===");

    String reqData = getMockData(TITLES_MOCK_DATA_PATH + "title_invalid_claiming_config.json");

    List<Error> errors = verifyPostResponse(TITLES_ENDPOINT, reqData, prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, ALL_DESIRED_ACQ_PERMISSIONS_HEADER), APPLICATION_JSON, 422)
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

    doReturn(succeededFuture(SAMPLE_INSTANCE_ID)).when(titleInstanceService).getOrCreateInstance(any(Title.class), any(RequestContext.class));

    Title titleWithPackagePoLineRS = verifyPostResponse(TITLES_ENDPOINT, JsonObject.mapFrom(titleWithPackagePoLineRQ).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, ALL_DESIRED_ACQ_PERMISSIONS_HEADER), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt()).as(Title.class);

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

    assertEquals(7, resp.getTitles().size());
  }

  @Test
  void testGetTitleById() {
    logger.info("=== Test Get Title By Id ===");

    final Title resp = verifySuccessGet(String.format(TITLES_ID_PATH, SAMPLE_TITLE_ID), Title.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(SAMPLE_TITLE_ID, resp.getId());
  }

  @Test
  void testPutTitlesByIdTest() {
    logger.info("=== Test update title by id - valid Id 204 ===");
    Title reqData = titleJsonReqData.mapTo(Title.class)
      .withId(SAMPLE_TITLE_ID)
      .withTitle("new title")
      .withAcqUnitIds(List.of(ACQ_UNIT_ID));

    doReturn(succeededFuture(SAMPLE_INSTANCE_ID)).when(titleInstanceService).getOrCreateInstance(any(Title.class), any(RequestContext.class));

    verifyPut(String.format(TITLES_ID_PATH, SAMPLE_TITLE_ID), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, ALL_DESIRED_ACQ_PERMISSIONS_HEADER), "", 204);
  }

  @Test
  void testPutTitlesByIdTestShouldFail() {
    logger.info("=== Test update Title by id should fail because it does not have permission ===");
    Title reqData = titleJsonReqData.mapTo(Title.class)
      .withId(SAMPLE_TITLE_ID)
      .withTitle("new title")
      .withAcqUnitIds(List.of(ACQ_UNIT_ID));

    List<Error> errors = verifyPut(String.format(TITLES_ID_PATH, SAMPLE_TITLE_ID), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 403)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(USER_HAS_NO_ACQ_PERMISSIONS.getDescription()));
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
    logger.info("=== Test Delete title from package by id ===");

    String titleId = UUID.randomUUID().toString();
    String poLineId = UUID.randomUUID().toString();
    String orderId = UUID.randomUUID().toString();
    String deleteHoldings = "false";

    var title = titleJsonReqData.mapTo(Title.class)
      .withId(titleId)
      .withPoLineId(poLineId);

    var poLine = getMinimalContentCompositePoLine()
      .withId(poLineId)
      .withPurchaseOrderId(orderId)
      .withIsPackage(true);

    addMockEntry(TITLES, JsonObject.mapFrom(title));
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));

    when(consortiumConfigurationService.isCentralOrderingEnabled(any())).thenReturn(succeededFuture(false));
    when(pieceStorageService.getPiecesByLineIdAndTitleId(any(), any(), any())).thenReturn(succeededFuture(List.of()));

    verifyDeleteResponse(String.format(TITLES_ID_PATH + "?deleteHoldings=%s", titleId, deleteHoldings), "", 204);
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

  static class ContextConfiguration {

    @Bean
    RestClient restClient(){
      return new RestClient();
    }

    @Bean
    AcquisitionsUnitsService acquisitionsUnitsService(RestClient restClient) {
      return new AcquisitionsUnitsService(restClient);
    }

    @Bean
    public InventoryItemManager inventoryItemManager() {
      return mock(InventoryItemManager.class);
    }

    @Bean
    public InventoryHoldingManager inventoryHoldingManager() {
      return mock(InventoryHoldingManager.class);
    }

    @Bean
    public InventoryService inventoryService() {
      return new InventoryService(restClient());
    }

    @Bean
    public InventoryCache inventoryCache() {
      return new InventoryCache(inventoryService());
    }

    @Bean PurchaseOrderLineService purchaseOrderLineService() {
      return new PurchaseOrderLineService(restClient(), inventoryHoldingManager());
    }

    @Bean
    public  ConsortiumConfigurationService consortiumConfigurationService() {
      return mock(ConsortiumConfigurationService.class);
    }

    @Bean
    public PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean
    TitlesService titlesService(RestClient restClient, ProtectionService protectionService, TitleInstanceService titleInstanceService,
                                InventoryHoldingManager inventoryHoldingManager, InventoryItemManager inventoryItemManager,
                                PurchaseOrderLineService purchaseOrderLineService, PieceStorageService pieceStorageService,
                                ConsortiumConfigurationService consortiumConfigurationService) {
      return new TitlesService(restClient, protectionService, titleInstanceService, inventoryHoldingManager, inventoryItemManager,
        purchaseOrderLineService, pieceStorageService, consortiumConfigurationService);
    }

    @Bean
    TitleValidationService titleValidationService() {
      return new TitleValidationService();
    }

    @Bean
    TitleInstanceService TitleInstanceService() {
      return mock(TitleInstanceService.class);
    }

    @Bean
    ProtectionService protectionService(AcquisitionsUnitsService acquisitionsUnitsService) {
      return spy(new ProtectionService(acquisitionsUnitsService));
    }

  }
}
