package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.RestTestUtils.checkPreventProtectedFieldsModificationRule;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.RestTestUtils.verifySuccessGet;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.ACTIVE_ACCESS_PROVIDER_B;
import static org.folio.TestConstants.BAD_QUERY;
import static org.folio.TestConstants.COMP_ORDER_MOCK_DATA_PATH;
import static org.folio.TestConstants.EMPTY_CONFIG_X_OKAPI_TENANT;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ID_BAD_FORMAT;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.INACTIVE_ACCESS_PROVIDER_A;
import static org.folio.TestConstants.INVALID_LANG;
import static org.folio.TestConstants.NON_EXIST_CONFIG_X_OKAPI_TENANT;
import static org.folio.TestConstants.PO_ID_CLOSED_STATUS;
import static org.folio.TestConstants.PO_ID_OPEN_STATUS;
import static org.folio.TestConstants.PO_ID_PENDING_STATUS_WITH_PO_LINES;
import static org.folio.TestConstants.PO_LINE_ID_FOR_SUCCESS_CASE;
import static org.folio.TestConstants.PO_LINE_NUMBER_VALUE;
import static org.folio.TestConstants.PROTECTED_READ_ONLY_TENANT;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.TestUtils.validatePoLineCreationErrorForNonPendingOrder;
import static org.folio.TestUtils.verifyLocationQuantity;
import static org.folio.orders.utils.ErrorCodes.COST_ADDITIONAL_COST_INVALID;
import static org.folio.orders.utils.ErrorCodes.COST_DISCOUNT_INVALID;
import static org.folio.orders.utils.ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID;
import static org.folio.orders.utils.ErrorCodes.COST_UNIT_PRICE_INVALID;
import static org.folio.orders.utils.ErrorCodes.ELECTRONIC_COST_LOC_QTY_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.INACTIVE_EXPENSE_CLASS;
import static org.folio.orders.utils.ErrorCodes.INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE;
import static org.folio.orders.utils.ErrorCodes.ISBN_NOT_VALID;
import static org.folio.orders.utils.ErrorCodes.LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN;
import static org.folio.orders.utils.ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY;
import static org.folio.orders.utils.ErrorCodes.NON_ZERO_COST_PHYSICAL_QTY;
import static org.folio.orders.utils.ErrorCodes.ORDER_CLOSED;
import static org.folio.orders.utils.ErrorCodes.ORDER_OPEN;
import static org.folio.orders.utils.ErrorCodes.PHYSICAL_COST_LOC_QTY_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.POL_LINES_LIMIT_EXCEEDED;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_ELECTRONIC_QTY;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_PHYSICAL_QTY;
import static org.folio.orders.utils.ErrorCodes.ZERO_LOCATION_QTY;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.ENCUMBRANCES;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.TRANSACTIONS_STORAGE_ENDPOINT;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ORDER_ID_WITH_PO_LINES;
import static org.folio.rest.impl.MockServer.PO_NUMBER_ERROR_X_OKAPI_TENANT;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.getOrderLineSearches;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.folio.rest.impl.MockServer.getRqRsEntries;
import static org.folio.rest.impl.MockServer.getTitlesSearches;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ID_FOR_PRINT_MONOGRAPH_ORDER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.INACTIVE_EXPENSE_CLASS_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.PURCHASE_ORDER_ID;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.INSTANCE_HOLDING_ITEM;
import static org.folio.rest.jaxrs.model.Eresource.CreateInventory.NONE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.endsWith;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.orders.events.handlers.HandlersTestHelper;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.POLineProtectedFields;
import org.folio.rest.acq.model.Title;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.Tags;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class PurchaseOrderLinesApiTest {

  private static final Logger logger = LogManager.getLogger();

  private static final String PO_LINE_ID_WITH_SOME_SUB_OBJECTS_ALREADY_REMOVED = "0009662b-8b80-4001-b704-ca10971f175d";
  static final String ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  public final static String LINES_PATH = "/orders/order-lines";
  public static final String LINE_BY_ID_PATH = "/orders/order-lines/%s";
  public static final String COMP_PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String PO_LINE_MIN_CONTENT_PATH = COMP_PO_LINES_MOCK_DATA_PATH + "minimalContent.json";
  public static final String ISBN_PRODUCT_TYPE_ID = "8261054f-be78-422d-bd51-4ed9f33c3422";
  public static final String INVALID_ISBN = "1234";
  private static final String ACQUISITIONS_UNIT_IDS = "acqUnitIds";
  private static final String NO_ACQ_UNIT_ASSIGNED_CQL = "cql.allRecords=1 not " + ACQUISITIONS_UNIT_IDS + " <> []";

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

  @Test
  void testPostOrderLine() {
    logger.info("=== Test Post Order Line (expected flow) ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    reqData.getCost().setPoLineEstimatedPrice(null);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");

    final CompositePoLine response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePoLine.class);

    assertThat(response.getPurchaseOrderId(), equalTo(reqData.getPurchaseOrderId()));
    assertThat(response.getInstanceId(), nullValue());

    JsonObject purchaseOrderOwner = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, reqData.getPurchaseOrderId());
    String expectedPoLineNumber = purchaseOrderOwner.getString(PO_NUMBER) + "-" + PO_LINE_NUMBER_VALUE;
    assertThat(response.getPoLineNumber(), equalTo(expectedPoLineNumber));
    // See MODORDERS-180
    assertThat(response.getCost().getPoLineEstimatedPrice(), equalTo(49.98d));
    Location location = response.getLocations().get(0);
    verifyLocationQuantity(location, response.getOrderFormat());
  }

  @Test
  void testPostOrdersLinePhysicalFormatIncorrectQuantity() {
    logger.info("=== Test Post Physical Order Line - incorrect quantity ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE);
    reqData.getCost().setQuantityPhysical(0);
    reqData.getCost().setQuantityElectronic(1);
    reqData.getLocations().get(0).setQuantityPhysical(1);
    reqData.getLocations().add(new Location()
      .withQuantityPhysical(0)
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    final Errors response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(4));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_PHYSICAL_QTY.getCode(),
      NON_ZERO_COST_ELECTRONIC_QTY.getCode(),
      PHYSICAL_COST_LOC_QTY_MISMATCH.getCode(),
      ZERO_LOCATION_QTY.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  void testPostOrdersLineElectronicFormatIncorrectQuantity() {
    logger.info("=== Test Post Electronic Order Line - incorrect quantity ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    reqData.getCost().setListUnitPrice(null);
    reqData.getCost().setListUnitPriceElectronic(0d);
    reqData.getCost().setQuantityPhysical(0);
    reqData.getCost().setQuantityElectronic(4);
    reqData.getLocations().get(0).setQuantityElectronic(3);
    reqData.getLocations().add(new Location()
      .withQuantityElectronic(0)
      .withLocationId(reqData.getLocations().get(0).getLocationId()));
    Eresource eresource = new  Eresource();
    eresource.setCreateInventory(NONE);
    reqData.setEresource(eresource);

    final Errors response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(3));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(ELECTRONIC_COST_LOC_QTY_MISMATCH.getCode(),
      PHYSICAL_COST_LOC_QTY_MISMATCH.getCode(),
      ZERO_LOCATION_QTY.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  void testPostOrdersLinePhysicalFormatDiscount() {
    logger.info("=== Test Post Physical Order Line - discount exceeds total price ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE);
    reqData.getCost().setQuantityPhysical(2);
    reqData.getCost().setListUnitPrice(10d);
    reqData.getCost().setDiscount(100d);
    reqData.getCost().setDiscountType(Cost.DiscountType.AMOUNT);
    reqData.getCost().setQuantityElectronic(0);
    reqData.getLocations().get(0).setQuantityPhysical(2);

    final Errors response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    assertThat(response.getErrors().get(0).getCode(), equalTo(COST_DISCOUNT_INVALID.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  void testPostWithEmptyConfigOverLimit() {
    logger.info("=== Test PO creation fail with default limit ===");

    JsonObject compPoLineJson = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final Errors errors = verifyPostResponse(LINES_PATH, compPoLineJson.encodePrettily(),
      prepareHeaders(EMPTY_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  void testPoLineCreationIfPoAlreadyReachedLimit() {
    logger.info("=== Test PO Line over limit creation ===");
    JsonObject compPoLineJson = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final Errors errors = verifyPostResponse(LINES_PATH, compPoLineJson.encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1), APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  void testPostOrdersLinesByIdPoLineWithoutId() throws IOException {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");

    Errors resp = verifyPostResponse(LINES_PATH, getMockData(PO_LINE_MIN_CONTENT_PATH),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(ErrorCodes.MISSING_ORDER_ID_IN_POL.getCode(), resp.getErrors().get(0).getCode());
  }

  @Test
  void testPostOrdersLinesByIdPoLineWithNonExistPurchaseOrder() {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");
    JsonObject reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);
    reqData.put(PURCHASE_ORDER_ID, ID_DOES_NOT_EXIST);

    Errors resp = verifyPostResponse(LINES_PATH, reqData.encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(ErrorCodes.ORDER_NOT_FOUND.getCode(), resp.getErrors().get(0).getCode());
  }

  @Test
  void testAddOrderLineToOrderInClosedStatus() {
    logger.info("=== Test add new line to Closed order ===");

    validateNoLineCreatedForNonPendingOrder(PO_ID_CLOSED_STATUS, ORDER_CLOSED.getCode());
  }

  @Test
  void testAddOrderLineToOrderInOpenStatus() {
    logger.info("=== Test add new line to Open order ===");

    validateNoLineCreatedForNonPendingOrder(PO_ID_OPEN_STATUS, ORDER_OPEN.getCode());
  }

  private void validateNoLineCreatedForNonPendingOrder(String orderId, String errorCode) {
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, orderId).mapTo(CompositePurchaseOrder.class);

    CompositePoLine poLine = order.getCompositePoLines().get(0);
    poLine.setId(null);

    Errors errors = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(poLine).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);
    validatePoLineCreationErrorForNonPendingOrder(errorCode, errors, 4);
  }

  @Test
  void testCreatePoLineWithGetPoLineNumberError() throws IOException {
    logger.info("=== Test Create PO Line - fail on PO Line number generation ===");
    JsonObject json = new JsonObject(getMockData(String.format("%s%s.json", COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_FOR_SUCCESS_CASE)));
    CompositePoLine poLine = json.mapTo(CompositePoLine.class);
    poLine.setId("0009662b-8b80-4001-b704-ca10971f175d");
    poLine.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");
    verifyPostResponse(LINES_PATH, JsonObject.mapFrom(poLine).encode(), prepareHeaders(PO_NUMBER_ERROR_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 500);
  }

  @Test
  void testPutOrderLineElectronicFormatIncorrectQuantityAndPrice() {
    logger.info("=== Test Put Electronic Order Line - incorrect quantity and Price ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    reqData.setId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    Cost cost = reqData.getCost();
    cost.setQuantityPhysical(1);
    cost.setQuantityElectronic(0);
    cost.setListUnitPriceElectronic(-1d);
    cost.setListUnitPrice(1d);
    cost.setAdditionalCost(-1d);
    cost.setDiscountType(Cost.DiscountType.PERCENTAGE);
    cost.setDiscount(100.1d);
    reqData.getLocations().get(0).setQuantityElectronic(1);

    final Errors response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData),
      APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(8));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_ELECTRONIC_QTY.getCode(),
      NON_ZERO_COST_PHYSICAL_QTY.getCode(),
      COST_UNIT_PRICE_INVALID.getCode(),
      COST_UNIT_PRICE_ELECTRONIC_INVALID.getCode(),
      COST_ADDITIONAL_COST_INVALID.getCode(),
      COST_DISCOUNT_INVALID.getCode(),
      ELECTRONIC_COST_LOC_QTY_MISMATCH.getCode(),
      PHYSICAL_COST_LOC_QTY_MISMATCH.getCode()));


    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  void testValidationOnPutLineWithoutBody() {
    logger.info("=== Test validation on PUT line with no body ===");
    verifyPut(String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST), "", TEXT_PLAIN, 400);
  }

  @Test
  void testValidationOnPutWithIncorrectLang() throws IOException {
    logger.info("=== Test validation on PUT line with invalid lang query parameter ===");
    verifyPut(String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST) + INVALID_LANG, getMockData(PO_LINE_MIN_CONTENT_PATH), TEXT_PLAIN, 400);
  }

  @Test
  void testValidationOnPutWithIncorrectLineId() throws IOException {
    logger.info("=== Test validation on PUT line with invalid line ID path parameter ===");
    verifyPut(String.format(LINE_BY_ID_PATH, ID_BAD_FORMAT), getMockData(PO_LINE_MIN_CONTENT_PATH), TEXT_PLAIN, 400);
  }

  @Test
  void testPutOrderLineByIdWithEmptyBody() throws IOException {
    logger.info("=== Test PUT Order Line By Id With Empty Json - validation error because of required properties like PO ID ===");

    String url = String.format(LINE_BY_ID_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);

    Errors resp = verifyPut(url, getMockData(PO_LINE_MIN_CONTENT_PATH), "", 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
  }

  @Test
  void testPutOrderLineById() {
    logger.info("=== Test PUT Order Line By Id - Success case ===");

    String lineId = PO_LINE_ID_FOR_SUCCESS_CASE;
    CompositePoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(CompositePoLine.class);

    // See MODORDERS-180
    Cost cost = body.getCost();
    cost.setDiscount(10d);
    cost.setDiscountType(Cost.DiscountType.PERCENTAGE);
    cost.setListUnitPrice(9.99d);
    // set wrong estimated price
    cost.setPoLineEstimatedPrice(100d);
    double expectedTotalPoLine = 8.99d;

    String url = String.format(LINE_BY_ID_PATH, lineId);

    final Response resp = verifyPut(url, JsonObject.mapFrom(body), "", 204);

    assertTrue(StringUtils.isEmpty(resp.getBody().asString()));

    //3 calls to get Order Line,Purchase Order for checking workflow status and ISBN validation
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(3, column.size());
    assertThat(column, hasKey(PO_LINES));
    assertThat(column, not(hasKey(PIECES)));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertEquals(1, column.size());
    assertThat(column.keySet(), containsInAnyOrder(REPORTING_CODES));
    assertThat(column.get(REPORTING_CODES), hasSize(3));

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertEquals(1, column.size());
    assertThat(column.keySet(), containsInAnyOrder(PO_LINES));

    // See MODORDERS-180
    PoLine poLine = column.get(PO_LINES).get(0).mapTo(PoLine.class);
    assertThat(poLine.getCost().getPoLineEstimatedPrice(), equalTo(expectedTotalPoLine));
    Location location = poLine.getLocations().get(0);
    assertEquals(location.getQuantityPhysical(), location.getQuantity());

    // Verify messages sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testPutOrderLineWithTagInheritance() {
    logger.info("=== Test PUT Order Line With Tag Inheritance ===");
    String lineId = "bb66b269-76ed-4616-8da9-730d9b817247";
    CompositePoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(CompositePoLine.class);
    body.setCheckinItems(false);
    body.setIsPackage(false);
    body.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
    body.setReportingCodes(new ArrayList<>());
    MockServer.addMockEntry(PO_LINES, body);
    MockServer.addMockEntry(PURCHASE_ORDER, new CompositePurchaseOrder()
      .withId(ID_FOR_PRINT_MONOGRAPH_ORDER)
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN)
      .withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME));
    String url = String.format(LINE_BY_ID_PATH, lineId);
    addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(body.getId())
      .withTitle("Title"));

    // edit POLine for new encumbrance
    body.setTags(new Tags().withTagList(Collections.singletonList("created")));
    body.setFundDistribution(Collections.singletonList(new FundDistribution()
      .withCode("EUROHIST")
      .withFundId("e9285a1c-1dfc-4380-868c-e74073003f43")
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D)));
    verifyPut(url, JsonObject.mapFrom(body), "", 204);

    Transaction createdEncumbrance = MockServer.getCreatedEncumbrances().get(0);
    assertEquals(Collections.singletonList("created"), createdEncumbrance.getTags().getTagList());

    // edit POLine for encumbrance update
    body.setTags(new Tags().withTagList(Collections.singletonList("updated")));
    body.setCost(new Cost().withCurrency("USD").withListUnitPrice(70.0).withQuantityPhysical(1));
    body.setFundDistribution(Collections.singletonList(new FundDistribution()
      .withCode("EUROHIST")
      .withFundId("a89eccf0-57a6-495e-898d-32b9b2210f2f")
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D)));
    verifyPut(url, JsonObject.mapFrom(body), "", 204);
    Transaction updatedEncumbrance = MockServer.getCreatedEncumbrances().get(1);
    assertEquals(Collections.singletonList("updated"), updatedEncumbrance.getTags().getTagList());
  }

  @Test
  void testPutOrderLineWithNoTags() {
    logger.info("=== Test PUT Order Line With No Tags ===");
    String lineId = "bb66b269-76ed-4616-8da9-730d9b817247";
    CompositePoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(CompositePoLine.class);
    body.setCheckinItems(false);
    body.setIsPackage(false);
    body.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
    body.setReportingCodes(new ArrayList<>());
    MockServer.addMockEntry(PO_LINES, body);
    MockServer.addMockEntry(PURCHASE_ORDER, new CompositePurchaseOrder()
      .withId(ID_FOR_PRINT_MONOGRAPH_ORDER)
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN)
      .withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME));
    String url = String.format(LINE_BY_ID_PATH, lineId);
    addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(body.getId())
      .withTitle("Title"));

    // edit POLine for new encumbrance
    body.setTags(null);
    body.setFundDistribution(Collections.singletonList(new FundDistribution()
      .withCode("EUROHIST")
      .withFundId("e9285a1c-1dfc-4380-868c-e74073003f43")
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D)));
    verifyPut(url, JsonObject.mapFrom(body), "", 204);

    Transaction createdEncumbrance = MockServer.getCreatedEncumbrances().get(0);
    assertNull(createdEncumbrance.getTags());

    // edit POLine for encumbrance update
    body.setTags(null);
    body.setCost(new Cost().withCurrency("USD").withListUnitPrice(70.0).withQuantityPhysical(1));
    body.setFundDistribution(Collections.singletonList(new FundDistribution()
      .withCode("EUROHIST")
      .withFundId("a89eccf0-57a6-495e-898d-32b9b2210f2f")
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D)));
    verifyPut(url, JsonObject.mapFrom(body), "", 204);
    Transaction updatedEncumbrance = MockServer.getUpdatedTransactions().get(0);
    assertNull(updatedEncumbrance.getTags());
  }

  @Test
  void testPutOrderLineByIdPiecesWillNotBeCreated() {
    logger.info("=== Test PUT Order Line By Id - Pieces will be created ===");

    String lineId = PO_LINE_ID_FOR_SUCCESS_CASE;
    CompositePoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(CompositePoLine.class);

    body.setCheckinItems(false);
    body.setIsPackage(false);
    body.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
    body.setReportingCodes(new ArrayList<>());
    MockServer.addMockEntry(PO_LINES, body);
    MockServer.addMockEntry(PURCHASE_ORDER, new CompositePurchaseOrder()
      .withId(ID_FOR_PRINT_MONOGRAPH_ORDER)
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN)
    .withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME));
    String url = String.format(LINE_BY_ID_PATH, lineId);

    addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(body.getId())
      .withTitle("Title"));

    verifyPut(url, JsonObject.mapFrom(body), "", 204);

    Map<String, List<JsonObject>> mockServerData = MockServer.serverRqRs.column(HttpMethod.GET);
    assertThat(mockServerData.get(PIECES), nullValue());

    mockServerData = MockServer.serverRqRs.column(HttpMethod.POST);
    assertThat(mockServerData.get(PIECES), nullValue());
  }

  @Test
  void testPutOrderLineByIdWithoutOrderUpdate() {
    logger.info("=== Test PUT Order Line By Id - No Order update event sent on success ===");

    String lineId = ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE;
    JsonObject body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    verifyPut(url, JsonObject.mapFrom(body), "", 204);

    // 2 calls each to fetch Order Line and Purchase Order
    // inaddition 2 calls for ISBN validation
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(4, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertEquals(3, column.size());
    assertThat(column.keySet(), containsInAnyOrder(PO_LINES, ALERTS, REPORTING_CODES));

    // Verify no message sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testUpdatePackagePoLineWithInstanceId() {
    logger.info("=== Test PUT Order Line By Id - No Order update event sent on success ===");

    String lineId = ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE;
    CompositePoLine compositePoLine = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(CompositePoLine.class);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    compositePoLine.setIsPackage(true);
    compositePoLine.setInstanceId(UUID.randomUUID().toString());

    Errors errors = verifyPut(url, JsonObject.mapFrom(compositePoLine), "", 422).then()
      .extract()
      .as(Errors.class);

    assertEquals(INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE.getCode(), errors.getErrors().get(0).getCode());
  }


  @Test
  void testPutOrderLineByIdProtectedFieldsChanged() {
    logger.info("=== Test PUT Order Line By Id - Protected fields changed ===");

    String lineId = "0009662b-8b80-4001-b704-ca10971f175d";
    JsonObject body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(POLineProtectedFields.ACQUISITION_METHOD.getFieldName(),
        CompositePoLine.AcquisitionMethod.APPROVAL_PLAN.value());
    allProtectedFieldsModification.put(POLineProtectedFields.TITLE_OR_PACKAGE.getFieldName(), "Testing ProtectedFields");
    allProtectedFieldsModification.put(POLineProtectedFields.DONOR.getFieldName(), "Donor");
    allProtectedFieldsModification.put(POLineProtectedFields.ERESOURCE_USER_LIMIT.getFieldName(), 100);
    // adding trial because a default value is added while sending the request
    allProtectedFieldsModification.put(POLineProtectedFields.ERESOURCE_TRIAL.getFieldName(), true);

    Contributor contributor = new Contributor();
    contributor.setContributor("Mr Test");
    contributor.setContributorNameTypeId("fbdd42a8-e47d-4694-b448-cc571d1b44c3");
    List<Contributor> contributors = new ArrayList<>();
    contributors.add(contributor);

    allProtectedFieldsModification.put(POLineProtectedFields.CONTRIBUTORS.getFieldName(), contributors);

    checkPreventProtectedFieldsModificationRule(url, body, allProtectedFieldsModification);

    // 2 calls each to fetch Order Line, Purchase Order
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(2, column.size());
    assertThat(column, hasKey(PO_LINES));

    // Verify no message sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testPutOrderLineByIdNotFound() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Not Found ===");

    String lineId = ID_DOES_NOT_EXIST;
    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId, PO_ID_PENDING_STATUS_WITH_PO_LINES);

    Response actual = verifyPut(url, body, APPLICATION_JSON, 404);

    assertEquals(lineId, actual.as(Errors.class).getErrors().get(0).getMessage());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.DELETE);
    assertTrue(column.isEmpty());
  }

  @Test
  void testPutOrderLineByIdWithInvalidContentInBody() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Body Validation Error ===");

    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);
    String body = getPoLineWithMinContentAndIds(ID_BAD_FORMAT, PO_ID_PENDING_STATUS_WITH_PO_LINES);

    Response resp = verifyPut(url, body, APPLICATION_JSON, 422);

    assertEquals(1, resp.as(Errors.class).getErrors().size());
    assertNotNull(resp.as(Errors.class).getErrors().get(0).getMessage());

    // Verify that no calls reached mock server
    assertTrue(MockServer.serverRqRs.isEmpty());
  }


  @Test
  void testPutOrderLineByIdWithIdMismatch() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Ids mismatch ===");

    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);
    String body = getPoLineWithMinContentAndIds(PO_LINE_ID_FOR_SUCCESS_CASE, PO_ID_PENDING_STATUS_WITH_PO_LINES);

    Response resp = verifyPut(url, body, APPLICATION_JSON, 422);

    assertEquals(1, resp.as(Errors.class).getErrors().size());
    assertThat(resp.as(Errors.class).getErrors().get(0).getCode(),is("idMismatch"));
    assertNotNull(resp.as(Errors.class).getErrors().get(0).getMessage());

    // Verify that no calls reached mock server
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  void testPutOrderLineById500FromStorageOnGetPoLine() throws IOException {
    logger.info("=== Test PUT Order Line By Id - 500 From Storage On Get PO Line ===");

    String lineId = ID_FOR_INTERNAL_SERVER_ERROR;

    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId, PO_ID_PENDING_STATUS_WITH_PO_LINES);

    Response actual = verifyPut(url, body, APPLICATION_JSON, 500);

    assertNotNull(actual.asString());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertTrue(column.isEmpty());

    column = MockServer.serverRqRs.column(HttpMethod.DELETE);
    assertTrue(column.isEmpty());
  }

  @Test
  void testDeleteOrderLineById() {
    logger.info("=== Test Delete Order Line By Id - Success case ===");

    deleteOrderLineByIdSuccess(PO_LINE_ID_FOR_SUCCESS_CASE);
  }

  @Test
  void testDeleteOrderLineByIdWithPartiallyDeletedSubObjects() {
    logger.info("=== Test Delete Order Line By Id - Success case ===");

    // This test should behave the same as regular test and only warning in log expected
    deleteOrderLineByIdSuccess(PO_LINE_ID_WITH_SOME_SUB_OBJECTS_ALREADY_REMOVED);
  }

  private void deleteOrderLineByIdSuccess(String lineId) {
    String url = String.format(LINE_BY_ID_PATH, lineId);

    final Response resp = verifyDeleteResponse(url, "", 204);
    assertTrue(StringUtils.isEmpty(resp.getBody().asString()));
  }

  @Test
  void testDeleteOrderLineByIdNotFound() {
    logger.info("=== Test Delete Order Line By Id - Not Found ===");

    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);
    Response actual = verifyDeleteResponse(url, APPLICATION_JSON, 404);

    assertEquals(ID_DOES_NOT_EXIST, actual.as(Errors.class).getErrors().get(0).getMessage());
  }

  @Test
  void testDeleteOrderLineById500FromStorageOnSubObjectDeletion() {
    logger.info("=== Test Delete Order Line By Id - 500 From Storage On Sub-Object deletion ===");

    String url = String.format(LINE_BY_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR);
    Response actual = verifyDeleteResponse(url, APPLICATION_JSON, 500);

    assertNotNull(actual.asString());
  }

  @Test
  void testGetOrderLineByIdWithoutTitle() {
    logger.info("=== Test Get Orderline By Id without title ===");

    final CompositePoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE), CompositePoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE, resp.getId());
    assertEquals(1, getPoLineSearches().size());
    assertNull(getOrderLineSearches());
    assertThat(getTitlesSearches(), hasSize(1));
  }

  @Test
  void testGetOrderLineByIdWithTitleNoInstanceId() {
    logger.info("=== Test Get Orderline By Id with title but without instanceId ===");

    addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE)
      .withTitle("Title"));

    final CompositePoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE), CompositePoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE, resp.getId());
    assertThat(getPoLineSearches(), hasSize(1));
    assertNull(getOrderLineSearches());
    assertThat(getTitlesSearches(), hasSize(1));
  }

  @Test
  void testGetOrderLineByIdWithTitle() {
    logger.info("=== Test Get Orderline By Id with title ===");

    String instanceId = UUID.randomUUID().toString();

    addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE)
      .withTitle("Title")
      .withInstanceId(instanceId));

    final CompositePoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE), CompositePoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE, resp.getId());
    assertThat(getPoLineSearches(), hasSize(1));
    assertNull(getOrderLineSearches());
    assertThat(getTitlesSearches(), hasSize(1));
    assertThat(instanceId, is(resp.getInstanceId()));
  }

  @Test
  void testGetPackageOrderLineByIdWithTitle() {
    logger.info("=== Test Get Orderline By Id with title ===");

    String instanceId = UUID.randomUUID().toString();
    String polineId = UUID.randomUUID().toString();
    addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(polineId)
      .withTitle("Title")
      .withInstanceId(instanceId));
    addMockEntry(PO_LINES, getMinimalContentCompositePoLine().withIsPackage(true).withId(polineId));

    final CompositePoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, polineId), CompositePoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(polineId, resp.getId());
    assertThat(getPoLineSearches(), hasSize(1));
    assertNull(getOrderLineSearches());
    assertThat(getTitlesSearches(), nullValue());
    assertThat(resp.getInstanceId(), nullValue());
  }

  @Test
  void testGetOrderLineByIdWith404() {
    logger.info("=== Test Get Orderline By Id - With 404 ===");

    String lineId = ID_DOES_NOT_EXIST;

    final Response resp = verifyGet(String.format(LINE_BY_ID_PATH, lineId), APPLICATION_JSON, 404);

    assertEquals(lineId, resp.getBody().as(Errors.class).getErrors().get(0).getMessage());
  }

  @Test
  void testGetOrderLineByIdWith500() {
    logger.info("=== Test Get Orderline By Id - With 500 ===");

    final Response resp = verifyGet(String.format(LINE_BY_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), APPLICATION_JSON, 500);

    assertEquals("Internal Server Error", resp.getBody().as(Errors.class).getErrors().get(0).getMessage());
  }

  @Test
  void testGetOrderLinesInternalServerError() {
    logger.info("=== Test Get Order Lines by query - emulating 500 from storage ===");

    String endpointQuery = String.format("%s?query=%s", LINES_PATH, ID_FOR_INTERNAL_SERVER_ERROR);

    verifyGet(endpointQuery, APPLICATION_JSON, 500);
  }

  @Test
  void testGetOrderLinesBadQuery() {
    logger.info("=== Test Get Order Lines by query - unprocessable query to emulate 400 from storage ===");

    String endpointQuery = String.format("%s?query=%s", LINES_PATH, BAD_QUERY);

    verifyGet(endpointQuery, APPLICATION_JSON, 400);

  }

  @Test
  void testGetOrderPoLinesByEmptyQuery() {
    logger.info("=== Test Get Orders lines - by empty query ===");

    verifySuccessGet(LINES_PATH, PoLineCollection.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(getOrderLineSearches(), nullValue());
    assertThat(getPoLineSearches(), hasSize(1));

    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(PO_LINES);
    assertThat(queryParams, hasSize(1));
    assertThat(queryParams.get(0), equalTo(NO_ACQ_UNIT_ASSIGNED_CQL));
  }

  @Test
  void testGetOrderPOLinesByPoId() {
    logger.info("=== Test Get Orders lines - by PO id ===");

    String sortBy = " sortBy poNumber";
    String cql = String.format("%s==%s", PURCHASE_ORDER_ID, ORDER_ID_WITH_PO_LINES);
    String endpointQuery = String.format("%s?query=%s%s", LINES_PATH, cql, sortBy);

    final PoLineCollection poLineCollection = verifySuccessGet(endpointQuery, PoLineCollection.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(poLineCollection.getTotalRecords(), is(2));
    assertThat(getOrderLineSearches(), hasSize(1));
    assertThat(getPoLineSearches(), nullValue());

    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(ORDER_LINES);
    assertThat(queryParams, hasSize(1));
    String queryToStorage = queryParams.get(0);
    assertThat(queryToStorage, containsString("(" + cql + ")"));
    assertThat(queryToStorage, containsString(ORDER_ID_WITH_PO_LINES));
    assertThat(queryToStorage, not(containsString(ACQUISITIONS_UNIT_IDS + "=")));
    assertThat(queryToStorage, containsString(NO_ACQ_UNIT_ASSIGNED_CQL));
    assertThat(queryToStorage, endsWith(sortBy));
  }

  @Test
  void testPostOrdersWithInvalidIsbn() {
    logger.info("=== Test Post Order line with invalid ISBN ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");

    String isbn = INVALID_ISBN;

    reqData.getDetails().getProductIds().get(0).setProductId(isbn);


    Response resp = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 400);

    Error err = resp.getBody()
        .as(Errors.class)
        .getErrors()
        .get(0);

    assertThat(err.getMessage(), equalTo(ISBN_NOT_VALID.getDescription()));
    assertThat(err.getCode(), equalTo(ISBN_NOT_VALID.getCode()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(isbn));
  }

  @Test
  void testPostOrderLineToConvertToIsbn13() {
    logger.info("=== Test Post order line to verify ISBN 10 is normalized to ISBN 13 ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");

    String isbn = "0-19-852663-6";
    ProductId productId = reqData.getDetails().getProductIds().get(0);
    reqData.getDetails().getProductIds().get(0).setProductId(isbn);
    reqData.getDetails().getProductIds().add(new ProductId()
      .withProductIdType(productId.getProductIdType())
      .withQualifier(productId.getQualifier())
      .withProductId(isbn));
    assertThat(reqData.getDetails().getProductIds(), hasSize(2));
    CompositePoLine resp = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePoLine.class);

    assertThat(resp.getDetails().getProductIds(), hasSize(1));
    assertThat(resp.getDetails().getProductIds().get(0).getProductId(), equalTo("9780198526636"));
  }

  @Test
  void testPostOrderLineToRemoveISBNDuplicates() {
    logger.info("=== Test Post order line to verify ISBN 13 is not repeated ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");
    final String invalidIsbn = "fcca2643-406a-482a-b760-7a7f8aec640e";

    reqData.getDetails().getProductIds().clear();


    final String isbn1 = "9780545010221";
    final String qualifier1 = "(q1)";
    final String qualifier2 = "(q2)";

    reqData.getDetails().getProductIds().add(createIsbnProductId(isbn1));
    reqData.getDetails().getProductIds().add(createIsbnProductId(isbn1).withQualifier(qualifier1));
    reqData.getDetails().getProductIds().add(createIsbnProductId(isbn1).withQualifier(qualifier1));
    reqData.getDetails().getProductIds().add(createIsbnProductId(isbn1).withQualifier(qualifier2));
    reqData.getDetails().getProductIds().add(createIsbnProductId(isbn1));

    final String isbn213 = "9780198526636";
    final String isbn210 = "0-19-852663-6";
    reqData.getDetails().getProductIds().add(createIsbnProductId(isbn210));
    reqData.getDetails().getProductIds().add(createIsbnProductId(isbn213));

    reqData.getDetails().getProductIds().add(new ProductId().withProductId(INVALID_ISBN).withProductIdType(invalidIsbn));

    assertThat(reqData.getDetails().getProductIds(), hasSize(8));
    CompositePoLine resp = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePoLine.class);

    assertThat(resp.getDetails().getProductIds(), hasSize(4));
    assertThat(resp.getDetails().getProductIds().get(0).getProductId(), equalTo(isbn1));
    assertThat(resp.getDetails().getProductIds().get(0).getQualifier(), equalTo(qualifier1));

    assertThat(resp.getDetails().getProductIds().get(1).getProductId(), equalTo(isbn1));
    assertThat(resp.getDetails().getProductIds().get(1).getQualifier(), equalTo(qualifier2));

    assertThat(resp.getDetails().getProductIds().get(2).getProductId(), equalTo(isbn213));

    assertThat(resp.getDetails().getProductIds().get(3).getQualifier(), nullValue());
    assertThat(resp.getDetails().getProductIds().get(3).getProductId(), equalTo(INVALID_ISBN));

  }

  private ProductId createIsbnProductId(String isbn) {
    return new ProductId().withProductIdType(ISBN_PRODUCT_TYPE_ID)
      .withProductId(isbn);
  }

  @Test
  void testPutOrdersWithInvalidIsbn() {
    logger.info("=== Test Put Order line with invalid ISBN ===");
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE)
      .mapTo(CompositePoLine.class);
    reqData.setId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);
    reqData.getLocations().get(0).setQuantity(2);
    String isbn = INVALID_ISBN;

    reqData.getDetails().getProductIds().get(0).setProductId(isbn);

    Response response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON,
        400);

    Error err = response.getBody().as(Errors.class).getErrors().get(0);

    assertThat(err.getMessage(), equalTo(ISBN_NOT_VALID.getDescription()));
    assertThat(err.getCode(), equalTo(ISBN_NOT_VALID.getCode()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(isbn));
  }

  @Test
  void testUpdatePolineForOpenedOrderWithInactiveAccessProvider() {
    logger.info("=== Test update poline with inactive access provider for opened order  ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);

    addMockEntry(PIECES, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    Errors errors = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422).as(Errors.class);
    assertEquals(1, errors.getErrors().size());
    assertEquals(POL_ACCESS_PROVIDER_IS_INACTIVE.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingCost() {
    logger.info("=== Test update poline for opened order with changed cost ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    double newCost = 12.09d;
    reqData.getCost().setListUnitPriceElectronic(newCost);
    reqData.getFundDistribution().get(0).setDistributionType(FundDistribution.DistributionType.AMOUNT);
    reqData.getFundDistribution().get(0).setValue(newCost);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);

   assertThat("One or more transactions have been updated", MockServer.getRqRsEntries(HttpMethod.PUT, ENCUMBRANCES).size() > 0);
  }

  @Test
  void testUpdatePolineForOpenedOrderWithInactiveExpenseClass() {
    logger.info("=== Test update poline for opened order with inactive expense class ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    reqData.getFundDistribution().get(0).setExpenseClassId(INACTIVE_EXPENSE_CLASS_ID);

    Errors errors = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData)
      .encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 400)
      .as(Errors.class);

    assertThat(JsonObject.mapFrom(errors).encodePrettily(), containsString(INACTIVE_EXPENSE_CLASS.getCode()));

  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingDistributionType() {
    logger.info("=== Test update poline for opened order with changed DistributionType ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    reqData.getFundDistribution().get(0).setDistributionType(FundDistribution.DistributionType.AMOUNT);
    reqData.getFundDistribution().get(0).setValue(reqData.getCost().getPoLineEstimatedPrice());

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);

    assertThat("One or more transactions have been updated", MockServer.getRqRsEntries(HttpMethod.PUT, ENCUMBRANCES).size() > 0);
  }

  @Test
  void testUpdatePolineForOpenedOrderWithoutUpdatingEncumbrances() {
    logger.info("=== Test update poline. Fund distributions not changed ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
    assertThat("No transactions have been released", MockServer.getRqRsEntries(HttpMethod.POST, TRANSACTIONS_STORAGE_ENDPOINT).size() == 0);
    assertThat("No transactions have been updated", MockServer.getRqRsEntries(HttpMethod.PUT, TRANSACTIONS_STORAGE_ENDPOINT).size() == 0);
    assertThat("No transactions have been created", MockServer.getRqRsEntries(HttpMethod.POST, TRANSACTIONS_STORAGE_ENDPOINT).size() == 0);

    // double check with updating status only (When only status updated via request from mod-invoices)
    CompositePoLine updatedLine = getRqRsEntries(HttpMethod.PUT, PO_LINES).get(0).mapTo(CompositePoLine.class);
    updatedLine.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(updatedLine).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
    assertThat("No transactions have been released", MockServer.getRqRsEntries(HttpMethod.POST, TRANSACTIONS_STORAGE_ENDPOINT).size() == 0);
    assertThat("No transactions have been updated", MockServer.getRqRsEntries(HttpMethod.PUT, TRANSACTIONS_STORAGE_ENDPOINT).size() == 0);
    assertThat("No transactions have been created", MockServer.getRqRsEntries(HttpMethod.POST, TRANSACTIONS_STORAGE_ENDPOINT).size() == 0);

  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingOnlyLocation() {
    logger.info("=== Test update poline for opened order with changed location ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING_ITEM);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    String newLocationId = "fcd64ce1-6995-48f0-840e-89ffa2288371";
    reqData.getLocations().get(0).setLocationId(newLocationId);

    Errors response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 400).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN.getCode()));
  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingQuantity() {
    logger.info("=== Test update poline for opened order with changed quantity ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING_ITEM);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    reqData.getLocations().get(0).setQuantityElectronic(3);
    reqData.getCost().setQuantityElectronic(3);

    Errors response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 400).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN.getCode()));
  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingQuantityAndCheckinItems() {
    logger.info("=== Test update poline for opened order with changed quantity and checkin items ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setCheckinItems(true);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING_ITEM);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    reqData.getLocations().get(0).setQuantityElectronic(3);
    reqData.getCost().setQuantityElectronic(3);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
  }

  @Test
  void testUpdatePolineForOpenedOrderWithoutUpdatingItems() {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING);

    addMockEntry(PIECES, new Piece()
      .withFormat(Piece.Format.ELECTRONIC)
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    int expQtyElectronic = 3;
    reqData.getLocations().get(0).setQuantityElectronic(expQtyElectronic);
    reqData.getLocations().get(0).setQuantity(expQtyElectronic);
    reqData.getCost().setQuantityElectronic(expQtyElectronic);
    Errors response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 400).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN.getCode()));

    assertEquals(0, getRqRsEntries(HttpMethod.POST, TITLES).size(), "Items should not created");
    assertEquals(0, getRqRsEntries(HttpMethod.PUT, TITLES).size(), "Items should not updated");
  }

  @Test
  void testUpdatePolineForOpenedOrderWithNewLocationWithoutUpdatingItems() {
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(CompositePoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING);
    reqData.getLocations().get(0).setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES, new Piece()
      .withFormat(Piece.Format.ELECTRONIC)
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().get(0).getLocationId()));

    addMockEntry(PO_LINES, reqData);

    String newLocationId = "fcd64ce1-6995-48f0-840e-89ffa2288371";
    reqData.getLocations().get(0).setLocationId(newLocationId);

    Errors response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 400).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN.getCode()));
    assertEquals(0, getRqRsEntries(HttpMethod.PUT, TITLES).size(), "Items should not updated");
  }

  private String getPoLineWithMinContentAndIds(String lineId, String orderId) throws IOException {
    CompositePoLine poLine = new JsonObject(getMockData(PO_LINE_MIN_CONTENT_PATH)).mapTo(CompositePoLine.class);
    poLine.setId(lineId);
    poLine.setPurchaseOrderId(orderId);
    return JsonObject.mapFrom(poLine).encode();
  }

}
