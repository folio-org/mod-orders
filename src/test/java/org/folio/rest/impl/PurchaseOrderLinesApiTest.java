package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.RestTestUtils.checkPreventProtectedFieldsModificationRule;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPatch;
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
import static org.folio.TestConstants.NON_EXIST_CONFIG_X_OKAPI_TENANT;
import static org.folio.TestConstants.PO_ID_CLOSED_STATUS;
import static org.folio.TestConstants.PO_ID_OPEN_STATUS;
import static org.folio.TestConstants.PO_LINE_ID_FOR_SUCCESS_CASE;
import static org.folio.TestConstants.PO_LINE_ID_WITHOUT_DETAILS;
import static org.folio.TestConstants.PO_LINE_ID_WRONG_EXPENSE_CLASS;
import static org.folio.TestConstants.PO_LINE_NUMBER_VALUE;
import static org.folio.TestConstants.PROTECTED_READ_ONLY_TENANT;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.TestUtils.getModifiedProtectedFields;
import static org.folio.TestUtils.validatePoLineCreationErrorForNonPendingOrder;
import static org.folio.TestUtils.verifyLocationQuantity;
import static org.folio.helper.PurchaseOrderLineHelper.ERESOURCE;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.FINANCE_BATCH_TRANSACTIONS;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.USER_TENANTS_ENDPOINT;
import static org.folio.rest.core.exceptions.ErrorCodes.COST_ADDITIONAL_COST_INVALID;
import static org.folio.rest.core.exceptions.ErrorCodes.COST_DISCOUNT_INVALID;
import static org.folio.rest.core.exceptions.ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID;
import static org.folio.rest.core.exceptions.ErrorCodes.COST_UNIT_PRICE_INVALID;
import static org.folio.rest.core.exceptions.ErrorCodes.ELECTRONIC_COST_LOC_QTY_MISMATCH;
import static org.folio.rest.core.exceptions.ErrorCodes.INACTIVE_EXPENSE_CLASS;
import static org.folio.rest.core.exceptions.ErrorCodes.INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE;
import static org.folio.rest.core.exceptions.ErrorCodes.LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN;
import static org.folio.rest.core.exceptions.ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY;
import static org.folio.rest.core.exceptions.ErrorCodes.NON_ZERO_COST_PHYSICAL_QTY;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_CLOSED;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_OPEN;
import static org.folio.rest.core.exceptions.ErrorCodes.PHYSICAL_COST_LOC_QTY_MISMATCH;
import static org.folio.rest.core.exceptions.ErrorCodes.PIECES_EXIST_FOR_POLINE;
import static org.folio.rest.core.exceptions.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.POL_LINES_LIMIT_EXCEEDED;
import static org.folio.rest.core.exceptions.ErrorCodes.PROHIBITED_FIELD_CHANGING;
import static org.folio.rest.core.exceptions.ErrorCodes.ZERO_COST_ELECTRONIC_QTY;
import static org.folio.rest.core.exceptions.ErrorCodes.ZERO_COST_PHYSICAL_QTY;
import static org.folio.rest.core.exceptions.ErrorCodes.ZERO_LOCATION_QTY;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ORDER_ID_WITH_PO_LINES;
import static org.folio.rest.impl.MockServer.PO_LINES_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.PO_NUMBER_ERROR_X_OKAPI_TENANT;
import static org.folio.rest.impl.MockServer.addMockEntry;
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
import org.folio.HttpStatus;
import org.folio.TestUtils;
import org.folio.config.ApplicationConfig;
import org.folio.orders.events.handlers.HandlersTestHelper;
import org.folio.orders.utils.FundDistributionUtils;
import org.folio.orders.utils.POLineFieldNames;
import org.folio.rest.acq.model.Title;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.PaymentStatus;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.ReplaceInstanceRef;
import org.folio.rest.jaxrs.model.Tags;
import org.folio.rest.jaxrs.model.ValidateFundDistributionsRequest;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import com.google.common.collect.Lists;

import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;

@ExtendWith(VertxExtension.class)
public class PurchaseOrderLinesApiTest {

  private static final Logger logger = LogManager.getLogger();

  private static final String PO_LINE_ID_WITH_SOME_SUB_OBJECTS_ALREADY_REMOVED = "0009662b-8b80-4001-b704-ca10971f175d";
  static final String ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  public final static String LINES_PATH = "/orders/order-lines";
  public static final String LINE_BY_ID_PATH = "/orders/order-lines/%s";
  public static final String COMP_PO_BY_ID_PATH = "/orders/composite-orders/%s";
  public static final String COMP_PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  public static final String PO_LINE_VALIDATE_FUND_DISTRIBUTIONS_PATH = LINES_PATH + "/fund-distributions/validate";
  private static final String PO_LINE_MIN_CONTENT_PATH = COMP_PO_LINES_MOCK_DATA_PATH + "minimalContent.json";
  public static final String ISBN_PRODUCT_TYPE_ID = "8261054f-be78-422d-bd51-4ed9f33c3422";
  public static final String INVALID_ISBN = "1234";
  private static final String ACQUISITIONS_UNIT_IDS = "purchaseOrder.acqUnitIds";
  private static final String NO_ACQ_UNIT_ASSIGNED_CQL = "cql.allRecords=1 not " + ACQUISITIONS_UNIT_IDS + " <> []";

  private static boolean runningOnOwn;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    // Disable caching to avoid test failures
    System.setProperty("orders.cache.consortium-data.expiration.time.seconds", "0");
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

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(PoLine.class);
    reqData.getCost().setPoLineEstimatedPrice(null);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");

    final PoLine response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(PoLine.class);

    assertThat(response.getPurchaseOrderId(), equalTo(reqData.getPurchaseOrderId()));
    assertThat(response.getInstanceId(), nullValue());

    JsonObject purchaseOrderOwner = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, reqData.getPurchaseOrderId());
    String expectedPoLineNumber = purchaseOrderOwner.getString(PO_NUMBER) + "-" + PO_LINE_NUMBER_VALUE;
    assertThat(response.getPoLineNumber(), equalTo(expectedPoLineNumber));
    // See MODORDERS-180
    assertThat(response.getCost().getPoLineEstimatedPrice(), equalTo(49.98d));
    Location location = response.getLocations().getFirst();
    verifyLocationQuantity(location, response.getOrderFormat());
  }

  @Test
  void testPostOrdersLinePhysicalFormatIncorrectQuantity() {
    logger.info("=== Test Post Physical Order Line - incorrect quantity ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(PoLine.class);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE);
    reqData.getCost().setQuantityPhysical(0);
    reqData.getCost().setQuantityElectronic(1);
    reqData.getLocations().getFirst().setQuantityPhysical(1);
    reqData.getLocations().add(new Location()
      .withQuantityPhysical(0)
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

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

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(PoLine.class);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    reqData.getCost().setListUnitPrice(null);
    reqData.getCost().setListUnitPriceElectronic(0d);
    reqData.getCost().setQuantityPhysical(0);
    reqData.getCost().setQuantityElectronic(4);
    reqData.getLocations().getFirst().setQuantityElectronic(3);
    reqData.getLocations().add(new Location()
      .withQuantityElectronic(0)
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));
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

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(PoLine.class);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE);
    reqData.getCost().setQuantityPhysical(2);
    reqData.getCost().setListUnitPrice(10d);
    reqData.getCost().setDiscount(100d);
    reqData.getCost().setDiscountType(Cost.DiscountType.AMOUNT);
    reqData.getCost().setQuantityElectronic(0);
    reqData.getLocations().getFirst().setQuantityPhysical(2);

    final Errors response = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    assertThat(response.getErrors().getFirst().getCode(), equalTo(COST_DISCOUNT_INVALID.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  void testPostWithEmptyConfigOverLimit() {
    logger.info("=== Test PO creation fail with default limit ===");

    JsonObject poLineJson = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final Errors errors = verifyPostResponse(LINES_PATH, poLineJson.encodePrettily(),
      prepareHeaders(EMPTY_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().getFirst().getMessage());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().getFirst().getCode());
  }

  @Test
  void testPoLineCreationIfPoAlreadyReachedLimit() {
    logger.info("=== Test PO Line over limit creation ===");
    JsonObject poLineJson = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    final Errors errors = verifyPostResponse(LINES_PATH, poLineJson.encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1), APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().getFirst().getMessage());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().getFirst().getCode());
  }

  @Test
  void testPostOrdersLinesByIdPoLineWithoutId() throws IOException {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");

    Errors resp = verifyPostResponse(LINES_PATH, getMockData(PO_LINE_MIN_CONTENT_PATH),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(ErrorCodes.MISSING_ORDER_ID_IN_POL.getCode(), resp.getErrors().getFirst().getCode());
  }

  @Test
  void testPostOrdersLinesByIdPoLineWithNonExistPurchaseOrder() {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");
    JsonObject reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);
    reqData.put(PURCHASE_ORDER_ID, ID_DOES_NOT_EXIST);

    Errors resp = verifyPostResponse(LINES_PATH, reqData.encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(ErrorCodes.ORDER_NOT_FOUND.getCode(), resp.getErrors().getFirst().getCode());
  }

  @Test
  void testPostOrdersLinesByIdPoLineWithWrongExpenseClasses() {
    logger.info("=== Test Post Order Lines By Id (expense class doesn't exist in budget) ===");
    JsonObject reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_WRONG_EXPENSE_CLASS);

    reqData.put(PURCHASE_ORDER_ID, "9a952cd0-842b-4e71-bddd-014eb128dc8e");

    Errors resp = verifyPostResponse(LINES_PATH, reqData.encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 400).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(ErrorCodes.BUDGET_EXPENSE_CLASS_NOT_FOUND.getCode(), resp.getErrors().getFirst().getCode());
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

    PoLine poLine = order.getPoLines().getFirst();
    poLine.setId(null);

    Errors errors = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(poLine).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);
    validatePoLineCreationErrorForNonPendingOrder(errorCode, errors, 2);
  }

  @Test
  void testCreatePoLineWithGetPoLineNumberError() throws IOException {
    logger.info("=== Test Create PO Line - fail on PO Line number generation ===");
    JsonObject json = new JsonObject(getMockData(String.format("%s%s.json", COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_FOR_SUCCESS_CASE)));
    PoLine poLine = json.mapTo(PoLine.class);
    poLine.setId("0009662b-8b80-4001-b704-ca10971f175d");
    poLine.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");
    verifyPostResponse(LINES_PATH, JsonObject.mapFrom(poLine).encode(), prepareHeaders(PO_NUMBER_ERROR_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 500);
  }

  @Test
  void testPutOrderLineElectronicFormatIncorrectQuantityAndPrice() {
    logger.info("=== Test Put Electronic Order Line - incorrect quantity and Price ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(PoLine.class);
    reqData.setId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);

    // Set incorrect cost and location quantities
    reqData.setOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    Cost cost = reqData.getCost();
    cost.setQuantityPhysical(1);
    cost.setQuantityElectronic(0);
    cost.setListUnitPriceElectronic(-1d);
    cost.setListUnitPrice(1d);
    cost.setAdditionalCost(-1d);
    cost.setDiscountType(Cost.DiscountType.PERCENTAGE);
    cost.setDiscount(100.1d);
    reqData.getLocations().getFirst().setQuantityElectronic(1);

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
  void testValidationOnPutWithIncorrectLineId() throws IOException {
    logger.info("=== Test validation on PUT line with invalid line ID path parameter ===");
    verifyPut(String.format(LINE_BY_ID_PATH, ID_BAD_FORMAT), getMockData(PO_LINE_MIN_CONTENT_PATH), TEXT_PLAIN, 400);
  }

  @Test
  void testPutOrderLineByIdWithEmptyBody() throws IOException {
    logger.info("=== Test PUT Order Line By Id With Empty Json - validation error because of required properties like PO ID ===");

    String url = String.format(LINE_BY_ID_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);

    JsonObject reqData = getMockAsJson(PO_LINE_MIN_CONTENT_PATH);
    reqData.remove("purchaseOrderId");

    Errors resp = verifyPut(url, reqData.encodePrettily(), "", 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
  }

  @Test
  void testPutOrderLineById() {
    logger.info("=== Test PUT Order Line By Id - Success case ===");

    String lineId = PO_LINE_ID_FOR_SUCCESS_CASE;
    PoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(PoLine.class);

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

    // 4 calls to get Order Line,Purchase Order for checking workflow status, ISBN validation and Consortium Configuration
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    // Change to assumeThat because after correcting the Caffeine Cache we cannot assert to get a non-cached result
    assertThat(column, hasKey(USER_TENANTS_ENDPOINT));
    assertThat(column, hasKey(PO_LINES_STORAGE));
    assertThat(column, hasKey((PIECES_STORAGE)));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertEquals(0, column.size());

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertEquals(1, column.size());
    assertThat(column.keySet(), containsInAnyOrder(PO_LINES_STORAGE));

    // See MODORDERS-180
    PoLine poLine = column.get(PO_LINES_STORAGE).getFirst().mapTo(PoLine.class);
    assertThat(poLine.getCost().getPoLineEstimatedPrice(), equalTo(expectedTotalPoLine));
    Location location = poLine.getLocations().getFirst();
    assertEquals(location.getQuantityPhysical(), location.getQuantity());

    // Verify messages sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(1);
  }

  @Test
  void testPutOrderLineWithTagInheritance() {
    logger.info("=== Test PUT Order Line With Tag Inheritance ===");
    String lineId = "bb66b269-76ed-4616-8da9-730d9b817247";
    PoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(PoLine.class);
    body.setCheckinItems(false);
    body.setIsPackage(false);
    body.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
    body.setPurchaseOrderId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    MockServer.addMockEntry(PO_LINES_STORAGE, body);
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, new CompositePurchaseOrder()
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

    Transaction createdEncumbrance = MockServer.getCreatedEncumbrances().getFirst();
    assertEquals(Collections.singletonList("created"), createdEncumbrance.getTags().getTagList());

    // edit POLine for encumbrance update
    body.setTags(new Tags().withTagList(Collections.singletonList("updated")));
    body.setCost(new Cost().withCurrency("USD").withListUnitPrice(70.0).withQuantityPhysical(1));
    body.setFundDistribution(Collections.singletonList(new FundDistribution()
      .withCode("MISCHIST")
      .withFundId("a89eccf0-57a6-495e-898d-32b9b2210f2f")
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D)));
    verifyPut(url, JsonObject.mapFrom(body), "", 204);
    Transaction updatedEncumbrance = MockServer.getUpdatedTransactions().get(1);
    assertEquals(Collections.singletonList("updated"), updatedEncumbrance.getTags().getTagList());
  }

  @Test
  void testPutOrderLineWithNoTags() {
    logger.info("=== Test PUT Order Line With No Tags ===");
    String lineId = "bb66b269-76ed-4616-8da9-730d9b817247";
    PoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(PoLine.class);
    body.setCheckinItems(false);
    body.setIsPackage(false);
    body.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
    MockServer.addMockEntry(PO_LINES_STORAGE, body);
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, new CompositePurchaseOrder()
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

    Transaction createdEncumbrance = MockServer.getCreatedEncumbrances().getFirst();
    assertNull(createdEncumbrance.getTags());

    // edit POLine for encumbrance update
    body.setTags(null);
    body.setCost(new Cost().withCurrency("USD").withListUnitPrice(70.0).withQuantityPhysical(1));
    body.setFundDistribution(Collections.singletonList(new FundDistribution()
      .withCode("MISCHIST")
      .withFundId("a89eccf0-57a6-495e-898d-32b9b2210f2f")
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D)));
    verifyPut(url, JsonObject.mapFrom(body), "", 204);
    Transaction updatedEncumbrance = MockServer.getUpdatedTransactions().getFirst();
    assertNull(updatedEncumbrance.getTags());
  }

  @Test
  void testPutOrderLineByIdPiecesWillNotBeCreated() {
    logger.info("=== Test PUT Order Line By Id - Pieces will be created ===");

    String lineId = PO_LINE_ID_FOR_SUCCESS_CASE;
    PoLine body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(PoLine.class);

    body.setCheckinItems(false);
    body.setIsPackage(false);
    body.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
    MockServer.addMockEntry(PO_LINES_STORAGE, body);
    MockServer.addMockEntry(PURCHASE_ORDER_STORAGE, new CompositePurchaseOrder()
      .withId(ID_FOR_PRINT_MONOGRAPH_ORDER)
      .withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN)
    .withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME));
    String url = String.format(LINE_BY_ID_PATH, lineId);

    addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(body.getId())
      .withTitle("Title"));

    verifyPut(url, JsonObject.mapFrom(body), "", 204);

    Map<String, List<JsonObject>> mockServerData = MockServer.serverRqRs.column(HttpMethod.GET);
    assertThat(mockServerData.get(PIECES_STORAGE), nullValue());

    mockServerData = MockServer.serverRqRs.column(HttpMethod.POST);
    assertThat(mockServerData.get(PIECES_STORAGE), nullValue());
  }

  @Test
  void testPutOrderLineByIdWithoutOrderUpdate() {
    logger.info("=== Test PUT Order Line By Id - No Order update event sent on success ===");

    String lineId = ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE;
    JsonObject body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    verifyPut(url, JsonObject.mapFrom(body), "", 204);

    // 2 calls each to fetch Order Line and Purchase Order
    // + 1 calls for ISBN validation. The next call can retrieve data from cache
    // + 1 call to get the line encumbrances
    // + 1 call to check invoice relationships
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertThat(column, hasKey(PO_LINES_STORAGE));

    column = MockServer.serverRqRs.column(HttpMethod.PUT);
    assertEquals(1, column.size());
    assertThat(column.keySet(), containsInAnyOrder(PO_LINES_STORAGE));

    column = MockServer.serverRqRs.column(HttpMethod.POST);
    assertEquals(1, column.size());
    assertThat(column.keySet(), containsInAnyOrder(FINANCE_BATCH_TRANSACTIONS));

    // Verify no message sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testUpdatePackagePoLineWithInstanceId() {
    logger.info("=== Test PUT Order Line By Id - No Order update event sent on success ===");

    String lineId = ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE;
    PoLine poLine = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId).mapTo(PoLine.class);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    poLine.setIsPackage(true);
    poLine.setInstanceId(UUID.randomUUID().toString());

    Errors errors = verifyPut(url, JsonObject.mapFrom(poLine), "", 422).then()
      .extract()
      .as(Errors.class);

    assertEquals(INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE.getCode(), errors.getErrors().getFirst().getCode());
  }


  @Test
  void testPutOrderLineByIdForPhysicalOrderAndProtectedFieldsChanged() {
    logger.info("=== Test PUT Order Line By Id - Protected fields changed ===");

    String lineId = "0009662b-8b80-4001-b704-ca10971f175d";
    JsonObject body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(POLineFieldNames.ACQUISITION_METHOD.getFieldName(),
      TestUtils.APPROVAL_PLAN_METHOD);
    allProtectedFieldsModification.put(POLineFieldNames.DONOR.getFieldName(), "Donor");
    // adding trial because a default value is added while sending the request
    allProtectedFieldsModification.put(POLineFieldNames.PHYSICAL_MATERIAL_TYPE.getFieldName(), UUID.randomUUID().toString());

    Contributor contributor = new Contributor();
    contributor.setContributor("Mr Test");
    contributor.setContributorNameTypeId("fbdd42a8-e47d-4694-b448-cc571d1b44c3");
    List<Contributor> contributors = new ArrayList<>();
    contributors.add(contributor);

    allProtectedFieldsModification.put(POLineFieldNames.CONTRIBUTORS.getFieldName(), contributors);

    checkPreventProtectedFieldsModificationRule(url, body, allProtectedFieldsModification);

    // 3 calls each to fetch Order Line, Purchase Order, Identifier Type
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertThat(column, hasKey(PO_LINES_STORAGE));

    // Verify no message sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
  }

  @Test
  void testPutOrderLineByIdNotFound() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Not Found ===");

    String lineId = ID_DOES_NOT_EXIST;
    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId);

    Response actual = verifyPut(url, body, APPLICATION_JSON, 404);

    assertEquals(lineId, actual.as(Errors.class).getErrors().getFirst().getMessage());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES_STORAGE));

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
    String body = getPoLineWithMinContentAndIds(ID_BAD_FORMAT);

    Response resp = verifyPut(url, body, APPLICATION_JSON, 422);

    assertEquals(1, resp.as(Errors.class).getErrors().size());
    assertNotNull(resp.as(Errors.class).getErrors().getFirst().getMessage());

    // Verify that no calls reached mock server
    assertTrue(MockServer.serverRqRs.isEmpty());
  }


  @Test
  void testPutOrderLineByIdWithIdMismatch() throws IOException {
    logger.info("=== Test PUT Order Line By Id - Ids mismatch ===");

    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);
    String body = getPoLineWithMinContentAndIds(PO_LINE_ID_FOR_SUCCESS_CASE);

    Response resp = verifyPut(url, body, APPLICATION_JSON, 422);

    assertEquals(1, resp.as(Errors.class).getErrors().size());
    assertThat(resp.as(Errors.class).getErrors().getFirst().getCode(),is("idMismatch"));
    assertNotNull(resp.as(Errors.class).getErrors().getFirst().getMessage());

    // Verify that no calls reached mock server
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  void testPutOrderLineById500FromStorageOnGetPoLine() throws IOException {
    logger.info("=== Test PUT Order Line By Id - 500 From Storage On Get PO Line ===");

    String lineId = ID_FOR_INTERNAL_SERVER_ERROR;

    String url = String.format(LINE_BY_ID_PATH, lineId);
    String body = getPoLineWithMinContentAndIds(lineId);

    Response actual = verifyPut(url, body, APPLICATION_JSON, 500);

    assertNotNull(actual.asString());

    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(1, column.size());
    assertThat(column, hasKey(PO_LINES_STORAGE));

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

    assertEquals(ID_DOES_NOT_EXIST, actual.as(Errors.class).getErrors().getFirst().getMessage());
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

    final PoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE), PoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE, resp.getId());
    assertEquals(1, getPoLineSearches().size());
    assertThat(getTitlesSearches(), hasSize(1));
  }

  @Test
  void testGetOrderLineByIdWithTitleNoInstanceId() {
    logger.info("=== Test Get Orderline By Id with title but without instanceId ===");

    addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString())
      .withPoLineId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE)
      .withTitle("Title"));

    final PoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE), PoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE, resp.getId());
    assertThat(getPoLineSearches(), hasSize(1));
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

    final PoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE), PoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE, resp.getId());
    assertThat(getPoLineSearches(), hasSize(1));
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
    addMockEntry(PO_LINES_STORAGE, getMinimalContentCompositePoLine().withIsPackage(true).withId(polineId));

    final PoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, polineId), PoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(polineId, resp.getId());
    assertThat(getPoLineSearches(), hasSize(1));
    assertThat(getTitlesSearches(), nullValue());
    assertThat(resp.getInstanceId(), nullValue());
  }

  @Test
  void testGetOrderLineByIdWith404() {
    logger.info("=== Test Get Orderline By Id - With 404 ===");

    String lineId = ID_DOES_NOT_EXIST;

    final Response resp = verifyGet(String.format(LINE_BY_ID_PATH, lineId), APPLICATION_JSON, 404);

    assertEquals(lineId, resp.getBody().as(Errors.class).getErrors().getFirst().getMessage());
  }

  @Test
  void testGetOrderLineByIdWith500() {
    logger.info("=== Test Get Orderline By Id - With 500 ===");

    final Response resp = verifyGet(String.format(LINE_BY_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), APPLICATION_JSON, 500);

    assertEquals("Internal Server Error", resp.getBody().as(Errors.class).getErrors().getFirst().getMessage());
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

    assertThat(getPoLineSearches(), hasSize(1));

    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(PO_LINES_STORAGE);
    assertThat(queryParams, hasSize(1));
    assertThat(queryParams.getFirst(), equalTo(NO_ACQ_UNIT_ASSIGNED_CQL));
  }

  @Test
  void testGetOrderPOLinesByPoId() {
    logger.info("=== Test Get Orders lines - by PO id ===");

    String sortBy = " sortBy poNumber";
    String cql = String.format("%s==%s", PURCHASE_ORDER_ID, ORDER_ID_WITH_PO_LINES);
    String endpointQuery = String.format("%s?query=%s%s", LINES_PATH, cql, sortBy);

    final PoLineCollection poLineCollection = verifySuccessGet(endpointQuery, PoLineCollection.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(poLineCollection.getTotalRecords(), is(2));
    assertThat(getPoLineSearches(), hasSize(1));

    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(PO_LINES_STORAGE);
    assertThat(queryParams, hasSize(1));
    String queryToStorage = queryParams.getFirst();
    assertThat(queryToStorage, containsString("(" + cql + ")"));
    assertThat(queryToStorage, containsString(ORDER_ID_WITH_PO_LINES));
    assertThat(queryToStorage, not(containsString(ACQUISITIONS_UNIT_IDS + "=")));
    assertThat(queryToStorage, containsString(NO_ACQ_UNIT_ASSIGNED_CQL));
    assertThat(queryToStorage, endsWith(sortBy));
  }

  @Test
  void testPostOrdersWithInvalidIsbn() {
    logger.info("=== Test Post Order line with invalid ISBN ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(PoLine.class);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");

    String isbn = INVALID_ISBN;

    reqData.getDetails().getProductIds().getFirst().setProductId(isbn);

    PoLine resp = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(PoLine.class);

    // order should be placed even with invalid ISBN
    assertThat(resp.getDetails().getProductIds(), hasSize(1));
    assertEquals(isbn, reqData.getDetails().getProductIds().getFirst().getProductId());
  }

  @Test
  void testPostOrderLineProcessWithoutIsbnConvertion() {
    logger.info("=== Test Post order line to verify that ISBN processed as it without convertion ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(PoLine.class);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");

    String isbn = "0-19-852663-6";
    ProductId productId = reqData.getDetails().getProductIds().getFirst();
    reqData.getDetails().getProductIds().getFirst().setProductId(isbn);
    reqData.getDetails().getProductIds().add(new ProductId()
      .withProductIdType(productId.getProductIdType())
      .withQualifier(productId.getQualifier())
      .withProductId(isbn));
    assertThat(reqData.getDetails().getProductIds(), hasSize(2));
    PoLine resp = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(PoLine.class);

    assertThat(resp.getDetails().getProductIds(), hasSize(2));
    assertThat(resp.getDetails().getProductIds().getFirst().getProductId(), equalTo(isbn));
  }

  @Test
  void testPostOrderLineShouldKeepISBNsWithoutRemovingDuplicates() {
    logger.info("=== Test Post order line to verify that product ids are the same even if have duplicates ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(PoLine.class);
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
    PoLine resp = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(PoLine.class);

    assertThat(resp.getDetails().getProductIds(), hasSize(8));
    assertEquals(reqData.getDetails().getProductIds(), resp.getDetails().getProductIds());
  }

  @Test
  void testUpdatePoLineForOpenedOrderShouldFailedWhenRelatedInvoiceLineApprovedForCurrentFY() {
    logger.info("=== Test update poline for opened order should failed when related invoice line is approved  ===");

    CompositePurchaseOrder compositePurchaseOrder = verifySuccessGet(String.format(COMP_PO_BY_ID_PATH, "d6966317-96c7-492f-8df6-dc6c19554452"), CompositePurchaseOrder.class);
    PoLine reqData = compositePurchaseOrder.getPoLines().getFirst();
    reqData.getFundDistribution().getFirst().setFundId("a89eccf0-57a6-495e-898d-32b9b2210f2f");

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 403);
  }

  private ProductId createIsbnProductId(String isbn) {
    return new ProductId().withProductIdType(ISBN_PRODUCT_TYPE_ID)
      .withProductId(isbn);
  }

  @Test
  void testPutOrdersWithInvalidIsbn() {
    logger.info("=== Test Put Order line with invalid ISBN ===");
    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE)
      .mapTo(PoLine.class);
    reqData.setId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);
    reqData.getLocations().getFirst().setQuantity(2);

    reqData.getDetails().getProductIds().getFirst().setProductId(INVALID_ISBN);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
  }

  @Test
  void testUpdatePolineForOpenedOrderWithInactiveAccessProvider() {
    logger.info("=== Test update poline with inactive access provider for opened order  ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    Errors errors = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422).as(Errors.class);
    assertEquals(1, errors.getErrors().size());
    assertEquals(POL_ACCESS_PROVIDER_IS_INACTIVE.getCode(), errors.getErrors().getFirst().getCode());
  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingCost() {
    logger.info("=== Test update poline for opened order with changed cost ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    double newCost = 12.09d;
    reqData.getCost().setListUnitPriceElectronic(newCost);
    reqData.getFundDistribution().getFirst().setDistributionType(FundDistribution.DistributionType.AMOUNT);
    reqData.getFundDistribution().getFirst().setValue(newCost);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);

   assertThat("One or more transactions have been updated",
     !MockServer.getRqRsEntries(HttpMethod.POST, FINANCE_BATCH_TRANSACTIONS).isEmpty());
  }

  @Test
  void testUpdatePolineForOpenedOrderWithInactiveExpenseClass() {
    logger.info("=== Test update poline for opened order with inactive expense class ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    reqData.getFundDistribution().getFirst().setExpenseClassId(INACTIVE_EXPENSE_CLASS_ID);

    Errors errors = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData)
      .encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 400)
      .as(Errors.class);

    assertThat(JsonObject.mapFrom(errors).encodePrettily(), containsString(INACTIVE_EXPENSE_CLASS.getCode()));

  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingDistributionType() {
    logger.info("=== Test update poline for opened order with changed DistributionType ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    reqData.getFundDistribution().getFirst().setDistributionType(FundDistribution.DistributionType.AMOUNT);
    reqData.getFundDistribution().getFirst().setValue(reqData.getCost().getPoLineEstimatedPrice());


    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingDFundistributionAndCost() {
    logger.info("=== Test update poline for opened order with changed DistributionType ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);
    Cost cost = reqData.getCost();
    cost.setCurrency("EUR");
    cost.setQuantityElectronic(1);
    cost.setPoLineEstimatedPrice(50d);
    reqData.setCost(cost);
    reqData.setFundDistribution(Collections.singletonList(new FundDistribution()
      .withCode("EUROHIST")
      .withFundId("e9285a1c-1dfc-4380-868c-e74073003f43")
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D)));
    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
  }

  @Test
  void testUpdatePolineForOpenedOrderWithoutUpdatingEncumbrances() {
    logger.info("=== Test update poline. Fund distributions not changed ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
    assertThat("No transaction has been created, updated or deleted",
      MockServer.getRqRsEntries(HttpMethod.POST, FINANCE_BATCH_TRANSACTIONS).isEmpty());

    // double check with updating status only (When only status updated via request from mod-invoices)
    PoLine updatedLine = getRqRsEntries(HttpMethod.PUT, PO_LINES_STORAGE).getFirst().mapTo(PoLine.class);
    updatedLine.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
    updatedLine.getDetails().getProductIds().getFirst().setQualifier(null);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(updatedLine).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
    assertThat("No transaction has been created, updated or deleted",
      MockServer.getRqRsEntries(HttpMethod.POST, FINANCE_BATCH_TRANSACTIONS).isEmpty());
  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingOnlyLocation() {
    logger.info("=== Test update poline for opened order with changed location ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING_ITEM);
    reqData.getLocations().getFirst().setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    String newLocationId = "fcd64ce1-6995-48f0-840e-89ffa2288371";
    reqData.getLocations().getFirst().setLocationId(newLocationId);

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
  void testUpdatePolineForPendingOrderWithRelatePieces() {
    logger.info("=== Test update po line for pending order with related pieces ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "fca5fa9e-15cb-4a3d-ab09-eeea99b97a47").mapTo(PoLine.class);
    String poLineId = "fca5fa9e-15cb-4a3d-ab09-eeea99b97a47";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("1ab7ef6a-d1d4-4a4f-90a2-882aed18af14");
    reqData.getLocations().getFirst().setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    String newLocationId = "fcd64ce1-6995-48f0-840e-89ffa2288371";
    reqData.getLocations().getFirst().setLocationId(newLocationId);

    Errors response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 400).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(PIECES_EXIST_FOR_POLINE.getCode()));
  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingQuantity() {
    logger.info("=== Test update poline for opened order with changed quantity ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING_ITEM);
    reqData.getLocations().getFirst().setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    reqData.getLocations().getFirst().setQuantityElectronic(3);
    reqData.getCost().setQuantityElectronic(3);

    Errors response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 400).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    List<String> errorCodes = response.getErrors()
      .stream()
      .map(Error::getCode)
      .collect(Collectors.toList());

    assertThat(errorCodes, containsInAnyOrder(LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN.getCode()));

    cleanMockEntry();
  }

  @Test
  void testUpdatePolineForOpenedOrderWithChangingQuantityAndCheckinItems() {
    logger.info("=== Test update poline for opened order with changed quantity and checkin items ===");

    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setCheckinItems(true);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING_ITEM);
    reqData.getLocations().getFirst().setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    reqData.getLocations().getFirst().setQuantityElectronic(3);
    reqData.getCost().setQuantityElectronic(3);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
  }

  @Test
  void testShouldReleaseEncumbrancesAfterCancelledPoLine() {
    logger.info("=== Test release encumbrances after cancelled PoLine  ===");

    PoLine lineFromStorage = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "740809a1-84ca-45d7-a7a8-accc21efd5bd").mapTo(PoLine.class);
    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "740809a1-84ca-45d7-a7a8-accc21efd5bd").mapTo(PoLine.class);
    reqData.setReceiptStatus(ReceiptStatus.CANCELLED);
    reqData.setPaymentStatus(PaymentStatus.CANCELLED);

    Fund fund = new Fund()
      .withId("7fbd5d84-62d1-44c6-9c45-6cb173998bbd")
      .withName("Fund")
      .withExternalAccountNo("externalNo")
      .withLedgerId("133a7916-f05e-4df4-8f7f-09eb2a7076d1");

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, lineFromStorage);
    addMockEntry(FUNDS, fund);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);

    cleanMockEntry();
  }

  @Test
  void testShouldChangeItemStatusAfterCancelledPoLine() {
    logger.info("=== Test change item status to 'Order closed' after cancelled PoLine  ===");

    PoLine firstLineFromStorage = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "740809a1-84ca-45d7-a7a8-accc21efd5bd").mapTo(PoLine.class);
    firstLineFromStorage.setReceiptStatus(ReceiptStatus.CANCELLED);
    firstLineFromStorage.setPaymentStatus(PaymentStatus.CANCELLED);
    PoLine secondLineFromStorage = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "cc189777-fd26-4ae8-b0e5-08abebb50b51").mapTo(PoLine.class);
    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "cc189777-fd26-4ae8-b0e5-08abebb50b51").mapTo(PoLine.class);
    reqData.setReceiptStatus(ReceiptStatus.CANCELLED);
    reqData.setPaymentStatus(PaymentStatus.CANCELLED);

    Fund fund = new Fund()
      .withId("7fbd5d84-62d1-44c6-9c45-6cb173998bbd")
      .withName("Fund")
      .withExternalAccountNo("externalNo")
      .withLedgerId("133a7916-f05e-4df4-8f7f-09eb2a7076d1");

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, firstLineFromStorage);
    addMockEntry(PO_LINES_STORAGE, secondLineFromStorage);
    addMockEntry(FUNDS, fund);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);

    cleanMockEntry();
  }

  @Test
  void testShouldUnreleasedEncumbrancesAfterUncancelledPoLine() {
    logger.info("=== Test unreleased encumbrances after uncancelled PoLine ===");

    PoLine lineFromStorage = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "740809a1-84ca-45d7-a7a8-accc21efd5bd").mapTo(PoLine.class);
    lineFromStorage.setReceiptStatus(ReceiptStatus.CANCELLED);
    lineFromStorage.setPaymentStatus(PaymentStatus.CANCELLED);
    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "740809a1-84ca-45d7-a7a8-accc21efd5bd").mapTo(PoLine.class);
    reqData.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
    reqData.setPaymentStatus(PaymentStatus.AWAITING_PAYMENT);

    Fund fund = new Fund()
      .withId("7fbd5d84-62d1-44c6-9c45-6cb173998bbd")
      .withName("Fund")
      .withExternalAccountNo("externalNo")
      .withLedgerId("133a7916-f05e-4df4-8f7f-09eb2a7076d1");

    addMockEntry(PIECES_STORAGE, new Piece()
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, lineFromStorage);
    addMockEntry(FUNDS, fund);

    verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
  }

  @Test
  void testUpdatePolineForOpenedOrderWithoutUpdatingItems() {
    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING);

    addMockEntry(PIECES_STORAGE, new Piece()
      .withFormat(Piece.Format.ELECTRONIC)
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    int expQtyElectronic = 3;
    reqData.getLocations().getFirst().setQuantityElectronic(expQtyElectronic);
    reqData.getLocations().getFirst().setQuantity(expQtyElectronic);
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
    PoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, "c2755a78-2f8d-47d0-a218-059a9b7391b4").mapTo(PoLine.class);
    String poLineId = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    reqData.setId(poLineId);
    reqData.setPurchaseOrderId("9d56b621-202d-414b-9e7f-5fefe4422ab3");
    reqData.getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    reqData.getEresource().setCreateInventory(INSTANCE_HOLDING);
    reqData.getLocations().getFirst().setLocationId("758258bc-ecc1-41b8-abca-f7b610822fff");

    addMockEntry(PIECES_STORAGE, new Piece()
      .withFormat(Piece.Format.ELECTRONIC)
      .withPoLineId(reqData.getId())
      .withLocationId(reqData.getLocations().getFirst().getLocationId()));

    addMockEntry(PO_LINES_STORAGE, reqData);

    String newLocationId = "fcd64ce1-6995-48f0-840e-89ffa2288371";
    reqData.getLocations().getFirst().setLocationId(newLocationId);

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

  @Test
  void testPatchPoLineWithNewInstanceId() {
    String url = String.format(LINE_BY_ID_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);

    PatchOrderLineRequest body = new PatchOrderLineRequest()
        .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    verifyPatch(url, JsonObject.mapFrom(body).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
  }

  @Test
  void testPatchPoLineWithEmptyDetails() {
    String url = String.format(LINE_BY_ID_PATH, PO_LINE_ID_WITHOUT_DETAILS);

    PatchOrderLineRequest body = new PatchOrderLineRequest()
      .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
      .withReplaceInstanceRef(new ReplaceInstanceRef()
        .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
        .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
        .withDeleteAbandonedHoldings(false));

    verifyPatch(url, JsonObject.mapFrom(body).encode(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), "", 204);
  }

  @Test
  void testPatchPoLineWithEmptyBody() {
    String url = String.format(LINE_BY_ID_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);

    PatchOrderLineRequest body = new PatchOrderLineRequest();

    Errors resp = verifyPatch(url, JsonObject.mapFrom(body).encode(), "", 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
  }

  @Test
  void testPatchPoLineByIdNotFound() {
    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);

    PatchOrderLineRequest body = new PatchOrderLineRequest()
        .withOperation(PatchOrderLineRequest.Operation.REPLACE_INSTANCE_REF)
        .withReplaceInstanceRef(new ReplaceInstanceRef()
            .withNewInstanceId("cd3288a4-898c-4347-a003-2d810ef70f03")
            .withHoldingsOperation(ReplaceInstanceRef.HoldingsOperation.CREATE)
            .withDeleteAbandonedHoldings(false));

    Response actual = verifyPatch(url, JsonObject.mapFrom(body).encode(), "", 404);

    assertEquals(ID_DOES_NOT_EXIST, actual.as(Errors.class).getErrors().getFirst().getMessage());
  }

  @Test
  public void testFundDistributionsValidationSuccessCase() {
    ValidateFundDistributionsRequest request = new ValidateFundDistributionsRequest()
      .withCost(new Cost()
        .withCurrency("USD")
        .withPoLineEstimatedPrice(100d))
      .withFundDistribution(Lists.newArrayList(
        new FundDistribution().withValue(50d).withFundId(UUID.randomUUID().toString()),
        new FundDistribution().withValue(50d).withFundId(UUID.randomUUID().toString())));

    verifyPut(PO_LINE_VALIDATE_FUND_DISTRIBUTIONS_PATH, JsonObject.mapFrom(request).encode(), "", 204);
  }

  @Test
  public void testFundDistributionsValidationWhenRequiredParamNotPresented() {
    // currency param is missed in request body
    ValidateFundDistributionsRequest request = new ValidateFundDistributionsRequest()
      .withCost(new Cost()
        .withPoLineEstimatedPrice(100d))
      .withFundDistribution(Lists.newArrayList(
        new FundDistribution().withValue(50d).withFundId(UUID.randomUUID().toString()),
        new FundDistribution().withValue(50d).withFundId(UUID.randomUUID().toString())));

    Response actual = verifyPut(PO_LINE_VALIDATE_FUND_DISTRIBUTIONS_PATH, JsonObject.mapFrom(request).encode(), "", 422);

    Error error = actual.as(Errors.class).getErrors().getFirst();
    assertEquals("must not be null", error.getMessage());
    assertEquals("cost.currency", error.getParameters().getFirst().getKey());
  }

  @Test
  public void testFundDistributionValidationWhenAmountNotMatches() {
    ValidateFundDistributionsRequest request = new ValidateFundDistributionsRequest()
      .withCost(new Cost()
        .withCurrency("USD")
        .withQuantityPhysical(1)
        .withListUnitPrice(100d))
      .withFundDistribution(Lists.newArrayList(
        new FundDistribution().withValue(49.99d).withFundId(UUID.randomUUID().toString()),
        new FundDistribution().withValue(50d).withFundId(UUID.randomUUID().toString())));

    Response actual = verifyPut(PO_LINE_VALIDATE_FUND_DISTRIBUTIONS_PATH, JsonObject.mapFrom(request).encode(), "", 422);

    Error error = actual.as(Errors.class).getErrors().getFirst();
    assertEquals(ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL.getDescription(), error.getMessage());
    assertEquals(ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL.getCode(), error.getCode());
    Parameter remainingAmountParam = error.getParameters().getFirst();
    assertEquals(FundDistributionUtils.REMAINING_AMOUNT_FIELD, remainingAmountParam.getKey());
    assertEquals(0.01d, Double.parseDouble(remainingAmountParam.getValue()));
  }

  @Test
  public void testFundDistributionValidationWhenZeroPriceAndDifferentDistributionTypes() {
    ValidateFundDistributionsRequest request = new ValidateFundDistributionsRequest()
      .withCost(new Cost()
        .withCurrency("USD")
        .withPoLineEstimatedPrice(0d))
      .withFundDistribution(Lists.newArrayList(
        new FundDistribution().withValue(49.99d).withFundId(UUID.randomUUID().toString()).withDistributionType(FundDistribution.DistributionType.PERCENTAGE),
        new FundDistribution().withValue(50d).withFundId(UUID.randomUUID().toString()).withDistributionType(FundDistribution.DistributionType.AMOUNT)));

    Response actual = verifyPut(PO_LINE_VALIDATE_FUND_DISTRIBUTIONS_PATH, JsonObject.mapFrom(request).encode(), "", 422);

    Error error = actual.as(Errors.class).getErrors().getFirst();
    assertEquals(ErrorCodes.CANNOT_MIX_TYPES_FOR_ZERO_PRICE.getDescription(), error.getMessage());
    assertEquals(ErrorCodes.CANNOT_MIX_TYPES_FOR_ZERO_PRICE.getCode(), error.getCode());
  }

  @ParameterizedTest
  @EnumSource(value = PoLine.OrderFormat.class, names = {"PHYSICAL_RESOURCE", "OTHER"}, mode = EnumSource.Mode.INCLUDE)
  void testPutPhysicalOrderLineByIdWhenSpecificElementIsPresentAndProtectedFieldsChanged(PoLine.OrderFormat orderFormat) {
    logger.info("=== Test PUT Order Line By Id - Protected fields changed ===");

    String lineId = "0009662b-8b80-4001-b704-ca10971f222d";
    JsonObject body = getMockAsJson(PO_LINES_MOCK_DATA_PATH, lineId);
    Object[] expected = new Object[]{ POLineFieldNames.ACQUISITION_METHOD.getFieldName()};
    if (PoLine.OrderFormat.ELECTRONIC_RESOURCE == orderFormat) {
      body.remove(ERESOURCE);
    }

    body.put(POLineFieldNames.ACQUISITION_METHOD.getFieldName(), TestUtils.APPROVAL_PLAN_METHOD);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    Errors errors = verifyPut(url, body, "", HttpStatus.HTTP_BAD_REQUEST.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));

    Error error = errors.getErrors().getFirst();
    assertThat(error.getCode(), equalTo(PROHIBITED_FIELD_CHANGING.getCode()));

    Object[] failedFieldNames = getModifiedProtectedFields(error);
    assertThat(failedFieldNames.length, is(expected.length));
    assertThat(expected, Matchers.arrayContainingInAnyOrder(failedFieldNames));

    // 3 calls each to fetch Order Line, Purchase Order, Identifier Type
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(2, column.size());
    assertThat(column, hasKey(PO_LINES_STORAGE));

    // Verify no message sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
  }

  @ParameterizedTest
  @EnumSource(value = PoLine.OrderFormat.class, names = {"ELECTRONIC_RESOURCE"}, mode = EnumSource.Mode.INCLUDE)
  void testPutElecOrderLineByIdWhenSpecificElementIsPresentAndProtectedFieldsChanged() {
    logger.info("=== Test PUT Order Line By Id - Protected fields changed ===");

    String lineId = "0009662b-8b80-4001-b704-ca10971f222d";
    JsonObject body = getMockAsJson(PO_LINES_MOCK_DATA_PATH, lineId);
    Object[] expected = new Object[]{ POLineFieldNames.ACQUISITION_METHOD.getFieldName()};
    body.remove(ERESOURCE);

    body.put(POLineFieldNames.ACQUISITION_METHOD.getFieldName(), TestUtils.APPROVAL_PLAN_METHOD);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    Errors errors = verifyPut(url, body, "", HttpStatus.HTTP_BAD_REQUEST.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));

    Error error = errors.getErrors().getFirst();
    assertThat(error.getCode(), equalTo(PROHIBITED_FIELD_CHANGING.getCode()));

    Object[] failedFieldNames = getModifiedProtectedFields(error);
    assertThat(failedFieldNames.length, is(expected.length));
    assertThat(expected, Matchers.arrayContainingInAnyOrder(failedFieldNames));

    // 3 calls each to fetch Order Line, Purchase Order, Identifier Type
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(2, column.size());
    assertThat(column, hasKey(PO_LINES_STORAGE));

    // Verify no message sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
  }

  @ParameterizedTest
  @EnumSource(value = PoLine.OrderFormat.class, names = {"P_E_MIX"}, mode = EnumSource.Mode.INCLUDE)
  void testPutMixedOrderLineByIdWhenSpecificElementIsPresentAndProtectedFieldsChanged() {
    logger.info("=== Test PUT Order Line By Id - Protected fields changed ===");

    String lineId = "0009662b-8b80-4001-b704-ca10971f222d";
    JsonObject body = getMockAsJson(PO_LINES_MOCK_DATA_PATH, lineId);
    Object[] expected = new Object[]{ POLineFieldNames.ACQUISITION_METHOD.getFieldName()};

    body.put(POLineFieldNames.ACQUISITION_METHOD.getFieldName(), TestUtils.APPROVAL_PLAN_METHOD);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    Errors errors = verifyPut(url, body, "", HttpStatus.HTTP_BAD_REQUEST.toInt()).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));

    Error error = errors.getErrors().getFirst();
    assertThat(error.getCode(), equalTo(PROHIBITED_FIELD_CHANGING.getCode()));

    Object[] failedFieldNames = getModifiedProtectedFields(error);
    assertThat(failedFieldNames.length, is(expected.length));
    assertThat(expected, Matchers.arrayContainingInAnyOrder(failedFieldNames));

    // 3 calls each to fetch Order Line, Purchase Order, Identifier Type
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(2, column.size());
    assertThat(column, hasKey(PO_LINES_STORAGE));

    // Verify no message sent via event bus
    HandlersTestHelper.verifyOrderStatusUpdateEvent(0);
  }

  private String getPoLineWithMinContentAndIds(String lineId) throws IOException {
    PoLine poLine = new JsonObject(getMockData(PO_LINE_MIN_CONTENT_PATH)).mapTo(PoLine.class);
    poLine.setId(lineId);
    poLine.setPurchaseOrderId(org.folio.TestConstants.PO_ID_PENDING_STATUS_WITH_PO_LINES);
    return JsonObject.mapFrom(poLine).encode();
  }

  private void cleanMockEntry() {
    try {
      clearServiceInteractions();
      Thread.sleep(5000);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
  }

}
