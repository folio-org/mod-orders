package org.folio.rest.impl;

import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.apache.commons.lang3.StringUtils;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.POLineProtectedFields;
import org.folio.rest.jaxrs.model.*;
import org.folio.rest.jaxrs.model.Error;
import org.junit.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.folio.orders.utils.ErrorCodes.*;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.rest.impl.AcquisitionsUnitsHelper.ACQUISITIONS_UNIT_IDS;
import static org.folio.rest.impl.AcquisitionsUnitsHelper.NO_ACQ_UNIT_ASSIGNED_CQL;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.ORDER_ID_WITH_PO_LINES;
import static org.folio.rest.impl.MockServer.PO_NUMBER_ERROR_X_OKAPI_TENANT;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getOrderLineSearches;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.folio.rest.impl.PurchaseOrdersApiTest.PURCHASE_ORDER_ID;
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
import static org.junit.Assert.*;

public class PurchaseOrderLinesApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PurchaseOrderLinesApiTest.class);

  private static final String PO_LINE_ID_WITH_SOME_SUB_OBJECTS_ALREADY_REMOVED = "0009662b-8b80-4001-b704-ca10971f175d";
  static final String ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  public final static String LINES_PATH = "/orders/order-lines";
  private static final String LINE_BY_ID_PATH = "/orders/order-lines/%s";
  static final String COMP_PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String PO_LINE_MIN_CONTENT_PATH = COMP_PO_LINES_MOCK_DATA_PATH + "minimalContent.json";

  @Test
  public void testPostOrderLine() {
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
  public void testPostOrdersLinePhysicalFormatIncorrectQuantity() {
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
  public void testPostOrdersLineElectronicFormatIncorrectQuantity() {
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
    eresource.setCreateInventory(org.folio.rest.jaxrs.model.Eresource.CreateInventory.NONE);
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
  public void testPostOrdersLinePhysicalFormatDiscount() {
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
  public void testPostWithEmptyConfigOverLimit() {
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
  public void testPoLineCreationIfPoAlreadyReachedLimit() {
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
  public void testPostOrdersLinesByIdPoLineWithoutId() throws IOException {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");

    Errors resp = verifyPostResponse(LINES_PATH, getMockData(PO_LINE_MIN_CONTENT_PATH),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(ErrorCodes.MISSING_ORDER_ID_IN_POL.getCode(), resp.getErrors().get(0).getCode());
  }

  @Test
  public void testPostOrdersLinesByIdPoLineWithNonExistPurchaseOrder() {
    logger.info("=== Test Post Order Lines By Id (empty id in body) ===");
    JsonObject reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);
    reqData.put(PURCHASE_ORDER_ID, ID_DOES_NOT_EXIST);

    Errors resp = verifyPostResponse(LINES_PATH, reqData.encodePrettily(),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(ErrorCodes.ORDER_NOT_FOUND.getCode(), resp.getErrors().get(0).getCode());
  }

  @Test
  public void testAddOrderLineToOrderInClosedStatus() {
    logger.info("=== Test add new line to Closed order ===");

    validateNoLineCreatedForNonPendingOrder(PO_ID_CLOSED_STATUS, ORDER_CLOSED.getCode());
  }

  @Test
  public void testAddOrderLineToOrderInOpenStatus() {
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
  public void testCreatePoLineWithGetPoLineNumberError() throws IOException {
    logger.info("=== Test Create PO Line - fail on PO Line number generation ===");
    JsonObject json = new JsonObject(getMockData(String.format("%s%s.json", COMP_PO_LINES_MOCK_DATA_PATH, PO_LINE_ID_FOR_SUCCESS_CASE)));
    CompositePoLine poLine = json.mapTo(CompositePoLine.class);
    poLine.setId("0009662b-8b80-4001-b704-ca10971f175d");
    poLine.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");
    verifyPostResponse(LINES_PATH, JsonObject.mapFrom(poLine).encode(), prepareHeaders(PO_NUMBER_ERROR_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 500);
  }

  @Test
  public void testPutOrderLineElectronicFormatIncorrectQuantityAndPrice() {
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

    assertThat(response.getErrors(), hasSize(9));
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
      PHYSICAL_COST_LOC_QTY_MISMATCH.getCode(),
      MISSING_MATERIAL_TYPE.getCode()));


    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testValidationOnPutLineWithoutBody() {
    logger.info("=== Test validation on PUT line with no body ===");
    verifyPut(String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST), "", TEXT_PLAIN, 400);
  }

  @Test
  public void testValidationOnPutWithIncorrectLang() throws IOException {
    logger.info("=== Test validation on PUT line with invalid lang query parameter ===");
    verifyPut(String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST) + INVALID_LANG, getMockData(PO_LINE_MIN_CONTENT_PATH), TEXT_PLAIN, 400);
  }

  @Test
  public void testValidationOnPutWithIncorrectLineId() throws IOException {
    logger.info("=== Test validation on PUT line with invalid line ID path parameter ===");
    verifyPut(String.format(LINE_BY_ID_PATH, ID_BAD_FORMAT), getMockData(PO_LINE_MIN_CONTENT_PATH), TEXT_PLAIN, 400);
  }

  @Test
  public void testPutOrderLineByIdWithEmptyBody() throws IOException {
    logger.info("=== Test PUT Order Line By Id With Empty Json - validation error because of required properties like PO ID ===");

    String url = String.format(LINE_BY_ID_PATH, PO_LINE_ID_FOR_SUCCESS_CASE);

    Errors resp = verifyPut(url, getMockData(PO_LINE_MIN_CONTENT_PATH), "", 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
  }

  @Test
  public void testPutOrderLineById() {
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
    verifyOrderStatusUpdateEvent(1);
  }

  @Test
  public void testPutOrderLineByIdWithoutOrderUpdate() {
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
    verifyOrderStatusUpdateEvent(0);
  }


  @Test
  public void testPutOrderLineByIdProtectedFieldsChanged() {
    logger.info("=== Test PUT Order Line By Id - Protected fields changed ===");

    String lineId = "0009662b-8b80-4001-b704-ca10971f175d";
    JsonObject body = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, lineId);
    String url = String.format(LINE_BY_ID_PATH, lineId);

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(POLineProtectedFields.ACQUISITION_METHOD.getFieldName(),
        CompositePoLine.AcquisitionMethod.APPROVAL_PLAN.value());
    allProtectedFieldsModification.put(POLineProtectedFields.TITLE.getFieldName(), "Testing ProtectedFields");
    allProtectedFieldsModification.put(POLineProtectedFields.DONOR.getFieldName(), "Donor");
    allProtectedFieldsModification.put(POLineProtectedFields.COST_CURRENCY.getFieldName(), "EUR");
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

    // 3 calls each to fetch Order Line , Purchase Order and ISBN validation
    Map<String, List<JsonObject>> column = MockServer.serverRqRs.column(HttpMethod.GET);
    assertEquals(3, column.size());
    assertThat(column, hasKey(PO_LINES));

    // Verify no message sent via event bus
    verifyOrderStatusUpdateEvent(0);
  }

  @Test
  public void testPutOrderLineByIdNotFound() throws IOException {
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
  public void testPutOrderLineByIdWithInvalidContentInBody() throws IOException {
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
  public void testPutOrderLineByIdWithIdMismatch() throws IOException {
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
  public void testPutOrderLineById500FromStorageOnGetPoLine() throws IOException {
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
  public void testDeleteOrderLineById() {
    logger.info("=== Test Delete Order Line By Id - Success case ===");

    deleteOrderLineByIdSuccess(PO_LINE_ID_FOR_SUCCESS_CASE);
  }

  @Test
  public void testDeleteOrderLineByIdWithPartiallyDeletedSubObjects() {
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
  public void testDeleteOrderLineByIdNotFound() {
    logger.info("=== Test Delete Order Line By Id - Not Found ===");

    String url = String.format(LINE_BY_ID_PATH, ID_DOES_NOT_EXIST);
    Response actual = verifyDeleteResponse(url, APPLICATION_JSON, 404);

    assertEquals(ID_DOES_NOT_EXIST, actual.as(Errors.class).getErrors().get(0).getMessage());
  }

  @Test
  public void testDeleteOrderLineById500FromStorageOnSubObjectDeletion() {
    logger.info("=== Test Delete Order Line By Id - 500 From Storage On Sub-Object deletion ===");

    String url = String.format(LINE_BY_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR);
    Response actual = verifyDeleteResponse(url, APPLICATION_JSON, 500);

    assertNotNull(actual.asString());
  }

  @Test
  public void testGetOrderLineById() {
    logger.info("=== Test Get Orderline By Id ===");

    final CompositePoLine resp = verifySuccessGet(String.format(LINE_BY_ID_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE), CompositePoLine.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE, resp.getId());
    assertEquals(1, getPoLineSearches().size());
    assertNull(getOrderLineSearches());
  }

  @Test
  public void testGetOrderLineByIdWith404() {
    logger.info("=== Test Get Orderline By Id - With 404 ===");

    String lineId = ID_DOES_NOT_EXIST;

    final Response resp = verifyGet(String.format(LINE_BY_ID_PATH, lineId), APPLICATION_JSON, 404);

    assertEquals(lineId, resp.getBody().as(Errors.class).getErrors().get(0).getMessage());
  }

  @Test
  public void testGetOrderLineByIdWith500() {
    logger.info("=== Test Get Orderline By Id - With 500 ===");

    final Response resp = verifyGet(String.format(LINE_BY_ID_PATH, ID_FOR_INTERNAL_SERVER_ERROR), APPLICATION_JSON, 500);

    assertEquals("Internal Server Error", resp.getBody().as(Errors.class).getErrors().get(0).getMessage());
  }

  @Test
  public void testGetOrderLinesInternalServerError() {
    logger.info("=== Test Get Order Lines by query - emulating 500 from storage ===");

    String endpointQuery = String.format("%s?query=%s", LINES_PATH, ID_FOR_INTERNAL_SERVER_ERROR);

    verifyGet(endpointQuery, APPLICATION_JSON, 500);
  }

  @Test
  public void testGetOrderLinesBadQuery() {
    logger.info("=== Test Get Order Lines by query - unprocessable query to emulate 400 from storage ===");

    String endpointQuery = String.format("%s?query=%s", LINES_PATH, BAD_QUERY);

    verifyGet(endpointQuery, APPLICATION_JSON, 400);

  }

  @Test
  public void testGetOrderPoLinesByEmptyQuery() {
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
  public void testGetOrderPOLinesByPoId() {
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
  public void testPostOrdersWithInvalidIsbn() {
    logger.info("=== Test Post Order line with invalid ISBN ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");

    String isbn = "1234";

    reqData.getDetails().getProductIds().get(0).setProductId(isbn);


    Response resp = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 400);

    Error err = resp.getBody()
        .as(Errors.class)
        .getErrors()
        .get(0);

      assertThat(err.getMessage(), equalTo(String.format("ISBN value %s is invalid", isbn)));
  }

  @Test
  public void testPostOrderLinetoConvertToIsbn13() {
    logger.info("=== Test Post order line to verify ISBN 10 is normalized to ISBN 13 ===");

    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE).mapTo(CompositePoLine.class);
    // To skip permission validation by units
    reqData.setId("0009662b-8b80-4001-b704-ca10971f175d");
    reqData.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");

    String isbn = "0-19-852663-6";

    reqData.getDetails().getProductIds().get(0).setProductId(isbn);

    CompositePoLine resp = verifyPostResponse(LINES_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePoLine.class);

    assertThat(resp.getDetails().getProductIds().get(0).getProductId(), equalTo("9780198526636"));
  }

  @Test
  public void testPutOrdersWithInvalidIsbn() {
    logger.info("=== Test Put Order line with invalid ISBN ===");
    CompositePoLine reqData = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE)
      .mapTo(CompositePoLine.class);
    reqData.setId(ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE);
    String isbn = "1234";

    reqData.getDetails().getProductIds().get(0).setProductId(isbn);

    Response response = verifyPut(String.format(LINE_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON,
        400);

    Error err = response.getBody().as(Errors.class).getErrors().get(0);

    assertThat(err.getMessage(), equalTo(String.format("ISBN value %s is invalid", isbn)));
  }

  private String getPoLineWithMinContentAndIds(String lineId, String orderId) throws IOException {
    CompositePoLine poLine = new JsonObject(getMockData(PO_LINE_MIN_CONTENT_PATH)).mapTo(CompositePoLine.class);
    poLine.setId(lineId);
    poLine.setPurchaseOrderId(orderId);
    return JsonObject.mapFrom(poLine).encode();
  }

}
