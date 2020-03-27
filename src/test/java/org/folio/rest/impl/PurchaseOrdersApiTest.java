package org.folio.rest.impl;

import static java.util.stream.Collectors.toList;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.containsAny;
import static org.folio.orders.utils.ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID;
import static org.folio.orders.utils.ErrorCodes.COST_UNIT_PRICE_INVALID;
import static org.folio.orders.utils.ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.ELECTRONIC_COST_LOC_QTY_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.orders.utils.ErrorCodes.GENERIC_ERROR_CODE;
import static org.folio.orders.utils.ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL;
import static org.folio.orders.utils.ErrorCodes.INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE;
import static org.folio.orders.utils.ErrorCodes.ISBN_NOT_VALID;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ErrorCodes.MISSING_MATERIAL_TYPE;
import static org.folio.orders.utils.ErrorCodes.MISSING_ONGOING;
import static org.folio.orders.utils.ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY;
import static org.folio.orders.utils.ErrorCodes.ONGOING_NOT_ALLOWED;
import static org.folio.orders.utils.ErrorCodes.ORDER_CLOSED;
import static org.folio.orders.utils.ErrorCodes.ORDER_OPEN;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.ORGANIZATION_NOT_A_VENDOR;
import static org.folio.orders.utils.ErrorCodes.PHYSICAL_COST_LOC_QTY_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.POL_LINES_LIMIT_EXCEEDED;
import static org.folio.orders.utils.ErrorCodes.VENDOR_ISSUE;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_ELECTRONIC_QTY;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_PHYSICAL_QTY;
import static org.folio.orders.utils.ErrorCodes.ZERO_LOCATION_QTY;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.INSTANCE_ID;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.calculateTotalEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.calculateTotalQuantity;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.orders.utils.ResourcePathResolver.PAYMENT_STATUS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.RECEIPT_STATUS;
import static org.folio.orders.utils.ResourcePathResolver.SEARCH_ORDERS;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.VENDOR_ID;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_PERMISSIONS;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.impl.AbstractHelper.MAX_IDS_FOR_GET_RQ;
import static org.folio.rest.impl.AcquisitionsUnitsHelper.ACQUISITIONS_UNIT_IDS;
import static org.folio.rest.impl.AcquisitionsUnitsHelper.NO_ACQ_UNIT_ASSIGNED_CQL;
import static org.folio.rest.impl.FinanceInteractionsTestHelper.verifyEncumbrancesOnPoCreation;
import static org.folio.rest.impl.FinanceInteractionsTestHelper.verifyEncumbrancesOnPoUpdate;
import static org.folio.rest.impl.InventoryHelper.DEFAULT_INSTANCE_STATUS_CODE;
import static org.folio.rest.impl.InventoryHelper.DEFAULT_INSTANCE_TYPE_CODE;
import static org.folio.rest.impl.InventoryHelper.DEFAULT_LOAN_TYPE_NAME;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_STATUS_ID;
import static org.folio.rest.impl.InventoryHelper.INSTANCE_TYPE_ID;
import static org.folio.rest.impl.InventoryHelper.ITEMS;
import static org.folio.rest.impl.InventoryHelper.ITEM_MATERIAL_TYPE_ID;
import static org.folio.rest.impl.InventoryInteractionTestHelper.joinExistingAndNewItems;
import static org.folio.rest.impl.InventoryInteractionTestHelper.verifyInstanceLinksForUpdatedOrder;
import static org.folio.rest.impl.InventoryInteractionTestHelper.verifyInventoryInteraction;
import static org.folio.rest.impl.InventoryInteractionTestHelper.verifyPiecesCreated;
import static org.folio.rest.impl.InventoryInteractionTestHelper.verifyPiecesQuantityForSuccessCase;
import static org.folio.rest.impl.MockServer.BUDGET_IS_INACTIVE_TENANT;
import static org.folio.rest.impl.MockServer.BUDGET_NOT_FOUND_FOR_TRANSACTION_TENANT;
import static org.folio.rest.impl.MockServer.FUND_CANNOT_BE_PAID_TENANT;
import static org.folio.rest.impl.MockServer.ITEM_RECORDS;
import static org.folio.rest.impl.MockServer.LEDGER_NOT_FOUND_FOR_TRANSACTION_TENANT;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.MockServer.getContributorNameTypesSearches;
import static org.folio.rest.impl.MockServer.getCreatedEncumbrances;
import static org.folio.rest.impl.MockServer.getCreatedHoldings;
import static org.folio.rest.impl.MockServer.getCreatedInstances;
import static org.folio.rest.impl.MockServer.getCreatedItems;
import static org.folio.rest.impl.MockServer.getCreatedOrderSummaries;
import static org.folio.rest.impl.MockServer.getCreatedPieces;
import static org.folio.rest.impl.MockServer.getHoldingsSearches;
import static org.folio.rest.impl.MockServer.getInstanceStatusesSearches;
import static org.folio.rest.impl.MockServer.getInstanceTypesSearches;
import static org.folio.rest.impl.MockServer.getInstancesSearches;
import static org.folio.rest.impl.MockServer.getItemUpdates;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getLoanTypesSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.MockServer.getPurchaseOrderUpdates;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.folio.rest.impl.PoNumberApiTest.EXISTING_PO_NUMBER;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.endsWith;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.orders.utils.AcqDesiredPermissions;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.POLineProtectedFields;
import org.folio.orders.utils.POProtectedFields;
import org.folio.rest.acq.model.Ongoing;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembershipCollection;
import org.folio.rest.jaxrs.model.CloseReason;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat;
import org.folio.rest.jaxrs.model.CompositePoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus;
import org.folio.rest.jaxrs.model.Contributor;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Physical.CreateInventory;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrders;
import org.folio.rest.jaxrs.model.Title;
import org.hamcrest.beans.HasPropertyWithValue;
import org.hamcrest.core.Every;
import org.hamcrest.core.Is;
import org.junit.Test;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class PurchaseOrdersApiTest extends ApiTestBase {

  private static final String PENDING_ORDER_APPROVED_FALSE = "e5ae4afd-3fa9-494e-a972-f541df9b877e";

  private static final Logger logger = LoggerFactory.getLogger(PurchaseOrdersApiTest.class);

  private static final String ORDER_WITHOUT_PO_LINES = "order_without_po_lines.json";
  private static final String ORDER_WITHOUT_VENDOR_ID = "order_without_vendor_id.json";
  public static final String ORDER_WITH_PO_LINES_JSON = "put_order_with_po_lines.json";
  private static final String ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON = "put_order_with_mismatch_id_in_po_lines.json";
  private static final String ORDER_WITHOUT_MATERIAL_TYPE_JSON = "order_po_line_without_material_type.json";

  static final String ACTIVE_VENDOR_ID = "d0fb5aa0-cdf1-11e8-a8d5-f2801f1b9fd1";
  static final String INACTIVE_VENDOR_ID = "b1ef7e96-98f3-4f0d-9820-98c322c989d2";
  static final String NON_EXIST_VENDOR_ID = "bba87500-6e71-4057-a2a9-a091bac7e0c1";
  static final String MOD_VENDOR_INTERNAL_ERROR_ID = "bba81500-6e41-4057-a2a9-a081bac7e0c1";
  static final String VENDOR_WITH_BAD_CONTENT = "5a34ae0e-5a11-4337-be95-1a20cfdc3161";
  static final String ORGANIZATION_NOT_VENDOR = "52a7669b-0e6d-4513-92be-14086c7d10e6";
  private static final String EXISTING_REQUIRED_VENDOR_UUID = "168f8a86-d26c-406e-813f-c7527f241ac3";

  static final String ACTIVE_ACCESS_PROVIDER_A = "858e80d2-f562-4c54-9934-6e274dee511d";
  static final String ACTIVE_ACCESS_PROVIDER_B = "d1b79c8d-4950-482f-8e42-04f9aae3cb40";
  static final String INACTIVE_ACCESS_PROVIDER_A = "f6cd1850-2587-4f6c-b680-9b27ff26d619";
  static final String INACTIVE_ACCESS_PROVIDER_B = "f64bbcae-e5ea-42b6-8236-55fefed0fb8f";
  static final String NON_EXIST_ACCESS_PROVIDER_A = "160501b3-52dd-31ec-a0ce-17762e6a9b47";

  static final String ID_FOR_PRINT_MONOGRAPH_ORDER = "00000000-1111-2222-8888-999999999999";
  private static final String PO_ID_FOR_FAILURE_CASE = "bad500aa-aaaa-500a-aaaa-aaaaaaaaaaaa";
  private static final String ORDER_ID_WITHOUT_PO_LINES = "50fb922c-3fa9-494e-a972-f2801f1b9fd1";
  private static final String ORDER_WITHOUT_WORKFLOW_STATUS = "41d56e59-46db-4d5e-a1ad-a178228913e5";
  static final String ORDER_WIT_PO_LINES_FOR_SORTING =  "9a952cd0-842b-4e71-bddd-014eb128dc8e";
  static final String VALID_FUND_ID =  "fb7b70f1-b898-4924-a991-0e4b6312bb5f";
  static final String FUND_ID_RESTRICTED =  "72330c92-087e-4cdc-a82e-acadd9332659";
  static final String VALID_LEDGER_ID =  "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d61";
  public static final String ORDER_WITHOUT_MATERIAL_TYPES_ID =  "0cb6741d-4a00-47e5-a902-5678eb24478d";

  // API paths
  public final static String COMPOSITE_ORDERS_PATH = "/orders/composite-orders";
  private final static String COMPOSITE_ORDERS_BY_ID_PATH = COMPOSITE_ORDERS_PATH + "/%s";

  static final String LISTED_PRINT_MONOGRAPH_PATH = "po_listed_print_monograph.json";
  private static final String ORDERS_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + "getOrders.json";
  private static final String ORDER_FOR_FAILURE_CASE_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + PO_ID_FOR_FAILURE_CASE + ".json";
  private static final String PE_MIX_PATH = "po_listed_print_monograph_pe_mix.json";
  private static final String MONOGRAPH_FOR_CREATE_INVENTORY_TEST = "print_monograph_for_create_inventory_test.json";
  private static final String LISTED_PRINT_SERIAL_PATH = "po_listed_print_serial.json";
  private static final String MINIMAL_ORDER_PATH = "minimal_order.json";
  private static final String PO_CREATION_FAILURE_PATH = "po_creation_failure.json";
  private static final String ELECTRONIC_FOR_CREATE_INVENTORY_TEST = "po_listed_electronic_monograph.json";

  private static final String NULL = "null";
  static final String PURCHASE_ORDER_ID = "purchaseOrderId";
  public static final String ORDER_DELETE_ERROR_TENANT = "order_delete_error";
  static final Header ERROR_ORDER_DELETE_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, ORDER_DELETE_ERROR_TENANT);

  public static final Header ALL_DESIRED_PERMISSIONS_HEADER = new Header(OKAPI_HEADER_PERMISSIONS, new JsonArray(AcqDesiredPermissions.getValues()).encode());
  public static final Header APPROVAL_PERMISSIONS_HEADER = new Header(OKAPI_HEADER_PERMISSIONS, new JsonArray(Collections.singletonList("orders.item.approve")).encode());
  static final String ITEMS_NOT_FOUND = UUID.randomUUID().toString();
  
  @Test
  public void testValidFundDistributionTotalPercentage() throws Exception {
    logger.info("=== Test fund distribution total must add upto totalEstimatedPrice - valid total percentage ===");

    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(reqData);

    reqData.setWorkflowStatus(WorkflowStatus.OPEN);

    // Make sure expected number of PO Lines available
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    // Calculated poLineEstimatedPrice = 47.98
    // Calculate remaining Percentage for fundDistribution1 = 47.98 - 23.99(50%) = 23.99
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setDistributionType(DistributionType.PERCENTAGE);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setValue(50d);

    // Calculate remaining Percentage for fundDistribution2 = 23.99 - 23.99(50%) = 0.0
    reqData.getCompositePoLines().get(0).getFundDistribution().get(1).setDistributionType(DistributionType.PERCENTAGE);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(1).setValue(50d);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
        prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);
    
    assertThat(resp.getCompositePoLines().get(0).getCost().getPoLineEstimatedPrice(), equalTo(47.98));
  }

  @Test
  public void testInvalidFundDistributionTotalPercentage() throws Exception {
    logger.info("===  Test fund distribution total must add upto totalEstimatedPrice - invalid total percentage ===");

    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(reqData);

    reqData.setWorkflowStatus(WorkflowStatus.OPEN);

    // Make sure expected number of PO Lines available
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    // Calculated poLineEstimatedPrice = 47.98
    // Calculate remaining Amount for fundDistribution1 = 47.98 - 47.98(100%) = 0
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setDistributionType(DistributionType.PERCENTAGE);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setValue(100d);

    // Calculate remaining Amount for fundDistribution2 = 0 - 47.98(100%) = -47.98
    reqData.getCompositePoLines().get(0).getFundDistribution().get(1).setDistributionType(DistributionType.PERCENTAGE);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(1).setValue(100d);

    // Amount < 0 is not allowed
    Errors errorResponse = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
        prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errorResponse.getErrors(), hasSize(1));

    Error error = errorResponse.getErrors().get(0);

    assertThat(error.getCode(), is(INCORRECT_FUND_DISTRIBUTION_TOTAL.getCode()));
  }

  @Test
  public void testInvalidFundDistributionTotalAmountPercentage() throws Exception {
    logger.info("===  Test fund distribution total must add upto totalEstimatedPrice - invalid total amount and percentage ===");

    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);

    reqData.setWorkflowStatus(WorkflowStatus.OPEN);

    // Make sure expected number of PO Lines available
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    // Calculated poLineEstimatedPrice = 47.98
    // Calculate remaining Amount for fundDistribution1 = 47.98 - 10 = 37.98
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setDistributionType(DistributionType.AMOUNT);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setValue(10d);

    // Calculate remaining percentage for fundDistribution2 = 37.98 - 40.783(85%) = -2.803
    reqData.getCompositePoLines().get(0).getFundDistribution().get(1).setDistributionType(DistributionType.PERCENTAGE);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(1).setValue(85d);

    Errors errorResponse = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
        prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errorResponse.getErrors(), hasSize(1));

    Error error = errorResponse.getErrors().get(0);

    assertThat(error.getCode(), is(INCORRECT_FUND_DISTRIBUTION_TOTAL.getCode()));
  }

  @Test
  public void testInvalidFundDistributionTotalAmount() throws Exception {
    logger.info("===  Test fund distribution total must add upto totalEstimatedPrice - invalid total amount ===");

    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_SERIAL_PATH));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);

    reqData.setWorkflowStatus(WorkflowStatus.OPEN);
    reqData.setTotalEstimatedPrice(200d);
    // Make sure expected number of PO Lines available
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    // Calculated poLineEstimatedPrice = 47.98
    // Calculate remaining Amount for fundDistribution1 = 47.98 - 40 = 7.98
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setDistributionType(DistributionType.AMOUNT);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(0).setValue(40d);

    // Calculate remaining Amount for fundDistribution2 = 7.98 - 8 = -0.02
    reqData.getCompositePoLines().get(0).getFundDistribution().get(1).setDistributionType(DistributionType.AMOUNT);
    reqData.getCompositePoLines().get(0).getFundDistribution().get(1).setValue(8d);

    // Amount < 0 is not allowed
    Errors errorResponse = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
        prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errorResponse.getErrors(), hasSize(1));

    Error error = errorResponse.getErrors().get(0);

    assertThat(error.getCode(), is(INCORRECT_FUND_DISTRIBUTION_TOTAL.getCode()));
  }

  @Test
  public void testListedPrintMonograph() throws Exception {
    logger.info("=== Test Listed Print Monograph ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(reqData);

    // Make sure expected number of PO Lines available
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    assertThat(reqData.getCompositePoLines().get(0).getOrderFormat(), equalTo(OrderFormat.P_E_MIX));
    assertThat(reqData.getCompositePoLines().get(1).getOrderFormat(), equalTo(OrderFormat.ELECTRONIC_RESOURCE));

    // Prepare cost details for the first PO Line (see MODORDERS-180 and MODORDERS-181)
    Cost cost = reqData.getCompositePoLines().get(0).getCost();
    cost.setAdditionalCost(10d);
    cost.setDiscount(3d);
    cost.setDiscountType(Cost.DiscountType.PERCENTAGE);
    cost.setQuantityElectronic(1);
    cost.setListUnitPriceElectronic(5.5d);
    cost.setQuantityPhysical(3);
    cost.setListUnitPrice(9.99d);
    cost.setPoLineEstimatedPrice(null);
    double expectedTotalPoLine1 = 44.41d;

    // Prepare cost details for the second PO Line (see MODORDERS-180 and MODORDERS-181)
    cost = reqData.getCompositePoLines().get(1).getCost();
    cost.setAdditionalCost(2d);
    cost.setDiscount(4.99d);
    cost.setDiscountType(Cost.DiscountType.AMOUNT);
    cost.setQuantityElectronic(3);
    cost.setListUnitPriceElectronic(11.99d);
    cost.setPoLineEstimatedPrice(null);
    double expectedTotalPoLine2 = 32.98d;

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertThat(poId, notNullValue());
    assertThat(poNumber, notNullValue());
    assertThat(resp.getCompositePoLines(), hasSize(2));
    assertThat(resp.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));

    for (int i = 0; i < resp.getCompositePoLines().size(); i++) {
      CompositePoLine line = resp.getCompositePoLines().get(i);
      String polNumber = line.getPoLineNumber();
      String polId = line.getId();

      assertThat(line.getPurchaseOrderId(), equalTo(poId));
      assertThat(polId, notNullValue());
      assertThat(polNumber, notNullValue());
      assertThat(polNumber, startsWith(poNumber));
      assertThat(line.getInstanceId(), nullValue());
      line.getLocations().forEach(location -> verifyLocationQuantity(location, line.getOrderFormat()));
    }

    // see MODORDERS-180 and MODORDERS-181
    CompositePoLine compositePoLine1 = resp.getCompositePoLines().get(0);
    CompositePoLine compositePoLine2 = resp.getCompositePoLines().get(1);
    assertThat(compositePoLine1.getCost().getPoLineEstimatedPrice(), equalTo(expectedTotalPoLine1));
    assertThat(compositePoLine2.getCost().getPoLineEstimatedPrice(), equalTo(expectedTotalPoLine2));

    // the sum might be wrong if using regular double e.g. 44.41d + 32.98d results to 77.38999999999999
    double expectedTotal = BigDecimal.valueOf(expectedTotalPoLine1)
                                     .add(BigDecimal.valueOf(expectedTotalPoLine2))
                                     .doubleValue();
    assertThat(resp.getTotalEstimatedPrice(), equalTo(expectedTotal));

    List<JsonObject> poLines = MockServer.serverRqRs.get(PO_LINES, HttpMethod.POST);
    assertThat(poLines, hasSize(2));
    poLines.forEach(line -> {
      PoLine poLine = line.mapTo(PoLine.class);
      Double poLineEstimatedPrice = poLine.getCost().getPoLineEstimatedPrice();
      if (compositePoLine1.getId().equals(poLine.getId())) {
        assertThat(poLineEstimatedPrice, equalTo(expectedTotalPoLine1));
      } else {
        assertThat(poLineEstimatedPrice, equalTo(expectedTotalPoLine2));
      }
    });
    assertThat(getCreatedEncumbrances(), empty());
  }

  @Test
  public void testPostOrderWithIncorrectCost() throws Exception {
    logger.info("=== Test Order creation - Cost validation fails ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Assert that there are 2 lines
    assertEquals(2, reqData.getCompositePoLines().size());

    // Set incorrect quantities for the first PO Line
    CompositePoLine firstPoLine = reqData.getCompositePoLines().get(0);
    firstPoLine.setOrderFormat(CompositePoLine.OrderFormat.P_E_MIX);
    firstPoLine.getCost().setQuantityPhysical(1);
    firstPoLine.getCost().setQuantityElectronic(0);
    firstPoLine.getCost().setListUnitPrice(-10d);
    firstPoLine.getCost().setListUnitPriceElectronic(-5d);
    firstPoLine.getLocations().forEach(location -> {
      location.setQuantityElectronic(1);
      location.setQuantityPhysical(2);
    });

    // Set incorrect quantities for the second PO Line
    CompositePoLine secondPoLine = reqData.getCompositePoLines().get(1);
    secondPoLine.setOrderFormat(CompositePoLine.OrderFormat.OTHER);
    secondPoLine.getCost().setQuantityPhysical(0);
    secondPoLine.getCost().setQuantityElectronic(1);
    secondPoLine.getCost().setListUnitPrice(-1d);
    secondPoLine.getCost().setListUnitPriceElectronic(10d);
    secondPoLine.getLocations().forEach(location -> {
      location.setQuantityElectronic(0);
      location.setQuantityPhysical(1);
    });

    final Errors response = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(response.getErrors(), hasSize(11));
    Set<String> errorCodes = response.getErrors()
                                     .stream()
                                     .map(Error::getCode)
                                     .collect(Collectors.toSet());

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_PHYSICAL_QTY.getCode(),
                                              ZERO_COST_ELECTRONIC_QTY.getCode(),
                                              NON_ZERO_COST_ELECTRONIC_QTY.getCode(),
                                              PHYSICAL_COST_LOC_QTY_MISMATCH.getCode(),
                                              ELECTRONIC_COST_LOC_QTY_MISMATCH.getCode(),
                                              COST_UNIT_PRICE_ELECTRONIC_INVALID.getCode(),
                                              COST_UNIT_PRICE_INVALID.getCode()));

    // Check that no other calls are made by the business logic to other services, except for ISBN validation
    assertEquals(2, MockServer.serverRqRs.size());
  }

  @Test
  public void testPostOneTimeOrderWithOngoingFields() {
    logger.info("=== Test Order creation - Ongoing field validation fails ===");

    CompositePurchaseOrder reqData = getMinimalContentCompositePurchaseOrder();
    reqData.setOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);
    reqData.setOngoing(new org.folio.rest.jaxrs.model.Ongoing());

    final Errors response = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(response.getErrors(), hasSize(1));
    Error error = response.getErrors().get(0);

    assertThat(error.getCode(), is(ONGOING_NOT_ALLOWED.getCode()));

    // Check that no other calls are made by the business logic to other services
    assertEquals(0, MockServer.serverRqRs.size());
  }

  @Test
  public void testPostOngoingOrderWithoutOngoingFields() {
    logger.info("=== Test Order creation - Ongoing field validation fails ===");

    CompositePurchaseOrder reqData = getMinimalContentCompositePurchaseOrder();
    reqData.setOrderType(CompositePurchaseOrder.OrderType.ONGOING);
    reqData.setOngoing(null);

    final Errors response = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(response.getErrors(), hasSize(1));
    Error error = response.getErrors().get(0);

    assertThat(error.getCode(), is(MISSING_ONGOING.getCode()));

    // Check that no other calls are made by the business logic to other services
    assertEquals(0, MockServer.serverRqRs.size());
  }

  @Test
  public void testPutOneTimeOrderWithOngoingField() {
    logger.info("=== Test Order update - Ongoing field validation fails ===");

    CompositePurchaseOrder reqData = getMinimalContentCompositePurchaseOrder();
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);
    reqData.setOngoing(new org.folio.rest.jaxrs.model.Ongoing());

    addMockEntry(PURCHASE_ORDER, reqData);

    final Errors response = verifyPut(COMPOSITE_ORDERS_PATH + "/" + reqData.getId(), JsonObject.mapFrom(reqData),
      APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    Error error = response.getErrors().get(0);

    assertThat(error.getCode(), is(ONGOING_NOT_ALLOWED.getCode()));

    MockServer.serverRqRs.columnKeySet().remove(HttpMethod.OTHER);
    // Check that no any calls made by the business logic to other services
    assertEquals(0, MockServer.serverRqRs.size());
  }

  @Test
  public void testPutOngoingOrderWithoutOngoingField() {
    logger.info("=== Test Order update - Ongoing field validation fails ===");

    CompositePurchaseOrder reqData = getMinimalContentCompositePurchaseOrder();
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setOrderType(CompositePurchaseOrder.OrderType.ONGOING);
    reqData.setOngoing(null);

    addMockEntry(PURCHASE_ORDER, reqData);

    final Errors response = verifyPut(COMPOSITE_ORDERS_PATH + "/" + reqData.getId(), JsonObject.mapFrom(reqData),
      APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(1));
    Error error = response.getErrors().get(0);

    assertThat(error.getCode(), is(MISSING_ONGOING.getCode()));

    MockServer.serverRqRs.columnKeySet().remove(HttpMethod.OTHER);
    // Check that no any calls made by the business logic to other services
    assertEquals(0, MockServer.serverRqRs.size());
  }

  @Test
  public void testPutOrderWithIncorrectQuantities() throws Exception {
    logger.info("=== Test Order update - Quantity validation fails for the first PO Line ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Set incorrect quantities for the first PO Line
    CompositePoLine firstPoLine = reqData.getCompositePoLines().get(0);
    firstPoLine.setOrderFormat(CompositePoLine.OrderFormat.P_E_MIX);
    firstPoLine.getCost().setQuantityPhysical(0);
    firstPoLine.getCost().setQuantityElectronic(0);
    firstPoLine.getLocations().forEach(location -> {
      location.setQuantityElectronic(1);
      location.setQuantityPhysical(1);
    });

    firstPoLine.getLocations().add(new Location()
                                    .withQuantityElectronic(0)
                                    .withQuantityPhysical(0)
                                    .withLocationId(firstPoLine.getLocations().get(0).getLocationId()));
    final Errors response = verifyPut(COMPOSITE_ORDERS_PATH + "/" + reqData.getId(), JsonObject.mapFrom(reqData),

      APPLICATION_JSON, 422).as(Errors.class);

    assertThat(response.getErrors(), hasSize(5));
    Set<String> errorCodes = response.getErrors()
                                     .stream()
                                     .map(Error::getCode)
                                     .collect(Collectors.toSet());

    assertThat(errorCodes, containsInAnyOrder(ZERO_COST_ELECTRONIC_QTY.getCode(),
                                              ZERO_COST_PHYSICAL_QTY.getCode(),
                                              ELECTRONIC_COST_LOC_QTY_MISMATCH.getCode(),
                                              PHYSICAL_COST_LOC_QTY_MISMATCH.getCode(),
                                              ZERO_LOCATION_QTY.getCode()));

    // Check that no any calls made by the business logic to other services
    assertEquals(2, MockServer.serverRqRs.size());
  }

  @Test
  public void testListedPrintMonographInOpenStatus() throws Exception {
    logger.info("=== Test Listed Print Monograph in Open status ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    prepareOrderForPostRequest(reqData);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.getCompositePoLines().forEach(poLine -> {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.PENDING);
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.PARTIALLY_RECEIVED);
    });
    CompositePoLine firstPoLine = reqData.getCompositePoLines().get(0);
    // remove productId from PO line to test scenario when it's not provided so there is no check for existing instance but new one will be created
    firstPoLine.getDetails().getProductIds().clear();
    // MODORDERS-117 only physical quantity will be used
    firstPoLine.setOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE);
    firstPoLine.setEresource(null);
    // Set locations quantities
    int totalQty = 0;
    for (int i = 0; i < firstPoLine.getLocations().size(); i++) {
      Location location = firstPoLine.getLocations().get(i);
      int quantityPhysical = i * (i + 1) + 1;

      location.setQuantityElectronic(0);
      location.setQuantityPhysical(quantityPhysical);
      totalQty += quantityPhysical;
    }
    // Set cost quantities
    firstPoLine.getCost().setQuantityPhysical(totalQty);
    firstPoLine.getCost().setQuantityElectronic(0);
    firstPoLine.getCost().setListUnitPrice(10d);
    firstPoLine.getCost().setListUnitPriceElectronic(0d);

    // Set status to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    LocalDate now = LocalDate.now();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    LocalDate dateOrdered = resp.getDateOrdered().toInstant().atZone(ZoneId.of(ZoneOffset.UTC.getId())).toLocalDate();
    assertThat(dateOrdered.getMonth(), equalTo(now.getMonth()));
    assertThat(dateOrdered.getYear(), equalTo(now.getYear()));

    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
    assertEquals(reqData.getCompositePoLines().size(), resp.getCompositePoLines().size());

    for (int i = 0; i < resp.getCompositePoLines().size(); i++) {
      CompositePoLine line = resp.getCompositePoLines().get(i);
      String polNumber = line.getPoLineNumber();
      String polId = line.getId();

      assertEquals(poId, line.getPurchaseOrderId());
      assertNotNull(polId);
      assertNotNull(polNumber);
      assertTrue(polNumber.startsWith(poNumber));
      assertNotNull(line.getInstanceId());
      line.getLocations().forEach(location -> verifyLocationQuantity(location, line.getOrderFormat()));
    }

    int polCount = resp.getCompositePoLines().size();

    List<JsonObject> instancesSearches = getInstancesSearches();
    assertNotNull(instancesSearches);
    // Check that search for existing instances was done not for all PO lines
    assertEquals(polCount - 1, instancesSearches.size());

    verifyInventoryInteraction(resp, polCount);
    verifyEncumbrancesOnPoCreation(reqData, resp);
    assertThat(getCreatedOrderSummaries(), hasSize(1));
    verifyCalculatedData(resp);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.PARTIALLY_RECEIVED.value(), reqData.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.AWAITING_PAYMENT.value(), reqData.getCompositePoLines().size());
  }

  @Test
  public void testOrderWithPoLinesWithoutSource() throws Exception {
    logger.info("=== Test Listed Print Monograph with POL without source ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Assert that there are 2 lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // remove source to verify validation for first POL
    reqData.getCompositePoLines().get(0).setSource(null);
    // Set source to null to verify validation for second POL
    reqData.getCompositePoLines().get(1).setSource(null);

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);
    assertEquals(reqData.getCompositePoLines().size(), errors.getErrors().size());
  }

  @Test
  public void testDateOrderedIsNotSetForPendingOrder() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - Date Ordered is empty ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that Order moves to Pending
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    // Verify dateOrdered is not set because Workflow status is not OPEN
    assertNull(resp.getDateOrdered());
  }

  @Test
  public void testPostOpenOrderInventoryUpdateWithOrderFormatOther() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that mock po has 2 po lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    // Make sure that mock po has the first PO line with 3 locations
    assertThat(reqData.getCompositePoLines().get(0).getLocations(), hasSize(3));

    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set the same location id for all locations of the first PO Line to confirm that only one holding created
    String locationId = reqData.getCompositePoLines().get(0).getLocations().get(0).getLocationId();
    reqData.getCompositePoLines().get(0).getLocations().forEach(location -> location.setLocationId(locationId));

    // Prepare second POL
    CompositePoLine secondPol = reqData.getCompositePoLines().get(1);
    List<Location> secondPolLocations = secondPol.getLocations();
    // MODORDERS-117 Setting OrderFormat to OTHER which means it behaves similar
    // to Physical order
    secondPol.setOrderFormat(CompositePoLine.OrderFormat.OTHER);
    Physical physical = new Physical();
    physical.setCreateInventory(CreateInventory.NONE);
    secondPol.setPhysical(physical);
    // Specify correct quantities for OTHER format
    secondPol.getCost().setQuantityElectronic(0);
    secondPol.getCost().setListUnitPriceElectronic(null);
    secondPol.getCost().setListUnitPrice(10d);
    secondPol.getCost().setQuantityPhysical(secondPolLocations.size());
    secondPol.setEresource(null);
    secondPolLocations.forEach(location -> {
      location.setQuantityElectronic(0);
      location.setQuantityPhysical(1);
    });

    LocalDate now = LocalDate.now();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);
    LocalDate dateOrdered = resp.getDateOrdered().toInstant().atZone(ZoneId.of(ZoneOffset.UTC.getId())).toLocalDate();
    assertThat(dateOrdered.getMonth(), equalTo(now.getMonth()));
    assertThat(dateOrdered.getYear(), equalTo(now.getYear()));

    // Check that search of the existing instances and items was done for first PO line only
    List<JsonObject> instancesSearches = getInstancesSearches();
    List<JsonObject> holdingsSearches = getHoldingsSearches();
    List<JsonObject> itemsSearches = getItemsSearches();
    assertNotNull(instancesSearches);
    assertNotNull(holdingsSearches);
    assertNotNull(itemsSearches);

    assertEquals(1, instancesSearches.size());
    //setting just one location in the above step
    assertEquals(1, holdingsSearches.size());
    assertEquals(1, itemsSearches.size());

    verifyInventoryInteraction(resp, 1);
    verifyCalculatedData(resp);
  }

  @Test
  public void testPostOpenOrderInventoryUpdateOnlyForFirstPOL() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that mock po has 2 po lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    // Make sure that mock po has the first PO line with 3 locations
    assertThat(reqData.getCompositePoLines().get(0).getLocations(), hasSize(3));

    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set the same location id for all locations of the first PO Line to confirm that only one holding created
    String locationId = reqData.getCompositePoLines().get(0).getLocations().get(0).getLocationId();
    reqData.getCompositePoLines().get(0).getLocations().forEach(location -> location.setLocationId(locationId));

    // Prepare second POL
    CompositePoLine secondPol = reqData.getCompositePoLines().get(1);
    // MODORDERS-117 Setting OrderFormat to OTHER which means create nothing in inventory for the second PO Line
    secondPol.setOrderFormat(CompositePoLine.OrderFormat.OTHER);
    // Specify correct quantities for OTHER format
    secondPol.getCost().setQuantityElectronic(0);
    secondPol.getCost().setListUnitPriceElectronic(null);
    secondPol.getCost().setListUnitPrice(10d);
    secondPol.getCost().setQuantityPhysical(3);
    secondPol.setPhysical(new Physical());
    secondPol.getPhysical().setCreateInventory(Physical.CreateInventory.NONE);
    secondPol.setEresource(null);
    secondPol.getLocations().clear();

    LocalDate now = LocalDate.now();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);
    LocalDate dateOrdered = resp.getDateOrdered().toInstant().atZone(ZoneId.of(ZoneOffset.UTC.getId())).toLocalDate();
    assertThat(dateOrdered.getMonth(), equalTo(now.getMonth()));
    assertThat(dateOrdered.getYear(), equalTo(now.getYear()));

    // Check that search of the existing instances and items was done for first PO line only
    List<JsonObject> instancesSearches = getInstancesSearches();
    List<JsonObject> holdingsSearches = getHoldingsSearches();
    List<JsonObject> itemsSearches = getItemsSearches();
    assertNotNull(instancesSearches);
    assertNotNull(holdingsSearches);
    assertNotNull(itemsSearches);

    assertEquals(1, instancesSearches.size());
    assertEquals(1, holdingsSearches.size());
    assertEquals(1, itemsSearches.size());

    verifyInventoryInteraction(resp, 1);
    verifyCalculatedData(resp);
  }

  @Test
  public void testPutOrdersByIdPEMixFormat() {
    logger.info("=== Test Put Order By Id create Pieces with P/E Mix format ===");
    CompositePurchaseOrder reqData = getMockAsJson(PE_MIX_PATH).mapTo(CompositePurchaseOrder.class);

    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setReEncumber(null);
    // Make sure that mock PO has 1 po line
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    reqData.setManualPo(false);
    CompositePoLine compositePoLine = reqData.getCompositePoLines().get(0);

    compositePoLine.setId(PO_LINE_ID_FOR_SUCCESS_CASE);
    compositePoLine.getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    compositePoLine.setPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM).withMaterialType("4b93736c-8731-46cd-9d6e-f9dce0f63bcd"));
    compositePoLine.getCost().setQuantityPhysical(3);
    compositePoLine.getCost().setQuantityElectronic(2);
    compositePoLine.setOrderFormat(OrderFormat.P_E_MIX);
    compositePoLine.getLocations().stream()
      .filter(location -> ObjectUtils.defaultIfNull(location.getQuantityPhysical(), 0) > 0)
      .forEach(location -> location.setQuantityElectronic(null));

    // MODORDERS-243
    removeAllEncumbranceLinks(reqData);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    List<JsonObject> createdPieces = getCreatedPieces();
    List<JsonObject> createdItems = getCreatedItems();
    assertThat(createdItems, notNullValue());
    assertThat(createdPieces, notNullValue());

    int piecesSize = createdPieces.size();
    logger.debug("------------------- piecesSize, itemSize --------------------\n" + piecesSize + " " + createdItems.size());
    // Verify total number of pieces created should be equal to total quantity
    assertEquals(calculateTotalQuantity(compositePoLine), piecesSize);

    verifyPiecesCreated(createdItems, reqData.getCompositePoLines(), createdPieces);
    verifyEncumbrancesOnPoUpdate(reqData);
    assertThat(getCreatedOrderSummaries(), hasSize(1));
  }

  @Test
  public void testPutOrdersByIdFundsNotFound() {
    logger.info("=== Test Put Order By Id Funds not found ===");
    CompositePurchaseOrder reqData = getMockAsJson(PE_MIX_PATH).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 1 po line
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    CompositePoLine compositePoLine = reqData.getCompositePoLines().get(0);

    removeAllEncumbranceLinks(reqData);
    compositePoLine.getFundDistribution().get(0).setFundId(ID_DOES_NOT_EXIST);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 400).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    Error error = errors.getErrors().get(0);
    assertThat(error.getCode(), equalTo(FUNDS_NOT_FOUND.getCode()));
    assertThat(error.getParameters().get(0).getValue(), equalTo(ID_DOES_NOT_EXIST));
    assertThat(getCreatedEncumbrances(), hasSize(0));
  }

  @Test
  public void testPutOrdersByIdCurrentFiscalYearNotFound() {
    logger.info("=== Test Put Order By Id Current fiscal year not found ===");
    CompositePurchaseOrder reqData = getMockAsJson(PE_MIX_PATH).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 1 po line
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    CompositePoLine compositePoLine = reqData.getCompositePoLines().get(0);
    Fund fund = new Fund().withCode("test").withName("name").withId(VALID_FUND_ID).withLedgerId(ID_DOES_NOT_EXIST);
    addMockEntry(FUNDS, fund);
    removeAllEncumbranceLinks(reqData);
    compositePoLine.getFundDistribution().forEach(fundDistribution -> fundDistribution.setFundId(fund.getId()));
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 400).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    Error error = errors.getErrors().get(0);
    assertThat(error.getCode(), equalTo(CURRENT_FISCAL_YEAR_NOT_FOUND.getCode()));
    assertThat(error.getParameters().get(0).getValue(), equalTo(ID_DOES_NOT_EXIST));
    assertThat(getCreatedEncumbrances(), hasSize(0));
  }

  @Test
  public void testPutOrdersByIdCurrentFiscalYearServerError() {
    logger.info("=== Test Put Order By Id, get Current fiscal year Internal Server Error ===");
    CompositePurchaseOrder reqData = getMockAsJson(PE_MIX_PATH).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 1 po line
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    CompositePoLine compositePoLine = reqData.getCompositePoLines().get(0);
    Fund fund = new Fund().withCode("test").withName("name").withId(VALID_FUND_ID).withLedgerId(ID_FOR_INTERNAL_SERVER_ERROR);
    addMockEntry(FUNDS, fund);
    removeAllEncumbranceLinks(reqData);
    compositePoLine.getFundDistribution().forEach(fundDistribution -> fundDistribution.setFundId(fund.getId()));
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 500).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    Error error = errors.getErrors().get(0);
    assertThat(error.getCode(), equalTo(GENERIC_ERROR_CODE.getCode()));
  }

  @Test
  public void testPutOrdersByIdEmptyFundDistributions() {
    logger.info("=== Test Put Order By Id Current empty fundDistributions ===");
    CompositePurchaseOrder reqData = getMockAsJson(PE_MIX_PATH).mapTo(CompositePurchaseOrder.class);

    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 1 po line
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    CompositePoLine compositePoLine = reqData.getCompositePoLines().get(0);

    compositePoLine.getFundDistribution().clear();
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    assertThat(getCreatedOrderSummaries(), hasSize(0));
    assertThat(getCreatedEncumbrances(), hasSize(0));
  }

  @Test
  public void testPutOrdersByIdTotalPiecesEqualsTotalQuantityWhenCreateInventoryIsFalse() throws Exception {
    logger.info("=== Test Put Order By Id create Pieces when Item record does not exist ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 2 po lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    MockServer.addMockTitles(reqData.getCompositePoLines());

    reqData.getCompositePoLines().get(1).getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
    List<JsonObject> items = joinExistingAndNewItems();
    List<JsonObject> createdPieces = getCreatedPieces();
    verifyPiecesQuantityForSuccessCase(reqData.getCompositePoLines(), createdPieces);
    verifyPiecesCreated(items, reqData.getCompositePoLines(), createdPieces);
  }

  @Test
  public void testPlaceOrderMinimal() throws Exception {
    logger.info("=== Test Placement of minimal order ===");

    String body = getMockData(MINIMAL_ORDER_PATH);
    JsonObject reqData = new JsonObject(body);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);


    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
    assertEquals(reqData.getJsonArray(COMPOSITE_PO_LINES).size(), resp.getCompositePoLines().size());

    for (int i = 0; i < resp.getCompositePoLines().size(); i++) {
      CompositePoLine line = resp.getCompositePoLines().get(i);
      String polNumber = line.getPoLineNumber();
      String polId = line.getId();

      assertEquals(poId, line.getPurchaseOrderId());
      assertNotNull(polId);
      assertNotNull(polNumber);
      assertTrue(polNumber.startsWith(poNumber));
    }
  }

  @Test
  public void testPostOrderFailsWithInvalidPONumber() {
    logger.info("=== Test Placement of minimal order failure with Invalid PO Number===");

    JsonObject request = new JsonObject();
    request.put("poNumber", "1234");
    String body= request.toString();

     verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
       prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422);

  }

  @Test
  public void testPostOrderFailsWithExistingPONumber() {
    logger.info("=== Test Placement of minimal order failure with Existing PO Number===");

    JsonObject request = new JsonObject();
    request.put("poNumber", PoNumberApiTest.EXISTING_PO_NUMBER);
    request.put("orderType", CompositePurchaseOrder.OrderType.ONE_TIME.value());
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);
    String body = request.toString();

     verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
       prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 400);

  }

  @Test
  public void testPostOrderPONumberAutoGenerated() {
    logger.info("=== Test Placement of Empty order with Auto Generated PO Number===");

    JsonObject request = new JsonObject();
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);
    request.put("orderType", CompositePurchaseOrder.OrderType.ONE_TIME.value());
    String body= request.toString();

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertNotNull(poId);
    assertNotNull(poNumber);
  }

  @Test
  public void testPoCreationFailure() throws Exception {
    logger.info("=== Test PO creation failure ===");

    String body = getMockData(PO_CREATION_FAILURE_PATH);

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());

    assertFalse(errors.getErrors().isEmpty());
    assertNotNull(errors.getErrors().get(0));
    assertEquals("must match \"^[a-zA-Z0-9]{1,16}$\"", errors.getErrors().get(0).getMessage());
    assertFalse(errors.getErrors().get(0).getParameters().isEmpty());
    assertNotNull(errors.getErrors().get(0).getParameters().get(0));
    assertEquals("poNumber", errors.getErrors().get(0).getParameters().get(0).getKey());
    assertEquals("123123123123123123123", errors.getErrors().get(0).getParameters().get(0).getValue());
  }

  @Test
  public void testPoCreationWithOverLimitPOLines() throws Exception {
    logger.info("=== Test PO, with over limit lines quantity, creation ===");

    String body = getMockDraftOrder().toString();

    final Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1), APPLICATION_JSON, 422).body().as(Errors.class);


    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }


  @Test
  public void testPostWithInvalidConfig() throws Exception {
    logger.info("=== Test PO creation fail if config is invalid ===");

    String body = getMockDraftOrder().toString();

    final Errors error = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(INVALID_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 500).body().as(Errors.class);

    assertEquals("Invalid limit value in configuration.", error.getErrors().get(0).getAdditionalProperties().get(PurchaseOrderHelper.ERROR_CAUSE));
  }


  @Test
  public void testSubObjectCreationFailure() throws Exception {
    logger.info("=== Test Details creation failure ===");

    String body = getMockDraftOrder().toString();
    int status403 = HttpStatus.HTTP_FORBIDDEN.toInt();
    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, new Header(X_ECHO_STATUS, String.valueOf(status403)));

    final Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body, headers, APPLICATION_JSON, 403);

    String respBody = resp.getBody().as(Errors.class).getErrors().get(0).getMessage();
    logger.info(respBody);

    assertEquals("Access requires permission: foo.bar.baz", respBody);
  }

  @Test
  public void testGetOrderById() throws Exception {
    logger.info("=== Test Get Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, id);

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(order.getCompositePoLines());
    final CompositePurchaseOrder resp = verifySuccessGet(url, CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(id, resp.getId());
    verifyCalculatedData(resp);
  }

  @Test
  public void testGetOrderByIdWithPoLinesSorting() {
    logger.info("=== Test Get Order By Id - PoLines sorting ===");

    String[] expectedPoLineNumbers = {"841240-001", "841240-02", "841240-3", "841240-21"};

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(order.getCompositePoLines());
    final CompositePurchaseOrder resp = verifySuccessGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_WIT_PO_LINES_FOR_SORTING), CompositePurchaseOrder.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertArrayEquals(expectedPoLineNumbers, resp.getCompositePoLines().stream().map(CompositePoLine::getPoLineNumber).toArray());
  }

  private void verifyCalculatedData(CompositePurchaseOrder resp) {
    Integer expectedQuantity = resp
      .getCompositePoLines()
      .stream()
      .mapToInt(poLine -> {
        int eQty = ObjectUtils.defaultIfNull(poLine.getCost().getQuantityElectronic(), 0);
        int pQty = ObjectUtils.defaultIfNull(poLine.getCost().getQuantityPhysical(), 0);
        return eQty + pQty;
      })
      .sum();

    double expectedPrice = calculateTotalEstimatedPrice(resp.getCompositePoLines());

    assertThat(resp.getTotalItems(), equalTo(expectedQuantity));
    assertThat(resp.getTotalEstimatedPrice(), equalTo(expectedPrice));

    resp.getCompositePoLines().forEach(line -> assertThat(line.getCost().getPoLineEstimatedPrice(), greaterThan(0d)));
  }

  @Test
  public void testGetOrderByIdWithPoLines() {
    logger.info("=== Test Get Order By Id - PoLines with items ===");

    String[] expectedPoLineNumbers = {"841240-001", "841240-02", "841240-3", "841240-21"};

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING));

    final CompositePurchaseOrder resp = verifySuccessGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_WIT_PO_LINES_FOR_SORTING), CompositePurchaseOrder.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertArrayEquals(expectedPoLineNumbers, resp.getCompositePoLines().stream().map(CompositePoLine::getPoLineNumber).toArray());
  }

  @Test
  public void testGetOrderByIdWithPoLinesWithInstanceId() {
    logger.info("=== Test Get Order By Id - PoLines with items ===");

    String[] expectedPoLineNumbers = {"841240-001", "841240-02", "841240-3", "841240-21"};
    String instanceId= UUID.randomUUID().toString();

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, ORDER_WIT_PO_LINES_FOR_SORTING).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> line.setInstanceId(instanceId));
    MockServer.addMockTitles(order.getCompositePoLines());
    order.getCompositePoLines().forEach(line -> line.setInstanceId(null));
    final CompositePurchaseOrder resp = verifySuccessGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_WIT_PO_LINES_FOR_SORTING), CompositePurchaseOrder.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertArrayEquals(expectedPoLineNumbers, resp.getCompositePoLines().stream().map(CompositePoLine::getPoLineNumber).toArray());
    assertThat(resp.getCompositePoLines(), (Every.everyItem(HasPropertyWithValue.hasProperty("instanceId", Is.is(instanceId)))));
  }

  @Test
  public void testGetPOByIdTotalItemsWithoutPOLines() {
    logger.info("=== Test Get Order By Id without PO Line totalItems value is 0 ===");

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_ID_WITHOUT_PO_LINES));
    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES);
    final CompositePurchaseOrder resp = verifySuccessGet(url, CompositePurchaseOrder.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(PO_ID_PENDING_STATUS_WITHOUT_PO_LINES, resp.getId());
    assertEquals(0, resp.getTotalItems().intValue());
  }

  @Test
  public void testGetOrderByIdWithOnePoLine() {
    logger.info("=== Test Get Order By Id - With one PO Line and empty source ===");

    String id = PO_ID_CLOSED_STATUS;
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    CompositePurchaseOrder order = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(order.getCompositePoLines());
    final CompositePurchaseOrder resp = verifySuccessGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(id, resp.getId());
    assertEquals(1, resp.getCompositePoLines().size());
    assertEquals(calculateTotalQuantity(resp.getCompositePoLines().get(0)), resp.getTotalItems().intValue());
  }

  @Test
  public void testGetOrderByIdIncorrectIdFormat() {
    logger.info("=== Test Get Order By Id - Incorrect Id format - 400 ===");

    String id = ID_BAD_FORMAT;
    final Response resp = verifyGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), TEXT_PLAIN, 400);

    String actual = resp.getBody().asString();
    logger.info(actual);

    assertNotNull(actual);
    assertTrue(actual.contains(id));
  }

  @Test
  public void testGetOrderByIdNotFound() {
    logger.info("=== Test Get Order By Id - Not Found ===");

    String id = ID_DOES_NOT_EXIST;
    final Response resp = verifyGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), APPLICATION_JSON, 404);

    String actual = resp.getBody().as(Errors.class).getErrors().get(0).getMessage();
    logger.info(actual);

    assertEquals(id, actual);
  }

  @Test
  public void testDeleteById() throws Exception {
    logger.info("=== Test Delete Order By Id ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));

    verifyDeleteResponse(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), "", 204);
  }

  @Test
  public void testDeleteByIdNoOrderFound() {
    logger.info("=== Test Delete Order By Id - Not Found ===");
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_DOES_NOT_EXIST, "", 404);
  }

  @Test
  public void testDeleteById500Error() {
    logger.info("=== Test Delete Order By Id - Storage Internal Server Error ===");
    Headers headers =  prepareHeaders(ERROR_ORDER_DELETE_TENANT_HEADER);
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + MIN_PO_ID, headers, APPLICATION_JSON, 500);
  }

  @Test
  public void testDeleteByIdWhenDeletingPoLine500Error() {
    logger.info("=== Test Delete Order By Id - Storage Internal Server Error on PO Line deletion ===");
    CompositePurchaseOrder order = getMinimalContentCompositePurchaseOrder();
    CompositePoLine line= getMinimalContentCompositePoLine(order.getId());
    line.setId(ID_FOR_INTERNAL_SERVER_ERROR);
    addMockEntry(PURCHASE_ORDER, JsonObject.mapFrom(order));
    addMockEntry(PO_LINES, JsonObject.mapFrom(line));

    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + order.getId(), APPLICATION_JSON, 500);
  }

  @Test
  public void testPutOrdersByIdWorkflowStatusOpenForStorageAndCurrentRequest() throws Exception {
    logger.info("=== Test Put Order By Id workflowStatus is Open from storage and workflowStatus is Open in current request  ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    JsonObject storageData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    storageData.put("workflowStatus", "Open");
    reqData.put("workflowStatus", "Open");
    verifyPoWithPoLinesUpdate(reqData, storageData);
  }

  private void verifyPoWithPoLinesUpdate(JsonObject reqData, JsonObject storageData) {
    JsonArray poLinesFromRequest = reqData.getJsonArray(COMPOSITE_PO_LINES);
    JsonArray poLinesFromStorage = storageData.getJsonArray(COMPOSITE_PO_LINES);
    int sameLinesCount = 0;
    for (int i = 0; i < poLinesFromRequest.size(); i++) {
      JsonObject lineFromRequest = poLinesFromRequest.getJsonObject(i);
      for (int j = 0; j < poLinesFromStorage.size(); j++) {
        JsonObject lineFromStorage = poLinesFromStorage.getJsonObject(j);
        if (StringUtils.equals(lineFromRequest.getString(ID), lineFromStorage.getString(ID))) {
          sameLinesCount++;
          break;
        }
      }
    }

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    assertEquals(MockServer.serverRqRs.get(PO_LINES, HttpMethod.POST).size(), poLinesFromRequest.size() - sameLinesCount);
    assertEquals(MockServer.serverRqRs.get(PO_LINES, HttpMethod.DELETE).size(), poLinesFromStorage.size() - sameLinesCount);
    assertNotNull(MockServer.getPoLineUpdates());
    assertEquals(MockServer.getPoLineUpdates().size(), sameLinesCount);
  }

  @Test
  public void testUpdateOrderWithDefaultStatus() throws Exception {
    logger.info("=== Test Put Order By Id - Make sure that default status is used ===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(MINIMAL_ORDER_PATH)).mapTo(CompositePurchaseOrder.class);
    reqData.setId(ORDER_WITHOUT_WORKFLOW_STATUS);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    String url = COMPOSITE_ORDERS_PATH + "/" + reqData.getId();
    verifyPut(url, JsonObject.mapFrom(reqData), "", 204);

    List<JsonObject> orderRetrievals = MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET);
    assertNotNull(orderRetrievals);
    assertEquals(1, orderRetrievals.size());
    PurchaseOrder storageOrderBeforeUpdate = orderRetrievals.get(0).mapTo(PurchaseOrder.class);
    // Assert default status is Pending
    assertEquals(PurchaseOrder.WorkflowStatus.PENDING, storageOrderBeforeUpdate.getWorkflowStatus());

    List<JsonObject> orderUpdates = MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT);
    assertNotNull(orderUpdates);
    assertEquals(1, orderUpdates.size());

    PurchaseOrder storageUpdatedOrder = orderUpdates.get(0).mapTo(PurchaseOrder.class);
    assertNotNull(storageUpdatedOrder.getWorkflowStatus());

    //MODORDERS-234 Status automatically changes to Open
    assertEquals(PurchaseOrder.WorkflowStatus.OPEN, storageUpdatedOrder.getWorkflowStatus());

  }

  @Test
  public void testPutOrdersByIdWithIdMismatch() throws Exception {
    logger.info("=== Test Put Order By Id with id mismatch  ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, APPLICATION_JSON, 422);
  }

  @Test
  public void testUpdatePoNumber() throws Exception {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject storData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));
    String newPoNumber = reqData.getString(PO_NUMBER) + "A";
    reqData.put(PO_NUMBER, newPoNumber);
    Pattern poLinePattern = Pattern.compile(String.format("(%s)(-[0-9]{1,3})", newPoNumber));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    assertEquals(MockServer.getPoLineUpdates().size(), storData.getJsonArray(COMPOSITE_PO_LINES).size());
    MockServer.getPoLineUpdates().forEach(poLine -> {
      Matcher matcher = poLinePattern.matcher(poLine.getString(PO_LINE_NUMBER));
      assertTrue(matcher.find());
    });
    assertNull(MockServer.serverRqRs.get(PO_LINES, HttpMethod.DELETE));
  }

  @Test
  public void testUpdatePoNumberWithPoLines() throws Exception {
    logger.info("=== Test Put Order By Id without POLines, with new PO number  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    JsonObject storageData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, id);
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    String newPoNumber = reqData.getString(PO_NUMBER) + "A";
    reqData.put(PO_NUMBER, newPoNumber);
    reqData.put(VENDOR_ID, EXISTING_REQUIRED_VENDOR_UUID);
    Pattern poLinePattern = Pattern.compile(String.format("(%s)(-[0-9]{1,3})", newPoNumber));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);
    verifyPoWithPoLinesUpdate(reqData, storageData);
    MockServer.getPoLineUpdates().forEach(poLine -> {
      Matcher matcher = poLinePattern.matcher(poLine.getString(PO_LINE_NUMBER));
      assertTrue(matcher.find());
    });
  }

  @Test
  public void testPutOrderWithoutPoNumberValidation() throws IOException {
    logger.info("=== Test Put Order By Id without po number validation  ===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITH_PO_LINES_JSON));
    reqData.remove(PO_NUMBER);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, APPLICATION_JSON, 422);
  }

  @Test
  public void testPutOrderFailsWithInvalidPONumber() throws Exception {
    logger.info("=== Test update order failure with Invalid PO Number===");
    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);

    JsonObject request = new JsonObject();
    request.put("poNumber", "1234");

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), request, APPLICATION_JSON, 422);

  }

  @Test
  public void testPutOrderFailsWithExistingPONumber() throws Exception {
    logger.info("=== Test update of order failure with Existing PO Number===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);

    JsonObject request = new JsonObject();
    request.put("poNumber", PoNumberApiTest.EXISTING_PO_NUMBER);
    request.put("orderType", CompositePurchaseOrder.OrderType.ONE_TIME.value());
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), request, APPLICATION_JSON, 400);
  }

  @Test
  public void testPoUpdateWithOverLimitPOLines() throws Exception {
    logger.info("=== Test PUT PO, with over limit lines quantity ===");

    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES);
    String body = getMockData(LISTED_PRINT_MONOGRAPH_PATH);
    Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_TOKEN, X_OKAPI_USER_ID);
    final Errors errors = verifyPut(url, body, headers, APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  public void testPoUpdateWithOverLimitPOLinesWithDefaultLimit() throws Exception {
    logger.info("=== Test PUT PO, with over limit lines quantity with default limit ===");

    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES);
    String body = getMockData(LISTED_PRINT_MONOGRAPH_PATH);
    Headers headers = prepareHeaders(X_OKAPI_URL, NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID);
    final Errors errors = verifyPut(url, body, headers, APPLICATION_JSON, 422).body().as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertFalse(errors.getErrors().isEmpty());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getDescription(), errors.getErrors().get(0).getMessage());
    assertEquals(POL_LINES_LIMIT_EXCEEDED.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpen() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // MODORDERS-178 guarantee electronic resource for the second PO Line but set "create items" to NONE
    reqData.getCompositePoLines().get(1).setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    reqData.getCompositePoLines().get(1).getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    reqData.getCompositePoLines().forEach(s -> s.setReceiptStatus(CompositePoLine.ReceiptStatus.PENDING));
    reqData.getCompositePoLines().forEach(s -> s.setPaymentStatus(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED));

    reqData.getCompositePoLines().forEach(this::createMockTitle);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    int polCount = reqData.getCompositePoLines().size();

    verifyInstanceLinksForUpdatedOrder(reqData);
    verifyInventoryInteraction(reqData, polCount - 1);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value(), reqData.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED.value(), reqData.getCompositePoLines().size());
  }

  @Test
  public void testPutOrdersByIdToOpenOrderWithLocationButCreateInventoryNoneForOneOfResources() {
    logger.info("=== Test Put Order By Id to open order - P/E Mix PO Line, location specified but create inventory is None for electronic resource ===");

    CompositePurchaseOrder reqData = getMinimalContentCompositePurchaseOrder();
    CompositePoLine line = getMinimalContentCompositePoLine();

    reqData.withVendor(ACTIVE_VENDOR_ID);
    line.setOrderFormat(OrderFormat.P_E_MIX);
    line.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.NONE).withAccessProvider(reqData.getVendor()));
    line.setPhysical(new Physical().withCreateInventory(CreateInventory.INSTANCE_HOLDING_ITEM).withMaterialType(ITEM_MATERIAL_TYPE_ID));
    line.setPoLineNumber(reqData.getPoNumber() + "-1");

    line.setLocations(Collections.singletonList(new Location().withLocationId(UUID.randomUUID()
        .toString())
        .withQuantityElectronic(2)
        .withQuantityPhysical(2)));

    line.setCost(new Cost().withCurrency("USD")
        .withListUnitPrice(1.0)
        .withListUnitPriceElectronic(1.0)
        .withQuantityElectronic(2)
        .withQuantityPhysical(2));

    addMockEntry(PURCHASE_ORDER, reqData);
    addMockEntry(PO_LINES, line);
    createMockTitle(line);

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    List<JsonObject> createdPieces = getCreatedPieces();
    verifyPiecesQuantityForSuccessCase(Collections.singletonList(line), createdPieces);
  }

  private void createMockTitle(CompositePoLine line) {
    Title title = new Title().withTitle(line.getTitleOrPackage()).withPoLineId(line.getId());
    MockServer.addMockEntry(TITLES, JsonObject.mapFrom(title));
  }

  @Test
  public void testPutOrdersByIdInstanceCreation() throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    Map<String, String> uuids = new HashMap<>();
    // Populate instanceIds
    reqData.getCompositePoLines().forEach(p -> p.setInstanceId(uuids.compute(p.getId(), (k, v) -> UUID.randomUUID().toString())));
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Update order
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
    verifyInstanceLinksForUpdatedOrder(reqData);
    // Verify instanceIds conformity
    reqData.getCompositePoLines().forEach(p -> assertThat(uuids.get(p.getId()), is(p.getInstanceId())));
    // Verify that new instances didn't created
    assertThat(MockServer.getCreatedInstances(), nullValue());
    assertThat(MockServer.getUpdatedTitles(), notNullValue());

    MockServer.getUpdatedTitles()
      .forEach(title -> assertTrue(uuids.containsValue(title.getString(INSTANCE_ID))));
    validateSavedPoLines();

  }

  @Test
  public void testPutOrdersWithPackagePolinesAndInstanceIdSpecified() throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);

    reqData.getCompositePoLines()
      .forEach(p -> {
        p.setInstanceId(UUID.randomUUID().toString());
        p.setIsPackage(true);
      });
    // Update order
    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 422)
      .then()
      .extract()
      .as(Errors.class);

    assertEquals(INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE.getCode(), errors.getErrors().get(0).getCode());

    // test with transition to open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 422)
      .then()
      .extract()
      .as(Errors.class);

    assertEquals(INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE.getCode(), errors.getErrors().get(0).getCode());
  }

  @Test
  public void testPostOrdersWithOpenStatusAndCheckinItems() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that mock po has 2 po lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    // Make sure that mock po has the first PO line with 3 locations
    assertThat(reqData.getCompositePoLines().get(0).getLocations(), hasSize(3));

    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set checkin items flag true
    reqData.getCompositePoLines().forEach(s -> s.setCheckinItems(true));

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_TOKEN, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  public void testPostOpenOrdersWithExtraLargeCost() throws Exception {
    logger.info("=== Test POST Order By Id with extra large unit cost - error expected ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());

    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set extra large cost for non electronic format
    reqData.getCompositePoLines()
      .forEach(s -> {
        if (s.getOrderFormat()!= OrderFormat.ELECTRONIC_RESOURCE) {
          s.getCost().setListUnitPrice((double) Integer.MAX_VALUE);
        }
      });

    // set restricted fund
    reqData.getCompositePoLines().stream()
      .flatMap(poline -> poline.getFundDistribution().stream())
      .forEach(fd -> fd.setFundId(FUND_ID_RESTRICTED));

    Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .toString(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_TOKEN, X_OKAPI_USER_ID), APPLICATION_JSON, 422)
        .as(Errors.class);

    assertEquals(FUND_CANNOT_BE_PAID.getCode(), errors.getErrors().get(0).getCode());

  }

  @Test
  public void testPostOrdersCreateInventoryPhysicalNone() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction not required for Physical Resource  ===");

    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create nothing in inventory + fix from MODORDERS-264
    reqData.getCompositePoLines().get(0).setOrderFormat(OrderFormat.PHYSICAL_RESOURCE);
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    reqData.getCompositePoLines().get(0).getCost().setQuantityElectronic(0);
    reqData.getCompositePoLines().get(0).getCost().setListUnitPriceElectronic(0d);
    reqData.getCompositePoLines().get(0).getLocations().get(0).setQuantityElectronic(0);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    verifyInstanceNotCreated();
  }

  @Test
  public void testPostOrdersCreateInventoryElcetronicNone() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction not required for Electronic Resource  ===");

    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create nothing in inventory + fix from MODORDERS-264
    reqData.getCompositePoLines().get(0).setOrderFormat(OrderFormat.ELECTRONIC_RESOURCE);
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).getCost().setQuantityPhysical(0);
    reqData.getCompositePoLines().get(0).getCost().setListUnitPrice(0d);
    reqData.getCompositePoLines().get(0).getLocations().get(0).setQuantityPhysical(0);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    verifyInstanceNotCreated();
  }

  private void verifyInstanceNotCreated() {
    assertNull(getInstancesSearches());
    assertNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
    assertNull(getCreatedInstances());
  }

  @Test
  public void testPostOrdersCreateInventoryInstance() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create inventory Instance
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE);
    MockServer.addMockTitles(reqData.getCompositePoLines());

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
  }

  @Test
  public void testPostOrdersCreateInventoryInstanceHolding() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // Set CreateInventory value to create inventory instances and holdings
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
  }

  @Test
  public void testPostOrdersCreateInventoryInstanceHoldingItem() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create inventory instances, holdings and items
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNotNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
  }

  @Test
  public void testPostOrdersWithEmptyCreateInventory() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Clear CreateInventory for setting default values
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(null);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(null);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNotNull(getItemsSearches());
    verifyInventoryInteraction(resp, 1);

    assertNotNull(getCreatedPieces());
  }

  @Test
  public void testPostOrdersWithEmptyCreateInventoryAndEmptyConfiguration() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Clear CreateInventory for setting default values
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(null);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(null);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EMPTY_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNotNull(getItemsSearches());

    // MODORDERS-239/240/241: default values will be used when config is empty
    verifyInventoryInteraction(EMPTY_CONFIG_X_OKAPI_TENANT, resp, 1);

    assertNotNull(getCreatedPieces());
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenWithCheckinItems() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that mock PO has 2 po lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // MODORDERS-183 Set the second POLine checkinItems true
    reqData.getCompositePoLines().get(1).setCheckinItems(true);
    reqData.getCompositePoLines().forEach(s -> s.setReceiptStatus(CompositePoLine.ReceiptStatus.PENDING));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    int polCount = reqData.getCompositePoLines().size();
    verifyInstanceLinksForUpdatedOrder(reqData);
    verifyInventoryInteraction(reqData, polCount);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value(), reqData.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.AWAITING_PAYMENT.value(), reqData.getCompositePoLines().size());
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenWithEmptyPoLines() throws Exception {
    logger.info("=== Test Put (WithEmptyPoLines) Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.getCompositePoLines().clear();
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value(), reqData.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.AWAITING_PAYMENT.value(), reqData.getCompositePoLines().size());
  }

  private void verifyReceiptStatusChangedTo(String expectedStatus, int poLinesQuantity) {
    List<JsonObject> polUpdates = MockServer.getPoLineUpdates();
    assertNotNull(polUpdates);
    // check receipt status of the last poLinesQuantity updated polines
    for (JsonObject jsonObj : polUpdates.subList(polUpdates.size() - poLinesQuantity, polUpdates.size())) {
      assertEquals(expectedStatus, jsonObj.getString(RECEIPT_STATUS));
    }
  }

  private void verifyPaymentStatusChangedTo(String expectedStatus, int poLinesQuantity) {
    List<JsonObject> polUpdates = MockServer.getPoLineUpdates();
    assertNotNull(polUpdates);

    for (JsonObject jsonObj : polUpdates.subList(polUpdates.size() - poLinesQuantity, polUpdates.size())) {
      assertEquals(expectedStatus, jsonObj.getString(PAYMENT_STATUS));
    }
  }

  @Test
  public void testUpdateOrderToOpenWithPartialItemsCreation() throws Exception {
    logger.info("=== Test Order update to Open status - Inventory items expected to be created partially ===");

    // One item for each location will be found
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Emulate items creation issue
    reqData.getCompositePoLines().get(0).getPhysical().setMaterialType(ID_FOR_INTERNAL_SERVER_ERROR);
    assertThat(reqData.getCompositePoLines().get(0).getLocations(), hasSize(3));
    // Second location has 2 physical resources
    reqData.getCompositePoLines().get(0).getLocations().get(1).setLocationId(ID_FOR_INTERNAL_SERVER_ERROR);
    // Let's have only one PO Line
    reqData.getCompositePoLines().remove(1);
    // Set status to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    // Set specific ID to let items search to return 1 item
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    int polCount = reqData.getCompositePoLines().size();
    // Assert that only one PO line presents
    assertEquals(1, polCount);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 500);

    // Verify inventory GET and POST requests for instance, holding and item records
    verifyInventoryInteraction(false);

    // All existing and created items
    List<JsonObject> items = joinExistingAndNewItems();

    // Verify that not all expected items created
    assertThat(items.size(), lessThan(calculateInventoryItemsQuantity(reqData.getCompositePoLines().get(0))));
  }


  @Test
  public void testPutOrdersByIdToChangeStatusToOpenButWithFailureFromStorage() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open - Storage errors expected and no interaction with Inventory===");

    CompositePurchaseOrder reqData = new JsonObject(getMockData(ORDER_FOR_FAILURE_CASE_MOCK_DATA_PATH)).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    final Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData),
      APPLICATION_JSON, 500)
        .body()
          .as(Errors.class);

    logger.info(JsonObject.mapFrom(errors).encodePrettily());
    assertEquals(2, errors.getErrors().size());
    assertNull(getInstancesSearches());
    assertNull(getItemsSearches());
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenButWithErrorCreatingItemsForSecondPOL() throws Exception {
    logger.info("=== Test Put Order By Id to change Order's status to Open - Inventory errors expected on items creation for second POL ===");

    /*==============  Preparation ==============*/

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    MockServer.addMockTitles(reqData.getCompositePoLines());

    int polCount = reqData.getCompositePoLines().size();
    // Make sure that mock PO has 2 lines
    assertThat(reqData.getCompositePoLines(), hasSize(2));
    // Make sure that inventory interaction is expected for each PO line
    for (CompositePoLine pol : reqData.getCompositePoLines()) {
      assertTrue(calculateInventoryItemsQuantity(pol) > 0);
    }

    // Set location ids to one which emulates item creation failure
    reqData.getCompositePoLines().get(1).getLocations().forEach(location -> location.setLocationId(ID_FOR_INTERNAL_SERVER_ERROR));

    String path = String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId());

    /*==============  Assert result ==============*/

    // Server Error expected as a result because not all items created
    verifyPut(path, JsonObject.mapFrom(reqData), APPLICATION_JSON, 500);

    // Check that search of the existing instances and items was done for each PO line
    List<JsonObject> instancesSearches = getInstancesSearches();
    assertNotNull(instancesSearches);
    assertNotNull(getItemsSearches());
    assertNotNull(getPieceSearches());
    assertEquals(polCount, instancesSearches.size());

    // Check that 2 new instances created and items created successfully only for first POL
    List<JsonObject> createdInstances = getCreatedInstances();
    List<JsonObject> createdPieces = getCreatedPieces();
    assertNotNull(createdInstances);
    assertNotNull(getCreatedItems());
    assertNotNull(createdPieces);
    assertEquals(polCount, createdInstances.size());

    List<JsonObject> items = joinExistingAndNewItems();

    // Check instance Ids not exist for polines
    verifyInstanceLinksNotCreatedForPoLine();

    // Verify pieces were created
    assertEquals(calculateTotalQuantity(reqData.getCompositePoLines().get(0)), createdPieces.size());

    verifyPiecesCreated(items, reqData.getCompositePoLines().subList(0, 1), createdPieces);
  }

  private void verifyInstanceLinksNotCreatedForPoLine() {
    List<JsonObject> polUpdates = MockServer.getPoLineUpdates();
    assertNotNull(polUpdates);
    boolean instanceIdExists = false;
    for (JsonObject jsonObj : polUpdates) {
      PoLine line = jsonObj.mapTo(PoLine.class);
      if (StringUtils.isNotEmpty(getInstanceId(line))) {
        instanceIdExists = true;
        break;
      }
    }

    assertFalse("The PO Line must NOT contain instance id", instanceIdExists);
  }

  @Test
  public void testPutOrderByIdWithPoLinesInRequestAndNoPoLinesInStorage() throws Exception {
    logger.info("=== Test Put Order By Id with PO lines and without PO lines in order from storage ===");

    JsonObject reqData = getMockDraftOrder();
    String poNumber = reqData.getString(PO_NUMBER);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    List<JsonObject> createdPoLines = MockServer.serverRqRs.get(PO_LINES, HttpMethod.POST);
    assertEquals(createdPoLines.size(), reqData.getJsonArray(COMPOSITE_PO_LINES).size());
    createdPoLines.forEach(poLine -> assertEquals(poNumber + "-" + PO_LINE_NUMBER_VALUE, poLine.getString(PO_LINE_NUMBER)));
    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.PENDING));
  }

  @Test
  public void testPutOrderByIdWithoutPoLinesInRequestDoesNotDeletePoLinesFromStorage() throws IOException {
    logger.info("=== Test Put Order By Id without PO lines doesn't delete lines from storage ===");

    JsonObject ordersList = new JsonObject(getMockData(ORDERS_MOCK_DATA_PATH));
    String id = ordersList.getJsonArray("compositePurchaseOrders").getJsonObject(0).getString(ID);
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
    JsonObject reqData = new JsonObject(getMockData(ORDER_WITHOUT_PO_LINES));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    assertNull(MockServer.serverRqRs.get(PO_LINES, HttpMethod.DELETE));
  }

  @Test
  public void testPutOrderByIdWith404InvalidId() {
    logger.info("=== Test Put Order By Id for 404 with Invalid Id or Order not found ===");

    JsonObject reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, "93f612a9-9a05-4eef-aac5-435be131454b"), reqData, APPLICATION_JSON, 404);
  }

  @Test
  public void testPutOrderByIdWithInvalidOrderIdInBody() {
    logger.info("=== Test Put Order By Id for 422 with Invalid Id in the Order ===");

    CompositePurchaseOrder reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH).mapTo(CompositePurchaseOrder.class);
    reqData.setId(PO_ID_PENDING_STATUS_WITH_PO_LINES);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_FOR_FAILURE_CASE),
      JsonObject.mapFrom(reqData), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));
    assertThat(errors.getErrors().get(0).getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
  }

  @Test
  public void testPutOrderByIdWithInvalidOrderIdInPol() {
    logger.info("=== Test Put Order By Id for 422 with invalid purchase order Id in the POL ===");

    CompositePurchaseOrder reqData = getMockAsJson(LISTED_PRINT_SERIAL_PATH).mapTo(CompositePurchaseOrder.class);
    reqData.getCompositePoLines().forEach(line -> line.setPurchaseOrderId(PO_ID_FOR_FAILURE_CASE));

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_PENDING_STATUS_WITH_PO_LINES),
      JsonObject.mapFrom(reqData), APPLICATION_JSON, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(reqData.getCompositePoLines().size()));
    errors.getErrors().forEach(error -> {
      assertThat(error.getCode(), equalTo(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.getCode()));
      assertThat(error.getParameters(), hasSize(1));
    });
  }

  @Test
  public void testPutOrderInOpenStatusAddingNewLine() {
    logger.info("=== Test update Open order - add new line ===");

    validateUpdateRejectedForNonPendingOrder(PO_ID_OPEN_STATUS, ORDER_OPEN.getCode());
  }

  @Test
  public void testPutOrderInClosedStatusAddingNewLine() {
    logger.info("=== Test update Closed order - add new line ===");

    validateUpdateRejectedForNonPendingOrder(PO_ID_CLOSED_STATUS, ORDER_CLOSED.getCode());
  }

  private void validateUpdateRejectedForNonPendingOrder(String orderId, String errorCode) {
    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, orderId).mapTo(CompositePurchaseOrder.class);

    // Delete PO Line id emulating case when new PO Line is being added
    reqData.getCompositePoLines().forEach(line -> line.setId(null));

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 422).as(Errors.class);
    validatePoLineCreationErrorForNonPendingOrder(errorCode, errors, 4);
  }

  @Test
  public void testValidationOnPost() throws IOException {
    logger.info("=== Test validation Annotation on POST API ===");

    logger.info("=== Test validation with no body ===");

    Headers headers = prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, "", headers, TEXT_PLAIN, 400);

    logger.info("=== Test validation on invalid lang query parameter ===");

    verifyPostResponse(COMPOSITE_ORDERS_PATH +INVALID_LANG, getMockData(MINIMAL_ORDER_PATH), headers, TEXT_PLAIN, 400);

  }

  @Test
  public void testValidationOnGetById() {
    logger.info("=== Test validation Annotation on GET ORDER BY ID API ===");
    String id = "non-existent-po-id";

    logger.info("=== Test validation on invalid lang query parameter ===");
    verifyGet(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id) + INVALID_LANG, TEXT_PLAIN, 400);
  }

  @Test
  public void testValidationDelete() {
    logger.info("=== Test validation Annotation on DELETE API ===");

    logger.info("=== Test validation on invalid lang query parameter ===");
    verifyDeleteResponse(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID_CLOSED_STATUS) + INVALID_LANG, TEXT_PLAIN, 400);

  }

  @Test
  public void testValidationOnPut() throws IOException {
    logger.info("=== Test validation Annotation on PUT API ===");
    String id = "non-existent-po-id";
    logger.info("=== Test validation with no body ===");

    verifyPut(COMPOSITE_ORDERS_PATH +"/"+id, "", TEXT_PLAIN, 400);

    logger.info("=== Test validation on invalid lang query parameter ===");

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id) + INVALID_LANG, getMockData(MINIMAL_ORDER_PATH), TEXT_PLAIN, 400);
  }

  @Test
  public void testPostOrdersWithMissingVendorId() throws IOException {
    logger.info("=== Test Post Order with missing Vendor Id ===");

    Errors resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, getMockData(ORDER_WITHOUT_VENDOR_ID),
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 422).as(Errors.class);

    assertEquals(1, resp.getErrors().size());
    assertEquals(VENDOR_ID, resp.getErrors().get(0).getParameters().get(0).getKey());
    assertEquals(NULL, resp.getErrors().get(0).getParameters().get(0).getValue());
  }


  @Test
  public void testGetOrdersNoParameters() {
    logger.info("=== Test Get Orders - With empty query ===");

    final PurchaseOrders purchaseOrders = verifySuccessGet(COMPOSITE_ORDERS_PATH, PurchaseOrders.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(SEARCH_ORDERS, HttpMethod.GET), nullValue());
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
    assertThat(purchaseOrders.getTotalRecords(), is(3));

    List<String> queryParams = getQueryParams(PURCHASE_ORDER);
    assertThat(queryParams, hasSize(1));
    assertThat(queryParams.get(0), equalTo(NO_ACQ_UNIT_ASSIGNED_CQL));
  }

  @Test
  public void testGetOrdersWithParameters() {
    logger.info("=== Test Get Orders - With empty query ===");
    String sortBy = " sortBy poNumber";
    String queryValue = "poNumber==" + EXISTING_PO_NUMBER;
    String endpointQuery = String.format("%s?query=%s%s", COMPOSITE_ORDERS_PATH, queryValue, sortBy);
    final PurchaseOrders purchaseOrders = verifySuccessGet(endpointQuery, PurchaseOrders.class, PROTECTED_READ_ONLY_TENANT);

    assertThat(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET), nullValue());
    assertThat(MockServer.serverRqRs.get(SEARCH_ORDERS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));
    assertThat(purchaseOrders.getTotalRecords(), is(1));

    List<String> queryParams = getQueryParams(SEARCH_ORDERS);
    assertThat(queryParams, hasSize(1));
    String queryToStorage = queryParams.get(0);
    assertThat(queryToStorage, containsString("(" + queryValue + ")"));
    assertThat(queryToStorage, not(containsString(ACQUISITIONS_UNIT_IDS + "=")));
    assertThat(queryToStorage, containsString(NO_ACQ_UNIT_ASSIGNED_CQL));
    assertThat(queryToStorage, endsWith(sortBy));
  }

  @Test
  public void testGetOrdersForUserAssignedToAcqUnits() {
    logger.info("=== Test Get Orders by query - user assigned to acq units ===");

    Headers headers = prepareHeaders(X_OKAPI_URL, NON_EXIST_CONFIG_X_OKAPI_TENANT, X_OKAPI_USER_ID_WITH_ACQ_UNITS);
    verifyGet(COMPOSITE_ORDERS_PATH, headers, APPLICATION_JSON, 200);

    assertThat(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(SEARCH_ORDERS, HttpMethod.GET), nullValue());
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_UNITS, HttpMethod.GET), hasSize(1));
    assertThat(MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET), hasSize(1));

    List<String> queryParams = getQueryParams(PURCHASE_ORDER);
    assertThat(queryParams, hasSize(1));
    String queryToStorage = queryParams.get(0);
    assertThat(queryToStorage, containsString(ACQUISITIONS_UNIT_IDS + "="));
    assertThat(queryToStorage, containsString(NO_ACQ_UNIT_ASSIGNED_CQL));

    MockServer.serverRqRs.get(ACQUISITIONS_MEMBERSHIPS, HttpMethod.GET)
      .get(0)
      .mapTo(AcquisitionsUnitMembershipCollection.class)
      .getAcquisitionsUnitMemberships()
      .forEach(member -> assertThat(queryToStorage, containsString(member.getAcquisitionsUnitId())));
  }

  @Test
  public void testGetOrdersBadQuery() {
    logger.info("=== Test Get Orders by query - unprocessable query to emulate 400 from storage ===");

    String endpointQuery = String.format("%s?query=%s", COMPOSITE_ORDERS_PATH, BAD_QUERY);

    verifyGet(endpointQuery, APPLICATION_JSON, 400);

  }

  @Test
  public void testGetOrdersInternalServerError() {
    logger.info("=== Test Get Orders by query - emulating 500 from storage ===");

    String endpointQuery = String.format("%s?query=%s", COMPOSITE_ORDERS_PATH, ID_FOR_INTERNAL_SERVER_ERROR);

    verifyGet(endpointQuery, APPLICATION_JSON, 500);

  }

  @Test
  public void testCreatePoWithDifferentVendorStatus() throws Exception {

    logger.info("=== Test POST PO with vendor's status ===");

    CompositePurchaseOrder reqData = getPoWithVendorId(ACTIVE_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B);
    for (int i = 0; i < reqData.getCompositePoLines().size(); i++) {
      reqData.getCompositePoLines().get(i).setPoLineNumber("number-" + i);
    }
    MockServer.addMockTitles(reqData.getCompositePoLines());

    // Purchase order is OK
    Errors activeVendorActiveAccessProviderErrors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(Errors.class);
    assertThat(activeVendorActiveAccessProviderErrors.getErrors(), empty());

    // Internal mod-vendor error
    reqData.setVendor(MOD_VENDOR_INTERNAL_ERROR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    Errors internalServerError = verifyPostResponseErrors(1, JsonObject.mapFrom(reqData).toString());
    assertThat(internalServerError.getErrors().get(0).getCode(), equalTo(VENDOR_ISSUE.getCode()));
    assertThat(internalServerError.getErrors().get(0).getAdditionalProperties().get(VendorHelper.ERROR_CAUSE), notNullValue());

    // Non-existed vendor
    reqData.setVendor(NON_EXIST_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_B);
    Errors nonExistedVendorError = verifyPostResponseErrors(1, JsonObject.mapFrom(reqData).toString());
    checkExpectedError(NON_EXIST_VENDOR_ID, nonExistedVendorError, 0, ORDER_VENDOR_NOT_FOUND, reqData, 0);

    // Active vendor and non-existed access provider
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(NON_EXIST_ACCESS_PROVIDER_A);
    Errors inactiveAccessProviderErrors = verifyPostResponseErrors(1, JsonObject.mapFrom(reqData).toString());
    checkExpectedError(NON_EXIST_ACCESS_PROVIDER_A, inactiveAccessProviderErrors, 0, POL_ACCESS_PROVIDER_NOT_FOUND, reqData, 1);

    // Inactive access provider
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    Errors nonExistedAccessProviderErrors = verifyPostResponseErrors(1, JsonObject.mapFrom(reqData).toString());
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, nonExistedAccessProviderErrors, 0, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);

    // Inactive vendor and inactive access providers
    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_B);
    Errors allInactiveErrors = verifyPostResponseErrors(3, JsonObject.mapFrom(reqData).toString());
    checkExpectedError(INACTIVE_VENDOR_ID, allInactiveErrors, 0, ORDER_VENDOR_IS_INACTIVE, reqData, 0);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, allInactiveErrors, 1, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_B, allInactiveErrors, 2, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);

  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenInactiveVendor() throws Exception {

    logger.info("=== Test Put Order By Id to change status of Order to Open for different vendor's status ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    for (int i = 0; i < reqData.getCompositePoLines().size(); i++) {
      reqData.getCompositePoLines().get(i).setPoLineNumber("number-" + i);
    }

    // Positive cases
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 204);

    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 204);

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 204);

    // Negative cases
    // Non-existed vendor
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(NON_EXIST_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    Errors nonExistedVendorErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);
    checkExpectedError(NON_EXIST_VENDOR_ID, nonExistedVendorErrors, 0, ORDER_VENDOR_NOT_FOUND, reqData, 0);

    // Inactive access provider
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    Errors inactiveAccessProviderErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, inactiveAccessProviderErrors, 0, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);

    // Inactive vendor and inactive access providers
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_B);
    Errors allInactiveErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);
    checkExpectedError(INACTIVE_VENDOR_ID, allInactiveErrors, 0, ORDER_VENDOR_IS_INACTIVE, reqData, 0);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, allInactiveErrors, 1, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_B, allInactiveErrors, 2, POL_ACCESS_PROVIDER_IS_INACTIVE, reqData, 1);
  }

  @Test
  public void testPutOrderToChangeStatusToOpenVendorWithUnexpectedContent() throws Exception {

    logger.info("=== Test Put Order to change status of Order to Open - vendor with unexpected content ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Prepare order
    reqData.setVendor(VENDOR_WITH_BAD_CONTENT);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(VENDOR_WITH_BAD_CONTENT);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(2));

    int errorsWithParam = (int) errors
      .getErrors()
      .stream()
      .filter(error -> !error.getParameters().isEmpty())
      .count();

    assertThat(errorsWithParam, is(1));

    errors.getErrors().forEach(error -> {
      assertThat(error.getCode(), equalTo(VENDOR_ISSUE.getCode()));
      assertThat(error.getAdditionalProperties().get(VendorHelper.ERROR_CAUSE), notNullValue());
      if (!error.getParameters().isEmpty()) {
        assertThat(error.getParameters(), hasSize(1));
        assertThat(error.getParameters().get(0).getKey(), equalTo(ID));
        assertThat(error.getParameters().get(0).getValue(), equalTo(VENDOR_WITH_BAD_CONTENT));
      }
    });
  }

  @Test
  public void testPutOrderToChangeStatusToOpenWithOrganizationNotVendor() throws Exception {

    logger.info("=== Test Put Order to change status of Order to Open - organization which is not vendor ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Prepare order
    reqData.setVendor(ORGANIZATION_NOT_VENDOR);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ORGANIZATION_NOT_VENDOR);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
        JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(1));

    checkExpectedError(ORGANIZATION_NOT_VENDOR, errors, 0, ORGANIZATION_NOT_A_VENDOR, reqData, 0);

  }

  @Test
  public void testPutOrderToAutomaticallyChangeStatusFromOpenToClosed() {

    logger.info("===  Test case when order status update is expected from Open to Closed ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setVendor(ACTIVE_VENDOR_ID);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    reqData.setReEncumber(false);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
    assertThat(purchaseOrder.getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.CLOSED));
    assertThat(purchaseOrder.getCloseReason(), notNullValue());
    assertThat(purchaseOrder.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));

  }

  @Test
  public void testPostOrderToAutomaticallyChangeStatusFromOpenToClosed() {

    logger.info("===  Test case when order status update is expected from Open to Closed ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setVendor(ACTIVE_VENDOR_ID);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    CompositePurchaseOrder respData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(respData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    assertThat(respData.getCloseReason(), notNullValue());
    assertThat(respData.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), notNullValue());
    assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==On order", reqData.getCompositePoLines().get(0).getId()));
  }

  @Test
  public void testPostOrderToAutomaticallyChangeStatusFromOpenToClosedNoItemsFound() {

    logger.info("===  Test case when order status update is expected from Open to Closed no On order items in inventory ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    reqData.getCompositePoLines().get(0).setId(ITEMS_NOT_FOUND);

    MockServer.addMockTitles(reqData.getCompositePoLines());
    addMockEntry(ITEM_RECORDS, new JsonObject());

    reqData.setVendor(ACTIVE_VENDOR_ID);

    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    CompositePurchaseOrder respData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(respData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    assertThat(respData.getCloseReason(), notNullValue());
    assertThat(respData.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), nullValue());

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==On order", reqData.getCompositePoLines().get(0).getId()));
  }

  @Test
  public void testPutOrderWithoutPoLinesToChangeStatusFromOpenToClosedItemsHaveToBeUpdate() {
    logger.info("===  Test case when order status updated from Open to Closed no PO Lines in payload - related items have to be updated ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    List<CompositePoLine> poLines = reqData.getCompositePoLines();
    poLines.forEach(line -> {
      line.setPurchaseOrderId(PO_ID_OPEN_TO_BE_CLOSED);
      line.setReceiptStatus(ReceiptStatus.AWAITING_RECEIPT);
      addMockEntry(PO_LINES, line);
    });
    MockServer.addMockTitles(poLines);

    reqData.setCompositePoLines(Collections.emptyList());
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);
    reqData.setCloseReason(new CloseReason().withReason("Test"));

    reqData.setReEncumber(false);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
    assertThat(purchaseOrder.getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.CLOSED));
    assertThat(purchaseOrder.getCloseReason(), notNullValue());
    assertThat(purchaseOrder.getCloseReason().getReason(), equalTo("Test"));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), notNullValue());
    assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==On order", poLines.get(0).getId()));

  }

  @Test
  public void testPutOrderWithoutPoLinesToReOpenItemsHaveToBeUpdate() {
    logger.info("===  Test case when order status updated from Closed to Open no PO Lines in payload - related items have to be updated ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_CLOSED_STATUS).mapTo(CompositePurchaseOrder.class);
    List<CompositePoLine> poLines = reqData.getCompositePoLines();
    poLines.forEach(line -> {
      line.setPurchaseOrderId(PO_ID_CLOSED_STATUS);
      line.setReceiptStatus(ReceiptStatus.FULLY_RECEIVED);
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.getEresource().setAccessProvider(ACTIVE_VENDOR_ID);
      addMockEntry(PO_LINES, line);
    });
    addMockEntry(PURCHASE_ORDER, reqData.withWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED));
    MockServer.addMockTitles(poLines);

    reqData.setCompositePoLines(Collections.emptyList());
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    reqData.setReEncumber(false);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
    assertThat(purchaseOrder.getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.OPEN));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), notNullValue());
    assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==Order closed", poLines.get(0).getId()));

  }

  @Test
  public void testPutOrderToAutomaticallyChangeStatusFromClosedToOpen() {

    logger.info("=== Test case when order status update is expected from Closed to Open ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_CLOSED_STATUS).mapTo(CompositePurchaseOrder.class);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.CLOSED));


    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.OPEN));

    assertThat(getItemsSearches(), notNullValue());
    assertThat(getItemsSearches(), hasSize(1));
    assertThat(getItemUpdates(), notNullValue());
    assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

    assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
    assertThat(getQueryParams(ITEM_RECORDS).get(0), containsAny("status.name==Order closed", reqData.getCompositePoLines().get(0).getId()));
  }

  @Test
  public void testUpdateOrderWithoutPoLineToWithClosedStatus() {
    logger.info("===  Test Put Order to change status of Order from Pending to Closed without PO Lines - no items searches and updates ===");

    CompositePurchaseOrder reqData = getMinimalContentCompositePurchaseOrder();
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.CLOSED);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.CLOSED));

    assertThat(getItemsSearches(), nullValue());
    assertThat(getItemUpdates(), nullValue());
    assertThat(getQueryParams(ITEM_RECORDS), hasSize(0));
  }

  @Test
  public void testUpdateNotRequiredForOpenOrder() {
    logger.info("=== Test case when no workflowStatus update is expected for Open order ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_STATUS).mapTo(CompositePurchaseOrder.class);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));


    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.OPEN));
  }

  @Test
  public void testNoUpdatesForPendingOrderWithLines() {
    logger.info("=== Test case when no order update is expected for Pending order with a few PO Lines ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_PENDING_STATUS_WITH_PO_LINES).mapTo(CompositePurchaseOrder.class);
    reqData.setAssignedTo(null);
    reqData.setId(PO_ID_PENDING_STATUS_WITH_PO_LINES);
    reqData.getCompositePoLines().forEach(poLine -> poLine.setPurchaseOrderId(PO_ID_PENDING_STATUS_WITH_PO_LINES));
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));


    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 204);

    assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(PurchaseOrder.WorkflowStatus.PENDING));
  }

  @Test
  public void testUpdateOrderWithProtectedFieldsChanging() {
    logger.info("=== Test case when OPEN order errors if protected fields are changed ===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.OPEN.value()));

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(POProtectedFields.APPROVED.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.PO_NUMBER.getFieldName(), "testPO");
    allProtectedFieldsModification.put(POProtectedFields.MANUAL_PO.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.RE_ENCUMBER.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.BILL_TO.getFieldName(), UUID.randomUUID()
      .toString());
    allProtectedFieldsModification.put(POProtectedFields.VENDOR.getFieldName(), "d1b79c8d-4950-482f-8e42-04f9aae3cb40");
    allProtectedFieldsModification.put(POProtectedFields.ORDER_TYPE.getFieldName(),
        CompositePurchaseOrder.OrderType.ONGOING.value());
    Ongoing ongoing = new Ongoing();
    ongoing.setManualRenewal(true);
    allProtectedFieldsModification.put(POProtectedFields.ONGOING.getFieldName(), JsonObject.mapFrom(ongoing));

    checkPreventProtectedFieldsModificationRule(COMPOSITE_ORDERS_BY_ID_PATH, reqData, allProtectedFieldsModification);
  }


  @Test
  public void testUpdateOrderCloseOrderWithCloseReason() {
    logger.info("=== Test case: Able to Close Order with close Reason ===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.OPEN.value()));
    reqData.put("workflowStatus", "Closed");
    // close reason must not be checked if the Order is in OPEN status in storage
    CloseReason closeReason = new CloseReason();
    closeReason.setNote("can set close reason");
    closeReason.setReason("Complete");
    reqData.put("closeReason", JsonObject.mapFrom(closeReason));

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getString("id")), JsonObject.mapFrom(reqData), "", 204);
  }

  @Test
  public void testUpdateOrderWithLineProtectedFieldsChanging() {
    logger.info("=== Test case when OPEN order errors if protected fields are changed in CompositePoLine===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.OPEN.value()));

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(COMPOSITE_PO_LINES_PREFIX.concat(POLineProtectedFields.CHECKIN_ITEMS.getFieldName()), true);
    allProtectedFieldsModification.put(COMPOSITE_PO_LINES_PREFIX.concat(POLineProtectedFields.ACQUISITION_METHOD.getFieldName()),
        "Depository");
    allProtectedFieldsModification.put(COMPOSITE_PO_LINES_PREFIX.concat(POLineProtectedFields.COST_ADDITIONAL_COST.getFieldName()),
        10.32);
    allProtectedFieldsModification.put(COMPOSITE_PO_LINES_PREFIX.concat(POLineProtectedFields.ERESOURCE_USER_LIMIT.getFieldName()),
        100);

    checkPreventProtectedFieldsModificationRule(COMPOSITE_ORDERS_BY_ID_PATH, reqData, allProtectedFieldsModification);
  }

  @Test
  public void testUpdateOrderWithProtectedFieldsChangingForClosedOrder() {
    logger.info("=== Test case when closed order errors if protected fields are changed ===");

    JsonObject reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_CLOSED_STATUS);
    assertThat(reqData.getString("workflowStatus"), is(CompositePurchaseOrder.WorkflowStatus.CLOSED.value()));

    Map<String, Object> allProtectedFieldsModification = new HashMap<>();

    allProtectedFieldsModification.put(POProtectedFields.APPROVED.getFieldName(), false);
    allProtectedFieldsModification.put(POProtectedFields.PO_NUMBER.getFieldName(), "testPO");
    allProtectedFieldsModification.put(POProtectedFields.MANUAL_PO.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.RE_ENCUMBER.getFieldName(), true);
    allProtectedFieldsModification.put(POProtectedFields.BILL_TO.getFieldName(), UUID.randomUUID()
      .toString());
    allProtectedFieldsModification.put(POProtectedFields.VENDOR.getFieldName(), "d1b79c8d-4950-482f-8e42-04f9aae3cb40");
    allProtectedFieldsModification.put(POProtectedFields.ORDER_TYPE.getFieldName(),
        CompositePurchaseOrder.OrderType.ONE_TIME.value());
    allProtectedFieldsModification.put(POProtectedFields.ONGOING.getFieldName(), null);
    CloseReason closeReason = new CloseReason();
    closeReason.setNote("testing reason on Closed Order");
    closeReason.setReason("Complete");
    allProtectedFieldsModification.put(POProtectedFields.CLOSE_REASON.getFieldName(), JsonObject.mapFrom(closeReason));

    checkPreventProtectedFieldsModificationRule(COMPOSITE_ORDERS_BY_ID_PATH, reqData, allProtectedFieldsModification);
  }

  @Test
  public void testPostOpenOrderCacheKayMustBeTenantSpecific() throws Exception {

    MockServer.serverRqRs.clear();
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat( reqData.getCompositePoLines(), hasSize(1));

    Headers headers = prepareHeaders(new Header(OKAPI_HEADER_TENANT, "existReferenceData"), X_OKAPI_USER_ID);

    //Create order first time for tenant, no contributor name type in cache
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);

    assertThat(getLoanTypesSearches().size() > 0, is(true));
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));
    clearServiceInteractions();

    //Create order second time for tenant, cache contains contributor name type for this tenant
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);


    assertThat(getLoanTypesSearches(), nullValue());
    assertThat(getInstanceStatusesSearches(), nullValue());
    assertThat(getInstanceTypesSearches(), nullValue());
    clearServiceInteractions();

    // Prepare X-Okapi-Tenant heander for tenant which has no contributor name type
    headers = prepareHeaders(new Header(OKAPI_HEADER_TENANT, "anotherExistReferenceData"), X_OKAPI_USER_ID);

    //Create order for another tenant, no contributor name type in cache for this tenant
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);

    assertThat(getLoanTypesSearches().size() > 0, is(true));
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));

  }

  @Test
  public void testInventoryHelperCacheDoesNotKeepEmptyValuesWhenFails() throws Exception {

    MockServer.serverRqRs.clear();
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat( reqData.getCompositePoLines(), hasSize(1));

    Headers headers = prepareHeaders(NON_EXIST_INSTANCE_STATUS_TENANT_HEADER, X_OKAPI_USER_ID);

    //Create order first time for tenant without instanceStatus, no instanceStatus in cache, so logic should call get /instance-types
    Error err = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 500).getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);

    assertThat(getLoanTypesSearches(), nullValue());
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));

    assertThat(err.getCode(), equalTo(ErrorCodes.MISSING_INSTANCE_STATUS.getCode()));
    assertThat(err.getMessage(), equalTo(ErrorCodes.MISSING_INSTANCE_STATUS.getDescription()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(DEFAULT_INSTANCE_STATUS_CODE));
    clearServiceInteractions();

    //Create order second time for same tenant, still no instanceStatus in cache, logic should call get /instance-types again
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 500);

    assertThat(getLoanTypesSearches(), nullValue());
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), nullValue());

  }

  @Test
  public void testInventoryHelperCacheContainsDifferentValuesForInstanceTypeAndInstanceStatusWithSameCode() throws Exception {

    MockServer.serverRqRs.clear();
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat(reqData.getCompositePoLines(), hasSize(1));
    Headers headers = prepareHeaders(INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT_HEADER, X_OKAPI_USER_ID);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);

    assertThat(getLoanTypesSearches().size() > 0, is(true));
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));
    clearServiceInteractions();
    MockServer.addMockTitles(reqData.getCompositePoLines());
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 201);

    assertThat(getLoanTypesSearches(), nullValue());
    assertThat(getInstanceStatusesSearches(), nullValue());
    assertThat(getInstanceTypesSearches(), nullValue());
    assertThat(getCreatedInstances(), hasSize(1));
    JsonObject instance = getCreatedInstances().get(0);
    String instanceStatusId = instance.getString(INSTANCE_STATUS_ID);
    String instanceTypeId = instance.getString(INSTANCE_TYPE_ID);
    assertThat(instanceStatusId, not(equalTo(instanceTypeId)));

  }

  @Test
  public void testInventoryHelperEmptyInstanceTypeThrowsProperError() throws Exception {
    Error err = verifyMissingInventoryEntryErrorHandling(NON_EXIST_INSTANCE_TYPE_TENANT_HEADER);

    assertThat(err.getCode(), equalTo(ErrorCodes.MISSING_INSTANCE_TYPE.getCode()));
    assertThat(err.getMessage(), equalTo(ErrorCodes.MISSING_INSTANCE_TYPE.getDescription()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(DEFAULT_INSTANCE_TYPE_CODE));
  }

  @Test
  public void testInventoryHelperEmptyContributors() throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines()
      .remove(1);
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    reqData.getCompositePoLines().get(0).getContributors().clear();

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), headers, APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);


    assertThat(getContributorNameTypesSearches(), nullValue());
    verifyInventoryInteraction(resp, 1);

  }


  @Test
  public void testInventoryHelperMissingContributorNameTypeThrowsProperError() throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines()
      .remove(1);
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    reqData.getCompositePoLines().get(0).getContributors().add(new Contributor().withContributor("Test").withContributorNameTypeId(ID_DOES_NOT_EXIST));

    Headers headers = prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID);

    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), headers, APPLICATION_JSON, 500);

    int expectedContributorNameTypesSearches = Math.toIntExact(reqData.getCompositePoLines().get(0).getContributors().stream()
      .map(Contributor::getContributorNameTypeId)
      .distinct()
      .count() / MAX_IDS_FOR_GET_RQ + 1);

    assertThat(getContributorNameTypesSearches(), hasSize(expectedContributorNameTypesSearches));

    Error err = resp.getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);

    assertThat(err.getCode(), equalTo(ErrorCodes.MISSING_CONTRIBUTOR_NAME_TYPE.getCode()));
    assertThat(err.getMessage(), equalTo(ErrorCodes.MISSING_CONTRIBUTOR_NAME_TYPE.getDescription()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(ID_DOES_NOT_EXIST));
  }

  @Test
  public void testInventoryHelperEmptyLoanTypeThrowsProperError() throws Exception {

    Error err = verifyMissingInventoryEntryErrorHandling(NON_EXIST_LOAN_TYPE_TENANT_HEADER);

    assertThat(err.getCode(), equalTo(ErrorCodes.MISSING_LOAN_TYPE.getCode()));
    assertThat(err.getMessage(), equalTo(ErrorCodes.MISSING_LOAN_TYPE.getDescription()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(DEFAULT_LOAN_TYPE_NAME));
  }

  @Test
  public void testPostOrdersWithInvalidIsbn() throws Exception {
    logger.info("=== Test Post Order with invalid ISBN ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(reqData);
    String isbn = "1234";

    reqData.getCompositePoLines().get(0).getDetails().getProductIds().get(0).setProductId(isbn);


    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
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
  public void testPostOrdersToConvertToIsbn13() throws Exception {
    logger.info("=== Test Post order to verify ISBN 10 is normalized to ISBN 13 ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    prepareOrderForPostRequest(reqData);
    String isbn = "0-19-852663-6";

    reqData.getCompositePoLines().get(0).getDetails().getProductIds().get(0).setProductId(isbn);
    CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(resp.getCompositePoLines().get(0).getDetails().getProductIds().get(0).getProductId(), equalTo("9780198526636"));
  }

  @Test
  public void testPutOrdersWithInvalidIsbn() throws Exception {
    logger.info("=== Test Put Order with invalid ISBN ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    String isbn = "1234";

    reqData.getCompositePoLines().get(0).getDetails().getProductIds().get(0).setProductId(isbn);

    Response resp = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), APPLICATION_JSON, 400);

    Error err = resp.getBody()
        .as(Errors.class)
        .getErrors()
        .get(0);


    assertThat(err.getMessage(), equalTo(ISBN_NOT_VALID.getDescription()));
    assertThat(err.getCode(), equalTo(ISBN_NOT_VALID.getCode()));
    assertThat(err.getParameters().get(0).getValue(), equalTo(isbn));
  }

  @Test
  public void testPutOrdersToConvertToIsbn13() throws Exception {
    logger.info("=== Test Put order to verify ISBN 10 is normalized to ISBN 13 ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    String isbn = "0-19-852663-6";

    reqData.getCompositePoLines().remove(1);
    reqData.getCompositePoLines().get(0).getDetails().getProductIds().get(0).setProductId(isbn);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData)
      .encodePrettily(), "", 204);

    assertThat(MockServer.getPoLineUpdates().get(0).mapTo(PoLine.class).getDetails().getProductIds().get(0).getProductId(), equalTo("9780198526636"));
  }


  @Test
  public void testPostOrderWithUserNotHavingApprovalPermissions() {
    logger.info("===  Test case when approval is required and user does not have required permission===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_USER_ID), APPLICATION_JSON, 403);

    Error err = resp.getBody()
        .as(Errors.class)
        .getErrors()
        .get(0);

    assertThat(err.getCode(), equalTo(ErrorCodes.USER_HAS_NO_APPROVAL_PERMISSIONS.getCode()));
  }

  @Test
  public void testPostOrderToFailOnNonApprovedOrder() {
    logger.info("===  Test case when approval is required to open order and order not approved ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setApproved(false);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_USER_ID, APPROVAL_PERMISSIONS_HEADER), APPLICATION_JSON, 400);

    Error err = resp.getBody()
        .as(Errors.class)
        .getErrors()
        .get(0);

    assertThat(err.getCode(), equalTo(ErrorCodes.APPROVAL_REQUIRED_TO_OPEN.getCode()));
  }

  @Test
  public void testPostOrderToSetRequiredFieldsOnApproval() {
    logger.info("===  Test required Fields are set/not set based on approval required field===");

    // -- test config set to "isApprovalRequired":true, approval details are set when order is approved in PENDING state

    CompositePurchaseOrder pendingData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES)
      .mapTo(CompositePurchaseOrder.class);
    pendingData.setVendor(ACTIVE_VENDOR_ID);
    pendingData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
    assertThat(pendingData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));

    CompositePurchaseOrder responseData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(pendingData)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_USER_ID, APPROVAL_PERMISSIONS_HEADER),
        APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertThat(responseData.getApprovalDate(), notNullValue());
    assertThat(responseData.getApprovedById(), notNullValue());
    assertThat(responseData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));


    // -- test config set to "isApprovalRequired":false, approval details are not set when order is approved in PENDING state

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES)
        .mapTo(CompositePurchaseOrder.class);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.PENDING);
      assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));

      CompositePurchaseOrder respData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
        .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID, APPROVAL_PERMISSIONS_HEADER),
          APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

      assertThat(respData.getApprovalDate(), nullValue());
      assertThat(respData.getApprovedById(), nullValue());
      assertThat(respData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.PENDING));
  }


  @Test
  public void testPostOrderApprovalNotRequired() {
    logger.info("===  Test case when approval is not required to open order, and order not approved, set required Fields when opening Order ===");

    CompositePurchaseOrder reqData = getMockAsJson(COMP_ORDER_MOCK_DATA_PATH, PO_ID_OPEN_TO_BE_CLOSED).mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.setApproved(false);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    assertThat(reqData.getWorkflowStatus(), is(CompositePurchaseOrder.WorkflowStatus.OPEN));

    CompositePurchaseOrder respData = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
        prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

      assertThat(respData.getApprovalDate(), notNullValue());
      assertThat(respData.getApprovedById(), notNullValue());
  }

  @Test
  public void testPutOrderWithUserNotHavingApprovalPermissions() {
    logger.info("=== Test PUT PO, with User not having Approval permission==");

    CompositePurchaseOrder reqData = getMockAsJson(PE_MIX_PATH).mapTo(CompositePurchaseOrder.class);
    reqData.setApproved(true);
    Headers headers = prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1, X_OKAPI_TOKEN, X_OKAPI_USER_ID);
    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, PENDING_ORDER_APPROVED_FALSE);
    verifyPut(url, JsonObject.mapFrom(reqData).encodePrettily(), headers, APPLICATION_JSON, 403).body().as(Errors.class);
  }

  @Test
  public void testPostOrdersInventoryInteractionWithReceiptNotRequired() throws Exception {
    logger.info("=== Test POST electronic PO, to create Instance and Holding even if receipt not required==");

    JsonObject order = new JsonObject(getMockData(ELECTRONIC_FOR_CREATE_INVENTORY_TEST));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order is Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Set CreateInventory value to create inventory instances and holdings
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING);
    reqData.getCompositePoLines().get(0).setReceiptStatus(ReceiptStatus.RECEIPT_NOT_REQUIRED);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getCreatedInstances());
    assertNotNull(getCreatedHoldings());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  public void testPostOrdersInventoryInteractionWithPackagePoLine() throws Exception {
    logger.info("=== Test POST electronic PO, to no interaction with inventory if poLine.isPackage=true ==");

    JsonObject order = new JsonObject(getMockData(ELECTRONIC_FOR_CREATE_INVENTORY_TEST));
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order is Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    reqData.getCompositePoLines().get(0).setIsPackage(true);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNull(getCreatedInstances());
    assertNull(getCreatedHoldings());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  public void testPostOrdersNoInventoryInteractionWithReceiptNotRequired() throws Exception {
    logger.info("=== Test POST PO, to have no inventory Interaction, with CreateInventory None and receipt not required==");

    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create nothing in inventory
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).setReceiptStatus(ReceiptStatus.RECEIPT_NOT_REQUIRED);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    // No inventory interation necessary with "Receipt Not Required" and CreateInventory "None"
    assertNull(getInstancesSearches());
    assertNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  public void testPostOpenOrderWithEncumbranceCreationError() throws Exception {
    logger.info("=== Test Placement of minimal order ===");

    MockServer.serverRqRs.clear();
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.getCompositePoLines().remove(1);
    assertThat( reqData.getCompositePoLines(), hasSize(1));


    Header errorHeader = new Header(OKAPI_HEADER_TENANT, FUND_CANNOT_BE_PAID_TENANT);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(errorHeader, X_OKAPI_USER_ID), APPLICATION_JSON, 422);

    errorHeader = new Header(OKAPI_HEADER_TENANT, BUDGET_IS_INACTIVE_TENANT);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(errorHeader, X_OKAPI_USER_ID), APPLICATION_JSON, 422);

    errorHeader = new Header(OKAPI_HEADER_TENANT, LEDGER_NOT_FOUND_FOR_TRANSACTION_TENANT);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(errorHeader, X_OKAPI_USER_ID), APPLICATION_JSON, 422);

    errorHeader = new Header(OKAPI_HEADER_TENANT, BUDGET_NOT_FOUND_FOR_TRANSACTION_TENANT);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).encodePrettily(),
      prepareHeaders(errorHeader, X_OKAPI_USER_ID), APPLICATION_JSON, 422);

  }

  private void prepareOrderForPostRequest(CompositePurchaseOrder reqData) {
    reqData.setDateOrdered(null);
    removeAllEncumbranceLinks(reqData);
  }

  private void removeAllEncumbranceLinks(CompositePurchaseOrder reqData) {
    reqData.getCompositePoLines().forEach(poLine ->
      poLine.getFundDistribution().forEach(fundDistribution -> {
        fundDistribution.setEncumbrance(null);
      })
    );
  }


  private Error verifyMissingInventoryEntryErrorHandling(Header header) throws Exception {
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    MockServer.addMockTitles(reqData.getCompositePoLines());
    reqData.getCompositePoLines()
      .remove(1);
    assertThat(reqData.getCompositePoLines(), hasSize(1));

    Headers headers = prepareHeaders(header, X_OKAPI_USER_ID);

    Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), headers, APPLICATION_JSON, 500);

    int expectedContributorNameTypesSearches = Math.toIntExact(reqData.getCompositePoLines().get(0).getContributors().stream()
      .map(Contributor::getContributorNameTypeId)
      .distinct()
      .count() / MAX_IDS_FOR_GET_RQ + 1);

    assertThat(getContributorNameTypesSearches(), hasSize(expectedContributorNameTypesSearches));
    assertThat(getInstanceStatusesSearches(), hasSize(1));
    assertThat(getInstanceTypesSearches(), hasSize(1));

    return resp.getBody()
      .as(Errors.class)
      .getErrors()
      .get(0);
  }

  private Errors verifyPostResponseErrors(int expectedErrorsNumber, String body) {
    Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(errors.getTotalRecords(), equalTo(expectedErrorsNumber));
    assertThat(errors.getErrors(), hasSize(expectedErrorsNumber));
    return errors;
  }


  private CompositePurchaseOrder getPoWithVendorId(String vendorId, String... accessProviderIds) throws Exception {
    CompositePurchaseOrder comPo = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    comPo.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    comPo.setVendor(vendorId);
    for(int i = 0; i < accessProviderIds.length; i++) {
      comPo.getCompositePoLines().get(i).getEresource().setAccessProvider(accessProviderIds[i]);
    }
    return comPo;
  }

  private void checkExpectedError(String id, Errors errors, int index, ErrorCodes expectedErrorCodes, CompositePurchaseOrder purchaseOrder, int expectedPoLineEntriesinErrors) {
    Error error = errors.getErrors().get(index);
    assertThat(error.getCode(), equalTo(expectedErrorCodes.getCode()));
    assertThat(error.getMessage(), equalTo(expectedErrorCodes.getDescription()));
    assertThat(error.getParameters(), hasSize(expectedPoLineEntriesinErrors + 1));
    assertThat(error.getParameters().get(0).getKey(), equalTo(ID));
    assertThat(error.getParameters().get(0).getValue(), equalTo(id));
    if(expectedPoLineEntriesinErrors > 0) {
      List<String> poLineNumbersFromError = error.getParameters().stream().filter(p -> p.getKey().equals("poLineNumber")).map(Parameter::getValue).collect(toList());
      List<String> poLineNumbers = purchaseOrder.getCompositePoLines().stream().map(CompositePoLine::getPoLineNumber).collect(toList());
      poLineNumbersFromError.forEach(p -> assertThat(poLineNumbers.contains(p), is(true)));
    }
  }

  @Test
  public void testPostShouldBeSuccessIfOrderInPendingStatusAndMaterialTypeIsAbsentInOrderLine() {
    logger.info("===Test Post Successful test completion if material type is absent and Order in PENDING status ===");
    CompositePurchaseOrder reqData = getMockAsJson(ORDER_WITHOUT_MATERIAL_TYPE_JSON).mapTo(CompositePurchaseOrder.class);
    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201);
  }

  @Test
  public void testPostShouldFailedIfOrderIsNotInPendingStatusAndMaterialTypeIsAbsentInOrderLine() {
    logger.info("===Test Post Failed test completion if material type is absent and Order in OPEN status ===");
    CompositePurchaseOrder reqData = getMockAsJson(ORDER_WITHOUT_MATERIAL_TYPE_JSON).mapTo(CompositePurchaseOrder.class);

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    List<Error> errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(MISSING_MATERIAL_TYPE.getDescription()));
  }

  @Test
  public void testPutShouldFailedIfOrderTransitFromPendingToOpenStatusAndMaterialTypeIsAbsentInOrderLine() {
    logger.info("===Test Success Post Failed test completion if material type is absent and Order in PENDING status");
    CompositePurchaseOrder reqData = getMockAsJson(ORDER_WITHOUT_MATERIAL_TYPE_JSON).mapTo(CompositePurchaseOrder.class);
    reqData.getCompositePoLines().forEach(this::removeMaterialType);

    CompositePurchaseOrder po = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 201)
      .as(CompositePurchaseOrder.class);

    CompositePurchaseOrder putReq = prepareCompositeOrderOpenRequest(po);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    List<Error> errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_WITHOUT_MATERIAL_TYPES_ID), JsonObject.mapFrom(putReq)
      .encodePrettily(), prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(MISSING_MATERIAL_TYPE.getDescription()));
  }

  private CompositePurchaseOrder prepareCompositeOrderOpenRequest(CompositePurchaseOrder po) {
    CompositePurchaseOrder compositeOrderPutRequest = new CompositePurchaseOrder();
    compositeOrderPutRequest.setId(ORDER_WITHOUT_MATERIAL_TYPES_ID);
    compositeOrderPutRequest.setOrderType(CompositePurchaseOrder.OrderType.ONE_TIME);
    compositeOrderPutRequest.setOrderType(po.getOrderType());
    compositeOrderPutRequest.setApproved(false);
    compositeOrderPutRequest.setPoNumber(po.getPoNumber());
    compositeOrderPutRequest.setTotalEstimatedPrice(po.getTotalEstimatedPrice());
    compositeOrderPutRequest.setTotalItems(po.getTotalItems());
    compositeOrderPutRequest.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    compositeOrderPutRequest.setVendor(po.getVendor());
    compositeOrderPutRequest.setMetadata(po.getMetadata());

    return compositeOrderPutRequest;
  }

  private void removeMaterialType(CompositePoLine poLine) {
    if (poLine.getPhysical() != null)
      poLine.getPhysical().setMaterialType(null);
    if (poLine.getEresource() != null)
      poLine.getEresource().setMaterialType(null);
  }

  private static JsonObject getMockDraftOrder() throws Exception {
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH));
    order.put("workflowStatus", "Pending");

    return order;
  }
}
