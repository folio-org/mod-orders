package org.folio.rest.impl;

import io.restassured.http.Header;
import io.restassured.http.Headers;
import io.restassured.response.Response;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.HttpStatus;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Physical.CreateInventory;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrders;
import org.junit.Test;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.folio.orders.utils.ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID;
import static org.folio.orders.utils.ErrorCodes.COST_UNIT_PRICE_INVALID;
import static org.folio.orders.utils.ErrorCodes.ELECTRONIC_COST_LOC_QTY_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ErrorCodes.MISSING_MATERIAL_TYPE;
import static org.folio.orders.utils.ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY;
import static org.folio.orders.utils.ErrorCodes.ORDER_CLOSED;
import static org.folio.orders.utils.ErrorCodes.ORDER_OPEN;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.ORDER_VENDOR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.PHYSICAL_COST_LOC_QTY_MISMATCH;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.POL_ACCESS_PROVIDER_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.POL_LINES_LIMIT_EXCEEDED;
import static org.folio.orders.utils.ErrorCodes.VENDOR_ISSUE;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_ELECTRONIC_QTY;
import static org.folio.orders.utils.ErrorCodes.ZERO_COST_PHYSICAL_QTY;
import static org.folio.orders.utils.ErrorCodes.ZERO_LOCATION_QTY;
import static org.folio.orders.utils.ErrorCodes.ORGANIZATION_NOT_A_VENDOR;
import static org.folio.orders.utils.ErrorCodes.ACCESSPROVIDER_NOT_A_VENDOR;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.calculateInventoryItemsQuantity;
import static org.folio.orders.utils.HelperUtils.calculateTotalEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.calculateTotalQuantity;
import static org.folio.orders.utils.ResourcePathResolver.SEARCH_ORDERS;
import static org.folio.orders.utils.ResourcePathResolver.PAYMENT_STATUS;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER;
import static org.folio.orders.utils.ResourcePathResolver.RECEIPT_STATUS;
import static org.folio.orders.utils.ResourcePathResolver.VENDOR_ID;
import static org.folio.rest.impl.InventoryInteractionTestHelper.joinExistingAndNewItems;
import static org.folio.rest.impl.InventoryInteractionTestHelper.verifyInstanceLinksForUpdatedOrder;
import static org.folio.rest.impl.InventoryInteractionTestHelper.verifyInventoryInteraction;
import static org.folio.rest.impl.InventoryInteractionTestHelper.verifyPiecesCreated;
import static org.folio.rest.impl.InventoryInteractionTestHelper.verifyPiecesQuantityForSuccessCase;
import static org.folio.rest.impl.MockServer.getCreatedInstances;
import static org.folio.rest.impl.MockServer.getCreatedItems;
import static org.folio.rest.impl.MockServer.getCreatedPieces;
import static org.folio.rest.impl.MockServer.getHoldingsSearches;
import static org.folio.rest.impl.MockServer.getInstancesSearches;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getPieceSearches;
import static org.folio.rest.impl.PoNumberApiTest.EXISTING_PO_NUMBER;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;


public class PurchaseOrdersApiTest extends ApiTestBase {

  private static final Logger logger = LoggerFactory.getLogger(PurchaseOrdersApiTest.class);

  private static final String ORDER_WITHOUT_PO_LINES = "order_without_po_lines.json";
  private static final String ORDER_WITHOUT_VENDOR_ID = "order_without_vendor_id.json";
  private static final String ORDER_WITH_PO_LINES_JSON = "put_order_with_po_lines.json";
  private static final String ORDER_WITH_MISMATCH_ID_INT_PO_LINES_JSON = "put_order_with_mismatch_id_in_po_lines.json";

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

  // API paths
  private final static String COMPOSITE_ORDERS_PATH = "/orders/composite-orders";
  private final static String COMPOSITE_ORDERS_BY_ID_PATH = COMPOSITE_ORDERS_PATH + "/%s";

  static final String LISTED_PRINT_MONOGRAPH_PATH = "po_listed_print_monograph.json";
  private static final String ORDERS_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + "getOrders.json";
  private static final String ORDER_FOR_FAILURE_CASE_MOCK_DATA_PATH = COMP_ORDER_MOCK_DATA_PATH + PO_ID_FOR_FAILURE_CASE + ".json";
  private static final String PE_MIX_PATH = "po_listed_print_monograph_pe_mix.json";
  private static final String MONOGRAPH_FOR_CREATE_INVENTORY_TEST = "print_monograph_for_create_inventory_test.json";
  private static final String LISTED_PRINT_SERIAL_PATH = "po_listed_print_serial.json";
  private static final String MINIMAL_ORDER_PATH = "minimal_order.json";
  private static final String PO_CREATION_FAILURE_PATH = "po_creation_failure.json";

  private static final String NULL = "null";
  static final String PURCHASE_ORDER_ID = "purchaseOrderId";


  @Test
  public void testListedPrintMonograph() throws Exception {
    logger.info("=== Test Listed Print Monograph ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);

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
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp));

    String poId = resp.getId();
    String poNumber = resp.getPoNumber();

    assertThat(poId, notNullValue());
    assertThat(poNumber, notNullValue());
    assertThat(resp.getCompositePoLines(), hasSize(2));

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
    assertThat(response.getErrors(), hasSize(12));
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
                                              COST_UNIT_PRICE_INVALID.getCode(),
                                              MISSING_MATERIAL_TYPE.getCode()));

    // Check that no any calls made by the business logic to other services
    assertTrue(MockServer.serverRqRs.isEmpty());
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
    assertTrue(MockServer.serverRqRs.isEmpty());
  }

  @Test
  public void testListedPrintMonographInOpenStatus() throws Exception {
    logger.info("=== Test Listed Print Monograph in Open status ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
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
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

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
    // Set source code to null to verify validation for second POL
    reqData.getCompositePoLines().get(1).getSource().setCode(null);

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
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    // Verify dateOrdered is not set because Workflow status is not OPEN
    assertNull(resp.getDateOrdered());
  }

  @Test
  public void testPostOpenOrderInventoryUpdateWithOrderFormatOther() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that mock po has the first PO line with 3 locations
    assertEquals(3, reqData.getCompositePoLines().get(0).getLocations().size());

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
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);
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
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that mock po has the first PO line with 3 locations
    assertEquals(3, reqData.getCompositePoLines().get(0).getLocations().size());

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
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);
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
  public void testPutOrdersByIdPEMixFormat() throws Exception {
    logger.info("=== Test Put Order By Id create Pieces with P/E Mix format ===");
    JsonObject order = new JsonObject(getMockData(PE_MIX_PATH));
    order.put("workflowStatus", "Pending");
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);

    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 1 po line
    assertEquals(1, reqData.getCompositePoLines().size());

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

    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
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
  }

  @Test
  public void testPutOrdersByIdTotalPiecesEqualsTotalQuantityWhenCreateInventoryIsFalse() throws Exception {
    logger.info("=== Test Put Order By Id create Pieces when Item record does not exist ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);
    // Make sure that mock PO has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

    reqData.getCompositePoLines().get(1).getEresource().setCreateInventory(Eresource.CreateInventory.NONE);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);
    List<JsonObject> items = joinExistingAndNewItems();
    List<JsonObject> createdPieces = getCreatedPieces();
    verifyPiecesQuantityForSuccessCase(reqData, createdPieces);
    verifyPiecesCreated(items, reqData.getCompositePoLines(), createdPieces);
  }

  @Test
  public void testPlaceOrderMinimal() throws Exception {
    logger.info("=== Test Placement of minimal order ===");

    String body = getMockData(MINIMAL_ORDER_PATH);
    JsonObject reqData = new JsonObject(body);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);


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
    request.put("orderType", "Ongoing");
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);
    String body= request.toString();

     verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
       prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 400);

  }

  @Test
  public void testPostOrderPONumberAutoGenerated() {
    logger.info("=== Test Placement of Empty order with Auto Generated PO Number===");

    JsonObject request = new JsonObject();
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);
    request.put("orderType", "Ongoing");
    String body= request.toString();
    
    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body,
      prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

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
    assertEquals("must match \"^[a-zA-Z0-9]{5,16}$\"", errors.getErrors().get(0).getMessage());
    assertFalse(errors.getErrors().get(0).getParameters().isEmpty());
    assertNotNull(errors.getErrors().get(0).getParameters().get(0));
    assertEquals("poNumber", errors.getErrors().get(0).getParameters().get(0).getKey());
    assertEquals("123", errors.getErrors().get(0).getParameters().get(0).getValue());
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

    final Response resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, body, headers, APPLICATION_JSON, 500);

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

    final CompositePurchaseOrder resp = verifySuccessGet(url, CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(id, resp.getId());
    verifyCalculatedData(resp);
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
  public void testGetPOByIdTotalItemsWithoutPOLines() {
    logger.info("=== Test Get Order By Id without PO Line totalItems value is 0 ===");

    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, ORDER_ID_WITHOUT_PO_LINES));
    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES);
    final CompositePurchaseOrder resp = verifySuccessGet(url, CompositePurchaseOrder.class);

    logger.info(JsonObject.mapFrom(resp).encodePrettily());

    assertEquals(ORDER_ID_WITHOUT_PO_LINES, resp.getId());
    assertEquals(0, resp.getTotalItems().intValue());
  }

  @Test
  public void testGetOrderByIdWithOnePoLine() {
    logger.info("=== Test Get Order By Id - With one PO Line and empty source ===");

    String id = PO_ID_CLOSED_STATUS;
    logger.info(String.format("using mock datafile: %s%s.json", COMP_ORDER_MOCK_DATA_PATH, id));
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
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_DOES_NOT_EXIST, "", 204);
  }

  @Test
  public void testDeleteById500Error() {
    logger.info("=== Test Delete Order By Id - Storage Internal Server Error ===");
    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_FOR_INTERNAL_SERVER_ERROR, APPLICATION_JSON, 500);
  }

  @Test
  public void testDeleteByIdWithoutOkapiUrlHeader() {
    logger.info("=== Test Delete Order By Id - 500 due to missing Okapi URL header ===");

    verifyDeleteResponse(COMPOSITE_ORDERS_PATH + "/" + ID_DOES_NOT_EXIST, prepareHeaders(NON_EXIST_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON,500);
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
    assertEquals(CompositePurchaseOrder.WorkflowStatus.CLOSED.value(), storageUpdatedOrder.getWorkflowStatus().value());

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
    request.put("orderType", "Ongoing");
    request.put("vendor", EXISTING_REQUIRED_VENDOR_UUID);

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, id), request, APPLICATION_JSON, 400);
  }

  @Test
  public void testPoUpdateWithOverLimitPOLines() throws Exception {
    logger.info("=== Test PUT PO, with over limit lines quantity ===");

    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES);
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

    String url = String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES);
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

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), "", 204);

    int polCount = reqData.getCompositePoLines().size();

    verifyInstanceLinksForUpdatedOrder(reqData);
    verifyInventoryInteraction(reqData, polCount - 1);
    verifyReceiptStatusChangedTo(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT.value(), reqData.getCompositePoLines().size());
    verifyPaymentStatusChangedTo(CompositePoLine.PaymentStatus.PAYMENT_NOT_REQUIRED.value(), reqData.getCompositePoLines().size());
  }

  @Test
  public void testPostOrdersWithOpenStatusAndCheckinItems() throws Exception {
    logger.info("=== Test POST Order By Id to change status of Order to Open - inventory interaction required only for first POL ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    // Make sure that mock po has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());
    // Make sure that mock po has the first PO line with 3 locations
    assertEquals(3, reqData.getCompositePoLines().get(0).getLocations().size());

    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set checkin items flag true
    reqData.getCompositePoLines().forEach(s -> s.setCheckinItems(true));

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNull(getCreatedPieces());
  }

  @Test
  public void testPostOrdersCreateInventoryNone() throws Exception {
    JsonObject order = new JsonObject(getMockData(MONOGRAPH_FOR_CREATE_INVENTORY_TEST));
    // Get Open Order
    CompositePurchaseOrder reqData = order.mapTo(CompositePurchaseOrder.class);
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Set CreateInventory value to create nothing in inventory
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(Physical.CreateInventory.NONE);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(Eresource.CreateInventory.NONE);

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNull(getInstancesSearches());
    assertNull(getHoldingsSearches());
    assertNull(getItemsSearches());
    assertNotNull(getCreatedPieces());
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

    verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

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
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

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
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

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
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Clear CreateInventory for setting default values
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(null);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(null);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

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
    // Make sure that Order moves to Open
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    // Clear CreateInventory for setting default values
    reqData.getCompositePoLines().get(0).getPhysical().setCreateInventory(null);
    reqData.getCompositePoLines().get(0).getEresource().setCreateInventory(null);

    final CompositePurchaseOrder resp = verifyPostResponse(COMPOSITE_ORDERS_PATH, JsonObject.mapFrom(reqData).toString(),
      prepareHeaders(EMPTY_CONFIG_X_OKAPI_TENANT), APPLICATION_JSON, 201).as(CompositePurchaseOrder.class);

    assertNotNull(getInstancesSearches());
    assertNotNull(getHoldingsSearches());
    assertNotNull(getItemsSearches());
    verifyInventoryInteraction(resp, 1);

    assertNotNull(getCreatedPieces());
  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenWithCheckinItems() throws Exception {
    logger.info("=== Test Put Order By Id to change status of Order to Open ===");

    // Get Open Order
    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Make sure that mock PO has 2 po lines
    assertEquals(2, reqData.getCompositePoLines().size());

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

    int polCount = reqData.getCompositePoLines().size();
    // Make sure that mock PO has 2 lines
    assertEquals(2, polCount);
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
      if (StringUtils.isNotEmpty(line.getInstanceId())) {
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

    verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, ORDER_ID_WITHOUT_PO_LINES), reqData, "", 204);

    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.PUT));
    List<JsonObject> createdPoLines = MockServer.serverRqRs.get(PO_LINES, HttpMethod.POST);
    assertEquals(createdPoLines.size(), reqData.getJsonArray(COMPOSITE_PO_LINES).size());
    createdPoLines.forEach(poLine -> assertEquals(poNumber + "-" + PO_LINE_NUMBER_VALUE, poLine.getString(PO_LINE_NUMBER)));
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
    reqData.setId(PO_ID);

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

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, PO_ID),
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
    validatePoLineCreationErrorForNonPendingOrder(errorCode, errors);
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

    final PurchaseOrders purchaseOrders = verifySuccessGet(COMPOSITE_ORDERS_PATH, PurchaseOrders.class);
    assertNotNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET));
    assertNull(MockServer.serverRqRs.get(SEARCH_ORDERS, HttpMethod.GET));
    assertEquals(3, purchaseOrders.getTotalRecords().intValue());
  }

  @Test
  public void testGetOrdersWithParameters() {
    logger.info("=== Test Get Orders - With empty query ===");
    String endpointQuery = String.format("%s?query=%s", COMPOSITE_ORDERS_PATH, "poNumber==" + EXISTING_PO_NUMBER);
    final PurchaseOrders purchaseOrders = verifySuccessGet(endpointQuery, PurchaseOrders.class);
    assertNull(MockServer.serverRqRs.get(PURCHASE_ORDER, HttpMethod.GET));
    assertNotNull(MockServer.serverRqRs.get(SEARCH_ORDERS, HttpMethod.GET));
    assertEquals(1, purchaseOrders.getTotalRecords().intValue());
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

    // Purchase order is OK
    Errors activeVendorActiveAccessProviderErrors = verifyPostResponse(COMPOSITE_ORDERS_PATH, getPoWithVendorId(ACTIVE_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 201).as(Errors.class);
    assertThat(activeVendorActiveAccessProviderErrors.getErrors(), empty());

    // Internal mod-vendor error
    Errors internalServerError = verifyPostResponseErrors(1, MOD_VENDOR_INTERNAL_ERROR_ID, ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B);
    assertThat(internalServerError.getErrors().get(0).getCode(), equalTo(VENDOR_ISSUE.getCode()));
    assertThat(internalServerError.getErrors().get(0).getAdditionalProperties().get(VendorHelper.ERROR_CAUSE), notNullValue());

    // Non-existed vendor
    Errors nonExistedVendorError = verifyPostResponseErrors(1, NON_EXIST_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B);
    checkExpectedError(NON_EXIST_VENDOR_ID, nonExistedVendorError, 0, ORDER_VENDOR_NOT_FOUND);

    // Active vendor and non-existed access provider
    Errors inactiveAccessProviderErrors = verifyPostResponseErrors(1, ACTIVE_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, NON_EXIST_ACCESS_PROVIDER_A);
    checkExpectedError(NON_EXIST_ACCESS_PROVIDER_A, inactiveAccessProviderErrors, 0, POL_ACCESS_PROVIDER_NOT_FOUND);

    // Inactive access provider
    Errors nonExistedAccessProviderErrors = verifyPostResponseErrors(1, ACTIVE_VENDOR_ID, ACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_A);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, nonExistedAccessProviderErrors, 0, POL_ACCESS_PROVIDER_IS_INACTIVE);

    // Inactive vendor and inactive access providers
    Errors allInactiveErrors = verifyPostResponseErrors(3, INACTIVE_VENDOR_ID, INACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_B);
    checkExpectedError(INACTIVE_VENDOR_ID, allInactiveErrors, 0, ORDER_VENDOR_IS_INACTIVE);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, allInactiveErrors, 1, POL_ACCESS_PROVIDER_IS_INACTIVE);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_B, allInactiveErrors, 2, POL_ACCESS_PROVIDER_IS_INACTIVE);

  }

  @Test
  public void testPutOrdersByIdToChangeStatusToOpenInactiveVendor() throws Exception {

    logger.info("=== Test Put Order By Id to change status of Order to Open for different vendor's status ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

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
    checkExpectedError(NON_EXIST_VENDOR_ID, nonExistedVendorErrors, 0, ORDER_VENDOR_NOT_FOUND);

    // Inactive access provider
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(ACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    Errors inactiveAccessProviderErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, inactiveAccessProviderErrors, 0, POL_ACCESS_PROVIDER_IS_INACTIVE);

    // Inactive vendor and inactive access providers
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    reqData.setVendor(INACTIVE_VENDOR_ID);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_A);
    reqData.getCompositePoLines().get(1).getEresource().setAccessProvider(INACTIVE_ACCESS_PROVIDER_B);
    Errors allInactiveErrors
      = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()), JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);
    checkExpectedError(INACTIVE_VENDOR_ID, allInactiveErrors, 0, ORDER_VENDOR_IS_INACTIVE);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_A, allInactiveErrors, 1, POL_ACCESS_PROVIDER_IS_INACTIVE);
    checkExpectedError(INACTIVE_ACCESS_PROVIDER_B, allInactiveErrors, 2, POL_ACCESS_PROVIDER_IS_INACTIVE);
  }

  @Test
  public void testPutOrderToChangeStatusToOpenVendorWithUnexpectedContent() throws Exception {

    logger.info("=== Test Put Order to change status of Order to Open - vendor with unexpected content ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
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
  public void testPutOrderToChangeStatusToOpenwithOrganizationNotVendor() throws Exception {

    logger.info("=== Test Put Order to change status of Order to Open - organization which is not vendor ===");

    CompositePurchaseOrder reqData = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    reqData.setId(ID_FOR_PRINT_MONOGRAPH_ORDER);

    // Prepare order
    reqData.setVendor(ORGANIZATION_NOT_VENDOR);
    reqData.getCompositePoLines().get(0).getEresource().setAccessProvider(ORGANIZATION_NOT_VENDOR);
    reqData.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);

    Errors errors = verifyPut(String.format(COMPOSITE_ORDERS_BY_ID_PATH, reqData.getId()),
      JsonObject.mapFrom(reqData), EMPTY, 422).as(Errors.class);

    assertThat(errors.getErrors(), hasSize(2));


    checkExpectedError(ORGANIZATION_NOT_VENDOR, errors, 0, ORGANIZATION_NOT_A_VENDOR);
    checkExpectedError(ORGANIZATION_NOT_VENDOR, errors, 1, ACCESSPROVIDER_NOT_A_VENDOR);

}

  private Errors verifyPostResponseErrors(int expectedErrorsNumber, String vendorId, String... accessProviderIds) throws Exception {
    Errors errors = verifyPostResponse(COMPOSITE_ORDERS_PATH, getPoWithVendorId(vendorId, accessProviderIds),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), APPLICATION_JSON, 422).as(Errors.class);
    assertThat(errors.getTotalRecords(), equalTo(expectedErrorsNumber));
    assertThat(errors.getErrors(), hasSize(expectedErrorsNumber));
    return errors;
  }

  private String getPoWithVendorId(String vendorId, String... accessProviderIds) throws Exception {
    CompositePurchaseOrder comPo = getMockDraftOrder().mapTo(CompositePurchaseOrder.class);
    comPo.setWorkflowStatus(CompositePurchaseOrder.WorkflowStatus.OPEN);
    comPo.setVendor(vendorId);
    for(int i = 0; i < accessProviderIds.length; i++) {
      comPo.getCompositePoLines().get(i).getEresource().setAccessProvider(accessProviderIds[i]);
    }
    return JsonObject.mapFrom(comPo).toString();
  }

  private void checkExpectedError(String id, Errors errors, int index, ErrorCodes expectedErrorCodes) {
    Error error = errors.getErrors().get(index);
    assertThat(error.getCode(), equalTo(expectedErrorCodes.getCode()));
    assertThat(error.getMessage(), equalTo(expectedErrorCodes.getDescription()));
    assertThat(error.getParameters(), hasSize(1));
    assertThat(error.getParameters().get(0).getKey(), equalTo(ID));
    assertThat(error.getParameters().get(0).getValue(), equalTo(id));
  }

  private static JsonObject getMockDraftOrder() throws Exception {
    JsonObject order = new JsonObject(getMockData(LISTED_PRINT_MONOGRAPH_PATH));
    order.put("workflowStatus", "Pending");

    return order;
  }

}
