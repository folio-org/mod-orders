package org.folio.orders.utils;

import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.*;

import io.vertx.core.json.JsonObject;
import org.folio.CopilotGenerated;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Details;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ProductId;
import org.folio.rest.jaxrs.model.acq.Location;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import org.folio.rest.jaxrs.model.Ongoing;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.folio.rest.core.exceptions.ErrorCodes.WRONG_ONGOING_SUBSCRIPTION_FIELDS_CHANGED;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import static org.folio.rest.core.exceptions.ErrorCodes.WRONG_ONGOING_NOT_SUBSCRIPTION_FIELDS_CHANGED;

@CopilotGenerated(partiallyGenerated = true)
public class PoLineCommonUtilTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Test
  void testOnlyInstanceUpdateNeededForPhysicalIfCreateInventoryInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getPoLines().forEach(line -> {
      line.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(PoLine.ReceiptStatus.FULLY_RECEIVED);
      line.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE);
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getPoLines().get(0));
    //Then
    assertTrue(actCheck);
  }

  @Test
  void testOnlyInstanceUpdateNeededForElectronicalIfCreateInventoryInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getPoLines().forEach(line -> {
      line.setOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
      line.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(PoLine.ReceiptStatus.FULLY_RECEIVED);
      line.setPhysical(null);
      line.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE));
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getPoLines().get(0));
    //Then
    assertTrue(actCheck);
  }

  @Test
  void testOnlyInstanceUpdateNeededIfCreateInventoryIsNotInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getPoLines().forEach(line -> {
      line.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(PoLine.ReceiptStatus.FULLY_RECEIVED);
      line.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getPoLines().get(0));
    //Then
    assertFalse(actCheck);
  }

  @Test
  void testShouldReturnLineWithoutChanges() {
    List<String> protectedFields = List.of("details.productIds");
    String productIdType = "8261054f-be78-422d-bd51-4ed9f33c3422";
    ProductId firstProductId = new ProductId()
      .withProductId("9780735245341")
      .withQualifier("Penguin Canada")
      .withProductIdType(productIdType);
    ProductId secondProductId = new ProductId()
      .withProductId("9780593492543")
      .withQualifier("Penguin Random House")
      .withProductIdType(productIdType);
    PoLine lineFromStorage = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(new Details().withProductIds(List.of(firstProductId, secondProductId)));
    PoLine requestObject = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(new Details().withProductIds(List.of(secondProductId, firstProductId)));

    JsonObject lineFromStorageJson = JsonObject.mapFrom(lineFromStorage);

    JsonObject result = PoLineCommonUtil
      .verifyProtectedFieldsChanged(protectedFields, lineFromStorageJson, JsonObject.mapFrom(requestObject));

    assertEquals(result, lineFromStorageJson);
  }

  @Test
  void testShouldHandleNullArrayFromStorage() {
    //given
    List<String> protectedFields = List.of("details.productIds");
    PoLine lineFromStorage = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(null);
    PoLine requestObject = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(new Details().withProductIds(List.of()));
    JsonObject lineFromStorageJson = JsonObject.mapFrom(lineFromStorage);

    //when
    JsonObject result = PoLineCommonUtil
      .verifyProtectedFieldsChanged(protectedFields, lineFromStorageJson, JsonObject.mapFrom(requestObject));

    //then
    assertEquals(result, lineFromStorageJson);
  }

  @Test
  void shouldHandleNullArrayFromRequest() {
    //given
    List<String> protectedFields = List.of("details.productIds");
    PoLine lineFromStorage = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(new Details().withProductIds(List.of()));
    PoLine requestObject = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(null);
    JsonObject lineFromStorageJson = JsonObject.mapFrom(lineFromStorage);

    //when
    JsonObject result = PoLineCommonUtil
      .verifyProtectedFieldsChanged(protectedFields, lineFromStorageJson, JsonObject.mapFrom(requestObject));

    //then
    assertEquals(result, lineFromStorageJson);
  }

  @Test
  void testShouldThrowExceptionBecauseRequiredFieldWasUpdated() {
    List<String> protectedFields = List.of("details.productIds");
    String productIdType = "8261054f-be78-422d-bd51-4ed9f33c3422";
    ProductId firstProductId = new ProductId()
      .withProductId("9780735245341")
      .withQualifier("Penguin Canada")
      .withProductIdType(productIdType);
    ProductId secondProductId = new ProductId()
      .withProductId("9780593492543")
      .withQualifier("Penguin Random House")
      .withProductIdType(productIdType);
    ProductId thirdProductId = new ProductId()
      .withProductId("9780593491234")
      .withQualifier("Test House")
      .withProductIdType(productIdType);

    PoLine lineFromStorage = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(new Details().withProductIds(List.of(firstProductId, secondProductId)));
    PoLine requestObject = new PoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(new Details().withProductIds(List.of(secondProductId, firstProductId, thirdProductId)));

    JsonObject lineFromStorageJson = JsonObject.mapFrom(lineFromStorage);

    HttpException exception = assertThrows(HttpException.class, () -> PoLineCommonUtil
      .verifyProtectedFieldsChanged(protectedFields, lineFromStorageJson, JsonObject.mapFrom(requestObject)));


    String errorMessage = "{\"message\":\"Protected fields can't be modified\",\"code\":\"protectedFieldChanging\",\"parameters\":[],\"protectedAndModifiedFields\":[\"details.productIds\"]}";
    assertEquals(400, exception.getCode());
    assertEquals(errorMessage, exception.getMessage());
  }

  @ParameterizedTest
  @CsvSource(value = {"false:true:Other::Instance:true",
    "true:false:Other:None::true",
    "true:false:Other:Instance::false",
    "true:false:Physical Resource:None::true",
    "true:false:Physical Resource:Instance::false",
    "false:true:Electronic Resource::None:true",
    "false:true:Electronic Resource::Instance:false",
    "true:true:P/E Mix:None:None:true",
    "true:true:P/E Mix:Instance:None:false",
    "true:true:P/E Mix:None:Instance:false"
  }, delimiter = ':')
  void testIsInventoryUpdateNotRequired(Boolean withPhysical, Boolean withEResource, String orderFormat,
      String physicalCreateInventory, String eresourceCreateInventory, Boolean updateNotRequired) {
    PoLine poLine = new PoLine();
    if (withPhysical) {
      poLine.setPhysical(new Physical());
    }
    if (withEResource) {
      poLine.setEresource(new Eresource());
    }
    poLine.setOrderFormat(PoLine.OrderFormat.fromValue(orderFormat));
    if (physicalCreateInventory != null) {
      poLine.getPhysical().setCreateInventory(Physical.CreateInventory.fromValue(physicalCreateInventory));
    }
    if (eresourceCreateInventory != null) {
      poLine.getEresource().setCreateInventory(Eresource.CreateInventory.fromValue(eresourceCreateInventory));
    }
    boolean result = PoLineCommonUtil.isInventoryUpdateNotRequired(poLine);
    assertEquals(result, updateNotRequired);
  }


  @Test
  void testExtractUnaffiliatedLocations() {
    List<Location> locations = List.of(
      createLocation("tenant1"),
      createLocation("tenant2"),
      createLocation("tenant3")
    );
    List<String> tenantIds = List.of("tenant1", "tenant2");
    var result = PoLineCommonUtil.extractUnaffiliatedLocations(locations, tenantIds);
    assertEquals(1, result.size());
    assertTrue(result.contains(locations.get(2)));
  }

  @Test
  void testExtractUnaffiliatedLocationsWhenLocationsListIsEmpty() {
    List<Location> locations = new ArrayList<>();
    List<String> tenantIds = List.of("tenant1", "tenant2");
    var result = PoLineCommonUtil.extractUnaffiliatedLocations(locations, tenantIds);
    assertTrue(result.isEmpty());

    locations.add(new Location().withTenantId("tenant1"));
    locations.add(new Location().withTenantId("tenant2"));
    result = PoLineCommonUtil.extractUnaffiliatedLocations(locations, tenantIds);
    assertTrue(result.isEmpty());
  }

  @Test
  void testExtractUnaffiliatedLocationsHandleNullLocationTenantId() {
    List<Location> locations = List.of(
      createLocation(null),
      createLocation("tenant2")
    );
    List<String> tenantIds = List.of("tenant1");
    var result = PoLineCommonUtil.extractUnaffiliatedLocations(locations, tenantIds);
    assertEquals(1, result.size());
    assertTrue(result.contains(locations.get(1)));
  }

    @ParameterizedTest
  @CsvSource(value = {
    "Physical Resource : Instance, Holding",
    "Physical Resource : Instance, Holding, Item",
    "P/E Mix : Instance, Holding",
    "P/E Mix : Instance, Holding, Item",
    "Other : Instance, Holding",
    "Other : Instance, Holding, Item"
  }, delimiter = ':')
  void testIsHoldingUpdateRequiredForPhysicalShouldReturnTrueWhenInventoryIncludesHolding(String orderFormat, String inventoryLevel) {
    // TestMate-c19f70b78381cff87f5d32b13397b20e
    // Given
    PoLine poLine = new PoLine()
      .withOrderFormat(PoLine.OrderFormat.fromValue(orderFormat.trim()))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.fromValue(inventoryLevel.trim())));
    // When
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine);
    // Then
    assertTrue(result);
  }

    @ParameterizedTest
  @CsvSource(value = {
    "Physical Resource : None",
    "Physical Resource : Instance",
    "P/E Mix : None",
    "P/E Mix : Instance",
    "Other : None",
    "Other : Instance"
  }, delimiter = ':')
  void testIsHoldingUpdateRequiredForPhysicalShouldReturnFalseWhenInventoryDoesNotIncludeHolding(String orderFormat, String inventoryLevel) {
    // TestMate-1581cfa06416eaa713d4f93217af3a4d
    // Given
    PoLine poLine = new PoLine()
      .withOrderFormat(PoLine.OrderFormat.fromValue(orderFormat.trim()))
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.fromValue(inventoryLevel.trim())));
    // When
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine);
    // Then
    assertFalse(result);
  }

    @Test
  void testIsHoldingUpdateRequiredForPhysicalWhenFormatIsElectronicShouldReturnFalse() {
    // TestMate-4d515f87662dde642e50ef85c32f079b
    // Given
    PoLine poLine = new PoLine()
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE)
      .withPhysical(new Physical().withCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING));
    // When
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine);
    // Then
    assertFalse(result);
  }

    @Test
  void testIsHoldingUpdateRequiredForPhysicalWhenPhysicalIsNullShouldReturnFalse() {
    // TestMate-c7faa34b2bf64523bc9bda50bfad4bf1
    // Given
    PoLine poLine = new PoLine()
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withPhysical(null);
    // When
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine);
    // Then
    assertFalse(result);
  }

    @ParameterizedTest
  @CsvSource(value = {
    "Electronic Resource : Instance, Holding",
    "Electronic Resource : Instance, Holding, Item",
    "P/E Mix : Instance, Holding",
    "P/E Mix : Instance, Holding, Item"
  }, delimiter = ':')
  void testIsHoldingUpdateRequiredForEresourceWhenInventoryPolicyRequiresHoldingsShouldReturnTrue(String orderFormat, String inventoryLevel) {
    // TestMate-b8be701b5fbf9194580df291e49713dd
    //given
    PoLine poLine = new PoLine()
      .withOrderFormat(PoLine.OrderFormat.fromValue(orderFormat.trim()))
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(inventoryLevel.trim())));
    //When
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForEresource(poLine);
    //Then
    assertTrue(result);
  }

    @ParameterizedTest
  @CsvSource(value = {
    "Electronic Resource : None",
    "Electronic Resource : Instance",
    "P/E Mix : None",
    "P/E Mix : Instance"
  }, delimiter = ':')
  void testIsHoldingUpdateRequiredForEresourceWhenInventoryPolicyDoesNotRequireHoldingsShouldReturnFalse(String orderFormat, String inventoryLevel) {
    // TestMate-3fb75e8194ed5edbace65b1d43f829c4
    // Given
    PoLine poLine = new PoLine()
      .withOrderFormat(PoLine.OrderFormat.fromValue(orderFormat.trim()))
      .withEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(inventoryLevel.trim())));
    // When
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForEresource(poLine);
    // Then
    assertFalse(result);
  }

    @Test
  void testVerifyOngoingFieldsChangedShouldDoNothingWhenOngoingDataIsMissing() {
    // TestMate-ae1b90fb694adb2b8be65a044ea862e0
    // Given
    JsonObject ongoingJson = new JsonObject().put("isSubscription", true).put("manualRenewal", true);
    Ongoing ongoingObject = new Ongoing().withIsSubscription(true).withManualRenewal(true);
    JsonObject storageWithOngoing = new JsonObject().put("ongoing", ongoingJson);
    JsonObject storageWithoutOngoing = new JsonObject();
    CompositePurchaseOrder requestWithOngoing = new CompositePurchaseOrder().withOngoing(ongoingObject);
    CompositePurchaseOrder requestWithoutOngoing = new CompositePurchaseOrder().withOngoing(null);
    // When & Then
    // Scenario 1: Stored JSON lacks the "ongoing" key, but the Request object has an Ongoing object.
    assertDoesNotThrow(() -> PoLineCommonUtil.verifyOngoingFieldsChanged(storageWithoutOngoing, requestWithOngoing));
    // Scenario 2: Stored JSON has the "ongoing" key, but the Request object's Ongoing field is null.
    assertDoesNotThrow(() -> PoLineCommonUtil.verifyOngoingFieldsChanged(storageWithOngoing, requestWithoutOngoing));
    // Scenario 3: Both the Stored JSON and the Request object lack "ongoing" information.
    assertDoesNotThrow(() -> PoLineCommonUtil.verifyOngoingFieldsChanged(storageWithoutOngoing, requestWithoutOngoing));
  }

    @ParameterizedTest
  @CsvSource(value = {
    "2023-01-01:false:2024-01-01:false:true:reviewDate",
    "2023-01-01:false:2023-01-01:true:true:manualRenewal",
    "2023-01-01:false:2023-01-01:false:false:",
    "2023-01-01:true:2023-01-01:true:false:"
  }, delimiter = ':')
  void testVerifyOngoingFieldsChangedWhenSubscriptionShouldValidateProtectedFields(String storageReviewDate, boolean storageManualRenewal,
                                                                                   String requestReviewDate, boolean requestManualRenewal,
                                                                                   boolean expectError, String expectedErrorField) {
    // TestMate-20b4038052c6c2603559ddc790258cbb
    // Given
    // To ensure type compatibility during comparison (String vs String or Long vs Long),
    // we use the same serialization mechanism for both storage and request objects.
    Date storageDate = Date.from(LocalDate.parse(storageReviewDate).atStartOfDay(ZoneId.systemDefault()).toInstant());
    Ongoing ongoingStorage = new Ongoing()
      .withIsSubscription(true)
      .withReviewDate(storageDate)
      .withManualRenewal(storageManualRenewal);
    JsonObject storageOrderJson = new JsonObject().put("ongoing", JsonObject.mapFrom(ongoingStorage));
    Date requestDate = Date.from(LocalDate.parse(requestReviewDate).atStartOfDay(ZoneId.systemDefault()).toInstant());
    Ongoing ongoingRequest = new Ongoing()
      .withIsSubscription(true)
      .withReviewDate(requestDate)
      .withManualRenewal(requestManualRenewal);
    CompositePurchaseOrder requestOrder = new CompositePurchaseOrder().withOngoing(ongoingRequest);
    // When / Then
    if (expectError) {
      HttpException exception = assertThrows(HttpException.class, () ->
        PoLineCommonUtil.verifyOngoingFieldsChanged(storageOrderJson, requestOrder)
      );
      assertEquals(400, exception.getCode());
      assertEquals(WRONG_ONGOING_SUBSCRIPTION_FIELDS_CHANGED.getCode(), exception.getError().getCode());
      // The parameters list contains the fields that triggered the validation error
      assertTrue(exception.getError().getParameters().stream()
        .anyMatch(param -> param.getKey().equals(expectedErrorField)));
    } else {
      assertDoesNotThrow(() -> PoLineCommonUtil.verifyOngoingFieldsChanged(storageOrderJson, requestOrder));
    }
  }

    @ParameterizedTest
  @CsvSource(value = {
    "12:2024-01-01:30:false:6:2024-01-01:30:false:true:interval",
    "12:2024-01-01:30:false:12:2024-02-01:30:false:true:renewalDate",
    "12:2024-01-01:30:false:12:2024-01-01:15:false:true:reviewPeriod",
    "12:2024-01-01:30:false:12:2024-01-01:30:true:true:manualRenewal",
    "12:2024-01-01:30:false:12:2024-01-01:30:false:false:"
  }, delimiter = ':')
  void testVerifyOngoingFieldsChangedWhenNotSubscriptionShouldValidateProtectedFields(
      int storageInterval, String storageRenewalDate, int storageReviewPeriod, boolean storageManualRenewal,
      int requestInterval, String requestRenewalDate, int requestReviewPeriod, boolean requestManualRenewal,
      boolean expectError, String expectedErrorField) {
    // TestMate-ba0a8a0aff3998c8914e0ea750b9c964
    // Given
    Date sRenewalDate = Date.from(LocalDate.parse(storageRenewalDate).atStartOfDay(ZoneId.systemDefault()).toInstant());
    Ongoing ongoingStorage = new Ongoing()
      .withIsSubscription(false)
      .withInterval(storageInterval)
      .withRenewalDate(sRenewalDate)
      .withReviewPeriod(storageReviewPeriod)
      .withManualRenewal(storageManualRenewal);
    JsonObject storageOrderJson = new JsonObject().put("ongoing", JsonObject.mapFrom(ongoingStorage));
    Date rRenewalDate = Date.from(LocalDate.parse(requestRenewalDate).atStartOfDay(ZoneId.systemDefault()).toInstant());
    Ongoing ongoingRequest = new Ongoing()
      .withIsSubscription(false)
      .withInterval(requestInterval)
      .withRenewalDate(rRenewalDate)
      .withReviewPeriod(requestReviewPeriod)
      .withManualRenewal(requestManualRenewal);
    CompositePurchaseOrder requestOrder = new CompositePurchaseOrder().withOngoing(ongoingRequest);
    // When / Then
    if (expectError) {
      HttpException exception = assertThrows(HttpException.class, () ->
        PoLineCommonUtil.verifyOngoingFieldsChanged(storageOrderJson, requestOrder)
      );
      assertEquals(400, exception.getCode());
      assertEquals(WRONG_ONGOING_NOT_SUBSCRIPTION_FIELDS_CHANGED.getCode(), exception.getError().getCode());
      assertTrue(exception.getError().getParameters().stream()
        .anyMatch(param -> param.getKey().equals(expectedErrorField)));
    } else {
      assertDoesNotThrow(() -> PoLineCommonUtil.verifyOngoingFieldsChanged(storageOrderJson, requestOrder));
    }
  }

    @Test
  void testVerifyOngoingFieldsChangedShouldHandleMultipleFieldViolations() {
    // TestMate-8d96dab5849b15bfd8b419096acb424d
    //given
    Date storageDate = Date.from(LocalDate.of(2023, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
    Ongoing ongoingStorage = new Ongoing()
      .withIsSubscription(true)
      .withReviewDate(storageDate)
      .withManualRenewal(false);
    JsonObject storageOrderJson = new JsonObject().put("ongoing", JsonObject.mapFrom(ongoingStorage));
    Date requestDate = Date.from(LocalDate.of(2024, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
    Ongoing ongoingRequest = new Ongoing()
      .withIsSubscription(true)
      .withReviewDate(requestDate)
      .withManualRenewal(true);
    CompositePurchaseOrder requestOrder = new CompositePurchaseOrder().withOngoing(ongoingRequest);
    //When
    HttpException exception = assertThrows(HttpException.class, () ->
      PoLineCommonUtil.verifyOngoingFieldsChanged(storageOrderJson, requestOrder)
    );
    //Then
    assertEquals(400, exception.getCode());
    assertEquals(WRONG_ONGOING_SUBSCRIPTION_FIELDS_CHANGED.getCode(), exception.getError().getCode());
    assertEquals(2, exception.getError().getParameters().size());
    boolean hasReviewDate = exception.getError().getParameters().stream()
      .anyMatch(param -> param.getKey().equals("reviewDate"));
    boolean hasManualRenewal = exception.getError().getParameters().stream()
      .anyMatch(param -> param.getKey().equals("manualRenewal"));
    assertTrue(hasReviewDate);
    assertTrue(hasManualRenewal);
  }

  private static Location createLocation(String tenantId) {
    return new Location().withTenantId(tenantId);
  }

}
