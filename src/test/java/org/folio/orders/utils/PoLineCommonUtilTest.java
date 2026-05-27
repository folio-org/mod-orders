package org.folio.orders.utils;

import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.*;

import io.vertx.core.json.JsonObject;
import org.folio.CopilotGenerated;
import org.folio.TestMate;
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
import static org.folio.rest.core.exceptions.ErrorCodes.PROHIBITED_FIELD_CHANGING;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import java.util.Collection;
import java.util.Map;
import org.folio.rest.jaxrs.model.Error;
import static org.hamcrest.Matchers.contains;
import io.vertx.core.json.JsonArray;
import org.folio.rest.core.exceptions.ErrorCodes;
import java.util.Collections;
import org.folio.rest.jaxrs.model.Ongoing;
import java.util.Date;
import java.time.Instant;
import org.folio.rest.jaxrs.model.Parameter;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import org.junit.jupiter.params.provider.ValueSource;

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

  @Test
  @TestMate(name = "TestMate-fcee0817e0fb477dda0345cf59133ea1")
  void testVerifyProtectedFieldsChangedShouldDetectScalarFieldChange() {
    // Given
    List<String> protectedFields = List.of("poLineNumber", "isPackage");
    JsonObject objectFromStorage = new JsonObject()
      .put("poLineNumber", "1000-1")
      .put("isPackage", true);
    JsonObject requestObject = new JsonObject()
      .put("poLineNumber", "1000-2")
      .put("isPackage", false);
    // When
    HttpException exception = assertThrows(HttpException.class, () ->
      PoLineCommonUtil.verifyProtectedFieldsChanged(protectedFields, objectFromStorage, requestObject)
    );
    // Then
    assertEquals(400, exception.getCode());
    Error error = exception.getError();
    assertEquals(PROHIBITED_FIELD_CHANGING.getCode(), error.getCode());
    Map<String, Object> additionalProperties = error.getAdditionalProperties();
    Object modifiedFields = additionalProperties.get("protectedAndModifiedFields");

    assertThat((Collection<String>) modifiedFields, containsInAnyOrder("poLineNumber", "isPackage"));
  }

  @Test
  @TestMate(name = "TestMate-7c59c6e9741685700ad64171c7f676d6")
  void testVerifyProtectedFieldsChangedShouldDetectArraySizeDecrease() {
    // Given
    String protectedField = "details.productIds";
    List<String> protectedFields = List.of(protectedField);
    String productIdType = UUID.randomUUID().toString();

    ProductId firstProductId = new ProductId()
      .withProductId("9780735245341")
      .withProductIdType(productIdType);
    ProductId secondProductId = new ProductId()
      .withProductId("9780593492543")
      .withProductIdType(productIdType);
    String poLineId = UUID.randomUUID().toString();
    PoLine lineFromStorage = new PoLine()
      .withId(poLineId)
      .withDetails(new Details().withProductIds(List.of(firstProductId, secondProductId)));

    PoLine requestObject = new PoLine()
      .withId(poLineId)
      .withDetails(new Details().withProductIds(List.of(firstProductId)));
    JsonObject lineFromStorageJson = JsonObject.mapFrom(lineFromStorage);
    JsonObject requestObjectJson = JsonObject.mapFrom(requestObject);
    // When
    HttpException exception = assertThrows(HttpException.class, () ->
      PoLineCommonUtil.verifyProtectedFieldsChanged(protectedFields, lineFromStorageJson, requestObjectJson)
    );
    // Then
    assertEquals(400, exception.getCode());
    Error error = exception.getError();
    assertEquals(PROHIBITED_FIELD_CHANGING.getCode(), error.getCode());

    Map<String, Object> additionalProperties = error.getAdditionalProperties();
    Object modifiedFields = additionalProperties.get("protectedAndModifiedFields");
    assertThat((Collection<String>) modifiedFields, contains(protectedField));
  }

  @Test
  @TestMate(name = "TestMate-557f9017f8ed07ba36247fa5b6b84c6b")
  void testVerifyProtectedFieldsChangedShouldDetectArrayContentMismatchSameSize() {
    // Given
    String protectedField = "tags.tagList";
    List<String> protectedFields = List.of(protectedField);
    JsonObject objectFromStorage = new JsonObject()
      .put("tags", new JsonObject()
        .put("tagList", new JsonArray().add("urgent")));
    JsonObject requestObject = new JsonObject()
      .put("tags", new JsonObject()
        .put("tagList", new JsonArray().add("normal")));
    // When
    HttpException exception = assertThrows(HttpException.class, () ->
      PoLineCommonUtil.verifyProtectedFieldsChanged(protectedFields, objectFromStorage, requestObject)
    );
    // Then
    assertEquals(400, exception.getCode());
    Error error = exception.getError();
    assertEquals(PROHIBITED_FIELD_CHANGING.getCode(), error.getCode());
    Map<String, Object> additionalProperties = error.getAdditionalProperties();
    Object modifiedFields = additionalProperties.get("protectedAndModifiedFields");
    assertThat((Collection<String>) modifiedFields, contains(protectedField));
  }

  @Test
  @TestMate(name = "TestMate-2cf9247ee4f691a5aa390dbf1d481c14")
  void testVerifyProtectedFieldsChangedShouldIgnoreUnprotectedFieldChanges() {
    // Given
    List<String> protectedFields = List.of("id");
    String id = "11111111-1111-1111-1111-111111111111";
    JsonObject objectFromStorage = new JsonObject()
      .put("id", id)
      .put("description", "Old Description");
    JsonObject requestObject = new JsonObject()
      .put("id", id)
      .put("description", "New Description");
    // When
    JsonObject result = PoLineCommonUtil.verifyProtectedFieldsChanged(protectedFields, objectFromStorage, requestObject);
    // Then
    assertEquals(objectFromStorage, result);
  }

  @Test
  @TestMate(name = "TestMate-defcd112d0e57c836878823cedcaef6c")
  void testVerifyProtectedFieldsChangedShouldHandleNestedFieldPaths() {
    // Given
    String protectedFieldPath = "cost.currency";
    List<String> protectedFields = List.of(protectedFieldPath);
    JsonObject objectFromStorage = new JsonObject()
      .put("cost", new JsonObject()
        .put("currency", "USD")
        .put("listUnitPrice", 10.0));
    JsonObject requestObject = new JsonObject()
      .put("cost", new JsonObject()
        .put("currency", "EUR")
        .put("listUnitPrice", 10.0));
    // When
    HttpException exception = assertThrows(HttpException.class, () ->
      PoLineCommonUtil.verifyProtectedFieldsChanged(protectedFields, objectFromStorage, requestObject)
    );
    // Then
    assertEquals(400, exception.getCode());
    Error error = exception.getError();
    assertEquals(PROHIBITED_FIELD_CHANGING.getCode(), error.getCode());
    Map<String, Object> additionalProperties = error.getAdditionalProperties();
    Object modifiedFields = additionalProperties.get("protectedAndModifiedFields");
    assertThat((Collection<String>) modifiedFields, contains(protectedFieldPath));
  }

  @Test
  @TestMate(name = "TestMate-69d03db6b9de005e4433ca20ca1445ee")
  void testVerifyProtectedFieldsChangedShouldHandleEmptyProtectedFieldsList() {
    // Given
    List<String> protectedFields = Collections.emptyList();
    String id = "00000000-0000-0000-0000-000000000001";

    JsonObject objectFromStorage = new JsonObject()
      .put("id", id)
      .put("name", "Original Name")
      .put("status", "Pending");

    JsonObject requestObject = new JsonObject()
      .put("id", UUID.fromString("00000000-0000-0000-0000-000000000002").toString())
      .put("name", "Changed Name")
      .put("status", "Active");
    // When
    JsonObject result = PoLineCommonUtil.verifyProtectedFieldsChanged(protectedFields, objectFromStorage, requestObject);
    // Then
    assertEquals(objectFromStorage, result);
  }

  @Test
  @TestMate(name = "TestMate-a9add8ed0d1f7a0fe476f740af1ee27e")
  void testVerifyProtectedFieldsChangedShouldDetectChangesInMultipleProtectedFields() {
    // Given
    List<String> protectedFields = List.of("id", "source");
    JsonObject objectFromStorage = new JsonObject()
      .put("id", "1")
      .put("source", "FOLIO")
      .put("description", "Old Description");
    JsonObject requestObject = new JsonObject()
      .put("id", "2")
      .put("source", "MARC")
      .put("description", "New Description");
    // When
    HttpException exception = assertThrows(HttpException.class, () ->
      PoLineCommonUtil.verifyProtectedFieldsChanged(protectedFields, objectFromStorage, requestObject)
    );
    // Then
    assertEquals(400, exception.getCode());
    Error error = exception.getError();
    assertEquals(PROHIBITED_FIELD_CHANGING.getCode(), error.getCode());
    Map<String, Object> additionalProperties = error.getAdditionalProperties();
    Object modifiedFields = additionalProperties.get("protectedAndModifiedFields");
    assertThat((Collection<String>) modifiedFields, containsInAnyOrder("id", "source"));
  }

  @ParameterizedTest
  @TestMate(name = "TestMate-1e7f8d73471208534f55a8712f35d569")
  @CsvSource(value = {
    "reviewDate|2024-01-01T00:00:00Z",
    "manualRenewal|true"
  }, delimiter = '|')
  void testVerifyOngoingFieldsChangedShouldThrowExceptionWhenSubscriptionFieldsModified(String fieldName, String newValue) {
    // Given
    JsonObject ongoingStorage = new JsonObject()
      .put("isSubscription", true)
      .put("reviewDate", "2023-01-01T00:00:00Z")
      .put("manualRenewal", false);
    JsonObject storageJson = new JsonObject().put("ongoing", ongoingStorage);
    // Map from storage to request to ensure fields not under test remain identical during comparison
    Ongoing ongoingRequest = ongoingStorage.mapTo(Ongoing.class);
    if ("reviewDate".equals(fieldName)) {
      ongoingRequest.setReviewDate(Date.from(Instant.parse(newValue)));
    } else if ("manualRenewal".equals(fieldName)) {
      ongoingRequest.setManualRenewal(Boolean.valueOf(newValue));
    }
    CompositePurchaseOrder requestCompPO = new CompositePurchaseOrder().withOngoing(ongoingRequest);
    // When
    HttpException exception = assertThrows(HttpException.class, () ->
      PoLineCommonUtil.verifyOngoingFieldsChanged(storageJson, requestCompPO)
    );
    // Then
    assertEquals(400, exception.getCode());
    assertEquals(ErrorCodes.WRONG_ONGOING_SUBSCRIPTION_FIELDS_CHANGED.getCode(), exception.getError().getCode());
    // Find the specific parameter related to the field being tested to avoid index-based failures
    Parameter parameter = exception.getErrors().getErrors().get(0).getParameters().stream()
      .filter(p -> p.getKey().equals(fieldName))
      .findFirst()
      .orElseThrow(() -> new AssertionError("Expected parameter not found: " + fieldName));
    String expectedValue = newValue;
    if ("reviewDate".equals(fieldName)) {
      // Vert.x JsonObject serializes java.util.Date to epoch milliseconds by default.
      // The method under test uses newObject.getValueAt(field).toString(), which results in the string of that long value.
      expectedValue = String.valueOf(Instant.parse(newValue).toEpochMilli());
    }
    assertEquals(fieldName, parameter.getKey());
    assertEquals(expectedValue, parameter.getValue());
  }

  @ParameterizedTest
  @TestMate(name = "TestMate-6800fce4f0684e383b323eea15366367")
  @CsvSource(value = {
    "interval|10",
    "renewalDate|2024-01-15T10:30:00Z",
    "reviewPeriod|5",
    "manualRenewal|true"
  }, delimiter = '|')
  void testVerifyOngoingFieldsChangedShouldThrowExceptionWhenNonSubscriptionFieldsModified(String fieldName, String newValue) {
    //given
    JsonObject ongoingStorage = new JsonObject()
      .put("isSubscription", false)
      .put("interval", 1)
      .put("renewalDate", "2023-01-15T10:30:00Z")
      .put("reviewPeriod", 1)
      .put("manualRenewal", false);
    JsonObject storageJson = new JsonObject().put("ongoing", ongoingStorage);
    Ongoing ongoingRequest = ongoingStorage.mapTo(Ongoing.class);
    if ("interval".equals(fieldName)) {
      ongoingRequest.setInterval(Integer.valueOf(newValue));
    } else if ("renewalDate".equals(fieldName)) {
      ongoingRequest.setRenewalDate(Date.from(Instant.parse(newValue)));
    } else if ("reviewPeriod".equals(fieldName)) {
      ongoingRequest.setReviewPeriod(Integer.valueOf(newValue));
    } else if ("manualRenewal".equals(fieldName)) {
      ongoingRequest.setManualRenewal(Boolean.valueOf(newValue));
    }
    CompositePurchaseOrder requestCompPO = new CompositePurchaseOrder().withOngoing(ongoingRequest);
    //when
    HttpException exception = assertThrows(HttpException.class, () ->
      PoLineCommonUtil.verifyOngoingFieldsChanged(storageJson, requestCompPO)
    );
    //then
    assertEquals(400, exception.getCode());
    assertEquals(ErrorCodes.WRONG_ONGOING_NOT_SUBSCRIPTION_FIELDS_CHANGED.getCode(), exception.getError().getCode());
    Parameter parameter = exception.getErrors().getErrors().get(0).getParameters().stream()
      .filter(p -> p.getKey().equals(fieldName))
      .findFirst()
      .orElseThrow(() -> new AssertionError("Expected parameter not found: " + fieldName));
    String expectedValue = newValue;
    if ("renewalDate".equals(fieldName)) {
      expectedValue = String.valueOf(Instant.parse(newValue).toEpochMilli());
    }
    assertEquals(fieldName, parameter.getKey());
    assertEquals(expectedValue, parameter.getValue());
  }

  @ParameterizedTest
  @TestMate(name = "TestMate-495401ee096d3b81e566755f6a634940")
  @ValueSource(booleans = {true, false})
  void testVerifyOngoingFieldsChangedShouldSucceedWhenAllowedFieldsModified(boolean isSubscription) {
    // Given
    JsonObject ongoingStorage = new JsonObject()
      .put("isSubscription", isSubscription)
      .put("notes", "Original Note")
      .put("manualRenewal", false);
    JsonObject storageJson = new JsonObject().put("ongoing", ongoingStorage);
    Ongoing ongoingRequest = new Ongoing()
      .withIsSubscription(isSubscription)
      .withNotes("Updated Note")
      .withManualRenewal(false);
    CompositePurchaseOrder requestCompPO = new CompositePurchaseOrder().withOngoing(ongoingRequest);
    // When & Then
    assertDoesNotThrow(() -> PoLineCommonUtil.verifyOngoingFieldsChanged(storageJson, requestCompPO));
  }

  private static Location createLocation(String tenantId) {
    return new Location().withTenantId(tenantId);
  }

}
