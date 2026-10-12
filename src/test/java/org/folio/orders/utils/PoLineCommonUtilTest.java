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
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

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
    "Physical Resource:Instance, Holding:true",
    "Physical Resource:Instance, Holding, Item:true",
    "P/E Mix:Instance, Holding:true",
    "Other:Instance, Holding, Item:true",
    "Physical Resource:Instance:false",
    "Physical Resource:None:false",
    "Electronic Resource:Instance, Holding:false"
  }, delimiter = ':')
  void testIsHoldingUpdateRequiredForPhysicalShouldReturnTrueOnlyForPhysicalFormatsWithHoldingInventory(
      String orderFormat, String createInventory, boolean expectedResult) {
    // TestMate-6290640ccd4b99fc5a9ce69b4a4d7cda
    //given
    PoLine poLine = new PoLine().withOrderFormat(PoLine.OrderFormat.fromValue(orderFormat));
    if (poLine.getOrderFormat() != PoLine.OrderFormat.ELECTRONIC_RESOURCE) {
      Physical physical = new Physical().withCreateInventory(Physical.CreateInventory.fromValue(createInventory));
      poLine.setPhysical(physical);
    } else {
      // For Electronic Resource, even if Eresource has holding policy, the method for Physical should return false
      Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(createInventory));
      poLine.setEresource(eresource);
    }
    //when
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine);
    //then
    assertEquals(expectedResult, result);
  }

    @Test
  void testIsHoldingUpdateRequiredForPhysicalWhenPhysicalIsNullShouldReturnFalse() {
    // TestMate-fbbe9654509d56e51a19f9b703eefeb0
    //given
    PoLine poLine = new PoLine().withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE);
    //When
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForPhysical(poLine);
    //Then
    assertFalse(result);
  }

    @ParameterizedTest
  @CsvSource(value = {
    "Electronic Resource:Instance, Holding:true",
    "Electronic Resource:Instance, Holding, Item:true",
    "Electronic Resource:Instance:false",
    "Electronic Resource:None:false",
    "P/E Mix:Instance, Holding:true",
    "P/E Mix:Instance, Holding, Item:true",
    "P/E Mix:Instance:false",
    "P/E Mix:None:false",
    "Physical Resource:Instance, Holding:false",
    "Other:Instance, Holding:false"
  }, delimiter = ':')
  void testIsHoldingUpdateRequiredForEresourceShouldReturnExpectedResultForVariousFormatsAndInventorySettings(
      String orderFormat, String createInventory, boolean expectedResult) {
    // TestMate-c80a703b6f3859353c7dda7201c02998
    //given
    PoLine poLine = new PoLine().withOrderFormat(PoLine.OrderFormat.fromValue(orderFormat));
    Eresource eresource = new Eresource().withCreateInventory(Eresource.CreateInventory.fromValue(createInventory));
    poLine.setEresource(eresource);
    //when
    boolean result = PoLineCommonUtil.isHoldingUpdateRequiredForEresource(poLine);
    //then
    assertEquals(expectedResult, result);
  }

  private static Location createLocation(String tenantId) {
    return new Location().withTenantId(tenantId);
  }

}
