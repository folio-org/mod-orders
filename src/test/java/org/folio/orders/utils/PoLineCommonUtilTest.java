package org.folio.orders.utils;

import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.*;

import io.vertx.core.json.JsonObject;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.*;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

public class PoLineCommonUtilTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  private static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Test
  void testOnlyInstanceUpdateNeededForPhysicalIfCreateInventoryInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.FULLY_RECEIVED);
      line.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE);
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getCompositePoLines().get(0));
    //Then
    assertTrue(actCheck);
  }

  @Test
  void testOnlyInstanceUpdateNeededForElectronicalIfCreateInventoryInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.FULLY_RECEIVED);
      line.setPhysical(null);
      line.setEresource(new Eresource().withCreateInventory(Eresource.CreateInventory.INSTANCE));
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getCompositePoLines().get(0));
    //Then
    assertTrue(actCheck);
  }

  @Test
  void testOnlyInstanceUpdateNeededIfCreateInventoryIsNotInstance() {
    //given
    CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);
    order.getCompositePoLines().forEach(line -> {
      line.setPaymentStatus(CompositePoLine.PaymentStatus.FULLY_PAID);
      line.setReceiptStatus(CompositePoLine.ReceiptStatus.FULLY_RECEIVED);
      line.getPhysical().setCreateInventory(Physical.CreateInventory.INSTANCE_HOLDING);
    });
    //When
    boolean actCheck = PoLineCommonUtil.isOnlyInstanceUpdateRequired(order.getCompositePoLines().get(0));
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
    CompositePoLine requestObject = new CompositePoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(new Details().withProductIds(List.of(secondProductId, firstProductId)));

    JsonObject lineFromStorageJson = JsonObject.mapFrom(lineFromStorage);

    JsonObject result = PoLineCommonUtil
      .verifyProtectedFieldsChanged(protectedFields, lineFromStorageJson, JsonObject.mapFrom(requestObject));

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
    CompositePoLine requestObject = new CompositePoLine()
      .withId(UUID.randomUUID().toString())
      .withDetails(new Details().withProductIds(List.of(secondProductId, firstProductId, thirdProductId)));

    JsonObject lineFromStorageJson = JsonObject.mapFrom(lineFromStorage);

    HttpException exception = assertThrows(HttpException.class, () -> PoLineCommonUtil
      .verifyProtectedFieldsChanged(protectedFields, lineFromStorageJson, JsonObject.mapFrom(requestObject)));


    String errorMessage = "{\"message\":\"Protected fields can't be modified\",\"code\":\"protectedFieldChanging\",\"parameters\":[],\"protectedAndModifiedFields\":[\"details.productIds\"]}";
    assertEquals(400, exception.getCode());
    assertEquals(errorMessage, exception.getMessage());
  }
}
