package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.ApiTestSuite.mockPort;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.folio.models.PoLineUpdateHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import io.restassured.http.Header;
import io.vertx.core.Context;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

public class InventoryHelperTest extends ApiTestBase {
  public static final String TENANT_ID = "ordertest";
  public static final Header X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, TENANT_ID);
  public static final String HOLDING_INSTANCE_ID = "5294d737-a04b-4158-857a-3f3c555bcc60";
  public static final String OLD_LOCATION_ID = "758258bc-ecc1-41b8-abca-f7b610822ffd";
  public static final String NEW_LOCATION_ID = "fcd64ce1-6995-48f0-840e-89ffa2288371";
  public static final String OLD_HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d63";
  public static final String NEW_HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d64";
  public static final String NON_EXISTED_NEW_HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d23";

  private Context ctxMock;
  private Map<String, String> okapiHeadersMock;
  private HttpClientInterface httpClient;


  @BeforeEach
  public void initMocks(){
    super.setUp();
    ctxMock = Vertx.vertx().getOrCreateContext();
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    httpClient = HttpClientFactory.getHttpClient(okapiURL, TENANT_ID);
    MockitoAnnotations.openMocks(this);
  }


  @Test
  public void testShouldUpdateAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).updateItem(any());
    //When
    JsonObject item1 = new JsonObject().put("id", UUID.randomUUID().toString());
    JsonObject item2 = new JsonObject().put("id", UUID.randomUUID().toString());
    List<JsonObject> items = Arrays.asList(item1, item2);
    inventoryHelper.updateItemRecords(items).join();
    //Then
    verify(inventoryHelper, times(1)).updateItem(item1);
    verify(inventoryHelper, times(1)).updateItem(item2);
  }

  @Test
  public void testShouldNotUpdateItemIfProvidedListEmpty() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).updateItem(any());
    //When
    inventoryHelper.updateItemRecords(Collections.emptyList()).join();
    //Then
    verify(inventoryHelper, times(0)).updateItem(any());
  }

  @Test
  public void testShouldDeleteAllItemOneByOneIfProvidedListNonEmpty() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    String itemId1 = UUID.randomUUID().toString();
    String itemId2 = UUID.randomUUID().toString();
    List<String> items = Arrays.asList(itemId1, itemId2);
    inventoryHelper.deleteItems(items).join();
    //Then
    verify(inventoryHelper, times(1)).deleteItem(itemId1);
    verify(inventoryHelper, times(1)).deleteItem(itemId2);
  }

  @Test
  public void testShouldNotDeleteItemIfProvidedListEmpty() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    inventoryHelper.deleteItems(Collections.emptyList()).join();
    //Then
    verify(inventoryHelper, times(0)).updateItem(any());
  }

  @Test
  public void testShouldUpdateHoldingsRecordIfOldAndNewLocationProvided() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
                                        .withOldLocationId(OLD_LOCATION_ID)
                                        .withNewLocationId(NEW_LOCATION_ID);
    inventoryHelper.updateHoldingsRecord(holder).join();
    //Then
    assertThat(OLD_HOLDING_ID, equalTo(holder.getOldHoldingId()));
    assertThat(NEW_HOLDING_ID, equalTo(holder.getNewHoldingId()));
  }

  @Test
  public void testShouldCreateNewHoldingsRecordIfOnlyOldLocationProvided() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
                                                        .withOldLocationId(OLD_LOCATION_ID)
                                                        .withNewLocationId(NON_EXISTED_NEW_HOLDING_ID);
    inventoryHelper.updateHoldingsRecord(holder).join();
    //Then
    verify(inventoryHelper, times(1)).getOrCreateHoldingsRecord(HOLDING_INSTANCE_ID, NON_EXISTED_NEW_HOLDING_ID);
    assertThat(OLD_HOLDING_ID, equalTo(holder.getOldHoldingId()));
    assertThat(NON_EXISTED_NEW_HOLDING_ID, equalTo(holder.getNewLocationId()));
  }

  @Test
  public void testShouldThrowExceptionIfHoldingWithOldLocationIsNotExist() {
    //given
    InventoryHelper inventoryHelper = spy(new InventoryHelper(httpClient, okapiHeadersMock, ctxMock, "en"));
    doReturn(completedFuture(null)).when(inventoryHelper).deleteItem(any());
    //When
    PoLineUpdateHolder holder = new PoLineUpdateHolder().withInstanceId(HOLDING_INSTANCE_ID)
      .withOldLocationId(UUID.randomUUID().toString())
      .withNewLocationId(NON_EXISTED_NEW_HOLDING_ID);
    CompletableFuture<Void> result = inventoryHelper.updateHoldingsRecord(holder);
    //Then
    CompletionException expectedException = assertThrows(CompletionException.class, result::join);

    HttpException httpException = (HttpException) expectedException.getCause();
    assertEquals(400, httpException.getCode());
  }
}
