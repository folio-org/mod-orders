package org.folio.helper;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.ApiTestSuite.mockPort;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
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
    inventoryHelper.updateItemRecords(items);
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
    inventoryHelper.updateItemRecords(Collections.emptyList());
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
    inventoryHelper.deleteItems(items);
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
    inventoryHelper.deleteItems(Collections.emptyList());
    //Then
    verify(inventoryHelper, times(0)).updateItem(any());
  }
}
