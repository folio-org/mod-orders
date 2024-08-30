package org.folio.service.inventory;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.ApiTestSuite;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.CirculationRequestsRetriever;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.orders.utils.CommonFields.CREATED_DATE;
import static org.folio.orders.utils.CommonFields.ID;
import static org.folio.orders.utils.CommonFields.METADATA;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.folio.service.inventory.util.RequestFields.DESTINATION_ITEM_ID;
import static org.folio.service.inventory.util.RequestFields.ITEM_ID;
import static org.folio.service.inventory.util.RequestFields.REQUESTER_ID;
import static org.folio.service.inventory.util.RequestFields.STATUS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(VertxExtension.class)
public class InventoryItemRequestServiceTest {

  private final JsonObject sampleMetadata = JsonObject.of(CREATED_DATE.getValue(), Instant.now().toString());

  @Autowired
  RestClient restClient;

  @Autowired
  InventoryItemRequestService inventoryItemRequestService;

  @Autowired
  CirculationRequestsRetriever circulationRequestsRetriever;

  private Map<String, String> okapiHeadersMock;
  private Context ctxMock;
  private RequestContext requestContext;
  private static boolean runningOnOwn;
  private AutoCloseable mockitoClosable;

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(InventoryItemRequestServiceTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @BeforeEach
  void beforeEach() {
    mockitoClosable = MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() throws Exception {
    mockitoClosable.close();
    clearServiceInteractions();
  }

  @Test
  void getItemIdsWithNoActiveRequestsTest(VertxTestContext vertxTestContext) {
    List<String> itemIds = generateUUIDs(5);
    Map<String, Long> itemReqMap = itemIds.stream().collect(Collectors.toMap(i -> i, i -> 0L));

    doReturn(Future.succeededFuture(itemReqMap)).when(circulationRequestsRetriever).getNumbersOfRequestsByItemIds(anyList(), eq(requestContext));

    Future<List<String>> future = inventoryItemRequestService.getItemIdsWithActiveRequests(itemIds, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      var res = f.result();
      assertEquals(res.size(), 0);
      vertxTestContext.completeNow();
    });
  }

  @Test
  void getItemIdsWithActiveRequestsTest(VertxTestContext vertxTestContext) {
    List<String> itemIds = generateUUIDs(3);
    String itemWithReq = itemIds.get(1);
    Map<String, Long> itemReqMap = itemIds.stream().collect(Collectors.toMap(i -> i, i -> 0L));
    itemReqMap.put(itemWithReq, 1L);

    doReturn(Future.succeededFuture(itemReqMap)).when(circulationRequestsRetriever).getNumbersOfRequestsByItemIds(anyList(), eq(requestContext));

    Future<List<String>> future = inventoryItemRequestService.getItemIdsWithActiveRequests(itemIds, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      var res = f.result();
      assertEquals(1, res.size());
      assertEquals(itemWithReq, res.get(0));
      vertxTestContext.completeNow();
    });
  }

  @Test
  void transferSingleItemRequest(VertxTestContext vertxTestContext) {
    var itemIds = generateUUIDs(1);
    var reqMap = generateRequests(itemIds);
    String destItemId = UUID.randomUUID().toString();
    JsonObject jsonObject =  JsonObject.of(DESTINATION_ITEM_ID.getValue(), destItemId);

    doReturn(Future.succeededFuture()).when(restClient).postJsonObject(any(RequestEntry.class), eq(jsonObject), eq(requestContext));
    doReturn(Future.succeededFuture(reqMap)).when(circulationRequestsRetriever).getRequesterIdsToRequestsByItemIds(anyList(), eq(requestContext));

    Future<Void> future = inventoryItemRequestService.transferItemRequests(itemIds, destItemId, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      verify(restClient, times(1)).postJsonObject(any(RequestEntry.class), eq(jsonObject), eq(requestContext));
      vertxTestContext.completeNow();
    });
  }

  @Test
  void transferManyItemRequests(VertxTestContext vertxTestContext) {
    var itemIds = generateUUIDs(10);
    var reqMap = generateRequests(itemIds);

    String destItemId = UUID.randomUUID().toString();
    JsonObject jsonObject =  JsonObject.of(DESTINATION_ITEM_ID.getValue(), destItemId);

    doReturn(Future.succeededFuture()).when(restClient).postJsonObject(any(RequestEntry.class), eq(jsonObject), eq(requestContext));
    doReturn(Future.succeededFuture(reqMap)).when(circulationRequestsRetriever).getRequesterIdsToRequestsByItemIds(anyList(), eq(requestContext));

    Future<Void> future = inventoryItemRequestService.transferItemRequests(itemIds, destItemId, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      verify(restClient, times(10)).postJsonObject(any(RequestEntry.class), eq(jsonObject), eq(requestContext));
      vertxTestContext.completeNow();
    });
  }

  @Test
  void transferManyItemRequestsAndCancelOne(VertxTestContext vertxTestContext) {
    var itemIds = generateUUIDs(10);
    var reqMap = generateRequests(itemIds);

    // Add request for a different item with same requester
    var entries = reqMap.entrySet().stream().toList();
    var reqEntry = entries.get(0);
    var newRequest = new JsonObject()
      .put(ID.getValue(), UUID.randomUUID().toString())
      .put(ITEM_ID.getValue(), UUID.randomUUID().toString())
      .put(METADATA.getValue(), JsonObject.of(CREATED_DATE.getValue(), Instant.now().plus(1, ChronoUnit.DAYS).toString()))
      .put(REQUESTER_ID.getValue(), reqEntry.getKey());
    reqEntry.getValue().add(newRequest);

    String destItemId = UUID.randomUUID().toString();
    JsonObject jsonObject =  JsonObject.of(DESTINATION_ITEM_ID.getValue(), destItemId);

    doReturn(Future.succeededFuture()).when(restClient).postJsonObject(any(RequestEntry.class), eq(jsonObject), eq(requestContext));
    doReturn(Future.succeededFuture()).when(restClient).put(any(RequestEntry.class), any(JsonObject.class), eq(requestContext));
    doReturn(Future.succeededFuture(reqMap)).when(circulationRequestsRetriever).getRequesterIdsToRequestsByItemIds(anyList(), eq(requestContext));

    // One requester will now have two requests, old one will be cancelled
    Future<Void> future = inventoryItemRequestService.transferItemRequests(itemIds, destItemId, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      newRequest.put(STATUS.getValue(), "Closed - Cancelled");
      verify(restClient, times(10)).postJsonObject(any(RequestEntry.class), eq(jsonObject), eq(requestContext));
      verify(restClient, times(1)).put(any(RequestEntry.class), eq(newRequest), eq(requestContext));
      vertxTestContext.completeNow();
    });
  }

  private List<String> generateUUIDs(int n) {
    List<String> ids = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      ids.add(UUID.randomUUID().toString());
    }
    return ids;
  }

  private Map<String, List<JsonObject>> generateRequests(List<String> itemIds) {
    Map<String, List<JsonObject>> reqMap = new HashMap<>();
    for (var itemId : itemIds) {
      var requesterId = UUID.randomUUID().toString();
      var requests = new ArrayList<JsonObject>();
      requests.add(new JsonObject()
        .put(ID.getValue(), UUID.randomUUID().toString())
        .put(ITEM_ID.getValue(), itemId)
        .put(METADATA.getValue(), sampleMetadata)
        .put(REQUESTER_ID.getValue(), requesterId));
      reqMap.put(UUID.randomUUID().toString(), requests);
    }
    return reqMap;
  }

  /**
   * Define unit test specific beans to override actual ones
   */
  static class ContextConfiguration {

    @Bean
    public RestClient restClient() {
      return spy(new RestClient());
    }

    @Bean
    public CirculationRequestsRetriever circulationRequestsRetriever(RestClient restClient) {
      return mock(CirculationRequestsRetriever.class);
    }

    @Bean
    public InventoryItemRequestService inventoryItemRequestService(RestClient restClient,
                                                                   CirculationRequestsRetriever circulationRequestsRetriever) {
      return new InventoryItemRequestService(restClient, circulationRequestsRetriever);
    }

  }

}
