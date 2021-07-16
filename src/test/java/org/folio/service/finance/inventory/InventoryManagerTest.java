package org.folio.service.finance.inventory;

import org.folio.ApiTestSuite;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.finance.transaction.OpenToPendingEncumbranceStrategy;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.flows.unopen.UnOpenCompositeOrderManagerTest;
import org.folio.service.pieces.PieceRetrieveService;
import org.hamcrest.core.IsInstanceOf;
import org.junit.Assert;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
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
import static org.folio.TestUtils.getMockData;
import static org.folio.helper.InventoryManagerTest.OLD_LOCATION_ID;
import static org.folio.orders.utils.ErrorCodes.PARTIALLY_RETURNED_COLLECTION;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.HOLDINGS_OLD_NEW_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.folio.service.inventory.InventoryManager.HOLDING_PERMANENT_LOCATION_ID;
import static org.folio.service.inventory.InventoryManager.ID;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class InventoryManagerTest {
  private static final String ORDER_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af20";
  public static final String ORDER_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/" + ORDER_ID + ".json";

  @Autowired
  InventoryManager inventoryManager;
  @Autowired
  private RestClient restClient;
  @Autowired
  private PieceRetrieveService pieceRetrieveService;
  @Autowired
  private ConfigurationEntriesService configurationEntriesService;


  private Map<String, String> okapiHeadersMock;
  private Context ctxMock;
  private RequestContext requestContext;

  private HttpClientInterface httpClient;
  private static boolean runningOnOwn;

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(InventoryManagerTest.ContextConfiguration.class);
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
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    httpClient = HttpClientFactory.getHttpClient(okapiURL, X_OKAPI_TENANT.getValue());
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  /**
   * Define unit test specific beans to override actual ones
   */
  static class ContextConfiguration {

    @Bean
    public RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean
    public PieceRetrieveService PieceRetrieveService() {
      return mock(PieceRetrieveService.class);
    }

    @Bean
    public ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }

    @Bean
    public InventoryManager inventoryManager(RestClient restClient, ConfigurationEntriesService configurationEntriesService,
        PieceRetrieveService pieceRetrieveService)  {
      return  new InventoryManager(restClient, configurationEntriesService, pieceRetrieveService);
    }
  }

  @Test
  void shouldReturnHoldingsByIdsIfTheyExist() throws IOException, ExecutionException, InterruptedException {
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
       .map(o -> ((JsonObject) o))
       .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());

    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    List<JsonObject> actHoldings = inventoryManager.getHoldingsByIds(holdingIds, requestContext).get();
    assertThat(actHoldings.size(), equalTo(holdings.size()));
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldTrowExceptionHoldingsByIdsIfNotAllOfThemExist() throws IOException, ExecutionException, InterruptedException {
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> holdingIds = holdings.stream().map(holding ->  holding.getString(ID)).collect(toList());
    holdingIds.add(UUID.randomUUID().toString());
    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    CompletableFuture<List<JsonObject>> resultFuture = inventoryManager.getHoldingsByIds(holdingIds, requestContext);

    ExecutionException executionException = assertThrows(ExecutionException.class, resultFuture::get);

    assertThat(executionException.getCause(), IsInstanceOf.instanceOf(HttpException.class));

    HttpException httpException = (HttpException) executionException.getCause();
    assertEquals(404, httpException.getCode());
    assertEquals(PARTIALLY_RETURNED_COLLECTION.toError().getCode(), httpException.getError().getCode());
  }

  @Test
  void shouldReturnHoldingsByInstanceIdAndLocationIdsIfTheyExist() throws IOException, ExecutionException, InterruptedException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());

    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    List<JsonObject> actHoldings = inventoryManager.getHoldingRecords(instanceId, locationIds, requestContext).get();
    assertThat(actHoldings.size(), equalTo(holdings.size()));
    verify(restClient, times(2)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }

  @Test
  void shouldReturnHoldingsByInstanceIdAndLocationIdIfTheyExist() throws IOException, ExecutionException, InterruptedException {
    String instanceId = UUID.randomUUID().toString();
    JsonObject holdingsCollection = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
    List<JsonObject> holdings = holdingsCollection.getJsonArray("holdingsRecords").stream()
      .map(o -> ((JsonObject) o))
      .collect(toList());

    List<String> locationIds = holdings.stream().map(holding ->  holding.getString(HOLDING_PERMANENT_LOCATION_ID)).collect(toList());

    doReturn(completedFuture(holdingsCollection)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    JsonObject actHoldings = inventoryManager.getFirstHoldingRecord(instanceId, locationIds.get(0), requestContext).get();
    verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
  }
}
