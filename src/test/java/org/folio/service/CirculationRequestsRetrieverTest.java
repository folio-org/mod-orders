package org.folio.service;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.ApiTestSuite;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.settings.SettingsRetriever;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

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
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.orders.utils.CommonFields.COLLECTION_TOTAL;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(VertxExtension.class)
public class CirculationRequestsRetrieverTest {

  private static final String REQUESTS_PATH = BASE_MOCK_DATA_PATH + "requests/";
  private static final String REQUESTS_MOCK = "requests";
  private static final List<String> MOCK_ITEM_IDS = List.of(
    "8d0350f6-3ca0-4876-9e44-0f6a6bd79e47",
    "e477dd71-3144-48ee-9972-d93f726ce0d8",
    "24645dbd-2dd9-429c-ac24-44cae19adae4"
  );

  private static final String PIECES_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String PIECES_MOCK = "pieces-for-requests";
  private static final List<String> MOCK_PIECE_IDS = List.of(
    "05a95f03-eb00-4248-9f2e-2bd05957ff04",
    "05a95f03-eb00-4248-9f2e-2bd05957ff05",
    "05a95f03-eb00-4248-9f2e-2bd05957ff06"
  );

  @Autowired
  RestClient restClient;

  @Autowired
  PieceStorageService pieceStorageService;

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
    initSpringContext(CirculationRequestsRetrieverTest.ContextConfiguration.class);
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
  void getNumberOfRequestsByItemIdTest(VertxTestContext vertxTestContext) {
    String itemId = UUID.randomUUID().toString();
    JsonObject jsonObject = JsonObject.of(COLLECTION_TOTAL.getValue(), 5);

    doReturn(Future.succeededFuture(jsonObject)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    Future<Integer> future = circulationRequestsRetriever.getNumberOfRequestsByItemId(itemId, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      assertEquals(5, f.result());
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      vertxTestContext.completeNow();
    });
  }

  @Test
  void getNumbersOfRequestsByItemIdsTest(VertxTestContext vertxTestContext) {
    JsonObject mockData = getMockAsJson(REQUESTS_PATH, REQUESTS_MOCK);

    doReturn(Future.succeededFuture(mockData)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    Future<Map<String, Long>> future = circulationRequestsRetriever.getNumbersOfRequestsByItemIds(MOCK_ITEM_IDS, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      var res = f.result();
      assertEquals(2, res.size());
      assertEquals(5, res.get(MOCK_ITEM_IDS.get(0)));
      assertEquals(2, res.get(MOCK_ITEM_IDS.get(1)));
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      vertxTestContext.completeNow();
    });
  }

  @Test
  void getRequesterIdsToRequestsByItemIdsTest(VertxTestContext vertxTestContext) {
    JsonObject mockData = getMockAsJson(REQUESTS_PATH, REQUESTS_MOCK);

    doReturn(Future.succeededFuture(mockData)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));

    Future<Map<String, List<JsonObject>>> future = circulationRequestsRetriever.getRequesterIdsToRequestsByItemIds(MOCK_ITEM_IDS, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      var reqMap = f.result();
      assertEquals(5, reqMap.size());
      assertEquals(7, reqMap.values().stream().mapToInt(Collection::size).sum());
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      vertxTestContext.completeNow();
    });
  }

  @Test
  void getRequesterIdsToRequestsByPieceIdsTest(VertxTestContext vertxTestContext) {
    var requestsMockData = getMockAsJson(REQUESTS_PATH, REQUESTS_MOCK);
    var piecesMockData = getMockAsJson(PIECES_PATH, PIECES_MOCK).mapTo(PieceCollection.class);
    var status = "Open - In transit";

    doReturn(Future.succeededFuture(requestsMockData)).when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(Future.succeededFuture(piecesMockData.getPieces())).when(pieceStorageService).getPiecesByIds(eq(MOCK_PIECE_IDS), eq(requestContext));

    var future = circulationRequestsRetriever.getRequesterIdsToRequestsByPieceIds(MOCK_PIECE_IDS, status, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      var requestsCollection = f.result();
      assertEquals(7, requestsCollection.getTotalRecords());
      assertEquals(7, requestsCollection.getCirculationRequests().size());
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      verify(pieceStorageService, times(1)).getPiecesByIds(eq(MOCK_PIECE_IDS), eq(requestContext));
      vertxTestContext.completeNow();
    });
  }

  @Test
  void getEmptyRequestersIdToRequestsByPieceIdsTestWhenAuthorizationProblem(VertxTestContext vertxTestContext) {
    var piecesMockData = getMockAsJson(PIECES_PATH, PIECES_MOCK).mapTo(PieceCollection.class);
    var status = "Open - In transit";

    doReturn(Future.failedFuture(new HttpException(403, "Invalid token: User with id 5d753594-016a-460c-aa0b-a4a174b92e9b does not exist")))
      .when(restClient).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
    doReturn(Future.succeededFuture(piecesMockData.getPieces())).when(pieceStorageService).getPiecesByIds(eq(MOCK_PIECE_IDS), eq(requestContext));

    var future = circulationRequestsRetriever.getRequesterIdsToRequestsByPieceIds(MOCK_PIECE_IDS, status, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      var requestsCollection = f.result();
      assertEquals(0, requestsCollection.getTotalRecords());
      assertEquals(0, requestsCollection.getCirculationRequests().size());
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      verify(pieceStorageService, times(1)).getPiecesByIds(eq(MOCK_PIECE_IDS), eq(requestContext));
      vertxTestContext.completeNow();
    });
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
    public PieceStorageService pieceStorageService(ConsortiumConfigurationService consortiumConfigurationService,
                                                   ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever,
                                                   SettingsRetriever settingsRetriever,
                                                   RestClient restClient) {
      return spy(new PieceStorageService(consortiumConfigurationService, consortiumUserTenantsRetriever, settingsRetriever, restClient));
    }

    @Bean
    public CirculationRequestsRetriever circulationRequestsRetriever(PieceStorageService pieceStorageService, RestClient restClient) {
      return new CirculationRequestsRetriever(pieceStorageService, restClient);
    }

    @Bean
    ConsortiumConfigurationService consortiumConfigurationService(RestClient restClient) {
      return new ConsortiumConfigurationService(restClient);
    }

    @Bean
    ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever(RestClient restClient) {
      return new ConsortiumUserTenantsRetriever(restClient);
    }

    @Bean
    SettingsRetriever settingsRetriever(RestClient restClient) {
      return new SettingsRetriever(restClient);
    }

  }

}

