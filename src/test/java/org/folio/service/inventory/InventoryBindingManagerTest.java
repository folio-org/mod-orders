package org.folio.service.inventory;

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
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
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

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class InventoryBindingManagerTest {

  @Autowired
  InventoryBindingManager inventoryBindingManager;

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
    initSpringContext(InventoryBindingManagerTest.ContextConfiguration.class);
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
  void getPiecesWithNoActiveRequestsTest(VertxTestContext vertxTestContext) {
    List<Piece> pieces = generatePieces(5);

    doReturn(Future.succeededFuture(0)).when(circulationRequestsRetriever).getNumberOfRequestsByItemId(anyString(), eq(requestContext));

    Future<List<Piece>> future = inventoryBindingManager.getPiecesWithActiveRequests(pieces, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      var res = f.result();
      assertEquals(res.size(), 0);
      vertxTestContext.completeNow();
    });
  }

  @Test
  void getPiecesWithActiveRequestsTest(VertxTestContext vertxTestContext) {
    List<Piece> pieces = generatePieces(3);
    Piece pieceWithReq = pieces.get(1);

    System.out.println(pieceWithReq.getItemId());
    System.out.println(circulationRequestsRetriever);
    doReturn(Future.succeededFuture(0)).when(circulationRequestsRetriever).getNumberOfRequestsByItemId(anyString(), eq(requestContext));
    doReturn(Future.succeededFuture(1)).when(circulationRequestsRetriever).getNumberOfRequestsByItemId(eq(pieceWithReq.getItemId()), eq(requestContext));

    Future<List<Piece>> future = inventoryBindingManager.getPiecesWithActiveRequests(pieces, requestContext);

    vertxTestContext.assertComplete(future).onComplete(f -> {
      assertTrue(f.succeeded());
      var res = f.result();
      assertEquals(1, res.size());
      assertEquals(pieceWithReq.getId(), res.get(0).getId());
      assertEquals(pieceWithReq.getItemId(), res.get(0).getItemId());
      vertxTestContext.completeNow();
    });
  }

  private List<Piece> generatePieces(int n) {
    List<Piece> pieces = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      pieces.add(new Piece()
        .withId(UUID.randomUUID().toString())
        .withItemId(UUID.randomUUID().toString())
      );
    }
    return pieces;
  }


  /**
   * Define unit test specific beans to override actual ones
   */
  static class ContextConfiguration {

    @Bean
    public RestClient restClient() {
      return new RestClient();
    }

    @Bean
    public CirculationRequestsRetriever circulationRequestsRetriever(RestClient restClient) {
      return mock(CirculationRequestsRetriever.class);
    }

    @Bean
    public InventoryBindingManager inventoryBindingManager(RestClient restClient,
                                                           CirculationRequestsRetriever circulationRequestsRetriever) {
      return new InventoryBindingManager(restClient, circulationRequestsRetriever);
    }

  }

}
