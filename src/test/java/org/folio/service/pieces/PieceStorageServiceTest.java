package org.folio.service.pieces;

import static io.vertx.core.Future.succeededFuture;
import static org.assertj.core.api.Assertions.assertThat;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.folio.service.ProtectionService;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;


@ExtendWith(VertxExtension.class)
public class PieceStorageServiceTest {

  private static final String USER_TENANTS_PATH = BASE_MOCK_DATA_PATH + "userTenants/";
  private static final String USER_TENANTS_MOCK = "userTenants";

  private static final String PIECES_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String PIECES_MOCK = "pieces-for-user-tenants";

  @Autowired
  PieceStorageService pieceStorageService;

  @Autowired
  ConsortiumConfigurationService consortiumConfigurationService;

  @Autowired
  ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;

  @Autowired
  private RestClient restClientMock;

  @Mock
  private Map<String, String> okapiHeadersMock;
  private final Context ctx = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(PieceStorageServiceTest.ContextConfiguration.class);
  }

  @AfterAll
  public static void after() {
    clearVertxContext();
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }

  @Test
  void testPiecesShouldBeReturnedByQuery(VertxTestContext vertxTestContext) {
    String pieceId = UUID.randomUUID()
      .toString();
    List<Piece> pieces = Collections.singletonList(new Piece().withId(pieceId));

    PieceCollection pieceCollection = new PieceCollection().withPieces(pieces)
      .withTotalRecords(1);

    when(restClientMock.get(any(RequestEntry.class), any(), any())).thenReturn(Future.succeededFuture(pieceCollection));
    when(consortiumConfigurationService.getConsortiumConfiguration(any(RequestContext.class))).thenReturn(Future.succeededFuture(Optional.empty()));

    String expectedQuery = String.format("id==%s", pieceId);
    var future = pieceStorageService.getPieces(Integer.MAX_VALUE, 0, expectedQuery, requestContext);

    verify(restClientMock).get(any(RequestEntry.class), eq(PieceCollection.class), eq(requestContext));
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertEquals(pieceCollection, result.result());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testShouldDeleteItems() {
    //given
    doReturn(succeededFuture(null)).when(pieceStorageService).deletePiece(any(String.class), eq(requestContext));
    //When
    pieceStorageService.deletePiecesByIds(List.of(UUID.randomUUID().toString()), requestContext).result();
    //Then
    verify(pieceStorageService, times(1)).deletePiece(any(String.class), eq(requestContext));
  }

  @Test
  void testGetPiecesFilterByUserTenants(VertxTestContext vertxTestContext) {
    var userTenantsMockData = getMockAsJson(USER_TENANTS_PATH, USER_TENANTS_MOCK);
    var piecesMockData = getMockAsJson(PIECES_PATH, PIECES_MOCK).mapTo(PieceCollection.class);
    var consortiumConfiguration = Optional.of(new ConsortiumConfiguration("tenantId0", UUID.randomUUID().toString()));

    doReturn(Future.succeededFuture(consortiumConfiguration)).when(consortiumConfigurationService).getConsortiumConfiguration(any(RequestContext.class));
    doReturn(Future.succeededFuture(piecesMockData)).when(restClientMock).get(any(RequestEntry.class), eq(PieceCollection.class), any(RequestContext.class));
    doReturn(Future.succeededFuture(userTenantsMockData)).when(restClientMock).getAsJsonObject(any(), any(RequestContext.class));

    var future = pieceStorageService.getPieces(Integer.MAX_VALUE, 0, null, requestContext);

    verify(restClientMock, times(1)).get(any(RequestEntry.class), eq(PieceCollection.class), eq(requestContext));
    verify(restClientMock, times(1)).getAsJsonObject(any(), any(RequestContext.class));
    verify(consortiumConfigurationService, times(1)).getConsortiumConfiguration(eq(requestContext));

    vertxTestContext.assertComplete(future)
      .onComplete(f -> {
        var result = f.result();
        assertThat(result).isNotNull();
        assertThat(result.getTotalRecords()).isEqualTo(2);
        assertThat(result.getPieces()).hasSize(2);
        vertxTestContext.completeNow();
      });
  }

  @Test
  void testGetPiecesFilterByUserTenantsNonECS(VertxTestContext vertxTestContext) {
    var piecesMockData = getMockAsJson(PIECES_PATH, PIECES_MOCK).mapTo(PieceCollection.class);

    doReturn(Future.succeededFuture(Optional.empty())).when(consortiumConfigurationService).getConsortiumConfiguration(any(RequestContext.class));
    doReturn(Future.succeededFuture(piecesMockData)).when(restClientMock).get(any(RequestEntry.class), eq(PieceCollection.class), any(RequestContext.class));

    var future = pieceStorageService.getPieces(Integer.MAX_VALUE, 0, null, requestContext);

    verify(restClientMock, times(1)).get(any(RequestEntry.class), eq(PieceCollection.class), eq(requestContext));
    verify(restClientMock, times(0)).getAsJsonObject(any(), any(RequestContext.class));
    verify(consortiumConfigurationService, times(1)).getConsortiumConfiguration(eq(requestContext));

    vertxTestContext.assertComplete(future)
      .onComplete(f -> {
        var result = f.result();
        assertThat(result).isNotNull();
        assertThat(result.getTotalRecords()).isEqualTo(3);
        assertThat(result.getPieces()).hasSize(3);
        vertxTestContext.completeNow();
      });
  }

  private static class ContextConfiguration {

    @Bean
    RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean
    ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }

    @Bean
    PieceStorageService pieceStorageService(ConsortiumConfigurationService consortiumConfigurationService,
                                                  ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever,
                                                  RestClient restClient) {
      return spy(new PieceStorageService(consortiumConfigurationService, consortiumUserTenantsRetriever, restClient));
    }

    @Bean
    ConsortiumConfigurationService consortiumConfigurationService(RestClient restClient) {
      return spy(new ConsortiumConfigurationService(restClient));
    }

    @Bean
    ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever(RestClient restClient) {
      return spy(new ConsortiumUserTenantsRetriever(restClient));
    }

  }

}
