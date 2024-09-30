package org.folio.service.pieces;

import static io.vertx.core.Future.succeededFuture;
import static org.assertj.core.api.Assertions.assertThat;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.RestClientTest.X_OKAPI_TENANT;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
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

  private static final String REQUEST_TENANT_ID = "tenantId";

  @Autowired
  PieceStorageService pieceStorageService;

  @Autowired
  ConsortiumConfigurationService consortiumConfigurationService;

  @Autowired
  ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;

  @Autowired
  private RestClient restClientMock;

  private final Context ctx = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    var okapiHeadersMock = new HashMap<String, String>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), REQUEST_TENANT_ID);
    requestContext = new RequestContext(ctx, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() {
    reset(restClientMock);
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
    var future = pieceStorageService.getAllPieces(expectedQuery, requestContext);

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

  @ParameterizedTest(name = "{index} {0}")
  @MethodSource("testGetPiecesFilterByUserTenantsParams")
  void testGetPiecesFilterByUserTenants(String testCaseName, Optional<ConsortiumConfiguration> consortiumConfiguration,
                                        boolean shouldFilter, VertxTestContext vertxTestContext) {
    var userTenantsMockData = getMockAsJson(USER_TENANTS_PATH, USER_TENANTS_MOCK);
    var piecesMockData = getMockAsJson(PIECES_PATH, PIECES_MOCK).mapTo(PieceCollection.class);

    doReturn(Future.succeededFuture(consortiumConfiguration))
      .when(consortiumConfigurationService).getConsortiumConfiguration(any(RequestContext.class));
    doReturn(Future.succeededFuture(piecesMockData))
      .when(restClientMock).get(any(RequestEntry.class), eq(PieceCollection.class), any(RequestContext.class));

    if (shouldFilter) {
      doReturn(Future.succeededFuture(userTenantsMockData)).when(restClientMock).getAsJsonObject(any(), any(RequestContext.class));
    }

    var future = pieceStorageService.getPieces(Integer.MAX_VALUE, 0, null, requestContext);

    verify(restClientMock, times(1)).get(any(RequestEntry.class), eq(PieceCollection.class), eq(requestContext));
    verify(restClientMock, times(shouldFilter ? 1 : 0)).getAsJsonObject(any(), any(RequestContext.class));
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

  @ParameterizedTest
  @MethodSource("getQueryForUserTenantsParamProvider")
  void testGetQueryForUserTenants(List<String> userTenants, String query, String expectedQuery) {
    String result = PieceStorageService.getQueryForUserTenants(userTenants, query);
    assertEquals(expectedQuery, result);
  }

  private static Stream<Arguments> testGetPiecesFilterByUserTenantsParams() {
    var consortiumConfigurationCentral = Optional.of(new ConsortiumConfiguration(REQUEST_TENANT_ID, UUID.randomUUID().toString()));
    var consortiumConfigurationMember = Optional.of(new ConsortiumConfiguration("centralTenantId", UUID.randomUUID().toString()));
    return Stream.of(
      Arguments.of("ECS - Central", consortiumConfigurationCentral, true),
      Arguments.of("ECS - Member", consortiumConfigurationMember, false),
      Arguments.of("Non-ECS", Optional.empty(), false)
    );
  }

  private static Stream<Arguments> getQueryForUserTenantsParamProvider() {
    return Stream.of(
      Arguments.of(null, "format==Physical", "format==Physical"),
      Arguments.of(new ArrayList<>(), "format==Physical", "(format==Physical) and (receivingTenantId==(null))"),
      Arguments.of(new ArrayList<>(List.of("tenant1")), "format==Physical", "(format==Physical) and (receivingTenantId==(tenant1 or null))"),
      Arguments.of(new ArrayList<>(List.of("tenant1", "tenant2")), "format==Physical", "(format==Physical) and (receivingTenantId==(tenant1 or tenant2 or null))"
      )
    );
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
