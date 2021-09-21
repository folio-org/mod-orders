package org.folio.service.pieces;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
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
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import io.vertx.core.Context;

public class PieceStorageServiceTest {

  @Autowired
  PieceStorageService pieceStorageService;

  @Autowired
  private RestClient restClientMock;

  @Mock
  private Map<String, String> okapiHeadersMock;
  @Spy
  private Context ctxMock = getFirstContextFromVertx(getVertx());

  private RequestContext requestContext;
  private static boolean runningOnOwn;

  @BeforeEach
  void initMocks(){
    MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
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
  void testPiecesShouldBeReturnedByQuery() {
    String pieceId = UUID.randomUUID()
      .toString();
    List<Piece> pieces = Collections.singletonList(new Piece().withId(pieceId));

    PieceCollection pieceCollection = new PieceCollection().withPieces(pieces)
      .withTotalRecords(1);

    when(restClientMock.get(any(), any(), any())).thenReturn(CompletableFuture.completedFuture(pieceCollection));

    String expectedQuery = String.format("id==%s", pieceId);
    PieceCollection retrievedPieces = pieceStorageService.getPieces(Integer.MAX_VALUE, 0, expectedQuery, requestContext)
      .join();

    verify(restClientMock).get(any(), eq(requestContext), eq(PieceCollection.class));
    assertEquals(pieceCollection, retrievedPieces);
  }

  @Test
  void testShouldDeleteItems() {
    //given
    doReturn(completedFuture(null)).when(pieceStorageService).deletePiece(any(String.class), eq(requestContext));
    //When
    pieceStorageService.deletePiecesByIds(List.of(UUID.randomUUID().toString()), requestContext).join();
    //Then
    verify(pieceStorageService, times(1)).deletePiece(any(String.class), eq(requestContext));
  }

  private static class ContextConfiguration {

    @Bean
    RestClient restClient() {
      return mock(RestClient.class);
    }

    @Bean PieceStorageService pieceStorageService(RestClient restClient) {
      return spy(new PieceStorageService(restClient));
    }
  }
}
