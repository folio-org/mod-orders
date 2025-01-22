package org.folio.service.pieces;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.rest.acq.model.Setting;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.WrapperPiece;
import org.folio.rest.jaxrs.model.WrapperPieceCollection;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.settings.SettingsRetriever;
import org.folio.service.settings.util.SettingKey;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(VertxExtension.class)
public class WrapperPieceStorageServiceTest {

  @Mock private ConsortiumConfigurationService consortiumConfigurationService;
  @Mock private ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;
  @Mock private SettingsRetriever settingsRetriever;
  @Mock private RestClient restClient;
  @InjectMocks private WrapperPieceStorageService wrapperPieceStorageService;

  private AutoCloseable openedMocks;

  @BeforeEach
  void setUp() {
    openedMocks = MockitoAnnotations.openMocks(this);

    when(consortiumConfigurationService.getConsortiumConfiguration(any()))
      .thenReturn(Future.succeededFuture(Optional.of(new ConsortiumConfiguration("diku", UUID.randomUUID().toString()))));
    when(settingsRetriever.getSettingByKey(eq(SettingKey.CENTRAL_ORDERING_ENABLED), any()))
      .thenReturn(Future.succeededFuture(Optional.of(new Setting().withValue("0"))));
    when(consortiumUserTenantsRetriever.getUserTenants(anyString(), anyString(), any()))
      .thenReturn(Future.succeededFuture());
  }

  @AfterEach
  void tearDown() throws Exception {
    openedMocks.close();
  }

  @Test
  void testGetWrapperPieces_success(VertxTestContext testContext) {
    var vendorId = UUID.randomUUID().toString();
    var pieceId = UUID.randomUUID().toString();
    var wrapperPieceCollection = new WrapperPieceCollection().withWrapperPieces(
      List.of(new WrapperPiece().withVendorId(vendorId).withPiece(new Piece().withId(pieceId))));
    var requestContext = mock(RequestContext.class);

    when(restClient.get(any(RequestEntry.class), any(), any()))
      .thenReturn(Future.succeededFuture(wrapperPieceCollection));

    wrapperPieceStorageService.getWrapperPieces(10, 0, "", requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(1, result.getWrapperPieces().size());
        assertEquals(vendorId, result.getWrapperPieces().get(0).getVendorId());
        assertEquals(pieceId, result.getWrapperPieces().get(0).getPiece().getId());
        testContext.completeNow();
      })));
  }

  @Test
  void testGetWrapperPieces3Pieces_success(VertxTestContext testContext) {
    var vendorId = UUID.randomUUID().toString();
    var pieceId1 = UUID.randomUUID().toString();
    var pieceId2 = UUID.randomUUID().toString();
    var pieceId3 = UUID.randomUUID().toString();
    var wrapperPieceCollection = new WrapperPieceCollection().withWrapperPieces(List.of(
      new WrapperPiece().withVendorId(vendorId).withPiece(new Piece().withId(pieceId1)),
      new WrapperPiece().withVendorId(vendorId).withPiece(new Piece().withId(pieceId2)),
      new WrapperPiece().withVendorId(vendorId).withPiece(new Piece().withId(pieceId3))
    ));
    var requestContext = mock(RequestContext.class);

    when(restClient.get(any(RequestEntry.class), any(), any()))
      .thenReturn(Future.succeededFuture(wrapperPieceCollection));

    wrapperPieceStorageService.getWrapperPieces(10, 0, "", requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(3, result.getWrapperPieces().size());
        assertTrue(result.getWrapperPieces().stream()
          .allMatch(wrapperPiece -> wrapperPiece.getVendorId().equals(vendorId)));
        assertEquals(pieceId1, result.getWrapperPieces().get(0).getPiece().getId());
        assertEquals(pieceId2, result.getWrapperPieces().get(1).getPiece().getId());
        assertEquals(pieceId3, result.getWrapperPieces().get(2).getPiece().getId());
        testContext.completeNow();
      })));
  }

  @Test
  void testGetWrapperPieceById_success(VertxTestContext testContext) {
    var vendorId = UUID.randomUUID().toString();
    var pieceId = UUID.randomUUID().toString();
    var wrapperPiece = new WrapperPiece().withVendorId(vendorId).withPiece(new Piece().withId(pieceId));
    var requestContext = mock(RequestContext.class);

    when(restClient.get(any(RequestEntry.class), any(), any()))
      .thenReturn(Future.succeededFuture(wrapperPiece));

    wrapperPieceStorageService.getWrapperPieceById(pieceId, requestContext)
      .onComplete(testContext.succeeding(result -> testContext.verify(() -> {
        assertEquals(vendorId, result.getVendorId());
        assertEquals(pieceId, result.getPiece().getId());
        testContext.completeNow();
      })));
  }
}
