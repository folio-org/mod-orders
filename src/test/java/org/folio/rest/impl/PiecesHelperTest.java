package org.folio.rest.impl;

import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.folio.rest.acq.model.Piece;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Title;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.impl.EventLoopContext;


public class PiecesHelperTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";
  private static final String PIECE_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  private static final String TILES_PATH = BASE_MOCK_DATA_PATH + "titles/";
  public static final String HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d61";

  @InjectMocks
  private PiecesHelper piecesHelper;
  @Mock
  private ProtectionHelper protectionHelper;
  @Mock
  private InventoryHelper inventoryHelper;
  @Mock
  private TitlesHelper titlesHelper;
  @Mock
  private Map<String, String> okapiHeadersMock;
  @Mock
  private EventLoopContext ctxMock;

  @Before
  public void initMocks(){
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testHoldingsItemShouldBeCreated() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = ApiTestBase.getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    org.folio.rest.acq.model.Piece piece = ApiTestBase.getMockAsJson(PIECE_PATH,"pieceRecord")
                .mapTo(org.folio.rest.acq.model.Piece.class);
    Title title = ApiTestBase.getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(CompletableFuture.completedFuture(HOLDING_ID))
      .when(inventoryHelper).getOrCreateHoldingsRecord(anyString(), anyString());
    doReturn(CompletableFuture.completedFuture(Collections.singletonList(piece)))
      .when(inventoryHelper).handleItemRecords(any(CompositePoLine.class), eq(HOLDING_ID), any(List.class));
    //When
    CompletableFuture<List<org.folio.rest.acq.model.Piece>> result =
      piecesHelper.handleHoldingsAndItemsRecords(line, title.getInstanceId(), piece.getLocationId());

    //Then
    List<Piece> pieces = result.get();
    verify(inventoryHelper).getOrCreateHoldingsRecord(title.getInstanceId(), piece.getLocationId());
    verify(inventoryHelper).handleItemRecords(any(CompositePoLine.class), eq(HOLDING_ID), any(List.class));
  }
}
