package org.folio.rest.impl;

import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
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
    //When
    CompletableFuture<String> result = piecesHelper.handleHoldingsRecord(line, piece.getLocationId(), title.getInstanceId());
    String actHoldingId = result.get();
    //Then
    verify(inventoryHelper).getOrCreateHoldingsRecord(title.getInstanceId(), piece.getLocationId());
    assertEquals(HOLDING_ID, actHoldingId);
  }

  @Test
  public void testHoldingsItemCreationShouldBeSkippedIfEresourceOrPhysicsIsAbsent() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = ApiTestBase.getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setEresource(null);
    line.setPhysical(null);

    org.folio.rest.acq.model.Piece piece = ApiTestBase.getMockAsJson(PIECE_PATH,"pieceRecord")
      .mapTo(org.folio.rest.acq.model.Piece.class);
    Title title = ApiTestBase.getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(CompletableFuture.completedFuture(HOLDING_ID))
      .when(inventoryHelper).getOrCreateHoldingsRecord(anyString(), anyString());
    //When
    CompletableFuture<String> result =
      piecesHelper.handleHoldingsRecord(line, piece.getLocationId(), title.getInstanceId());

    //Then
    String holdingId = result.get();
    verify(inventoryHelper,never()).getOrCreateHoldingsRecord(title.getInstanceId(), piece.getLocationId());
    assertNull(holdingId);
  }

  @Test
  public void testShouldCreateInstanceIfInstanceIdIsNotProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = ApiTestBase.getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    title.setInstanceId(null);
    PiecesHelper piecesHelper = mock(PiecesHelper.class, CALLS_REAL_METHODS);
    doReturn(CompletableFuture.completedFuture(UUID.randomUUID().toString())).when(piecesHelper).getInstanceRecord(any(Title.class));
    //When
    piecesHelper.handleInstanceRecord(title).get();
    //Then
    verify(piecesHelper, times(1)).getInstanceRecord(title);
  }

  @Test
  public void testShouldSkippCreationNewInstanceIfInstanceIdIsProvided() throws ExecutionException, InterruptedException {
    //given
    Title title = ApiTestBase.getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    CompletableFuture<Title> result = piecesHelper.handleInstanceRecord(title);
    //Then
    Title actTitle = result.get();
    assertEquals(title, actTitle);
  }

  @Test
  public void testShouldCreateItemRecordForPhysical() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = ApiTestBase.getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);

    String expItemId = UUID.randomUUID().toString();
    doReturn(CompletableFuture.completedFuture(Arrays.asList(expItemId)))
      .when(inventoryHelper).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1));

    //When
    CompletableFuture<String> result = piecesHelper.createItemRecord(line, HOLDING_ID);
    String actItemId = result.get();
    //Then
    verify(inventoryHelper).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1));
    assertEquals(expItemId, actItemId);
  }

  @Test
  public void testShouldCreateItemRecordForEresources() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = ApiTestBase.getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Eresource eresource = new Eresource().withMaterialType(line.getPhysical().getMaterialType())
            .withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    line.setPhysical(null);
    line.setEresource(eresource);
    line.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    String expItemId = UUID.randomUUID().toString();
    doReturn(CompletableFuture.completedFuture(Arrays.asList(expItemId)))
      .when(inventoryHelper).createMissingElectronicItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1));

    //When
    CompletableFuture<String> result = piecesHelper.createItemRecord(line, HOLDING_ID);
    String actItemId = result.get();
    //Then
    verify(inventoryHelper).createMissingElectronicItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1));
    assertEquals(expItemId, actItemId);
  }
}
