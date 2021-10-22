package org.folio.service.orders.flows.open;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.clearVertxContext;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.PIECE_PATH;
import static org.folio.TestConstants.TILES_PATH;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.titles.TitlesService;
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

public class OpenCompositeOrderInventoryServiceTest {
  public static final String LINE_ID = "c0d08448-347b-418a-8c2f-5fb50248d67e";
  public static final String HOLDING_ID = "65cb2bf0-d4c2-4886-8ad0-b76f1ba75d61";
  private static final String COMPOSITE_LINES_PATH = BASE_MOCK_DATA_PATH + "compositeLines/";

  @Autowired
  private OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  @Autowired
  private InventoryManager inventoryManager;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private  PieceStorageService pieceStorageService;

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
    initSpringContext(OpenCompositeOrderInventoryServiceTest.ContextConfiguration.class);
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
  void testShouldCreateItemRecordForEresources() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Eresource eresource = new Eresource().withMaterialType(line.getPhysical().getMaterialType())
      .withCreateInventory(Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
    line.setPhysical(null);
    line.setEresource(eresource);
    line.setOrderFormat(CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE);
    String expItemId = UUID.randomUUID().toString();
    doReturn(completedFuture(Collections.singletonList(expItemId)))
      .when(inventoryManager).createMissingElectronicItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));

    //When
    CompletableFuture<String> result = openCompositeOrderInventoryService.createItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    verify(inventoryManager).createMissingElectronicItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));
    assertEquals(expItemId, actItemId);
  }

  @Test
  void testShouldSkipCreationItemRecord() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setCheckinItems(true);
    //When
    CompletableFuture<String> result = openCompositeOrderInventoryService.createItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    assertNull(actItemId);
  }

  @Test
  void testShouldCreateItemRecordForPhysical() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    String expItemId = UUID.randomUUID().toString();
    doReturn(completedFuture(Collections.singletonList(expItemId)))
      .when(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));
    //When
    CompletableFuture<String> result = openCompositeOrderInventoryService.createItemRecord(line, HOLDING_ID, requestContext);
    String actItemId = result.get();
    //Then
    verify(inventoryManager).createMissingPhysicalItems(any(CompositePoLine.class), eq(HOLDING_ID), eq(1), eq(requestContext));
    assertEquals(expItemId, actItemId);
  }

  @Test
  void testHoldingsItemCreationShouldBeSkippedIfEresourceOrPhysicsIsAbsent() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    line.setEresource(null);
    line.setPhysical(null);

    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    doReturn(completedFuture(HOLDING_ID)).when(inventoryManager).getOrCreateHoldingsRecord(anyString(), any(Location.class), eq(requestContext));
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    CompletableFuture<String> result = openCompositeOrderInventoryService.handleHoldingsRecord(line, location, title.getInstanceId(), requestContext);

    //Then
    String holdingId = result.get();
    verify(inventoryManager,never()).getOrCreateHoldingsRecord(title.getInstanceId(), location, requestContext);
    assertNull(holdingId);
  }

  @Test
  void testHoldingsItemShouldNotBeCreatedIfPOLIsNull() {
    //given
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    CompletableFuture<String> result = openCompositeOrderInventoryService.handleHoldingsRecord(null, location, title.getInstanceId(), requestContext);
    //Then
    assertTrue(result.isCompletedExceptionally());
  }

  @Test
  void testHoldingsRecordShouldBeCreated() throws ExecutionException, InterruptedException {
    //given
    CompositePoLine line = getMockAsJson(COMPOSITE_LINES_PATH, LINE_ID).mapTo(CompositePoLine.class);
    Piece piece = getMockAsJson(PIECE_PATH,"pieceRecord").mapTo(Piece.class);
    Title title = getMockAsJson(TILES_PATH,"title").mapTo(Title.class);

    when(inventoryManager.getOrCreateHoldingsRecord(anyString(), any(Location.class), eq(requestContext))).thenReturn(completedFuture(HOLDING_ID));
    //When
    Location location = new Location().withLocationId(piece.getLocationId());
    CompletableFuture<String> result = openCompositeOrderInventoryService.handleHoldingsRecord(line, location, title.getInstanceId(), requestContext);
    String actHoldingId = result.get();
    //Then
    verify(inventoryManager).getOrCreateHoldingsRecord(eq(title.getInstanceId()), eq(location), eq(requestContext));
    assertEquals(HOLDING_ID, actHoldingId);
  }

  @Test
  void testUpdateInventoryNegativeCaseIfPOLIsNull() {
    //When
    CompletableFuture<String> result =  openCompositeOrderInventoryService.createItemRecord (null, UUID.randomUUID().toString(), requestContext);
    //Then
    assertTrue(result.isCompletedExceptionally());
  }

  private static class ContextConfiguration {
    @Bean TitlesService titlesService() {
      return mock(TitlesService.class);
    }

    @Bean InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }

    @Bean PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }

    @Bean OpenCompositeOrderInventoryService openCompositeOrderInventoryService(TitlesService titlesService, InventoryManager inventoryManager,
      PieceStorageService pieceStorageService) {
      return spy(new OpenCompositeOrderInventoryService(titlesService, inventoryManager, pieceStorageService));
    }
  }
}
