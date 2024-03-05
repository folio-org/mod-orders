package org.folio.helper;

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
import static org.folio.rest.core.exceptions.ErrorCodes.BARCODE_IS_NOT_UNIQUE;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.service.ProtectionService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.create.PieceCreateFlowInventoryManager;
import org.folio.service.titles.TitlesService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

public class CheckinHelperTest {

  @Autowired
  PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;

  private Map<String, String> okapiHeadersMock;
  private RequestContext requestContext;

  private static boolean runningOnOwn;

  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(CheckinHelperTest.ContextConfiguration.class);
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
    var ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }

  @AfterEach
  void resetMocks() {
    clearServiceInteractions();
  }


  @Test
  void shouldTestGetItemCreateNeededCheckinPieces() {
    String poLine1 = UUID.randomUUID().toString();
    String poLine2 = UUID.randomUUID().toString();
    CheckinCollection checkinCollection = new CheckinCollection();
    ToBeCheckedIn toBeCheckedIn1 = new ToBeCheckedIn().withPoLineId(poLine1);
    CheckInPiece checkInPiece1 = new CheckInPiece().withId(UUID.randomUUID().toString()).withCreateItem(true).withDisplaySummary("1");
    CheckInPiece checkInPiece2 = new CheckInPiece().withId(UUID.randomUUID().toString()).withCreateItem(false).withDisplaySummary("2");
    toBeCheckedIn1.withCheckInPieces(List.of(checkInPiece1, checkInPiece2));

    ToBeCheckedIn toBeCheckedIn2 = new ToBeCheckedIn().withPoLineId(poLine1);
    CheckInPiece checkInPiece3 = new CheckInPiece().withId(UUID.randomUUID().toString()).withCreateItem(true).withDisplaySummary("3")
                        .withEnumeration("Enum1").withCopyNumber("CN1").withChronology("Ch1").withDiscoverySuppress(true).withDisplayOnHolding(true);
    toBeCheckedIn2.withCheckInPieces(List.of(checkInPiece3));

    ToBeCheckedIn toBeCheckedIn3 = new ToBeCheckedIn().withPoLineId(poLine2);
    CheckInPiece checkInPiece4 = new CheckInPiece().withId(UUID.randomUUID().toString()).withCreateItem(true).withDisplaySummary("4")
                        .withEnumeration("Enum2").withCopyNumber("CN2").withChronology("Ch2").withDiscoverySuppress(false).withDisplayOnHolding(false);
    toBeCheckedIn3.withCheckInPieces(List.of(checkInPiece4));

    checkinCollection.withToBeCheckedIn(List.of(toBeCheckedIn1, toBeCheckedIn2, toBeCheckedIn3));
    checkinCollection.setTotalRecords(3);
    CheckinHelper checkinHelper = spy(new CheckinHelper(checkinCollection, okapiHeadersMock, requestContext.getContext()));
    //When
    Map<String, List<CheckInPiece>> map = checkinHelper.getItemCreateNeededCheckinPieces(checkinCollection);

    assertEquals(2, map.values().size());
    assertEquals(3, map.get(poLine1).size());

    assertEquals(1, map.get(poLine2).size());
    CheckInPiece actCheckInPiece1 = map.get(poLine1).stream()
                              .filter(checkInPiece -> "3".equals(checkInPiece.getDisplaySummary()))
                              .findFirst().get();
    assertEquals("3", actCheckInPiece1.getDisplaySummary());
    assertEquals("Enum1", actCheckInPiece1.getEnumeration());
    assertEquals("CN1", actCheckInPiece1.getCopyNumber());
    assertEquals("Ch1", actCheckInPiece1.getChronology());
    assertEquals(true, actCheckInPiece1.getDiscoverySuppress());
    assertEquals(true, actCheckInPiece1.getDisplayOnHolding());
    CheckInPiece actCheckInPiece2 = map.get(poLine2).get(0);
    assertEquals("4", actCheckInPiece2.getDisplaySummary());
    assertEquals("Enum2", actCheckInPiece2.getEnumeration());
    assertEquals("CN2", actCheckInPiece2.getCopyNumber());
    assertEquals("Ch2", actCheckInPiece2.getChronology());
    assertEquals(false, actCheckInPiece2.getDiscoverySuppress());
    assertEquals(false, actCheckInPiece2.getDisplayOnHolding());
  }

  @Test
  void allCheckingPieceFieldsShouldBePopulated() {
    String poLineId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    CheckinCollection checkinCollection = new CheckinCollection();
    checkinCollection.withToBeCheckedIn(Collections.singletonList(new ToBeCheckedIn()
      .withPoLineId(poLineId)
      .withCheckInPieces(Collections.singletonList(new CheckInPiece()
        .withId(pieceId)
        .withDisplaySummary("displaySummary")
        .withComment("comment")
        .withEnumeration("enumeration")
        .withChronology("chronology")
        .withCopyNumber("copyNumber")
        .withAccessionNumber("accessionNumber")
        .withDisplayOnHolding(true)
        .withDiscoverySuppress(true)
        .withSupplement(true)
        .withBarcode("barcode")
        .withReceiptDate(new Date())
        .withCallNumber("callNumber")
      ))));
    CheckinHelper checkinHelper = spy(new CheckinHelper(checkinCollection, okapiHeadersMock, requestContext.getContext()));
    Map<String, List<Piece>> map = new HashMap<>();
    List<Piece> pieces = new ArrayList<>();
    pieces.add(new Piece()
      .withId(pieceId)
      .withPoLineId(poLineId));
    map.put(poLineId, pieces);
    Map<String, List<Piece>> res = checkinHelper.updatePieceRecordsWithoutItems(map);

    Piece piece = res.get(poLineId).get(0);
    assertEquals("displaySummary", piece.getDisplaySummary());
    assertEquals("comment", piece.getComment());
    assertEquals("enumeration", piece.getEnumeration());
    assertEquals("chronology", piece.getChronology());
    assertEquals("copyNumber", piece.getCopyNumber());
    assertEquals("accessionNumber", piece.getAccessionNumber());
    assertTrue(piece.getDisplayOnHolding());
    assertTrue(piece.getDiscoverySuppress());
    assertTrue(piece.getSupplement());
    assertEquals("barcode", piece.getBarcode());
    assertNotNull(piece.getReceiptDate());
    assertEquals("callNumber", piece.getCallNumber());
    assertNotNull(piece.getReceivedDate());
    assertEquals(Piece.ReceivingStatus.RECEIVED, piece.getReceivingStatus());
  }

  @Test
  void testProduceOnErrorWithBarcodeIsUnique() {
    String poLineId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    CheckinCollection checkinCollection = new CheckinCollection();
    checkinCollection.withToBeCheckedIn(Collections.singletonList(new ToBeCheckedIn()
      .withPoLineId(poLineId)
      .withCheckInPieces(Collections.singletonList(new CheckInPiece()
        .withId(pieceId)
        .withDisplaySummary("displaySummary")
        .withDisplayOnHolding(true)
        .withDiscoverySuppress(true)
        .withSupplement(true)
        .withBarcode("12345")
        .withReceiptDate(new Date())
        .withCallNumber("callNumber")))));
    CheckinHelper checkinHelper = spy(new CheckinHelper(checkinCollection, okapiHeadersMock, requestContext.getContext()));
    Map<String, List<Piece>> map = new HashMap<>();
    List<Piece> pieces = new ArrayList<>();
    pieces.add(new Piece().withId(pieceId).withPoLineId(poLineId));
    map.put(poLineId, pieces);
    Map<String, List<Piece>> res = checkinHelper.updatePieceRecordsWithoutItems(map);

    Piece piece = res.get(poLineId).get(0);
    assertNotNull(piece.getReceivedDate());
    assertEquals(Piece.ReceivingStatus.RECEIVED, piece.getReceivingStatus());
  }

  @Test
  void testProduceOnErrorWithBarcodeNotUnique() {
    String poLineId = UUID.randomUUID().toString();
    String pieceId = UUID.randomUUID().toString();
    CheckinCollection checkinCollection = new CheckinCollection();
    checkinCollection.withToBeCheckedIn(Collections.singletonList(new ToBeCheckedIn()
      .withPoLineId(poLineId)
      .withCheckInPieces(Collections.singletonList(new CheckInPiece()
        .withId(pieceId)
        .withDisplaySummary("displaySummary")
        .withDisplayOnHolding(true)
        .withDiscoverySuppress(true)
        .withSupplement(true)
        .withBarcode("12345")
        .withReceiptDate(new Date())
        .withCallNumber("callNumber")))));
    CheckinHelper checkinHelper = spy(new CheckinHelper(checkinCollection, okapiHeadersMock, requestContext.getContext()));
    Map<String, List<Piece>> map = new HashMap<>();
    List<Piece> pieces = new ArrayList<>();
    pieces.add(new Piece().withId(pieceId).withPoLineId(poLineId));
    map.put(poLineId, pieces);
    Map<String, List<Piece>> res = checkinHelper.updatePieceRecordsWithoutItems(map);
    Piece piece = res.get(poLineId).get(0);

    assertNotNull(piece.getReceivedDate());
    assertEquals(Piece.ReceivingStatus.RECEIVED, piece.getReceivingStatus());

    String poLineId2 = UUID.randomUUID().toString();
    String pieceId2 = UUID.randomUUID().toString();
    CheckinCollection checkinCollection2 = new CheckinCollection();
    checkinCollection.withToBeCheckedIn(Collections.singletonList(new ToBeCheckedIn()
      .withPoLineId(poLineId)
      .withCheckInPieces(Collections.singletonList(new CheckInPiece()
        .withId(pieceId)
        .withDisplaySummary("displaySummary")
        .withDisplayOnHolding(true)
        .withDiscoverySuppress(true)
        .withSupplement(true)
        .withBarcode("12345")
        .withReceiptDate(new Date())
        .withCallNumber("callNumber")))));

    assertNotNull(new CheckinHelper(checkinCollection2, okapiHeadersMock, requestContext.getContext()).getErrors());

    Map<String, List<Piece>> map2 = new HashMap<>();
    List<Piece> pieces2 = new ArrayList<>();
    pieces2.add(new Piece().withId(pieceId2).withPoLineId(poLineId2));

    assertNull(map2.put(poLineId2, pieces2));
  }

  private static class ContextConfiguration {
    @Bean
    PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager() {
      return mock(PieceCreateFlowInventoryManager.class);
    }

    @Bean ConfigurationEntriesService configurationEntriesService() {
      return mock(ConfigurationEntriesService.class);
    }

    @Bean
    ProtectionService protectionService() {
      return mock(ProtectionService.class);
    }
    @Bean
    TitlesService titlesService() {
      return mock(TitlesService.class);
    }
    @Bean
    PieceStorageService pieceStorageService() {
      return mock(PieceStorageService.class);
    }
    @Bean
    InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }
    @Bean
    PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

  }
}
