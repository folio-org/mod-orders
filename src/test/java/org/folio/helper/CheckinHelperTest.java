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
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckInPiece;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.ToBeCheckedIn;
import org.folio.rest.tools.client.HttpClientFactory;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.ProtectionService;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
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

import io.vertx.core.Context;

public class CheckinHelperTest {

  @Autowired
  PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager;

  private Map<String, String> okapiHeadersMock;
  private Context ctxMock;
  private RequestContext requestContext;

  private HttpClientInterface httpClient;
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
    ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    httpClient = HttpClientFactory.getHttpClient(okapiURL, X_OKAPI_TENANT.getValue());
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
    CheckInPiece checkInPiece1 = new CheckInPiece().withId(UUID.randomUUID().toString()).withCreateItem(true).withCaption("1");
    CheckInPiece checkInPiece2 = new CheckInPiece().withId(UUID.randomUUID().toString()).withCreateItem(false).withCaption("2");
    toBeCheckedIn1.withCheckInPieces(List.of(checkInPiece1, checkInPiece2));

    ToBeCheckedIn toBeCheckedIn2 = new ToBeCheckedIn().withPoLineId(poLine1);
    CheckInPiece checkInPiece3 = new CheckInPiece().withId(UUID.randomUUID().toString()).withCreateItem(true).withCaption("3");
    toBeCheckedIn2.withCheckInPieces(List.of(checkInPiece3));

    ToBeCheckedIn toBeCheckedIn3 = new ToBeCheckedIn().withPoLineId(poLine2);
    CheckInPiece checkInPiece4 = new CheckInPiece().withId(UUID.randomUUID().toString()).withCreateItem(true).withCaption("4");
    toBeCheckedIn3.withCheckInPieces(List.of(checkInPiece4));

    checkinCollection.withToBeCheckedIn(List.of(toBeCheckedIn1, toBeCheckedIn2, toBeCheckedIn3));
    checkinCollection.setTotalRecords(3);
    CheckinHelper checkinHelper = spy(new CheckinHelper(checkinCollection, okapiHeadersMock, requestContext.getContext(), ""));
    //When
    Map<String, List<CheckInPiece>> map = checkinHelper.getItemCreateNeededCheckinPieces(checkinCollection);

    assertEquals(2, map.values().size());
    assertEquals(3, map.get(poLine1).size());
    assertEquals(1, map.get(poLine2).size());
    assertEquals("4", map.get(poLine2).get(0).getCaption());
  }

  private static class ContextConfiguration {
    @Bean PieceCreateFlowInventoryManager pieceCreateFlowInventoryManager() {
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
    InventoryManager inventoryManager() {
      return mock(InventoryManager.class);
    }
    @Bean
    PurchaseOrderLineService purchaseOrderLineService() {
      return mock(PurchaseOrderLineService.class);
    }

  }
}
