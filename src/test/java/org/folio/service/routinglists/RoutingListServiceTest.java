package org.folio.service.routinglists;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.TestConstants.ROUTING_LIST_ID;
import static org.folio.TestUtils.getLocationPhysicalCopies;
import static org.folio.TestUtils.getMinimalContentPoLine;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.rest.core.exceptions.ErrorCodes.INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT;
import static org.folio.rest.core.exceptions.ErrorCodes.ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE;
import static org.folio.rest.impl.MockServer.ROUTING_LISTS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.USERS_MOCK_DATA_PATH;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.folio.models.UserCollection;
import org.folio.rest.acq.model.Setting;
import org.folio.rest.acq.model.SettingCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.model.RoutingListCollection;
import org.folio.service.UserService;
import org.folio.service.orders.PurchaseOrderLineService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class RoutingListServiceTest {

  private static final String ROUTING_LIST_SAMPLE = ROUTING_LISTS_MOCK_DATA_PATH + "routing-list.json";

  private static final String PO_LINE_UUID = "0009662b-8b80-4001-b704-ca10971f222d";

  private PoLine samplePoLine;
  private RoutingList sampleRoutingList;

  @Mock
  private RestClient restClient;

  @Mock
  private PurchaseOrderLineService poLineService;

  @Mock
  private UserService userService;

  @Mock
  private RequestContext requestContextMock;

  @InjectMocks
  private RoutingListService routingListService;

  private AutoCloseable mockitoMocks;

  @BeforeEach
  void before() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
    sampleRoutingList = getMockAsJson(ROUTING_LIST_SAMPLE).mapTo(RoutingList.class);
    samplePoLine = getMinimalContentPoLine()
      .withId(PO_LINE_UUID)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withLocations(getLocationPhysicalCopies(1));
  }

  @AfterEach
  void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void testCreateRoutingList(VertxTestContext vertxTestContext) {
    doReturn(succeededFuture(getRoutingListCollection(0))).when(restClient).get(any(RequestEntry.class), eq(RoutingListCollection.class), any());
    doReturn(succeededFuture(sampleRoutingList)).when(restClient).post(any(RequestEntry.class), any(RoutingList.class), eq(RoutingList.class), any());
    doReturn(succeededFuture(samplePoLine)).when(poLineService).getOrderLineById(any(), any());

    Future<RoutingList> future = routingListService.createRoutingList(sampleRoutingList, requestContextMock);
    vertxTestContext.assertComplete(future).onComplete(result -> {
      assertTrue(result.succeeded());
      assertEquals(sampleRoutingList.getId(), result.result().getId());
      vertxTestContext.completeNow();
    });
  }

  @Test
  void testCreateRoutingListWithPOLineLimitReached(VertxTestContext vertxTestContext) {
    doReturn(succeededFuture(getRoutingListCollection(1))).when(restClient).get(any(RequestEntry.class), eq(RoutingListCollection.class), any());
    doReturn(succeededFuture(samplePoLine)).when(poLineService).getOrderLineById(any(), any());

    var distinctRoutingList = getMockAsJson(ROUTING_LIST_SAMPLE).mapTo(RoutingList.class).withId(UUID.randomUUID().toString());
    Future<RoutingList> future = routingListService.createRoutingList(distinctRoutingList, requestContextMock);
    vertxTestContext.assertFailure(future).onComplete(result -> {
      assertTrue(result.failed());
      var exception = result.cause().getMessage();
      assertTrue(exception.contains(ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE.getDescription()));
      vertxTestContext.completeNow();
    });

  }

  @Test
  void testCreateRoutingListWithPOLineInvalidOrderFormat(VertxTestContext vertxTestContext) {
    samplePoLine.setOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    doReturn(succeededFuture(getRoutingListCollection(0))).when(restClient).get(any(RequestEntry.class), eq(RoutingListCollection.class), any());
    doReturn(succeededFuture(samplePoLine)).when(poLineService).getOrderLineById(any(), any());

    Future<RoutingList> future = routingListService.createRoutingList(sampleRoutingList, requestContextMock);
    vertxTestContext.assertFailure(future).onComplete(result -> {
      assertTrue(result.failed());
      var exception = result.cause().getMessage();
      assertTrue(exception.contains(INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT.getDescription()));
      vertxTestContext.completeNow();
    });
  }

  @Test
  void processTemplate(VertxTestContext vertxTestContext) throws IOException {
    var routingList = new JsonObject(getMockData(ROUTING_LISTS_MOCK_DATA_PATH + ROUTING_LIST_ID + ".json")).mapTo(RoutingList.class);
    var users = new JsonObject(getMockData(USERS_MOCK_DATA_PATH + "user_collection.json")).mapTo(UserCollection.class);
    var expectedTemplateRequest = new JsonObject(getMockData(ROUTING_LISTS_MOCK_DATA_PATH + ROUTING_LIST_ID + "-expected-template-request.json"));
    var setting = new Setting().withId(UUID.randomUUID().toString())
      .withKey("routing-list")
      .withValue("1c4b225f-f669-4e9b-afcd-ebc0e273a34e");
    var settingCollection = new SettingCollection().withSettings(List.of(setting));

    doReturn(succeededFuture(routingList)).when(restClient).get(any(RequestEntry.class), eq(RoutingList.class), any());
    doReturn(succeededFuture(samplePoLine)).when(poLineService).getOrderLineById(any(), any());
    doReturn(succeededFuture(users)).when(userService).getUsersByIds(eq(routingList.getUserIds()), any());
    doReturn(succeededFuture(settingCollection)).when(restClient).get(any(RequestEntry.class), eq(SettingCollection.class), any());
    doReturn(succeededFuture(new JsonObject())).when(restClient).postJsonObject(any(RequestEntry.class), eq(expectedTemplateRequest), any());

    Future<JsonObject> future = routingListService.processTemplateRequest(ROUTING_LIST_ID, requestContextMock);

    vertxTestContext.assertComplete(future).onComplete(result -> {
      assertTrue(result.succeeded());
      vertxTestContext.completeNow();
    });
  }

  @Test
  void throwErrorWhenSettingNotFound(VertxTestContext vertxTestContext) throws IOException {
    var routingList = new JsonObject(getMockData(ROUTING_LISTS_MOCK_DATA_PATH + ROUTING_LIST_ID + ".json")).mapTo(RoutingList.class);
    var users = new JsonObject(getMockData(USERS_MOCK_DATA_PATH + "user_collection.json")).mapTo(UserCollection.class);

    doReturn(succeededFuture(routingList)).when(restClient).get(any(RequestEntry.class), eq(RoutingList.class), any());
    doReturn(succeededFuture(samplePoLine)).when(poLineService).getOrderLineById(any(), any());
    doReturn(succeededFuture(users)).when(userService).getUsersByIds(eq(routingList.getUserIds()), any());
    doReturn(succeededFuture(new SettingCollection().withSettings(new ArrayList<>())))
      .when(restClient).get(any(RequestEntry.class), eq(SettingCollection.class), any());

    Future<JsonObject> future = routingListService.processTemplateRequest(ROUTING_LIST_ID, requestContextMock);

    vertxTestContext.assertFailure(future).onComplete(result -> {
      assertTrue(result.failed());
      var exception = result.cause().getMessage();
      assertTrue(exception.contains("Setting is not found with key=ROUTING_USER_ADDRESS_TYPE_ID"));
      vertxTestContext.completeNow();
    });
  }

  private RoutingListCollection getRoutingListCollection(int n) {
    List<RoutingList> lists = new ArrayList<>();
    for (int i = 0; i < n; i++) {
      lists.add(sampleRoutingList);
    }
    return new RoutingListCollection()
      .withRoutingLists(lists)
      .withTotalRecords(n);
  }

}
