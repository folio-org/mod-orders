package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.HttpStatus;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.model.RoutingListCollection;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.orders.PurchaseOrderLineService;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static io.vertx.core.Future.succeededFuture;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyDeleteResponse;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.RestTestUtils.verifyPostResponse;
import static org.folio.RestTestUtils.verifyPut;
import static org.folio.RestTestUtils.verifySuccessGet;
import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConfig.autowireDependencies;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.getFirstContextFromVertx;
import static org.folio.TestConfig.getVertx;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.EMPTY_CONFIG_X_OKAPI_TENANT;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.ID_BAD_FORMAT;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestConstants.X_OKAPI_USER_ID_WITH_ACQ_UNITS;
import static org.folio.TestUtils.getLocationPhysicalCopies;
import static org.folio.TestUtils.getMinimalContentPoLine;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.exceptions.ErrorCodes.INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT;
import static org.folio.rest.core.exceptions.ErrorCodes.ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE;
import static org.folio.rest.impl.MockServer.ROUTING_LISTS_MOCK_DATA_PATH;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class RoutingListsApiTest {
  private static final Logger logger = LogManager.getLogger();

  public static final String ROUTING_LISTS_ENDPOINT = "/orders/routing-lists";
  private static final String ROUTING_LISTS_ID_PATH = ROUTING_LISTS_ENDPOINT + "/%s";
  private static final String ROUTING_LIST_UUID = "c0d13648-347b-4ac9-8c2f-5bc47248b87e";
  private static final String PO_LINE_UUID = "0009662b-8b80-4001-b704-ca10971f222d";
  private static final String TEMPLATE_PROCESSING_REQUEST_ENDPOINT = "orders/routing-lists/" + ROUTING_LIST_ID + "/template";

  private final JsonObject routingListJsonReqData = getMockAsJson(ROUTING_LISTS_MOCK_DATA_PATH + "routing-list.json");
  private static boolean runningOnOwn;

  @Autowired
  private RoutingListService routingListService;
  private PurchaseOrderLineService purchaseOrderLineService;
  private RequestContext requestContext;
  private Context ctxMock;
  private Map<String, String> okapiHeadersMock;
  private AutoCloseable mockitoMocks;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(RoutingListsApiTest.ContextConfiguration.class);
  }

  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
    autowireDependencies(this);
    ctxMock = getFirstContextFromVertx(getVertx());
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    requestContext = new RequestContext(ctxMock, okapiHeadersMock);
  }


  @AfterEach
  void afterEach() throws Exception {
    mockitoMocks.close();
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void testPostRoutingList() {
    logger.info("=== Test POST Routing List (Create Routing List) ===");

    PoLine poLine = getMinimalContentPoLine()
      .withId(PO_LINE_UUID)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withLocations(getLocationPhysicalCopies(1));
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));

    RoutingList rListRq = routingListJsonReqData.mapTo(RoutingList.class)
      .withPoLineId(PO_LINE_UUID);

    verifyPostResponse(ROUTING_LISTS_ENDPOINT, JsonObject.mapFrom(rListRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID_WITH_ACQ_UNITS), APPLICATION_JSON, HttpStatus.HTTP_CREATED.toInt());
  }

  @Test
  void testPostRoutingListShouldFailForInvalidOrderType() {
    logger.info("=== Test POST Routing List should fail because it's POL has invalid order type ===");

    PoLine poLine = getMinimalContentPoLine()
      .withId(PO_LINE_UUID)
      .withOrderFormat(PoLine.OrderFormat.ELECTRONIC_RESOURCE);
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));

    RoutingList rListRq = routingListJsonReqData.mapTo(RoutingList.class)
      .withPoLineId(PO_LINE_UUID);

    List<Error> errors = verifyPostResponse(ROUTING_LISTS_ENDPOINT, JsonObject.mapFrom(rListRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422)
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT.getDescription()));
  }

  @Test
  void testPostRoutingListShouldFailForLimitReached() {
    logger.info("=== Test POST Routing List should fail because it's POL has reached limit of Routing Lists ===");

    PoLine poLine = getMinimalContentPoLine()
      .withId(PO_LINE_UUID)
      .withOrderFormat(PoLine.OrderFormat.PHYSICAL_RESOURCE)
      .withLocations(getLocationPhysicalCopies(0));
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));

    RoutingList rListRq = routingListJsonReqData.mapTo(RoutingList.class)
      .withPoLineId(PO_LINE_UUID);

    List<Error> errors = verifyPostResponse(ROUTING_LISTS_ENDPOINT, JsonObject.mapFrom(rListRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 422
    )
      .as(Errors.class)
      .getErrors();

    assertThat(errors.get(0).getMessage(), equalTo(ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE.getDescription()));
  }

  @Test
  void testPostRoutingListWithInvalidPoLineId() {
    logger.info("=== Test POST Routing List should fail because it's POL does not exist ===");

    RoutingList rListRq = routingListJsonReqData.mapTo(RoutingList.class)
      .withPoLineId(PO_LINE_UUID);

    verifyPostResponse(ROUTING_LISTS_ENDPOINT, JsonObject.mapFrom(rListRq).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10, X_OKAPI_USER_ID), APPLICATION_JSON, 404);
  }

  @Test
  void testGetRoutingLists() {
    logger.info("=== Test Get Routing Lists  ===");

    final RoutingListCollection respCollection = verifySuccessGet(ROUTING_LISTS_ENDPOINT, RoutingListCollection.class);
    logger.info(JsonObject.mapFrom(respCollection).encodePrettily());
    assertEquals(1, respCollection.getRoutingLists().size());
  }

  @Test
  void testGetRoutingListById() {
    logger.info("=== Test Get Routing Lists  ===");

    final RoutingList resp = verifySuccessGet(String.format(ROUTING_LISTS_ID_PATH, ROUTING_LIST_UUID), RoutingList.class);
    logger.info(JsonObject.mapFrom(resp).encodePrettily());
    assertEquals(ROUTING_LIST_UUID, resp.getId());
  }

  @Test
  void testPutRoutingList() {
    logger.info("=== Test update Routing List by id ===");
    RoutingList reqData = routingListJsonReqData.mapTo(RoutingList.class)
      .withNotes("new notes");

     verifyPut(String.format(ROUTING_LISTS_ID_PATH, ROUTING_LIST_UUID), JsonObject.mapFrom(reqData).encode(),
      prepareHeaders(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10), "", 204);
  }

  @Test
  void testPutRoutingListByNonExistentId() {
    logger.info("=== Test update Routing List by invalid id ===");

    RoutingList reqData = routingListJsonReqData.mapTo(RoutingList.class)
      .withId(ID_DOES_NOT_EXIST);
    verifyPut(String.format(ROUTING_LISTS_ID_PATH, ID_DOES_NOT_EXIST), JsonObject.mapFrom(reqData).encode(), APPLICATION_JSON, 404);
  }

  @Test
  void deleteRoutingListByIdTest() {
    logger.info("=== Test delete Routing List by id ===");

    verifyDeleteResponse(String.format(ROUTING_LISTS_ID_PATH, ROUTING_LIST_UUID), "", 204);
  }

  @Test
  void deleteRoutingListByIdWithInvalidFormatTest() {
    logger.info("=== Test delete Routing List by id ===");
    verifyDeleteResponse(String.format(ROUTING_LISTS_ID_PATH, ID_BAD_FORMAT), APPLICATION_JSON, 400);
  }

  @Test
  void deleteNotExistentRoutingListTest() {
    logger.info("=== Test delete Routing List by id ===");
    verifyDeleteResponse(String.format(ROUTING_LISTS_ID_PATH, ID_DOES_NOT_EXIST), APPLICATION_JSON, 404);
  }

  @Test
  void testProcessTemplateRequest() {
    logger.info("=== Test Execute template processing request ===");

    doReturn(succeededFuture(new JsonObject())).when(routingListService).processTemplateRequest(eq(ROUTING_LIST_ID), any(RequestContext.class));

    verifyGet(TEMPLATE_PROCESSING_REQUEST_ENDPOINT, prepareHeaders(X_OKAPI_URL, EMPTY_CONFIG_X_OKAPI_TENANT),
      APPLICATION_JSON, 200);

    verify(routingListService, times(1)).processTemplateRequest(eq(ROUTING_LIST_ID), any(RequestContext.class));
  }

  static class ContextConfiguration {

    @Bean
    public RoutingListService routingListService() {
      return mock(RoutingListService.class);
    }

  }

}
