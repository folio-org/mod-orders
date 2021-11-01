package org.folio.orders.events.handlers;

import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConfig.mockPort;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.PO_ID_CLOSED_STATUS;
import static org.folio.TestConstants.PO_ID_GET_LINES_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.PO_ID_OPEN_STATUS;
import static org.folio.TestConstants.PO_ID_OPEN_TO_BE_CLOSED;
import static org.folio.TestConstants.PO_ID_PENDING_STATUS_WITHOUT_PO_LINES;
import static org.folio.TestConstants.PO_ID_PENDING_STATUS_WITH_PO_LINES;
import static org.folio.TestConstants.X_OKAPI_TOKEN;
import static org.folio.TestConstants.X_OKAPI_USER_ID;
import static org.folio.TestUtils.checkVertxContextCompletion;
import static org.folio.helper.AbstractHelper.ORDER_ID;
import static org.folio.helper.CheckinHelper.IS_ITEM_ORDER_CLOSED_PRESENT;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.impl.MockServer.ITEM_RECORDS;
import static org.folio.rest.impl.MockServer.getItemUpdates;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPurchaseOrderRetrievals;
import static org.folio.rest.impl.MockServer.getPurchaseOrderUpdates;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.folio.rest.impl.PurchaseOrdersApiTest.X_OKAPI_TENANT;
import static org.folio.service.inventory.InventoryManager.ITEMS;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.helper.CheckinHelper;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus;
import org.folio.service.finance.transaction.EncumbranceService;
import org.folio.service.orders.PurchaseOrderStorageService;
import org.folio.spring.SpringContextUtil;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.ReplyException;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class CheckInOrderStatusChangeChangeHandlerTest {
  private static final Logger logger = LogManager.getLogger();
  private static final String PO_ID_OPEN_TO_BE_CLOSED_500_ON_UPDATE = "bad500cc-cccc-500c-accc-cccccccccccc";
  private static boolean runningOnOwn;
  private static Vertx vertx;

  @Autowired
  private EncumbranceService encumbranceService;
  @Autowired
  private PurchaseOrderStorageService purchaseOrderStorageService;
  @Autowired
  private PurchaseOrderHelper purchaseOrderHelper;

  HashMap<String, String> okapiHeadersMock;

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    vertx = Vertx.vertx();
    SpringContextUtil.init(vertx, vertx.getOrCreateContext(), ApplicationConfig.class);
  }

  @BeforeEach
  void initMocks(){
    okapiHeadersMock = new HashMap<>();
    okapiHeadersMock.put(OKAPI_URL, "http://localhost:" + mockPort);
    okapiHeadersMock.put(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    okapiHeadersMock.put(X_OKAPI_TENANT.getName(), X_OKAPI_TENANT.getValue());
    okapiHeadersMock.put(X_OKAPI_USER_ID.getName(), X_OKAPI_USER_ID.getValue());
    String okapiURL = okapiHeadersMock.getOrDefault(OKAPI_URL, "");
    SpringContextUtil.autowireDependencies(this, vertx.getOrCreateContext());
    vertx.eventBus().consumer(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE.address, new CheckInOrderStatusChangeChangeHandler(vertx, encumbranceService,
                          purchaseOrderStorageService, purchaseOrderHelper));
  }

  @AfterEach
  void afterEach() {
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }

  @Test
  void testUpdateOpenOrderToClosed(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order status update is expected from Open to Closed ===");
    sendEvent(createBody(false, PO_ID_OPEN_TO_BE_CLOSED), context.succeeding(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), hasSize(1));
      assertThat(getPurchaseOrderUpdates(), hasSize(1));

      PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
      assertThat(purchaseOrder.getWorkflowStatus(), is(WorkflowStatus.CLOSED));
      assertThat(purchaseOrder.getCloseReason(), notNullValue());
      assertThat(purchaseOrder.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));

      assertThat(getItemsSearches(), notNullValue());
      assertThat(getItemsSearches(), hasSize(1));
      assertThat(getItemUpdates(), notNullValue());
      assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

      assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
      assertThat(getQueryParams(ITEM_RECORDS).get(0), containsString("status.name==On order"));
      context.completeNow();
    }));

    checkVertxContextCompletion(context);
  }



  @Test
  void testUpdateNotRequiredForOpenOrder(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no order update is expected for Open order ===");
     sendEvent(createBody(false, PO_ID_OPEN_STATUS), context.succeeding(result -> {
      context.verify(() -> {
        assertThat(getPurchaseOrderRetrievals(), hasSize(1));
        assertThat(getPoLineSearches(), hasSize(1));
        assertThat(getPurchaseOrderUpdates(), nullValue());
        assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      });
      context.completeNow();

    }));

    checkVertxContextCompletion(context);
  }

  @Test
  void testUpdateClosedOrderToOpen(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order update is expected for Closed order ===");
    sendEvent(createBody(false, PO_ID_CLOSED_STATUS), context.succeeding(result -> {
      context.verify(() -> {
        assertThat(getPurchaseOrderRetrievals(), hasSize(1));
        assertThat(getPoLineSearches(), hasSize(1));
        assertThat(getPurchaseOrderUpdates(), hasSize(1));
        assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(WorkflowStatus.OPEN));
        assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));

        assertThat(getItemsSearches(), notNullValue());
        assertThat(getItemsSearches(), hasSize(1));
        assertThat(getItemUpdates(), notNullValue());
        assertThat(getItemUpdates(), hasSize(getItemsSearches().get(0).getJsonArray(ITEMS).size()));

        assertThat(getQueryParams(ITEM_RECORDS), hasSize(1));
        assertThat(getQueryParams(ITEM_RECORDS).get(0), containsString("status.name==Order closed"));
      });

      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testNoUpdatesForPendingOrderWithoutLines(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no order update is expected for Pending order without lines ===");
    sendEvent(createBody(false, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES), context.succeeding(result -> {
      context.verify(() -> {
                assertThat(getPurchaseOrderRetrievals(), hasSize(1));
                assertThat(getPoLineSearches(), nullValue());
                assertThat(getPurchaseOrderUpdates(), nullValue());
                assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
              });
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testNoUpdatesForPendingOrderWithLines(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no order update is expected for Pending order with a few PO Lines===");
    sendEvent(createBody(false, PO_ID_PENDING_STATUS_WITH_PO_LINES), context.succeeding(result -> {
      context.verify(() -> {
        assertThat(getPurchaseOrderRetrievals(), hasSize(1));
        assertThat(getPoLineSearches(), nullValue());
        assertThat(getPurchaseOrderUpdates(), nullValue());
        assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      });
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testUpdateClosedOrderToOpenAndNoUpdateForOpenOrder(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order update is expected for Closed order ===");
    sendEvent(createBody(false, PO_ID_CLOSED_STATUS, PO_ID_OPEN_STATUS), context.succeeding(result -> {
      context.verify(() -> {
        assertThat(getPurchaseOrderRetrievals(), hasSize(2));
        assertThat(getPoLineSearches(), hasSize(2));
        assertThat(getPurchaseOrderUpdates(), hasSize(1));
        assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(WorkflowStatus.OPEN));
        assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      });

      context.completeNow();
    }));
    checkVertxContextCompletion(context);

  }

  @Test
  void testClosedOrderStillCloseAndNoUpdateForOpenOrder(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order update is expected for Closed order ===");
    sendEvent(createBody(true, PO_ID_CLOSED_STATUS), context.succeeding(result -> {
      context.verify(() -> {
        assertThat(getPurchaseOrderRetrievals(), hasSize(1));
        assertThat(getPoLineSearches(), nullValue());
        assertThat(getPurchaseOrderUpdates(), nullValue());
        assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      });
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testNonexistentOrder(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no order update is expected ===");
    sendEvent(createBody(false, ID_DOES_NOT_EXIST), context.failing(result -> {
      context.verify(() -> {
        assertThat(getPurchaseOrderRetrievals(), nullValue());
        assertThat(getPoLineSearches(), nullValue());
        assertThat(getPurchaseOrderUpdates(), nullValue());
        assertThat(result, instanceOf(ReplyException.class));
        assertThat(((ReplyException) result).failureCode(), is(404));
      });

      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testOrderRetrievalFailure(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order retrieval fails ===");
    sendEvent(createBody(false, ID_FOR_INTERNAL_SERVER_ERROR), context.failing(result -> {
      context.verify(() -> {
        assertThat(getPurchaseOrderRetrievals(), nullValue());
        assertThat(getPoLineSearches(), nullValue());
        assertThat(getPurchaseOrderUpdates(), nullValue());
        assertThat(result, instanceOf(ReplyException.class));
        assertThat(((ReplyException) result).failureCode(), is(500));
      });

      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testOrderLinesRetrievalFailure(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order lines retrieval fails ===");
    sendEvent(createBody(false, PO_ID_GET_LINES_INTERNAL_SERVER_ERROR), context.failing(result -> {
      context.verify(() -> {
        assertThat(getPurchaseOrderRetrievals(), hasSize(1));
        assertThat(getPoLineSearches(), nullValue());
        assertThat(getPurchaseOrderUpdates(), nullValue());
        assertThat(result, instanceOf(ReplyException.class));
        assertThat(((ReplyException) result).failureCode(), is(500));
      });
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  void testOrderUpdateFailure(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order update fails ===");
    sendEvent(createBody(false, PO_ID_OPEN_TO_BE_CLOSED_500_ON_UPDATE), context.failing(result -> {

        context.verify(() -> {
          assertThat(getPurchaseOrderRetrievals(), hasSize(1));
          assertThat(getPoLineSearches(), hasSize(1));
          assertThat(getPurchaseOrderUpdates(), hasSize(1));
          assertThat(result, instanceOf(ReplyException.class));
          assertThat(((ReplyException) result).failureCode(), is(500));

          PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
          assertThat(purchaseOrder.getWorkflowStatus(), is(WorkflowStatus.CLOSED));
          assertThat(purchaseOrder.getCloseReason(), notNullValue());
          assertThat(purchaseOrder.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));
        });

        context.completeNow();

    }));
    checkVertxContextCompletion(context);
  }

  private JsonObject createBody(boolean isItemOrderClose, String... ids) {
    List<JsonObject> orderObjects =
      Stream.of(ids)
            .map(id -> new JsonObject().put(ORDER_ID, id).put(IS_ITEM_ORDER_CLOSED_PRESENT, isItemOrderClose))
            .collect(Collectors.toList());
    return new JsonObject().put(CheckinHelper.EVENT_PAYLOAD, new JsonArray(orderObjects));
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());

    vertx.eventBus().request(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE.address, data, deliveryOptions, replyHandler);
  }
}
