package org.folio.orders.events.handlers;

import static org.folio.helper.AbstractHelper.ORDER_ID;
import static org.folio.helper.CheckinHelper.IS_ITEM_ORDER_CLOSED_PRESENT;
import static org.folio.helper.InventoryHelper.ITEMS;
import static org.folio.rest.impl.MockServer.ITEM_RECORDS;
import static org.folio.rest.impl.MockServer.getItemUpdates;
import static org.folio.rest.impl.MockServer.getItemsSearches;
import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPurchaseOrderRetrievals;
import static org.folio.rest.impl.MockServer.getPurchaseOrderUpdates;
import static org.folio.rest.impl.MockServer.getQueryParams;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.ws.rs.core.Response;

import org.folio.config.ApplicationConfig;
import org.folio.helper.CheckinHelper;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus;
import org.folio.spring.SpringContextUtil;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.ReplyException;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class CheckInOrderStatusChangeChangeHandlerTest extends ApiTestBase {
  private static final Logger logger = LoggerFactory.getLogger(CheckInOrderStatusChangeChangeHandlerTest.class);

  private static final String PO_ID_OPEN_TO_BE_CLOSED_500_ON_UPDATE = "bad500cc-cccc-500c-accc-cccccccccccc";


  private static Vertx vertx;


  @BeforeAll
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    ApiTestBase.before();
    vertx = Vertx.vertx();
    SpringContextUtil.init(vertx, vertx.getOrCreateContext(), ApplicationConfig.class);
    vertx.eventBus().consumer(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE.address, new CheckInOrderStatusChangeChangeHandler(vertx));
  }

  @Test
  public void testUpdateOpenOrderToClosed(VertxTestContext context) throws Throwable {
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
  public void testUpdateNotRequiredForOpenOrder(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no order update is expected for Open order ===");
    sendEvent(createBody(false, PO_ID_OPEN_STATUS), context.succeeding(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), hasSize(1));
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testUpdateClosedOrderToOpen(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order update is expected for Closed order ===");
    sendEvent(createBody(false, PO_ID_CLOSED_STATUS), context.succeeding(result -> {
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
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testNoUpdatesForPendingOrderWithoutLines(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no order update is expected for Pending order without lines ===");
    sendEvent(createBody(false, PO_ID_PENDING_STATUS_WITHOUT_PO_LINES), context.succeeding(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testNoUpdatesForPendingOrderWithLines(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no order update is expected for Pending order with a few PO Lines===");
    sendEvent(createBody(false, PO_ID_PENDING_STATUS_WITH_PO_LINES), context.succeeding(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testUpdateClosedOrderToOpenAndNoUpdateForOpenOrder(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order update is expected for Closed order ===");
    sendEvent(createBody(false, PO_ID_CLOSED_STATUS, PO_ID_OPEN_STATUS), context.succeeding(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(2));
      assertThat(getPoLineSearches(), hasSize(2));
      assertThat(getPurchaseOrderUpdates(), hasSize(1));
      assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(WorkflowStatus.OPEN));
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testClosedOrderStillCloseAndNoUpdateForOpenOrder(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order update is expected for Closed order ===");
    sendEvent(createBody(true, PO_ID_CLOSED_STATUS), context.succeeding(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testNonexistentOrder(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when no order update is expected ===");
    sendEvent(createBody(false, ID_DOES_NOT_EXIST), context.failing(result -> {
      assertThat(getPurchaseOrderRetrievals(), nullValue());
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(404));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testOrderRetrievalFailure(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order retrieval fails ===");
    sendEvent(createBody(false, ID_FOR_INTERNAL_SERVER_ERROR), context.failing(result -> {
      assertThat(getPurchaseOrderRetrievals(), nullValue());
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(500));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testOrderLinesRetrievalFailure(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order lines retrieval fails ===");
    sendEvent(createBody(false, PO_ID_GET_LINES_INTERNAL_SERVER_ERROR), context.failing(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(500));
      context.completeNow();
    }));
    checkVertxContextCompletion(context);
  }

  @Test
  public void testOrderUpdateFailure(VertxTestContext context) throws Throwable {
    logger.info("=== Test case when order update fails ===");
    sendEvent(createBody(false, PO_ID_OPEN_TO_BE_CLOSED_500_ON_UPDATE), context.failing(result -> {
      try {
        assertThat(getPurchaseOrderRetrievals(), hasSize(1));
        assertThat(getPoLineSearches(), hasSize(1));
        assertThat(getPurchaseOrderUpdates(), hasSize(1));
        assertThat(result, instanceOf(ReplyException.class));
        assertThat(((ReplyException) result).failureCode(), is(500));

        PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
        assertThat(purchaseOrder.getWorkflowStatus(), is(WorkflowStatus.CLOSED));
        assertThat(purchaseOrder.getCloseReason(), notNullValue());
        assertThat(purchaseOrder.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));
        context.completeNow();
      } catch (AssertionError e) {
        context.failNow(e);
      }
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
