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

import io.vertx.ext.unit.junit.RunTestOnContext;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.ws.rs.core.Response;

import org.folio.config.ApplicationConfig;
import org.folio.orders.utils.HelperUtils;
import org.folio.helper.AbstractHelper;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus;
import org.folio.spring.SpringContextUtil;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.runner.RunWith;

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
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class ReceiveOrderStatusChangeHandlerTest extends ApiTestBase {
  private static final Logger logger = LoggerFactory.getLogger(ReceiveOrderStatusChangeHandlerTest.class);

  private static final String PO_ID_OPEN_TO_BE_CLOSED_500_ON_UPDATE = "bad500cc-cccc-500c-accc-cccccccccccc";

  private static Vertx vertx;

  @ClassRule
  public static RunTestOnContext rule = new RunTestOnContext();

  @BeforeClass
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    ApiTestBase.before();
    vertx = rule.vertx();
    SpringContextUtil.init(vertx, vertx.getOrCreateContext(), ApplicationConfig.class);
    vertx.eventBus().consumer(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE.address, new ReceiveOrderStatusChangeHandler(vertx));
  }

  @Test
  public void testUpdateOpenOrderToClosed(TestContext context) {
    logger.info("=== Test case when order status update is expected from Open to Closed ===");
    sendEvent(createBody(PO_ID_OPEN_TO_BE_CLOSED), context.asyncAssertSuccess(result -> {
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
    }));
  }

  @Test
  public void testUpdateNotRequiredForOpenOrder(TestContext context) {
    logger.info("=== Test case when no order update is expected for Open order ===");
    sendEvent(createBody(PO_ID_OPEN_STATUS), context.asyncAssertSuccess(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), hasSize(1));
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
    }));
  }

  @Test
  public void testUpdateClosedOrderToOpen(TestContext context) {
    logger.info("=== Test case when order update is expected for Closed order ===");
    sendEvent(createBody(PO_ID_CLOSED_STATUS), context.asyncAssertSuccess(result -> {
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
    }));
  }

  @Test
  public void testNoUpdatesForPendingOrderWithoutLines(TestContext context) {
    logger.info("=== Test case when no order update is expected for Pending order without lines ===");
    sendEvent(createBody(PO_ID_PENDING_STATUS_WITHOUT_PO_LINES), context.asyncAssertSuccess(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
    }));
  }

  @Test
  public void testNoUpdatesForPendingOrderWithLines(TestContext context) {
    logger.info("=== Test case when no order update is expected for Pending order with a few PO Lines===");
    sendEvent(createBody(PO_ID_PENDING_STATUS_WITH_PO_LINES), context.asyncAssertSuccess(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
    }));
  }

  @Test
  public void testUpdateClosedOrderToOpenAndNoUpdateForOpenOrder(TestContext context) {
    logger.info("=== Test case when order update is expected for Closed order ===");
    sendEvent(createBody(PO_ID_CLOSED_STATUS, PO_ID_OPEN_STATUS), context.asyncAssertSuccess(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(2));
      assertThat(getPoLineSearches(), hasSize(2));
      assertThat(getPurchaseOrderUpdates(), hasSize(1));
      assertThat(getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class).getWorkflowStatus(), is(WorkflowStatus.OPEN));
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
    }));
  }

  @Test
  public void testNonexistentOrder(TestContext context) {
    logger.info("=== Test case when no order update is expected ===");
    sendEvent(createBody(ID_DOES_NOT_EXIST), context.asyncAssertFailure(result -> {
      assertThat(getPurchaseOrderRetrievals(), nullValue());
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(404));
    }));
  }

  @Test
  public void testOrderRetrievalFailure(TestContext context) {
    logger.info("=== Test case when order retrieval fails ===");
    sendEvent(createBody(ID_FOR_INTERNAL_SERVER_ERROR), context.asyncAssertFailure(result -> {
      assertThat(getPurchaseOrderRetrievals(), nullValue());
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(500));
    }));
  }

  @Test
  public void testOrderLinesRetrievalFailure(TestContext context) {
    logger.info("=== Test case when order lines retrieval fails ===");
    sendEvent(createBody(PO_ID_GET_LINES_INTERNAL_SERVER_ERROR), context.asyncAssertFailure(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), nullValue());
      assertThat(getPurchaseOrderUpdates(), nullValue());
      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(500));
    }));
  }

  @Test
  public void testOrderUpdateFailure(TestContext context) {
    logger.info("=== Test case when order update fails ===");
    sendEvent(createBody(PO_ID_OPEN_TO_BE_CLOSED_500_ON_UPDATE), context.asyncAssertFailure(result -> {
      assertThat(getPurchaseOrderRetrievals(), hasSize(1));
      assertThat(getPoLineSearches(), hasSize(1));
      assertThat(getPurchaseOrderUpdates(), hasSize(1));
      assertThat(result, instanceOf(ReplyException.class));
      assertThat(((ReplyException) result).failureCode(), is(500));

      PurchaseOrder purchaseOrder = getPurchaseOrderUpdates().get(0).mapTo(PurchaseOrder.class);
      assertThat(purchaseOrder.getWorkflowStatus(), is(WorkflowStatus.CLOSED));
      assertThat(purchaseOrder.getCloseReason(), notNullValue());
      assertThat(purchaseOrder.getCloseReason().getReason(), equalTo(HelperUtils.REASON_COMPLETE));
    }));
  }

  private JsonObject createBody(String... ids) {
    List<JsonObject> orderObjects =
      Stream.of(ids)
        .map(id -> new JsonObject().put(ORDER_ID, id).put(IS_ITEM_ORDER_CLOSED_PRESENT, false))
        .collect(Collectors.toList());
    return new JsonObject().put(AbstractHelper.EVENT_PAYLOAD, new JsonArray(orderObjects));
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());

    vertx.eventBus().request(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE.address, data, deliveryOptions, replyHandler);
  }
}
