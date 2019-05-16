package org.folio.orders.events.handlers;

import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.rest.impl.MockServer.getPurchaseOrderRetrievals;
import static org.folio.rest.impl.MockServer.getPurchaseOrderUpdates;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import java.util.Arrays;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import javax.ws.rs.core.Response;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.impl.AbstractHelper;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrder.WorkflowStatus;
import org.junit.BeforeClass;
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
public class OrderStatusTest extends ApiTestBase {
  private static final Logger logger = LoggerFactory.getLogger(OrderStatusTest.class);

  private static final String TEST_ADDRESS = "testAddress";
  private static final String PO_ID_OPEN_TO_BE_CLOSED = "9d56b621-202d-414b-9e7f-5fefe4422ab3";
  private static final String PO_ID_OPEN_TO_BE_CLOSED_500_ON_UPDATE = "bad500cc-cccc-500c-accc-cccccccccccc";

  private static Vertx vertx;

  @BeforeClass
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    ApiTestBase.before();

    vertx = Vertx.vertx();
    vertx.eventBus().consumer(TEST_ADDRESS, new OrderStatus(vertx));
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
    return new JsonObject().put(AbstractHelper.ORDER_IDS, new JsonArray(Arrays.asList(ids)));
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());

    vertx.eventBus().send(TEST_ADDRESS, data, deliveryOptions, replyHandler);
  }
}
