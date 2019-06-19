package org.folio.orders.events.handlers;

import static org.folio.rest.impl.MockServer.getPoLineSearches;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.rest.impl.MockServer.getPieceUpdates;
import static org.folio.rest.impl.MockServer.getPurchaseOrderUpdates;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

import org.folio.rest.impl.AbstractHelper;
import org.folio.rest.impl.ApiTestBase;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ws.rs.core.Response;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.ReplyException;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

@RunWith(VertxUnitRunner.class)
public class ReceiptStatusConsistencyTest extends ApiTestBase {
  private static final Logger logger = LoggerFactory.getLogger(ReceiptStatusConsistencyTest.class);

  private static final String TEST_ADDRESS = "testReceiptStatusAddress";

  private static Vertx vertx;

  @BeforeClass
  public static void before() throws InterruptedException, ExecutionException, TimeoutException {
    ApiTestBase.before();

    vertx = Vertx.vertx();
    vertx.eventBus().consumer(TEST_ADDRESS, new ReceiptStatusConsistency(vertx));
  }

  @Test
  public void testReceiptStatusReceivedToExpected(TestContext context) {
    logger.info("=== Test case when piece receipt status changes from Received to Expected ===");
    sendEvent(createBody("RECEIVED"), context.asyncAssertSuccess(result -> {
      //serverRqRs.get(PIECES, HttpMethod.PUT);
      //assertThat(getPieceUpdates(), hasSize(1));
      //assertThat(getPurchaseOrderUpdates(), hasSize(1));
      assertThat(result.body(), equalTo(Response.Status.OK.getReasonPhrase()));
    }));
  }

  private JsonObject createBody(String receiptStatus) {
    JsonObject jsonObj = new JsonObject();

    // explicitly setting RECEIVED to create inconsistency for testing
    jsonObj.put("receivingStatusBeforeUpdate", receiptStatus);
    jsonObj.put("pieceIdUpdate", "af372ac8-5ffb-4560-8b96-3945a12e121b");
    return jsonObj;
  }

  private void sendEvent(JsonObject data, Handler<AsyncResult<Message<String>>> replyHandler) {
    // Add okapi url header
    DeliveryOptions deliveryOptions = new DeliveryOptions().addHeader(X_OKAPI_URL.getName(), X_OKAPI_URL.getValue());
    deliveryOptions.addHeader(X_OKAPI_TOKEN.getName(), X_OKAPI_TOKEN.getValue());
    deliveryOptions.addHeader(EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10.getName(),EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10.getValue());
    
    vertx.eventBus().send(TEST_ADDRESS, data, deliveryOptions, replyHandler);
  }
}
