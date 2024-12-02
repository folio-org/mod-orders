package org.folio.orders.events.handlers;

import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.awaitility.Awaitility.await;
import static org.folio.helper.BaseHelper.EVENT_PAYLOAD;
import static org.folio.orders.events.utils.EventUtils.POL_UPDATE_FIELD;
import static org.folio.rest.impl.EventBusContextConfiguration.eventMessages;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.emptyIterable;
import static org.hamcrest.Matchers.emptyOrNullString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.iterableWithSize;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;

import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;

public class HandlersTestHelper {

  private static final Logger logger = LogManager.getLogger();

  public static void verifyCheckinOrderStatusUpdateEvent(int msgQty) {
    // logger.debug("Verifying event bus messages");
    // Wait until event bus registers message
    await().atLeast(50, MILLISECONDS)
      .atMost(5, SECONDS)
      .until(() -> eventMessages, hasSize(msgQty));
    for (int i = 0; i < msgQty; i++) {
      Message<JsonObject> message = eventMessages.get(i);
      assertThat(message.address(), equalTo(MessageAddress.CHECKIN_ORDER_STATUS_UPDATE.address));
      assertThat(message.headers(), not(emptyIterable()));
      assertThat(message.body(), notNullValue());
      assertThat(message.body()
        .getJsonArray(EVENT_PAYLOAD), iterableWithSize(1));
    }
  }

  public static void verifyOrderStatusUpdateEvent(int msgQty) {
    logger.debug("Verifying event bus messages");
    // Wait until event bus registers message

    await().atLeast(100, MILLISECONDS)
      .atMost(20, SECONDS)
      .until(() -> eventMessages, hasSize(msgQty));
    for (int i = 0; i < msgQty; i++) {
      Message<JsonObject> message = eventMessages.get(i);
      assertThat(message.address(), equalTo(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE.address));
      assertThat(message.headers(), not(emptyIterable()));
      assertThat(message.body(), notNullValue());
      assertThat(message.body()
        .getJsonArray(EVENT_PAYLOAD), iterableWithSize(1));
    }
  }

  public static void verifyReceiptStatusUpdateEvent(int msgQty) {
    logger.debug("Verifying event bus messages");
    // Wait until event bus registers message
    await().atLeast(50, MILLISECONDS)
      .atMost(5, SECONDS)
      .until(() -> eventMessages, hasSize(msgQty));
    for (int i = 0; i < msgQty; i++) {
      Message<JsonObject> message = eventMessages.get(i);
      assertThat(message.address(), equalTo(MessageAddress.RECEIPT_STATUS.address));
      assertThat(message.headers(), not(emptyIterable()));
      assertThat(message.body(), notNullValue());
      assertThat(message.body()
        .getString(POL_UPDATE_FIELD), not(is(emptyOrNullString())));
      assertThat(message.body()
        .getString(HelperUtils.LANG), not(is(emptyOrNullString())));
    }
  }
}
