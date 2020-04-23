package org.folio.orders.events.handlers;

import static org.folio.orders.utils.HelperUtils.getOkapiHeaders;

import java.util.Map;

import org.folio.rest.impl.AbstractHelper;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public abstract class AbstractOrderStatusHandler extends AbstractHelper implements Handler<Message<JsonObject>> {

  @Autowired
  public AbstractOrderStatusHandler(Vertx vertx) {
    super(vertx.getOrCreateContext());
  }

  protected JsonArray messageAsJsonArray(String rootElement, Message<JsonObject> message) {
    JsonObject body = message.body();
    logger.debug("Received message body: {}", body);
    return body.getJsonArray(rootElement);
  }

  protected HttpClientInterface getHttpClient(Message<JsonObject> message) {
    Map<String, String> okapiHeaders = getOkapiHeaders(message);
    return getHttpClient(okapiHeaders, true);
  }
}
