package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.helper.PoNumberHelper;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.jaxrs.resource.OrdersPoNumber;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class PoNumberAPI implements OrdersPoNumber {

  private static final Logger logger = LoggerFactory.getLogger(PoNumberAPI.class);

  @Override
  @Validate
  public void getOrdersPoNumber(String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {
    logger.info("Receiving generated poNumber ...");

    new PoNumberHelper(okapiHeaders, vertxContext, lang).getPoNumber()
      .thenAccept(response -> asyncResultHandler.handle(succeededFuture(response)));
  }

  @Override
  @Validate
  public void postOrdersPoNumberValidate(String lang, PoNumber poNumber, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PoNumberHelper helper = new PoNumberHelper(okapiHeaders, vertxContext, lang);
    logger.info("Validating a PO Number");

    // @Validate asserts the pattern of a PO Number, the below method is used to
    // check for uniqueness
    helper.checkPONumberUnique(poNumber)
      .thenAccept(response -> asyncResultHandler.handle(succeededFuture(response)));
  }
}
