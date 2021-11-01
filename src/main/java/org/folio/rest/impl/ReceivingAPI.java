package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.handleErrorResponse;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.CheckinHelper;
import org.folio.helper.ReceivingHelper;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.resource.OrdersCheckIn;
import org.folio.rest.jaxrs.resource.OrdersReceive;
import org.folio.rest.jaxrs.resource.OrdersReceivingHistory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

public class ReceivingAPI implements OrdersReceive, OrdersCheckIn, OrdersReceivingHistory {

  private static final Logger logger = LogManager.getLogger();

  @Override
  @Validate
  public void postOrdersReceive(String lang, ReceivingCollection entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Receiving {} items", entity.getTotalRecords());
    ReceivingHelper helper = new ReceivingHelper(entity, okapiHeaders, vertxContext, lang);
    helper.receiveItems(entity, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(result -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(result))))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersCheckIn(String lang, CheckinCollection entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Checkin {} items", entity.getTotalRecords());
    CheckinHelper helper = new CheckinHelper(entity, okapiHeaders, vertxContext, lang);
    helper.checkinPieces(entity, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(result -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(result))))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersReceivingHistory(int offset, int limit, String query, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    ReceivingHelper helper = new ReceivingHelper(okapiHeaders, vertxContext, lang);

    helper.getReceivingHistory(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(receivingHistory -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved receiving history: {} ", JsonObject.mapFrom(receivingHistory).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(receivingHistory)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }
}
