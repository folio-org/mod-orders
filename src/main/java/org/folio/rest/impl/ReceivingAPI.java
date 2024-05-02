package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.handleErrorResponse;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.BindHelper;
import org.folio.helper.CheckinHelper;
import org.folio.helper.ExpectHelper;
import org.folio.helper.ReceivingHelper;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.BindPiecesCollection;
import org.folio.rest.jaxrs.model.CheckinCollection;
import org.folio.rest.jaxrs.model.ExpectCollection;
import org.folio.rest.jaxrs.model.ReceivingCollection;
import org.folio.rest.jaxrs.resource.OrdersBindPieces;
import org.folio.rest.jaxrs.resource.OrdersCheckIn;
import org.folio.rest.jaxrs.resource.OrdersExpect;
import org.folio.rest.jaxrs.resource.OrdersReceive;
import org.folio.rest.jaxrs.resource.OrdersReceivingHistory;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

public class ReceivingAPI implements OrdersReceive, OrdersCheckIn, OrdersExpect, OrdersBindPieces, OrdersReceivingHistory {

  private static final Logger logger = LogManager.getLogger();

  @Override
  @Validate
  public void postOrdersReceive(ReceivingCollection entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Receiving {} items", entity.getTotalRecords());
    ReceivingHelper helper = new ReceivingHelper(entity, okapiHeaders, vertxContext);
    helper.receiveItems(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(result -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(result))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void postOrdersCheckIn(CheckinCollection entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Checkin {} items", entity.getTotalRecords());
    CheckinHelper helper = new CheckinHelper(entity, okapiHeaders, vertxContext);
    helper.checkinPieces(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(result -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(result))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  public void postOrdersExpect(ExpectCollection entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Expect {} pieces", entity.getTotalRecords());
    ExpectHelper helper = new ExpectHelper(entity, okapiHeaders, vertxContext);
    helper.expectPieces(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(result -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(result))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  public void postOrdersBindPieces(BindPiecesCollection entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    logger.info("Bind {} pieces", entity.getTotalRecords());
    BindHelper helper = new BindHelper(entity, okapiHeaders, vertxContext);
    helper.bindPieces(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(result -> asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(result))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getOrdersReceivingHistory(String totalRecords, int offset, int limit, String query, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    ReceivingHelper helper = new ReceivingHelper(okapiHeaders, vertxContext);

    helper.getReceivingHistory(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(receivingHistory -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved receiving history: {} ", JsonObject.mapFrom(receivingHistory).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(receivingHistory)));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }
}
