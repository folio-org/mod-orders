package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.PoNumberHelper;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoNumber;
import org.folio.rest.jaxrs.resource.OrdersPoNumber;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;

public class PoNumberAPI extends BaseApi implements OrdersPoNumber {
  private static final Logger logger = LogManager.getLogger();

  @Autowired
  private PoNumberHelper poNumberHelper;

  public PoNumberAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void getOrdersPoNumber(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {
    logger.debug("Receiving generated poNumber ...");

    poNumberHelper.getPoNumber(new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(result -> asyncResultHandler.handle(succeededFuture(buildOkResponse(result))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postOrdersPoNumberValidate(PoNumber poNumber, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    // @Validate asserts the pattern of a PO Number, the below method is used to
    // check for uniqueness
    poNumberHelper.checkPONumberUnique(poNumber, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(result -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }
}
