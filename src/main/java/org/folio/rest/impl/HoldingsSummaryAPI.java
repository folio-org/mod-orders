package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.resource.OrdersHoldings;
import org.folio.service.inventory.HoldingsSummaryService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

public class HoldingsSummaryAPI extends BaseApi implements OrdersHoldings {

  private static final Logger logger = LogManager.getLogger();

  @Autowired
  private HoldingsSummaryService holdingsSummaryService;

  public HoldingsSummaryAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void getOrdersHoldingsById(String holdingId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    var requestContext = new RequestContext(vertxContext, okapiHeaders);

    holdingsSummaryService.getHoldingsSummary(holdingId, requestContext)
      .thenAccept(holdingSummary -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved : {}", JsonObject.mapFrom(holdingSummary).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(this.buildOkResponse(holdingSummary)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

}
