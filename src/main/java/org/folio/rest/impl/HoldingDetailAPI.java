package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingDetailCollection;
import org.folio.rest.jaxrs.resource.OrdersHoldingDetail;
import org.folio.service.orders.HoldingDetailService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import javax.ws.rs.core.Response;
import java.util.Map;

import static io.vertx.core.Future.succeededFuture;

public class HoldingDetailAPI extends BaseApi implements OrdersHoldingDetail {

  @Autowired
  private HoldingDetailService holdingDetailService;

  public HoldingDetailAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void postOrdersHoldingDetail(HoldingDetailCollection entity, Map<String, String> okapiHeaders,
                                      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    var requestContext = new RequestContext(vertxContext, okapiHeaders);
    holdingDetailService.postOrdersHoldingDetail(entity.getHoldingIds(), requestContext)
      .onSuccess(holdingSummary -> asyncResultHandler.handle(succeededFuture(this.buildOkResponse(holdingSummary))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }
}
