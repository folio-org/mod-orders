package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.resource.OrdersExportHistory;
import org.folio.service.ExportHistoryService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;

public class ExportHistoryImpl extends BaseApi implements OrdersExportHistory {

  @Autowired
  ExportHistoryService exportHistoryService;

  public ExportHistoryImpl() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  public void getOrdersExportHistory(String totalRecords, int offset, int limit, String query, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    exportHistoryService.getExportHistoryByQuery(query, offset, limit, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(exportHistoryCollection -> asyncResultHandler.handle(succeededFuture(buildOkResponse(exportHistoryCollection))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
