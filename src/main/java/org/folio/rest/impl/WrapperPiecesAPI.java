package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.resource.OrdersWrapperPieces;
import org.folio.service.pieces.WrapperPieceStorageService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import javax.ws.rs.core.Response;
import java.util.Map;

import static io.vertx.core.Future.succeededFuture;

@Log4j2
public class WrapperPiecesAPI extends BaseApi implements OrdersWrapperPieces {

  @Autowired
  private WrapperPieceStorageService wrapperPieceStorageService;

  public WrapperPiecesAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  public void getOrdersWrapperPieces(String query, String totalRecords, int offset, int limit,
                                     Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    wrapperPieceStorageService.getWrapperPieces(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(wrapperPieces -> asyncResultHandler.handle(succeededFuture(buildOkResponse(wrapperPieces))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  public void getOrdersWrapperPiecesById(String id, Map<String, String> okapiHeaders,
                                         Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    wrapperPieceStorageService.getWrapperPieceById(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(wrapperPiece -> asyncResultHandler.handle(succeededFuture(buildOkResponse(wrapperPiece))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
