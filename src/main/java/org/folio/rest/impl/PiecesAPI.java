package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.resource.OrdersPieces;
import org.folio.service.pieces.PieceStorageService;
import org.folio.service.pieces.flows.create.PieceCreateFlowManager;
import org.folio.service.pieces.flows.delete.PieceDeleteFlowManager;
import org.folio.service.pieces.flows.update.PieceUpdateFlowManager;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

public class PiecesAPI extends BaseApi implements OrdersPieces {

  private static final Logger logger = LogManager.getLogger();
  @Autowired
  private PieceStorageService pieceStorageService;
  @Autowired
  private PieceCreateFlowManager pieceCreateFlowManager;
  @Autowired
  private PieceDeleteFlowManager pieceDeleteFlowManager;
  @Autowired
  private PieceUpdateFlowManager pieceUpdateFlowManager;

  public PiecesAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void getOrdersPieces(String totalRecords, int offset, int limit, String query, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    pieceStorageService.getPiecesWithAcqUnitCheck(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(pieces -> asyncResultHandler.handle(succeededFuture(buildOkResponse(pieces))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersPieces(boolean createItem, Piece entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    pieceCreateFlowManager.createPiece(entity, createItem, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(piece -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created piece: {}", JsonObject.mapFrom(piece)
            .encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(buildCreatedResponse(piece)));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getOrdersPiecesById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    pieceStorageService.getPieceById(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(piece -> asyncResultHandler.handle(succeededFuture(buildOkResponse(piece))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersPiecesById(String pieceId, boolean createItem, boolean deleteHolding, Piece piece,
      Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (StringUtils.isEmpty(piece.getId())) {
      piece.setId(pieceId);
    }

    pieceUpdateFlowManager.updatePiece(piece, createItem, deleteHolding, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void deleteOrdersPiecesById(String pieceId, boolean deleteHolding, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    pieceDeleteFlowManager.deletePiece(pieceId, deleteHolding, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(ok -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
