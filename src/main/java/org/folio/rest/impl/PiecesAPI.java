package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.handleErrorResponse;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.PiecesHelper;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.resource.OrdersPieces;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

public class PiecesAPI implements OrdersPieces {

  private static final Logger logger = LogManager.getLogger();

  @Override
  @Validate
  public void postOrdersPieces(String lang, Piece entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PiecesHelper helper = new PiecesHelper(okapiHeaders, vertxContext, lang);
    helper.createPiece(entity)
      .thenAccept(piece -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created piece: {}", JsonObject.mapFrom(piece)
            .encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildCreatedResponse(piece)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putOrdersPiecesById(String pieceId, String lang, Piece piece, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PiecesHelper putPieceHelper = new PiecesHelper(okapiHeaders, vertxContext, lang);

    if (StringUtils.isEmpty(piece.getId())) {
      piece.setId(pieceId);
    }

    putPieceHelper.updatePieceRecord(piece)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(putPieceHelper.buildNoContentResponse())))
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, putPieceHelper, t));
  }

  @Override
  @Validate
  public void deleteOrdersPiecesById(String pieceId, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    PiecesHelper deletePieceHelper = new PiecesHelper(okapiHeaders, vertxContext, lang);
    deletePieceHelper.deletePiece(pieceId)
      .thenAccept(ok -> asyncResultHandler.handle(succeededFuture(deletePieceHelper.buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, deletePieceHelper, fail));
  }
}
