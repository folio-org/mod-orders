package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.PoLine;

import io.vertx.core.Context;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

public class PiecesHelper extends AbstractHelper {

  private static final String DELETE_PIECE_BY_ID = resourceByIdPath(PIECES, "%s") + "?lang=%s";
  
  public PiecesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  CompletableFuture<Piece> createRecordInStorage(Piece entity) {
    // On success set id of the created entity to piece object and return it back
    return createRecordInStorage(JsonObject.mapFrom(entity), resourcesPath(PIECES)).thenApply(entity::withId);
  }
  
  public CompletableFuture<Void> updatePieceRecord(Piece piece) {
    //ReceivingStatus r = piece.getReceivingStatus();
    JsonObject jsonObj = new JsonObject();
    jsonObj.put("receivingStatusBeforeUpdate", "RECEIVED");
    jsonObj.put("pieceIdUpdate", piece.getId());
    jsonObj.put("okapiHeaders", okapiHeaders);
    
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    handlePutRequest(resourceByIdPath(PIECES, piece.getId()), JsonObject.mapFrom(piece), httpClient, ctx, okapiHeaders, logger)
      .thenAccept(a -> receiptConsistencyPiecePoLine(jsonObj))
      .exceptionally(e -> {
        logger.error("Retry to OPEN existing Order failed", e);
        return null;
      });

    return future;
  }
  
  private void receiptConsistencyPiecePoLine(JsonObject jsonObj) {

      logger.debug("Sending event to verify receipt status");

      
      sendEvent(MessageAddress.RECEIPT_STATUS, jsonObj);

      logger.debug("Event to verify receipt status - sent");
    
  }
  
  public CompletableFuture<Void> deletePiece(String id) {
    return handleDeleteRequest(String.format(DELETE_PIECE_BY_ID, id, lang), httpClient, ctx, okapiHeaders, logger);
  }
}
