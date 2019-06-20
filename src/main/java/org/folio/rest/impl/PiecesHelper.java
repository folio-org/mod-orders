package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.jaxrs.model.Piece;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

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
    JsonObject jsonObj = new JsonObject();

    // explicitly setting RECEIVED to create inconsistency for testing
    //jsonObj.put("receivingStatusBeforeUpdate", "RECEIVED");
    jsonObj.put("receivingStatusBeforeUpdate", piece.getReceivingStatus());
    jsonObj.put("pieceIdUpdate", piece.getId());

    return handlePutRequest(resourceByIdPath(PIECES, piece.getId()), JsonObject.mapFrom(piece), httpClient, ctx, okapiHeaders, logger)
      .thenAccept(sendToEventBus -> receiptConsistencyPiecePoLine(jsonObj));
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
