package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PiecesHelper extends AbstractHelper {

  private static final String DELETE_PIECE_BY_ID = resourceByIdPath(PIECES, "%s") + "?lang=%s";

  public PiecesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  CompletableFuture<Piece> createRecordInStorage(Piece entity) {
    // On success set id of the created entity to piece object and return it back
    return createRecordInStorage(JsonObject.mapFrom(entity), resourcesPath(PIECES)).thenApply(entity::withId);
  }

  // storage - expected
  // update - received
  // flow
  // -> get piece by id
  // -> check if receivingStatus is not consistent with storage before sending message to eventbus. if yes only do below
  // -> send message
  // -> get all pieces by poLineId
  // -> get Po Line
  // -> calculate receipt status and update Po Line in storage
  public CompletableFuture<Void> updatePieceRecord(Piece piece) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    getPieceById(piece.getId(), lang, httpClient, ctx, okapiHeaders, logger).thenAccept(jsonPiece -> {
      Piece pieceStorage = jsonPiece.mapTo(Piece.class);

      JsonObject messageToEventBus = new JsonObject();
      messageToEventBus.put("poLineIdUpdate", piece.getPoLineId());

      ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();
      ReceivingStatus receivingStatusUpdate = piece.getReceivingStatus();
      logger.info("receivingStatusStorage -- " + receivingStatusStorage);
      logger.info("receivingStatusBeforeUpdate -- " + receivingStatusUpdate);

      handlePutRequest(resourceByIdPath(PIECES, piece.getId()), JsonObject.mapFrom(piece), httpClient, ctx, okapiHeaders, logger)
        .thenAccept(v -> future.complete(v))
        .exceptionally(e -> {
          logger.error("The error updating piece by id to storage {}", piece.getId(), e);
          future.completeExceptionally(e);
          return null;
        })
        .thenAccept(afterUpdate -> {
          if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
            receiptConsistencyPiecePoLine(messageToEventBus);
          }
        })
        .exceptionally(e -> {
          logger.error("Error sending message to event bus {}", messageToEventBus.encodePrettily(), e);
          future.completeExceptionally(e);
          return null;
        });
    })
      .exceptionally(e -> {
        logger.error("The error getting piece by id from storage {}", piece.getId(), e);
        future.completeExceptionally(e);
        return null;
      });
    return future;
  }

  public static CompletableFuture<JsonObject> getPieceById(String pieceId, String lang, HttpClientInterface httpClient, Context ctx,
      Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PIECES, pieceId), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
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
