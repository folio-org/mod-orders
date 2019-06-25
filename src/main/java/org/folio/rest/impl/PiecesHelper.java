package org.folio.rest.impl;

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
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;

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
  // -> check if receivingStatus is inconsistent before sending message to eventbus. if yes only do below
  // -> send message
  // -> get all pieces by poLineId
  // -> get Po Line
  // -> calculate receipt status and update Po Line in storage
  public CompletableFuture<Void> updatePieceRecord(Piece piece) {
    JsonObject messageToEventBus = new JsonObject();

    messageToEventBus.put("receivingStatusBeforeUpdate", piece.getReceivingStatus()); // received
    messageToEventBus.put("pieceIdUpdate", piece.getId());
    messageToEventBus.put("poLineIdUpdate", piece.getPoLineId());

    return getPieceById(piece.getId(), lang, httpClient, ctx, okapiHeaders, logger).thenAccept(jsonPiece -> {
      Piece pieceStorage = jsonPiece.mapTo(Piece.class);
      logger.info("pieces --- " + piece.toString());
      ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();
      ReceivingStatus receivingStatusBeforeUpdate = piece.getReceivingStatus();
      logger.info("receivingStatusStorage -- " + receivingStatusStorage);
      logger.info("receivingStatusBeforeUpdate -- " + receivingStatusBeforeUpdate);
      
      if (receivingStatusStorage.compareTo(receivingStatusBeforeUpdate) != 0) {
        receiptConsistencyPiecePoLine(messageToEventBus);
        return;
      }
    }).thenAccept(thenDo -> {
        handlePutRequest(resourceByIdPath(PIECES, piece.getId()), JsonObject.mapFrom(piece), httpClient, ctx, okapiHeaders, logger);
    });
    
      //.thenAccept(sendToEventBus -> receiptConsistencyPiecePoLine(messageToEventBus));
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
