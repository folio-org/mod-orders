package org.folio.rest.impl;

import static org.folio.orders.utils.ErrorCodes.USER_HAS_NOT_PERMISSIONS;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.HttpStatus;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PiecesHelper extends AbstractHelper {

  private ProtectionHelper protectionHelper;
  private PurchaseOrderLineHelper purchaseOrderLineHelper;

  private static final String DELETE_PIECE_BY_ID = resourceByIdPath(PIECES, "%s") + "?lang=%s";

  public PiecesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
    protectionHelper = new ProtectionHelper(okapiHeaders, ctx, lang, ProtectedOperationType.CREATE);
    purchaseOrderLineHelper = new PurchaseOrderLineHelper(okapiHeaders, ctx, lang);
  }

  CompletableFuture<Piece> createRecordInStorage(Piece entity) {
    String poLineId = entity.getPoLineId();
    return purchaseOrderLineHelper.getCompositePoLine(poLineId)
      .thenApply(CompositePoLine::getPurchaseOrderId)
      .thenCompose(recordId -> protectionHelper.isOperationRestricted(recordId))
      .thenCompose(isRestricted -> {
        if(isRestricted) {
          throw new HttpException(HttpStatus.HTTP_FORBIDDEN.toInt(), USER_HAS_NOT_PERMISSIONS);
        } else {
          return createRecordInStorage(JsonObject.mapFrom(entity), resourcesPath(PIECES)).thenApply(entity::withId);
        }
      });
  }

  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> updatePieceRecord(Piece piece) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    getPieceById(piece.getId(), lang, httpClient, ctx, okapiHeaders, logger).thenAccept(jsonPiece -> {
      Piece pieceStorage = jsonPiece.mapTo(Piece.class);
      ReceivingStatus receivingStatusStorage = pieceStorage.getReceivingStatus();

      handlePutRequest(resourceByIdPath(PIECES, piece.getId()), JsonObject.mapFrom(piece), httpClient, ctx, okapiHeaders, logger)
        .thenAccept(future::complete)
        .thenAccept(afterUpdate -> {

          JsonObject messageToEventBus = new JsonObject();
          messageToEventBus.put("poLineIdUpdate", piece.getPoLineId());

          ReceivingStatus receivingStatusUpdate = piece.getReceivingStatus();
          logger.debug("receivingStatusStorage -- " + receivingStatusStorage);
          logger.debug("receivingStatusUpdate -- " + receivingStatusUpdate);

          if (receivingStatusStorage.compareTo(receivingStatusUpdate) != 0) {
            receiptConsistencyPiecePoLine(messageToEventBus);
          }
        })
        .exceptionally(e -> {
          logger.error("Error updating piece by id to storage {}", piece.getId(), e);
          future.completeExceptionally(e);
          return null;
        });
    })
      .exceptionally(e -> {
        logger.error("Error getting piece by id from storage {}", piece.getId(), e);
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
