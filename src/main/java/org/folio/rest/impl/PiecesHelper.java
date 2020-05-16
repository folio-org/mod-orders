package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.*;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.PoLine;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;

public class PiecesHelper extends AbstractHelper {

  private ProtectionHelper protectionHelper;
  private InventoryHelper inventoryHelper;

  private static final String DELETE_PIECE_BY_ID = resourceByIdPath(PIECES, "%s") + "?lang=%s";

  private static final String GET_PIECES_BY_QUERY = resourcesPath(PIECES) + SEARCH_PARAMS;

  public PiecesHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
    protectionHelper = new ProtectionHelper(httpClient, okapiHeaders, ctx, lang);
    inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
  }

  public PiecesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
    protectionHelper = new ProtectionHelper(httpClient, okapiHeaders, ctx, lang);
    inventoryHelper = new InventoryHelper(httpClient, okapiHeaders, ctx, lang);
  }

  CompletableFuture<Piece> createPiece(Piece piece) {
      return getOrderByPoLineId(piece.getPoLineId())
        .thenCompose(order -> protectionHelper.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.CREATE))
        .thenCompose(v -> inventoryHelper.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId()))
        .thenCompose(v -> createRecordInStorage(JsonObject.mapFrom(piece), resourcesPath(PIECES))
        .thenApply(piece::withId));
  }

  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> updatePieceRecord(Piece piece) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    getOrderByPoLineId(piece.getPoLineId())
      .thenCompose(order -> protectionHelper.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.UPDATE))
      .thenCompose(v -> inventoryHelper.updateItemWithPoLineId(piece.getItemId(), piece.getPoLineId()))
      .thenAccept(vVoid ->
        getPieceById(piece.getId()).thenAccept(pieceStorage -> {
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
          })
    )
      .exceptionally(t -> {
        logger.error("User with id={} is forbidden to update piece with id={}", t.getCause(), getCurrentUserId(), piece.getId());
        future.completeExceptionally(t);
        return null;
    });
    return future;
  }

  public CompletableFuture<Piece> getPieceById(String pieceId) {
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PIECES, pieceId), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(jsonPiece -> jsonPiece.mapTo(Piece.class));
  }

  private void receiptConsistencyPiecePoLine(JsonObject jsonObj) {
    logger.debug("Sending event to verify receipt status");

    sendEvent(MessageAddress.RECEIPT_STATUS, jsonObj);

    logger.debug("Event to verify receipt status - sent");
  }

  public CompletableFuture<Void> deletePiece(String id) {
    return getPieceById(id)
      .thenCompose(piece -> getOrderByPoLineId(piece.getPoLineId())
        .thenCompose(purchaseOrder -> protectionHelper.isOperationRestricted(purchaseOrder.getAcqUnitIds(), DELETE))
        .thenCompose(vVoid -> inventoryHelper.getRequestsByItemId(piece.getItemId()))
        .thenAccept(items -> {
          if (CollectionUtils.isNotEmpty(items)) {
            throw new HttpException(422, ErrorCodes.REQUEST_FOUND.toError());
          }
        })
        .thenCompose(aVoid -> handleDeleteRequest(String.format(DELETE_PIECE_BY_ID, id, lang), httpClient, ctx, okapiHeaders, logger))
      );
  }


  public CompletableFuture<CompositePurchaseOrder> getOrderByPoLineId(String poLineId) {
    return getPoLineById(poLineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(PoLine.class))
      .thenCompose(poLine -> getPurchaseOrderById(poLine.getPurchaseOrderId(), lang, httpClient, ctx, okapiHeaders, logger))
      .thenApply(jsonObject -> jsonObject.mapTo(CompositePurchaseOrder.class));
  }

  public CompletableFuture<PieceCollection> getPieces(int limit, int offset, String query) {
    String endpoint = String.format(GET_PIECES_BY_QUERY, limit, offset, buildQuery(query, logger), lang);
    return HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenCompose(json -> VertxCompletableFuture.supplyBlockingAsync(ctx, () -> json.mapTo(PieceCollection.class)));
  }
}
