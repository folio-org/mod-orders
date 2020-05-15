package org.folio.rest.impl;

import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.getPurchaseOrderById;
import static org.folio.orders.utils.HelperUtils.handleDeleteRequest;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ProtectedOperationType.DELETE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import javax.ws.rs.core.Response;

import org.folio.orders.events.handlers.MessageAddress;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.rest.exceptions.InventoryException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.ProtectedOperationType;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Piece.ReceivingStatus;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;

public class PiecesHelper extends AbstractHelper {

  public static final String INVENTORY_UPDATE_FAILED_FOR_PIECE = "Holding, Instance, Item failed for piece : ";
  private ProtectionHelper protectionHelper;
  private InventoryHelper inventoryHelper;

  private static final String DELETE_PIECE_BY_ID = resourceByIdPath(PIECES, "%s") + "?lang=%s";
  private PurchaseOrderLineHelper orderLineHelper;

  public PiecesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
    protectionHelper = new ProtectionHelper(httpClient, okapiHeaders, ctx, lang);
    inventoryHelper = new InventoryHelper(okapiHeaders, ctx, lang);
    orderLineHelper = new PurchaseOrderLineHelper(okapiHeaders, ctx, lang);
  }

  public CompletableFuture<Piece> createPiece(Piece piece) {
      return getCompositeOrderByPoLineId(piece.getPoLineId())
        .thenCompose(order ->
          protectionHelper.isOperationRestricted(order.getAcqUnitIds(), ProtectedOperationType.CREATE)
                          .thenApply(v -> order)
        )
        .thenCompose(order -> updateInventory(order.getCompositePoLines().get(0)))
        .thenCompose(v -> createRecordInStorage(JsonObject.mapFrom(piece), resourcesPath(PIECES))
        .thenApply(piece::withId))
        .exceptionally(throwable -> {
          logger.error(INVENTORY_UPDATE_FAILED_FOR_PIECE + " {}", piece.getId(), throwable);
          throw new InventoryException(INVENTORY_UPDATE_FAILED_FOR_PIECE + piece.getId());
        });
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  CompletableFuture<Void> updateInventory(CompositePoLine compPOL) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return inventoryHelper.handleInstanceRecord(compPOL)
                            .thenCompose(line -> orderLineHelper.updateOrderLineSummary(line.getId(), JsonObject.mapFrom(line))
                                                                .thenApply(json -> line))
                            .thenAccept(inventoryHelper::handleHoldingsAndItemsRecords);
    }
    return CompletableFuture.completedFuture(null);
  }


  // Flow to update piece
  // 1. Before update, get piece by id from storage and store receiving status
  // 2. Update piece with new content and complete future
  // 3. Create a message and check if receivingStatus is not consistent with storage; if yes - send a message to event bus
  public CompletableFuture<Void> updatePieceRecord(Piece piece) {
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);
    getCompositeOrderByPoLineId(piece.getPoLineId())
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
      .thenCompose(piece -> getCompositeOrderByPoLineId(piece.getPoLineId()))
      .thenCompose(purchaseOrder -> protectionHelper.isOperationRestricted(purchaseOrder.getAcqUnitIds(), DELETE))
      .thenCompose(aVoid -> handleDeleteRequest(String.format(DELETE_PIECE_BY_ID, id, lang), httpClient, ctx, okapiHeaders, logger));
  }


  public CompletableFuture<CompositePurchaseOrder> getCompositeOrderByPoLineId(String poLineId) {
    return getPoLineById(poLineId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(CompositePoLine.class))
      .thenCompose(poLine ->
        getCompositePurchaseOrder(poLine.getPurchaseOrderId())
        .thenApply(purchaseOrder -> purchaseOrder.withCompositePoLines(Collections.singletonList(poLine)))
      );
  }


  public CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrder(String purchaseOrderId) {
    return getPurchaseOrderById(purchaseOrderId, lang, httpClient, ctx, okapiHeaders, logger)
      .thenApply(HelperUtils::convertToCompositePurchaseOrder)
      .exceptionally(t -> {
        Throwable cause = t.getCause();
        // The case when specified order does not exist
        if (cause instanceof HttpException && ((HttpException) cause).getCode() == Response.Status.NOT_FOUND.getStatusCode()) {
          throw new HttpException(422, ErrorCodes.ORDER_NOT_FOUND);
        }
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
  }
}
