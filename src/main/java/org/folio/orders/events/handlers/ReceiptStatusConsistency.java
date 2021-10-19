package org.folio.orders.events.handlers;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.updatePoLineReceiptStatus;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.Logger;
import org.folio.helper.AbstractHelper;
import org.folio.completablefuture.AsyncUtil;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.folio.service.orders.PurchaseOrderLineService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

@Component("receiptStatusHandler")
public class ReceiptStatusConsistency extends AbstractHelper implements Handler<Message<JsonObject>> {

  private static final int LIMIT = Integer.MAX_VALUE;
  private static final String PIECES_ENDPOINT = resourcesPath(PIECES_STORAGE) + "?query=poLineId==%s&limit=%s";

  private PurchaseOrderLineService purchaseOrderLineService;

  @Autowired
  public ReceiptStatusConsistency(Vertx vertx, PurchaseOrderLineService purchaseOrderLineService) {
    super(vertx.getOrCreateContext());
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  @Override
  public void handle(Message<JsonObject> message) {
    JsonObject messageFromEventBus = message.body();

    logger.info("Received message body: {}", messageFromEventBus);

    Map<String, String> okapiHeaders = org.folio.orders.utils.HelperUtils.getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    CompletableFuture<Void> future = new CompletableFuture<>();
    futures.add(future);

    String poLineIdUpdate = messageFromEventBus.getString("poLineIdUpdate");
    String query = String.format(PIECES_ENDPOINT, poLineIdUpdate, LIMIT);

    // 1. Get all pieces for poLineId
    getPieces(query, httpClient, okapiHeaders, logger).thenAccept(piecesCollection -> {
      List<org.folio.rest.acq.model.Piece> listOfPieces = piecesCollection.getPieces();

      // 2. Get PoLine for the poLineId which will be used to calculate PoLineReceiptStatus
      purchaseOrderLineService.getOrderLineById(poLineIdUpdate, new RequestContext(ctx, okapiHeaders))
        .thenAccept(poLine -> {
          if (poLine.getReceiptStatus().equals(PoLine.ReceiptStatus.ONGOING)) {
            return;
          }
          calculatePoLineReceiptStatus(poLine, listOfPieces)
          .thenCompose(status -> updatePoLineReceiptStatus(poLine, status, httpClient, okapiHeaders, logger))
          .thenAccept(updatedPoLineId -> {
            if (updatedPoLineId != null) {
              // send event to update order status
              updateOrderStatus(poLine, okapiHeaders);
            }
          })
          .thenAccept(future::complete);
      })
        .exceptionally(e -> {
          logger.error("The error getting poLine by id {}", poLineIdUpdate, e);
          future.completeExceptionally(e);
          return null;
        });
    })
      .exceptionally(e -> {
        logger.error("The error happened getting all pieces by poLine {}", poLineIdUpdate, e);
        future.completeExceptionally(e);
        return null;
      });

    // Now wait for all operations to be completed and send reply
    completeAllFutures(httpClient, futures, message);
  }

  private void updateOrderStatus(PoLine poLine, Map<String, String> okapiHeaders) {
    List<JsonObject> poIds = StreamEx
      .of(poLine)
      .map(PoLine::getPurchaseOrderId)
      .distinct()
      .map(orderId -> new JsonObject().put(ORDER_ID, orderId))
      .toList();
    JsonObject messageContent = new JsonObject();
    messageContent.put(OKAPI_HEADERS, okapiHeaders);
    // Collect order ids which should be processed
    messageContent.put(EVENT_PAYLOAD, new JsonArray(poIds));
    sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, messageContent);
  }

  private CompletableFuture<PoLine.ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine,
      List<org.folio.rest.acq.model.Piece> pieces) {

    if (pieces.isEmpty()) {
      return completedFuture(poLine.getReceiptStatus());
    } else {
      return getPiecesQuantityByPoLineAndStatus(ReceivingStatus.EXPECTED, pieces)
        .thenCompose(expectedQty -> calculatePoLineReceiptStatus(expectedQty, pieces))
        .exceptionally(e -> {
          logger.error("The expected receipt status for PO Line '{}' cannot be calculated", poLine.getId(), e);
          return null;
        });
    }
  }

  private CompletableFuture<ReceiptStatus> calculatePoLineReceiptStatus(int expectedPiecesQuantity,
      List<org.folio.rest.acq.model.Piece> pieces) {

    if (expectedPiecesQuantity == 0) {
      return CompletableFuture.completedFuture(FULLY_RECEIVED);
    }
    // Partially Received: In case there is at least one successfully received
    // piece
    if (StreamEx.of(pieces)
      .anyMatch(piece -> ReceivingStatus.RECEIVED == piece.getReceivingStatus())) {
      return CompletableFuture.completedFuture(PARTIALLY_RECEIVED);
    }
    // Pieces were rolled-back to Expected. In this case we have to check if
    // there is any Received piece in the storage
    return getPiecesQuantityByPoLineAndStatus(ReceivingStatus.RECEIVED, pieces)
      .thenApply(receivedQty -> receivedQty == 0 ? AWAITING_RECEIPT : PARTIALLY_RECEIVED);
  }

  private CompletableFuture<Integer> getPiecesQuantityByPoLineAndStatus(ReceivingStatus receivingStatus, List<Piece> pieces) {
    return AsyncUtil.executeBlocking(ctx, false, () -> (int) pieces.stream()
      .filter(piece -> piece.getReceivingStatus() == receivingStatus)
      .count());
  }

  CompletableFuture<PieceCollection> getPieces(String endpoint, HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Logger logger) {
    CompletableFuture<PieceCollection> future = new CompletableFuture<>();
    try {
      handleGetRequest(endpoint, httpClient, okapiHeaders, logger).thenAccept(jsonPieces -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved all pieces: {}", jsonPieces.encodePrettily());
        }
        future.complete(jsonPieces.mapTo(PieceCollection.class));
      })
      .exceptionally(t -> {
        future.completeExceptionally(t);
        return null;
      });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }
    return future;
  }
}
