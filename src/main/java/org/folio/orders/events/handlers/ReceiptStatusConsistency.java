package org.folio.orders.events.handlers;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.impl.AbstractHelper;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

@Component("receiptStatusHandler")
public class ReceiptStatusConsistency extends AbstractHelper implements Handler<Message<JsonObject>> {

  private static final int LIMIT = Integer.MAX_VALUE;
  private static final String PIECES_ENDPOINT = resourcesPath(PIECES) + "?query=poLineId==%s&limit=%s";

  @Autowired
  public ReceiptStatusConsistency(Vertx vertx) {
    super(vertx.getOrCreateContext());
  }

  @Override
  public void handle(Message<JsonObject> message) {
    JsonObject messageFromEventBus = message.body();

    logger.info("Received message body: {}", messageFromEventBus);

    Map<String, String> okapiHeaders = org.folio.orders.utils.HelperUtils.getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    List<CompletableFuture<String>> futures = new ArrayList<>();
    CompletableFuture<String> future = new VertxCompletableFuture<>(ctx);
    futures.add(future);

    String poLineIdUpdate = messageFromEventBus.getString("poLineIdUpdate");
    String query = String.format(PIECES_ENDPOINT, poLineIdUpdate, LIMIT);

    // 1. Get all pieces for poLineId
    getPieces(query, httpClient, okapiHeaders, logger).thenAccept(piecesCollection -> {
      List<org.folio.rest.acq.model.Piece> listOfPieces = piecesCollection.getPieces();

      // 2. Get PoLine for the poLineId which will be used to calculate PoLineReceiptStatus
      getPoLineById(poLineIdUpdate, lang, httpClient, ctx, okapiHeaders, logger).thenAccept(poLineJson -> {
        PoLine poLine = poLineJson.mapTo(PoLine.class);
        calculatePoLineReceiptStatus(poLine, listOfPieces)
          .thenCompose(status -> updatePoLineReceiptStatus(poLine, status, httpClient, ctx, okapiHeaders, logger))
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
    completeAllFutures(ctx, httpClient, futures, message);
  }

  private CompletableFuture<PoLine.ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine,
      List<org.folio.rest.acq.model.Piece> pieces) {

    if (pieces.isEmpty()) {
      return completedFuture(poLine.getReceiptStatus());
    } else {
      return getPiecesQuantityByPoLineAndStatus(ReceivingStatus.EXPECTED, pieces)
        .thenCompose(expectedQty -> calculatePoLineReceiptStatus(expectedQty, poLine, pieces))
        .exceptionally(e -> {
          logger.error("The expected receipt status for PO Line '{}' cannot be calculated", e, poLine.getId());
          return null;
        });
    }
  }

  private CompletableFuture<ReceiptStatus> calculatePoLineReceiptStatus(int expectedPiecesQuantity, PoLine poLine,
      List<org.folio.rest.acq.model.Piece> pieces) {
    if (!isCheckin(poLine) && expectedPiecesQuantity == 0) {
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
    return CompletableFuture.supplyAsync(() -> (int) pieces.stream()
      .filter(piece -> piece.getReceivingStatus() == receivingStatus)
      .count());
  }

  CompletableFuture<PieceCollection> getPieces(String endpoint, HttpClientInterface httpClient, Map<String, String> okapiHeaders,
      Logger logger) {
    CompletableFuture<PieceCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger).thenAccept(jsonPieces -> {
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