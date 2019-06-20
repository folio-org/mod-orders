package org.folio.orders.events.handlers;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.LANG;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.impl.AbstractHelper;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.folio.rest.impl.CheckinReceivePiecesHelper;

import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

@Component("receiptStatusHandler")
public class ReceiptStatusConsistency extends AbstractHelper implements Handler<Message<JsonObject>> {

  private static final int LIMIT = 500;
  private static final int OFFSET = 0;
  private static final String PIECES_ENDPOINT = resourcesPath(PIECES) + "?query=poLineId==%s&lang=%s";
  private static final String PIECE_ID_UPDATE = "pieceIdUpdate";
  private static final String RECEIVING_STATUS_BEFORE_UPDATE = "receivingStatusBeforeUpdate";
  
  @Autowired
  public ReceiptStatusConsistency(Vertx vertx) {
    super(vertx.getOrCreateContext());
  }

  @Override
  public void handle(Message<JsonObject> message) {
    JsonObject body = message.body();
    String lang = body.getString(LANG);

    logger.info("Received message body: {}", body);

    Map<String, String> okapiHeaders = org.folio.orders.utils.HelperUtils.getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    String pieceIdUpdate = body.getString(PIECE_ID_UPDATE);

    List<CompletableFuture<String>> futures = new ArrayList<>();
    CompletableFuture<String> future = new VertxCompletableFuture<>(ctx);

    futures.add(future);
    
    // 1. Get updated piece by id from storage
    getPieceById(pieceIdUpdate, lang, httpClient, ctx, okapiHeaders, logger).thenAccept(jsonPiece -> {
      Piece piece = jsonPiece.mapTo(Piece.class);
      logger.info("piece for pieceRecord.json" + piece + " pieceById: " + pieceIdUpdate);
      ReceivingStatus receivingStatus = piece.getReceivingStatus();
      String receivingStatusBeforeUpdate = body.getString(RECEIVING_STATUS_BEFORE_UPDATE);

      // 2. if receivingStatus is different - before writing to storage vs after writing to storage
      if (!receivingStatus.toString().equalsIgnoreCase(receivingStatusBeforeUpdate)) {
        String poLineId = piece.getPoLineId();
        
        String endpoint = String.format(PIECES_ENDPOINT, poLineId, LANG);
        
        // 3. Get all pieces for poLineId above
        getPieces("", endpoint, httpClient, ctx, okapiHeaders,
            logger).thenAccept(piecesCollection -> {
              List<org.folio.rest.acq.model.Piece> listOfPieces = piecesCollection.getPieces();
              logger.info("list of pieces : " + listOfPieces.toString());
              // 4. Get PoLine for the poLineId which will used to calculate PoLineReceiptStatus
              getPoLineById(poLineId, lang, httpClient, ctx, okapiHeaders, logger).thenAccept(poLineJson -> {
                PoLine poLine = poLineJson.mapTo(PoLine.class);
                logger.info("PoLine poLine by id receipt status" + poLine.getReceiptStatus().toString());
                logger.info("PoLine id  " + poLineId);
                calculatePoLineReceiptStatus(poLine, listOfPieces)
                  .thenCompose(status -> updatePoLineReceiptStatus(poLine, status, httpClient, ctx, okapiHeaders, logger))
                  .thenAccept(future::complete);
              })
                .exceptionally(e -> {
                  logger.error("The error getting poLine by id {}", poLineId, e);
                  future.completeExceptionally(e);
                  return null;
                });
            })
              .exceptionally(e -> {
                logger.error("The error happened getting all pieces by poLine {}", poLineId, e);
                future.completeExceptionally(e);
                return null;
              });
      } else {
        logger.debug("Receipt status of pieces and po line are consistent");
        future.complete(null);
      }
    })
      .exceptionally(e -> {
        logger.error("The error happened getting a piece by id {}", pieceIdUpdate, e);
        future.completeExceptionally(e);
        return null;
      });

    // Now wait for all operations to be completed and send reply
    allOf(ctx, futures.toArray(new CompletableFuture[0])).thenAccept(v -> {
      // Sending reply message just in case some logic requires it
      message.reply(Response.Status.OK.getReasonPhrase());
      httpClient.closeClient();
    })
      .exceptionally(e -> {
        message.fail(handleProcessingError(e), getErrors().get(0)
          .getMessage());
        httpClient.closeClient();
        return null;
      });
  }

  private CompletableFuture<PoLine.ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine,
      List<org.folio.rest.acq.model.Piece> pieces) {
    
    logger.info("All pieces in calculatePoLineReceiptStatus -------->" + pieces.toString());
    if (pieces.isEmpty())
      return completedFuture(poLine.getReceiptStatus());
    else {
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

  boolean isCheckin(PoLine poLine) {
    return defaultIfNull(poLine.getCheckinItems(), false);
  }

  private CompletableFuture<Integer> getPiecesQuantityByPoLineAndStatus(ReceivingStatus receivingStatus,
      List<Piece> pieces) {
    Integer count = 0;
    for (Piece piece : pieces) {
      if (piece.getReceivingStatus()
        .value()
        .equalsIgnoreCase(receivingStatus.toString())) {
        count++;
      }
    }
    final Integer expectedQty = count;

    return CompletableFuture.supplyAsync(() -> expectedQty);
  }

  CompletableFuture<PieceCollection> getPieces(String query, String path,
      HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<PieceCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String queryParam = isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
      String endpoint = String.format(path, LIMIT, OFFSET, queryParam, LANG);
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger).thenAccept(jsonOrderLines -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved all pieces: {}", jsonOrderLines.encodePrettily());
        }
        future.complete(jsonOrderLines.mapTo(PieceCollection.class));
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

  public static CompletableFuture<JsonObject> getPieceById(String pieceId, String lang, HttpClientInterface httpClient, Context ctx,
      Map<String, String> okapiHeaders, Logger logger) {
    String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PIECES, pieceId), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }
}