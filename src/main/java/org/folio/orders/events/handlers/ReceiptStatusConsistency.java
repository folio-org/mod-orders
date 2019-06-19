package org.folio.orders.events.handlers;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static me.escoffier.vertx.completablefuture.VertxCompletableFuture.allOf;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.HelperUtils.handlePutRequest;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.ws.rs.core.Response;

import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.impl.AbstractHelper;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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

  @Autowired
  public ReceiptStatusConsistency(Vertx vertx) {
    super(vertx.getOrCreateContext());
  }

  @Override
  public void handle(Message<JsonObject> message) {
    JsonObject body = message.body();
    String lang = body.getString(HelperUtils.LANG);

    logger.info("Received message body: {}", body);

    Map<String, String> okapiHeaders = getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    String pieceIdUpdate = body.getString("pieceIdUpdate");

    List<CompletableFuture<Void>> futures = new ArrayList<>();
    CompletableFuture<Void> future = new VertxCompletableFuture<>(ctx);

    // 1. Get updated piece by id from storage
    getPieceById(pieceIdUpdate, lang, httpClient, ctx, okapiHeaders, logger).thenAccept(jsonPiece -> {
      Piece piece = jsonPiece.mapTo(Piece.class);
      ReceivingStatus receivingStatus = piece.getReceivingStatus();
      String receivingStatusBeforeUpdate = body.getString("receivingStatusBeforeUpdate");

      // 2. if receivingStatus is different - before writing to storage vs after writing to storage
      if (!receivingStatus.toString()
        .equalsIgnoreCase(receivingStatusBeforeUpdate)) {
        String poLineId = piece.getPoLineId();
        int limit = 500;
        int offset = 0;
        // 3. Get all pieces for poLineId above
        getPieces(limit, offset, "", "/orders-storage/pieces?query=poLineId==" + poLineId, lang, httpClient, ctx, okapiHeaders,
            logger).thenAccept(piecesCollection -> {
              List<org.folio.rest.acq.model.Piece> listOfPieces = piecesCollection.getPieces();

              // 4. Get PoLine for the poLineId which will used to calculate PoLineReceiptStatus
              getPoLineById(poLineId, lang, httpClient, ctx, okapiHeaders, logger).thenAccept(poLineJson -> {
                PoLine poLine = poLineJson.mapTo(PoLine.class);
                calculatePoLineReceiptStatus(poLine, listOfPieces)
                  .thenCompose(status -> updatePoLineReceiptStatus(poLine, status, httpClient, ctx, okapiHeaders, logger));
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
    if (pieces.isEmpty())
      return completedFuture(poLine.getReceiptStatus());
    else {
      return getPiecesQuantityByPoLineAndStatus(poLine.getId(), ReceivingStatus.EXPECTED, pieces)
        .thenCompose(expectedQty -> calculatePoLineReceiptStatus(expectedQty, poLine, pieces))
        .exceptionally(e -> {
          logger.error("The expected receipt status for PO Line '{}' cannot be calculated", e, poLine.getId());
          return null;
        });
    }
  }
  
  private CompletableFuture<String> updatePoLineReceiptStatus(PoLine poLine, ReceiptStatus status, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    // For testing
    status = FULLY_RECEIVED;
    
    if (status == null || poLine.getReceiptStatus() == status) {
      return completedFuture(null);
    }
    // Update receipt date and receipt status
    if (status == FULLY_RECEIVED) {
      poLine.setReceiptDate(new Date());
    } else if (isCheckin(poLine) && poLine.getReceiptStatus()
      .equals(ReceiptStatus.AWAITING_RECEIPT) && status == ReceiptStatus.PARTIALLY_RECEIVED) {
      // if checking in, set the receipt date only for the first piece
      poLine.setReceiptDate(new Date());
    } else {
      poLine.setReceiptDate(null);
    }

    poLine.setReceiptStatus(status);

    // Update PO Line in storage
    return handlePutRequest(resourceByIdPath(PO_LINES, poLine.getId()), JsonObject.mapFrom(poLine), httpClient, ctx, okapiHeaders,
        logger).thenApply(v -> poLine.getId())
          .exceptionally(e -> {
            logger.error("The PO Line '{}' cannot be updated with new receipt status", e, poLine.getId());
            return null;
          });
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
    return getPiecesQuantityByPoLineAndStatus(poLine.getId(), ReceivingStatus.RECEIVED, pieces)
      .thenApply(receivedQty -> receivedQty == 0 ? AWAITING_RECEIPT : PARTIALLY_RECEIVED);
  }

  boolean isCheckin(PoLine poLine) {
    return defaultIfNull(poLine.getCheckinItems(), false);
  }

  private CompletableFuture<Integer> getPiecesQuantityByPoLineAndStatus(String poLineId, ReceivingStatus receivingStatus,
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

  CompletableFuture<PieceCollection> getPieces(int limit, int offset, String query, String path, String lang,
      HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<PieceCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String queryParam = isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
      String endpoint = String.format(path, limit, offset, queryParam, lang);
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
    // String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES, lineId), lang);
    String endpoint = "/orders-storage/pieces/" + pieceId;
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }

  private Map<String, String> getOkapiHeaders(Message<JsonObject> message) {
    Map<String, String> okapiHeaders = new CaseInsensitiveMap<>();
    message.headers()
      .entries()
      .forEach(entry -> okapiHeaders.put(entry.getKey(), entry.getValue()));
    return okapiHeaders;
  }
}