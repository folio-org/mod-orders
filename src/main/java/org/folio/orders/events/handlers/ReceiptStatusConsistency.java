package org.folio.orders.events.handlers;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.getPoLineById;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.rest.impl.CheckinReceivePiecesHelper.PIECES_BY_POL_ID_AND_STATUS_QUERY;
import static org.folio.rest.impl.CheckinReceivePiecesHelper.PIECES_WITH_QUERY_ENDPOINT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.impl.AbstractHelper;
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

    logger.info("Received message body: {}", body);

    Map<String, String> okapiHeaders = getOkapiHeaders(message);
    HttpClientInterface httpClient = getHttpClient(okapiHeaders, true);

    String pieceIdUpdate = body.getString("pieceIdUpdate");

    CompletableFuture<PieceCollection> future = new VertxCompletableFuture<>(ctx);

    getPieceById(pieceIdUpdate, lang, httpClient, ctx, okapiHeaders, logger).thenAccept(jsonPiece -> {
      Piece piece = jsonPiece.mapTo(Piece.class);
      ReceivingStatus receivingStatus = piece.getReceivingStatus();
      String receivingStatusBeforeUpdate = body.getString("receivingStatusBeforeUpdate");
      //Map<String, String> okapiHeadersReceiptstatus = (Map<String, String>) body.getValue("okapiHeaders");
      
      if (!receivingStatus.toString()
        .equalsIgnoreCase(receivingStatusBeforeUpdate)) {
        String poLineId = piece.getPoLineId();

        getPieces(500, 0, "", "/orders-storage/pieces?query=poLineId==" + poLineId, lang, httpClient, ctx, okapiHeaders, logger)
            .thenAccept(piecesCollection -> {
              //int totalRecords = piecesCollection.getTotalRecords();
              List<org.folio.rest.acq.model.Piece> listOfPieces = piecesCollection.getPieces();
              logger.info("pieces = " + listOfPieces);
//              Collection<org.folio.rest.acq.model.Piece> pieces1 = pieces;
              getPoLineById(poLineId, lang, httpClient, ctx, okapiHeaders, logger).thenAccept(poLineJson -> {
                PoLine poLine = poLineJson.mapTo(PoLine.class);
                calculatePoLineReceiptStatus(poLine, listOfPieces);
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
  }

  private CompletableFuture<PoLine.ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine, List<org.folio.rest.acq.model.Piece> pieces) {
    if(pieces.isEmpty())
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
  
  private CompletableFuture<ReceiptStatus> calculatePoLineReceiptStatus(int expectedPiecesQuantity, PoLine poLine,
    List<org.folio.rest.acq.model.Piece> pieces) {
    if(!isCheckin(poLine) && expectedPiecesQuantity == 0) {
      return CompletableFuture.completedFuture(FULLY_RECEIVED);
    }
    // Partially Received: In case there is at least one successfully received
    // piece
    if (StreamEx.of(pieces).anyMatch(piece -> ReceivingStatus.RECEIVED == piece.getReceivingStatus())) {
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
  
  private CompletableFuture<Integer> getPiecesQuantityByPoLineAndStatus(String poLineId,
      ReceivingStatus receivingStatus, List<Piece> pieces) {
    //String query = String.format(PIECES_BY_POL_ID_AND_STATUS_QUERY, poLineId, receivingStatus.value());
    // Limit to 0 because only total number is important
    //String endpoint = String.format(PIECES_WITH_QUERY_ENDPOINT, 0, lang, encodeQuery(query, logger));
    // Search for pieces with Expected status
    String endpoint = "/orders-storage/pieces?query=poLineId==" + poLineId + " and receivingStatus==" + receivingStatus.toString();
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      // Return total records quantity
      .thenApply(json -> json.mapTo(PieceCollection.class).getTotalRecords());
  }
  
  CompletableFuture<PieceCollection> getPieces(int limit, int offset, String query, String path, String lang, HttpClientInterface httpClient, Context ctx, Map<String, String> okapiHeaders, Logger logger) {
    CompletableFuture<PieceCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String queryParam = isEmpty(query) ? EMPTY : "&query=" + encodeQuery(query, logger);
      String endpoint = String.format(path, limit, offset, queryParam, lang);
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(jsonOrderLines -> {
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
    //String endpoint = String.format(URL_WITH_LANG_PARAM, resourceByIdPath(PO_LINES, lineId), lang);
    String endpoint = "/orders-storage/pieces/" + pieceId;
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger);
  }
  
  private Map<String, String> getOkapiHeaders(Message<JsonObject> message) {
    Map<String, String> okapiHeaders = new CaseInsensitiveMap<>();
    message.headers().entries().forEach(entry -> okapiHeaders.put(entry.getKey(), entry.getValue()));
    return okapiHeaders;
  }
}
