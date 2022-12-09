package org.folio.orders.events.handlers;

import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.AWAITING_RECEIPT;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.FULLY_RECEIVED;
import static org.folio.rest.jaxrs.model.PoLine.ReceiptStatus.PARTIALLY_RECEIVED;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.folio.completablefuture.AsyncUtil;
import org.folio.helper.BaseHelper;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.Piece.ReceivingStatus;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLine.ReceiptStatus;
import org.folio.service.orders.PurchaseOrderLineService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import one.util.streamex.StreamEx;

@Component("receiptStatusHandler")
public class ReceiptStatusConsistency extends BaseHelper implements Handler<Message<JsonObject>> {

  private static final int LIMIT = Integer.MAX_VALUE;
  private static final String PIECES_ENDPOINT = resourcesPath(PIECES_STORAGE) + "?query=poLineId==%s&limit=%s";

  private final PurchaseOrderLineService purchaseOrderLineService;


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
    var requestContext = new RequestContext(ctx, okapiHeaders);
    List<Future<Void>> futures = new ArrayList<>();
    Promise<Void> promise = Promise.promise();
    futures.add(promise.future());

    String poLineIdUpdate = messageFromEventBus.getString("poLineIdUpdate");
    String query = String.format(PIECES_ENDPOINT, poLineIdUpdate, LIMIT);

    // 1. Get all pieces for poLineId
    getPieces(query, requestContext)
      .onSuccess(listOfPieces ->
      // 2. Get PoLine for the poLineId which will be used to calculate PoLineReceiptStatus
      purchaseOrderLineService.getOrderLineById(poLineIdUpdate, requestContext)
        .map(poLine -> {
          if (poLine.getReceiptStatus().equals(PoLine.ReceiptStatus.ONGOING)) {
            promise.complete();
          } else {
            calculatePoLineReceiptStatus(poLine, listOfPieces)
              .compose(status -> purchaseOrderLineService.updatePoLineReceiptStatus(poLine, status, requestContext))
              .map(updatedPoLineId -> {
                if (updatedPoLineId != null) {
                  // send event to update order status
                  updateOrderStatus(poLine, okapiHeaders, requestContext);
                }
                promise.complete();
                return null;
              })
              .onFailure(e -> {
                logger.error("The error updating poLine by id {}", poLineIdUpdate, e);
                promise.fail(e);
              });
          }
          return null;
        })
        .onFailure(e -> {
          logger.error("The error getting poLine by id {}", poLineIdUpdate, e);
          promise.fail(e);
        }))
      .onFailure(e -> {
        logger.error("The error happened getting all pieces by poLine {}", poLineIdUpdate, e);
        promise.fail(e);
      });

    // Now wait for all operations to be completed and send reply
    completeAllFutures(futures, message);
  }

  private void updateOrderStatus(PoLine poLine, Map<String, String> okapiHeaders, RequestContext requestContext) {
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
    HelperUtils.sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, messageContent, requestContext);
  }

  private Future<ReceiptStatus> calculatePoLineReceiptStatus(PoLine poLine,
      List<org.folio.rest.acq.model.Piece> pieces) {

    if (pieces.isEmpty()) {
      return Future.succeededFuture(poLine.getReceiptStatus());
    } else {
      return getPiecesQuantityByPoLineAndStatus(ReceivingStatus.EXPECTED, pieces)
        .compose(expectedQty -> calculatePoLineReceiptStatus(expectedQty, pieces))
        .onFailure(e -> logger.error("The expected receipt status for PO Line '{}' cannot be calculated", poLine.getId(), e));
    }
  }

  private Future<ReceiptStatus> calculatePoLineReceiptStatus(int expectedPiecesQuantity,
      List<org.folio.rest.acq.model.Piece> pieces) {

    if (expectedPiecesQuantity == 0) {
      return Future.succeededFuture(FULLY_RECEIVED);
    }
    // Partially Received: In case there is at least one successfully received
    // piece
    if (StreamEx.of(pieces)
      .anyMatch(piece -> ReceivingStatus.RECEIVED == piece.getReceivingStatus())) {
      return Future.succeededFuture(PARTIALLY_RECEIVED);
    }
    // Pieces were rolled-back to Expected. In this case we have to check if
    // there is any Received piece in the storage
    return getPiecesQuantityByPoLineAndStatus(ReceivingStatus.RECEIVED, pieces)
      .map(receivedQty -> receivedQty == 0 ? AWAITING_RECEIPT : PARTIALLY_RECEIVED);
  }

  private Future<Integer> getPiecesQuantityByPoLineAndStatus(ReceivingStatus receivingStatus, List<Piece> pieces) {
    return AsyncUtil.executeBlocking(ctx, false, () -> (int) pieces.stream()
      .filter(piece -> piece.getReceivingStatus() == receivingStatus)
      .count());
  }

  Future<List<Piece>> getPieces(String endpoint, RequestContext requestContext) {
    return new RestClient().get(endpoint, PieceCollection.class, requestContext)
      .map(PieceCollection::getPieces);
  }
}
