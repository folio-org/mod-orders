package org.folio.orders.events.handlers;

import static org.folio.orders.events.utils.EventUtils.getPoLineId;
import static org.folio.orders.utils.HelperUtils.getOkapiHeaders;
import static org.folio.service.orders.utils.StatusUtils.calculatePoLineReceiptStatus;

import java.util.List;
import java.util.Map;

import org.folio.helper.BaseHelper;
import org.folio.orders.utils.HelperUtils;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PieceStorageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

@Component("receiptStatusHandler")
public class ReceiptStatusConsistency extends BaseHelper implements Handler<Message<JsonObject>> {

  private final PieceStorageService pieceStorageService;
  private final PurchaseOrderLineService purchaseOrderLineService;


  @Autowired
  public ReceiptStatusConsistency(Vertx vertx, PieceStorageService pieceStorageService, PurchaseOrderLineService purchaseOrderLineService) {
    super(vertx.getOrCreateContext());
    this.pieceStorageService = pieceStorageService;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  @Override
  public void handle(Message<JsonObject> message) {
    var messageFromEventBus = message.body();
    logger.info("Received message body: {}", messageFromEventBus);

    var okapiHeaders = getOkapiHeaders(message);
    var requestContext = new RequestContext(ctx, okapiHeaders);

    var poLineId = getPoLineId(messageFromEventBus);
    var future = purchaseOrderLineService.getOrderLineById(poLineId, requestContext)
      .compose(poLine -> pieceStorageService.getPiecesByLineId(poLineId, requestContext)
        .compose(pieces -> updatePoLineAndOrderStatuses(pieces, poLine, requestContext))
        .onFailure(e -> logger.error("Exception occurred while fetching PoLine by id: '{}'", poLineId, e)))
      .onFailure(e -> logger.error("Exception occurred while fetching pieces by PoLine id: '{}'", poLineId, e));

    completeAllFutures(List.of(future), message);
  }

  private Future<Void> updatePoLineAndOrderStatuses(List<Piece> pieces, PoLine poLine, RequestContext requestContext) {
    if (PoLineCommonUtil.isCancelledOrOngoingStatus(poLine)) {
      return Future.succeededFuture();
    }
    var newStatus = pieces.isEmpty()
      ? poLine.getReceiptStatus()
      : calculatePoLineReceiptStatus(poLine.getId(), pieces);
    boolean statusUpdated = purchaseOrderLineService.updatePoLineReceiptStatusWithoutSave(poLine, newStatus);
    if (!statusUpdated) {
      return Future.succeededFuture();
    }
    return purchaseOrderLineService.saveOrderLine(poLine, requestContext)
      .compose(v -> updateOrderStatus(poLine, okapiHeaders, requestContext))
      .onFailure(e -> logger.error("Exception occurred while updating PoLine by id: '{}'", poLine.getId(), e));
  }

  private Future<Void> updateOrderStatus(PoLine poLine, Map<String, String> okapiHeaders, RequestContext requestContext) {
    var messageContent = JsonObject.of(
      OKAPI_HEADERS, okapiHeaders,
      EVENT_PAYLOAD, JsonArray.of(JsonObject.of(ORDER_ID, poLine.getPurchaseOrderId()))
    );
    HelperUtils.sendEvent(MessageAddress.RECEIVE_ORDER_STATUS_UPDATE, messageContent, requestContext);
    return Future.succeededFuture();
  }

}
