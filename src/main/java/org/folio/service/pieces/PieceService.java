package org.folio.service.pieces;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.models.RequestContext;

import io.vertx.core.json.JsonObject;

public class PieceService {
  private static final Logger logger = LogManager.getLogger(PieceService.class);

  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;

  public PieceService(PieceChangeReceiptStatusPublisher receiptStatusPublisher) {
    this.receiptStatusPublisher = receiptStatusPublisher;
  }

  public void receiptConsistencyPiecePoLine(JsonObject jsonObj, RequestContext requestContext) {
    logger.debug("Sending event to verify receipt status");

    receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, jsonObj, requestContext);

    logger.debug("Event to verify receipt status - sent");
  }
}
