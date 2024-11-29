package org.folio.service.pieces;

import lombok.extern.log4j.Log4j2;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.models.RequestContext;

import static org.folio.orders.events.utils.EventUtils.createPoLineUpdateEvent;

@Log4j2
public class PieceService {

  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;

  public PieceService(PieceChangeReceiptStatusPublisher receiptStatusPublisher) {
    this.receiptStatusPublisher = receiptStatusPublisher;
  }

  public void receiptConsistencyPiecePoLine(String poLineId, RequestContext requestContext) {
    log.debug("Sending event to verify receipt status for poLineId: {}", poLineId);
    receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, createPoLineUpdateEvent(poLineId), requestContext);
    log.debug("Event to verify receipt status is sent for poLineId: {}", poLineId);
  }

}
