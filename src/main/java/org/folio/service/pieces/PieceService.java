package org.folio.service.pieces;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;

import io.vertx.core.json.JsonObject;

public class PieceService {
  private static final Logger logger = LogManager.getLogger(PieceService.class);

  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final InventoryManager inventoryManager;
  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;

  public PieceService(PieceStorageService pieceStorageService, ProtectionService protectionService,
                      PurchaseOrderLineService purchaseOrderLineService,
                      InventoryManager inventoryManager, PieceChangeReceiptStatusPublisher receiptStatusPublisher,
                      PieceUpdateInventoryService pieceUpdateInventoryService) {
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryManager = inventoryManager;
    this.receiptStatusPublisher = receiptStatusPublisher;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
  }

  public void receiptConsistencyPiecePoLine(JsonObject jsonObj, RequestContext requestContext) {
    logger.debug("Sending event to verify receipt status");

    receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, jsonObj, requestContext);

    logger.debug("Event to verify receipt status - sent");
  }
}
