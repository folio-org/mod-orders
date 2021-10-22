package org.folio.service.pieces;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.ProtectionService;
import org.folio.service.inventory.InventoryManager;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.PurchaseOrderService;

import io.vertx.core.json.JsonObject;

public class PieceService {
  private static final Logger logger = LogManager.getLogger(PieceService.class);

  private final PieceStorageService pieceStorageService;
  private final ProtectionService protectionService;
  private final PurchaseOrderService purchaseOrderService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final InventoryManager inventoryManager;
  private final PieceChangeReceiptStatusPublisher receiptStatusPublisher;
  private final PieceUpdateInventoryService pieceUpdateInventoryService;

  public PieceService(PieceStorageService pieceStorageService, ProtectionService protectionService,
                      PurchaseOrderLineService purchaseOrderLineService,
                      InventoryManager inventoryManager, PieceChangeReceiptStatusPublisher receiptStatusPublisher,
                      PurchaseOrderService purchaseOrderService, PieceUpdateInventoryService pieceUpdateInventoryService) {
    this.pieceStorageService = pieceStorageService;
    this.protectionService = protectionService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.inventoryManager = inventoryManager;
    this.receiptStatusPublisher = receiptStatusPublisher;
    this.purchaseOrderService = purchaseOrderService;
    this.pieceUpdateInventoryService = pieceUpdateInventoryService;
  }

  public CompletableFuture<CompositePurchaseOrder> getCompositePurchaseOrder(String purchaseOrderId, RequestContext requestContext) {
    return purchaseOrderService.getCompositeOrderById(purchaseOrderId, requestContext)
      .exceptionally(t -> {
        Throwable cause = t.getCause();
        // The case when specified order does not exist
        if (cause instanceof HttpException && ((HttpException) cause).getCode() == Response.Status.NOT_FOUND.getStatusCode()) {
          throw new HttpException(404, ErrorCodes.ORDER_NOT_FOUND);
        }
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
  }

  public void receiptConsistencyPiecePoLine(JsonObject jsonObj, RequestContext requestContext) {
    logger.debug("Sending event to verify receipt status");

    receiptStatusPublisher.sendEvent(MessageAddress.RECEIPT_STATUS, jsonObj, requestContext);

    logger.debug("Event to verify receipt status - sent");
  }
}
