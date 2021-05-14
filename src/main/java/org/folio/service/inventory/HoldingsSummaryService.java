package org.folio.service.inventory;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.HoldingSummary;
import org.folio.service.orders.PurchaseOrderService;
import org.springframework.beans.factory.annotation.Autowired;

public class HoldingsSummaryService {

  private static final Logger logger = LogManager.getLogger();

  private RestClient restClient;

  @Autowired
  private PurchaseOrderService purchaseOrderService;

  public HoldingsSummaryService(RestClient restClient, PurchaseOrderService purchaseOrderService) {
    this.restClient = restClient;
    this.purchaseOrderService = purchaseOrderService;
  }

  public CompletionStage<HoldingSummary> getHoldingsSummary(String holdingId, RequestContext requestContext) {
    HoldingSummary holdingSummary = new HoldingSummary();

    purchaseOrderService.getPurchaseOrderById()
    holdingSummary.withCurrency(null);
    holdingSummary.withOrderStatus(null);
    holdingSummary.withOrderStatusDate(null);
    holdingSummary.withPolNumber(null);
    holdingSummary.withPrice(null);

    return CompletableFuture.completedFuture(holdingSummary);
  }
}
