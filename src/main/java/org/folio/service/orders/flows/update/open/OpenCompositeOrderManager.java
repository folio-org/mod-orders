package org.folio.service.orders.flows.update.open;

import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.service.UserService.getCurrentUserId;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.orders.flows.update.unopen.UnOpenCompositeOrderManager;
import org.folio.service.titles.TitlesService;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class OpenCompositeOrderManager {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderManager.class);

  public static final String DISABLE_INSTANCE_MARCHING_CONFIG_NAME = "disableInstanceMatching";
  public static final String DISABLE_INSTANCE_MARCHING_CONFIG_KEY = "isInstanceMatchingDisabled";

  private final PurchaseOrderLineService purchaseOrderLineService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final TitlesService titlesService;
  private final OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  private final OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;
  private final UnOpenCompositeOrderManager unOpenCompositeOrderManager;

  public OpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService,
      EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
      TitlesService titlesService, OpenCompositeOrderInventoryService openCompositeOrderInventoryService,
      OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator,
      UnOpenCompositeOrderManager unOpenCompositeOrderManager) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.titlesService = titlesService;
    this.openCompositeOrderInventoryService = openCompositeOrderInventoryService;
    this.openCompositeOrderFlowValidator = openCompositeOrderFlowValidator;
    this.unOpenCompositeOrderManager = unOpenCompositeOrderManager;
  }

  /**
   * Handles transition of given order to OPEN status.
   *
   * @param compPO Purchase Order to open
   * @return CompletableFuture that indicates when transition is completed
   */
  public Future<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, JsonObject config, RequestContext requestContext) {
    logger.debug("OpenCompositeOrderManager.process compPO.id={}", compPO.getId());
    updateIncomingOrder(compPO, poFromStorage, requestContext);
    return openCompositeOrderFlowValidator.validate(compPO, poFromStorage, requestContext)
      .compose(aCompPO -> titlesService.fetchNonPackageTitles(compPO, requestContext))
      .map(lineIdTitles -> populateInstanceId(lineIdTitles, compPO.getPoLines()))
      .compose(linesIdTitles -> openCompositeOrderInventoryService.processInventory(linesIdTitles, compPO, isInstanceMatchingDisabled(config), requestContext))
      .compose(v -> finishProcessingEncumbrancesForOpenOrder(compPO, poFromStorage, requestContext))
      .map(v -> asFuture(() -> changePoLineStatuses(compPO)))
      .compose(v -> openOrderUpdatePoLinesSummary(compPO.getPoLines(), requestContext));
  }

  private boolean isInstanceMatchingDisabled(JsonObject config) {
    return Optional.ofNullable(config.getString(DISABLE_INSTANCE_MARCHING_CONFIG_NAME))
      .map(instMatch -> new JsonObject(instMatch).getBoolean(DISABLE_INSTANCE_MARCHING_CONFIG_KEY))
      .orElse(false);
  }

  public Future<Void> openOrderUpdatePoLinesSummary(List<PoLine> poLines, RequestContext requestContext) {
    logger.debug("OpenCompositeOrderManager.openOrderUpdatePoLinesSummary");
    return GenericCompositeFuture.join(poLines.stream()
      .map(this::removeLocationId)
      .map(line -> purchaseOrderLineService.saveOrderLine(line, requestContext))
      .toList())
      .mapEmpty();
  }

  private PoLine removeLocationId(PoLine poLine) {
    poLine.getLocations().forEach(location ->
    {
      if (location.getHoldingId() != null) {
        location.setLocationId(null);
      }
    });
    return poLine;
  }


  private Map<String, List<Title>> populateInstanceId(Map<String, List<Title>> lineIdsTitles, List<PoLine> lines) {
    getNonPackageLines(lines).forEach(line -> {
      if (lineIdsTitles.containsKey(line.getId())) {
        line.setInstanceId(lineIdsTitles.get(line.getId()).getFirst().getInstanceId());
      }
    });
    return lineIdsTitles;
  }

  private void changePoLineStatuses(CompositePurchaseOrder compPO) {
    compPO.getPoLines().forEach(poLine -> {
      changeReceiptStatus(compPO, poLine);
      changePaymentStatus(compPO, poLine);
    });
  }

  private void changePaymentStatus(CompositePurchaseOrder compPO, PoLine poLine) {
    if (compPO.getOrderType().equals(CompositePurchaseOrder.OrderType.ONGOING)
    && !poLine.getPaymentStatus().equals(PoLine.PaymentStatus.PAYMENT_NOT_REQUIRED) ) {

      poLine.setPaymentStatus(PoLine.PaymentStatus.ONGOING);
    }
    else if (poLine.getPaymentStatus() == PoLine.PaymentStatus.PENDING) {
      poLine.setPaymentStatus(PoLine.PaymentStatus.AWAITING_PAYMENT);
    }
  }

  private void changeReceiptStatus(CompositePurchaseOrder compPO, PoLine poLine) {
    if (compPO.getOrderType().equals(CompositePurchaseOrder.OrderType.ONGOING)
    && !poLine.getReceiptStatus().equals(PoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED)) {

      poLine.setReceiptStatus(PoLine.ReceiptStatus.ONGOING);
    }
    else if (poLine.getReceiptStatus() == PoLine.ReceiptStatus.PENDING) {

      poLine.setReceiptStatus(PoLine.ReceiptStatus.AWAITING_RECEIPT);
    }
  }

  private List<PoLine> getNonPackageLines(List<PoLine> poLines) {
    return poLines.stream().filter(line -> !line.getIsPackage()).toList();
  }

  private Future<Void> finishProcessingEncumbrancesForOpenOrder(CompositePurchaseOrder compPO,
      CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    logger.debug("OpenCompositeOrderManager.finishProcessingEncumbrancesForOpenOrder compPO.id={}", compPO.getId());
    EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.PENDING_TO_OPEN);
    return strategy.processEncumbrances(compPO, poFromStorage, requestContext)
      .onSuccess(v -> logger.info("Finished processing encumbrances to open the order, order id={}", compPO.getId()))
      .recover(t -> {
        logger.error("Error when processing encumbrances to open the order, order id={}", compPO.getId(), t);
        // There was an error when processing the encumbrances despite the previous validations.
        // Try to rollback inventory changes
        return unOpenCompositeOrderManager.rollbackInventory(compPO, requestContext)
          .onSuccess(v -> logger.info("Successfully rolled back inventory changes, order id={}", compPO.getId()))
          .onFailure(t2 -> logger.error("Error when trying to rollback inventory changes, order id={}", compPO.getId(), t2))
          .transform(v -> Future.failedFuture(t));
      });
  }

  private void updateIncomingOrder(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
    compPO.setWorkflowStatus(OPEN);
    compPO.setOpenedById(getCurrentUserId(requestContext.getHeaders()));
    compPO.setDateOrdered(new Date());
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      CompositePurchaseOrder clonedPoFromStorage = JsonObject.mapFrom(poFromStorage).mapTo(CompositePurchaseOrder.class);
      compPO.setPoLines(clonedPoFromStorage.getPoLines());
    }
    compPO.getPoLines().forEach(poLine -> PoLineCommonUtil.updateLocationsQuantity(poLine.getLocations()));
  }

}
