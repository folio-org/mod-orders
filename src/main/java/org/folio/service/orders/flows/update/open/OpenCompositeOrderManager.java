package org.folio.service.orders.flows.update.open;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.AsyncUtil;
import org.folio.orders.utils.PoLineCommonUtil;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.titles.TitlesService;

import io.vertx.core.json.JsonObject;

public class OpenCompositeOrderManager {
  private static final Logger logger = LogManager.getLogger(OpenCompositeOrderManager.class);

  private final PurchaseOrderLineService purchaseOrderLineService;
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final TitlesService titlesService;
  private final OpenCompositeOrderInventoryService openCompositeOrderInventoryService;
  private final OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator;

  public OpenCompositeOrderManager(PurchaseOrderLineService purchaseOrderLineService, EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
                    TitlesService titlesService, OpenCompositeOrderInventoryService openCompositeOrderInventoryService,
                    OpenCompositeOrderFlowValidator openCompositeOrderFlowValidator) {
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.titlesService = titlesService;
    this.openCompositeOrderInventoryService = openCompositeOrderInventoryService;
    this.openCompositeOrderFlowValidator = openCompositeOrderFlowValidator;
  }

  /**
   * Handles transition of given order to OPEN status.
   *
   * @param compPO Purchase Order to open
   * @return CompletableFuture that indicates when transition is completed
   */
  public CompletableFuture<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage, RequestContext requestContext) {
      return AsyncUtil.executeBlocking(requestContext.getContext(), false, () -> updateIncomingOrder(compPO, poFromStorage))
        .thenCompose(aVoid -> openCompositeOrderFlowValidator.validate(compPO, poFromStorage, requestContext))
        .thenCompose(aCompPO -> titlesService.fetchNonPackageTitles(compPO, requestContext))
        .thenCompose(linesIdTitles -> {
          populateInstanceId(linesIdTitles, compPO.getCompositePoLines());
          return openCompositeOrderInventoryService.processInventory(linesIdTitles, compPO, requestContext);
        })
        .thenCompose(v -> finishProcessingEncumbrancesForOpenOrder(compPO, poFromStorage, requestContext))
        .thenAccept(ok -> changePoLineStatuses(compPO))
        .thenCompose(ok -> openOrderUpdatePoLinesSummary(compPO.getCompositePoLines(), requestContext));
  }

  public CompletableFuture<Void> openOrderUpdatePoLinesSummary(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return CompletableFuture.allOf( compositePoLines.stream()
      .map(this::removeLocationId)
      .map(this::convertToPoLine)
      .map(line -> purchaseOrderLineService.saveOrderLine(line, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  public PoLine convertToPoLine(CompositePoLine compPoLine) {
    JsonObject pol = JsonObject.mapFrom(compPoLine);
    pol.remove(ALERTS);
    pol.remove(REPORTING_CODES);
    PoLine poLine = pol.mapTo(PoLine.class);
    poLine.setAlerts(compPoLine.getAlerts().stream().map(Alert::getId).collect(toList()));
    poLine.setReportingCodes(compPoLine.getReportingCodes().stream().map(ReportingCode::getId).collect(toList()));
    return poLine;
  }

  private CompositePoLine removeLocationId(CompositePoLine compositePoLine) {
    compositePoLine.getLocations().forEach(location ->
    {
      if (location.getHoldingId() != null) {
        location.setLocationId(null);
      }
    });
    return compositePoLine;
  }


  private void populateInstanceId(Map<String, List<Title>> lineIdsTitles, List<CompositePoLine> lines) {
    getNonPackageLines(lines).forEach(line -> {
      if (lineIdsTitles.containsKey(line.getId())) {
        line.setInstanceId(lineIdsTitles.get(line.getId()).get(0).getInstanceId());
      }
    });
  }

  private void changePoLineStatuses(CompositePurchaseOrder compPO) {
    compPO.getCompositePoLines().forEach(poLine -> {
      changeReceiptStatus(compPO, poLine);
      changePaymentStatus(compPO, poLine);
    });
  }

  private void changePaymentStatus(CompositePurchaseOrder compPO, CompositePoLine poLine) {
    if (compPO.getOrderType().equals(CompositePurchaseOrder.OrderType.ONGOING)) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.ONGOING);
    }
    else if (poLine.getPaymentStatus() == CompositePoLine.PaymentStatus.PENDING) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
    }
  }

  private void changeReceiptStatus(CompositePurchaseOrder compPO, CompositePoLine poLine) {
    if (compPO.getOrderType().equals(CompositePurchaseOrder.OrderType.ONGOING)) {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.ONGOING);
    }
    else if (poLine.getReceiptStatus() == CompositePoLine.ReceiptStatus.PENDING) {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    }
  }

  private List<CompositePoLine> getNonPackageLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).collect(toList());
  }

  private CompletableFuture<Void> finishProcessingEncumbrancesForOpenOrder(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
                            RequestContext requestContext) {
    EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.PENDING_TO_OPEN);
    return strategy.processEncumbrances(compPO, poFromStorage, requestContext)
      .handle((v, t) -> {
        if (t == null)
          return CompletableFuture.completedFuture(v);
        // There was an error when processing the encumbrances despite the previous validations.
        // Order lines should be saved to avoid leaving an open order with locationId instead of holdingId.
        return openOrderUpdatePoLinesSummary(compPO.getCompositePoLines(), requestContext)
          .handle((vv, tt) -> {
            throw new CompletionException(t.getCause());
          });
      })
      .thenCompose(v -> v) // wait for future returned by handle
      .thenApply(v -> null);
  }

  private void updateIncomingOrder(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage) {
    compPO.setWorkflowStatus(OPEN);
    compPO.setDateOrdered(new Date());
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      CompositePurchaseOrder clonedPoFromStorage = JsonObject.mapFrom(poFromStorage).mapTo(CompositePurchaseOrder.class);
      compPO.setCompositePoLines(clonedPoFromStorage.getCompositePoLines());
    }
    compPO.getCompositePoLines().forEach(poLine -> PoLineCommonUtil.updateLocationsQuantity(poLine.getLocations()));
  }
}
