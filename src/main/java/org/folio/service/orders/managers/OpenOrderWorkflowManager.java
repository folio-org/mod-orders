package org.folio.service.orders.managers;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.inventoryUpdateNotRequired;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.OPEN;
import static org.folio.rest.jaxrs.model.CompositePurchaseOrder.WorkflowStatus.PENDING;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.InventoryManager;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.FundDistributionUtils;
import org.folio.orders.utils.validators.CompositePoLineValidationUtil;
import org.folio.orders.utils.validators.OngoingOrderValidator;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.orders.PurchaseOrderLineService;
import org.folio.service.pieces.PiecesService;
import org.folio.service.titles.TitlesService;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.json.JsonObject;

public class OpenOrderWorkflowManager implements OrderWorkflowManager {
  private static final Logger logger = LogManager.getLogger();

  @Autowired
  private ExpenseClassValidationService expenseClassValidationService;
  @Autowired
  private EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  @Autowired
  private PurchaseOrderLineService purchaseOrderLineService;
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private PiecesService piecesService;
  @Autowired
  private InventoryManager inventoryManager;

  @Override
  public CompletableFuture<Void> process(CompositePurchaseOrder compPO, RequestContext requestContext) {
    compPO.setWorkflowStatus(OPEN);
    compPO.setDateOrdered(new Date());
    return expenseClassValidationService.validateExpenseClasses(compPO.getCompositePoLines(), requestContext)
      .thenAccept(v -> FundDistributionUtils.validateFundDistributionTotal(compPO.getCompositePoLines()))
      .thenAccept(v -> OngoingOrderValidator.validate(compPO))
      .thenApply(v -> this.validateMaterialTypes(compPO))
      .thenCompose(v -> fetchNonPackageTitles(compPO, requestContext))
      .thenCompose(linesIdTitles -> {
        populateInstanceId(linesIdTitles, compPO.getCompositePoLines());
        return openOrderUpdateInventory(linesIdTitles, compPO, requestContext);
      })
      .thenApply(aVoid -> encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.PENDING_TO_OPEN))
      .thenCompose(strategy -> strategy.processEncumbrances(compPO, requestContext))
      .thenAccept(ok -> changePoLineStatuses(compPO))
      .thenCompose(ok -> openOrderUpdatePoLinesSummary(compPO.getCompositePoLines(), requestContext));
  }

  public CompletableFuture<Void> openOrderUpdatePoLinesSummary(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
    return CompletableFuture.allOf( compositePoLines.stream()
      .map(this::openOrderConvertToPoLine)
      .map(line -> purchaseOrderLineService.updateOrderLine(line, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  public PoLine openOrderConvertToPoLine(CompositePoLine compPoLine) {
    JsonObject pol = JsonObject.mapFrom(compPoLine);
    pol.remove(ALERTS);
    pol.remove(REPORTING_CODES);
    PoLine poLine = pol.mapTo(PoLine.class);
    poLine.setAlerts(compPoLine.getAlerts().stream().map(Alert::getId).collect(toList()));
    poLine.setReportingCodes(compPoLine.getReportingCodes().stream().map(ReportingCode::getId).collect(toList()));
    return poLine;
  }

  /**
   * Creates Inventory records associated with given PO line and updates PO line with corresponding links.
   *
   * @param compPOL Composite PO line to update Inventory for
   * @return CompletableFuture with void.
   */
  private CompletableFuture<Void> openOrderUpdateInventory(CompositePoLine compPOL, String titleId, RequestContext requestContext) {
    if (Boolean.TRUE.equals(compPOL.getIsPackage())) {
      return completedFuture(null);
    }
    if (inventoryUpdateNotRequired(compPOL)) {
      // don't create pieces, if no inventory updates and receiving not required
      if (isReceiptNotRequired(compPOL.getReceiptStatus())) {
        return completedFuture(null);
      }
      return piecesService.createPieces(compPOL, titleId, Collections.emptyList(), true, requestContext).thenRun(
        () -> logger.info("Create pieces for PO Line with '{}' id where inventory updates are not required", compPOL.getId()));
    }

    return inventoryManager.handleInstanceRecord(compPOL, requestContext)
      .thenCompose(poLineWithInstance -> inventoryManager.handleHoldingsAndItemsRecords(poLineWithInstance, requestContext))
      .thenCompose(piecesWithItemId -> {
        if (isReceiptNotRequired(compPOL.getReceiptStatus())) {
          return completedFuture(null);
        }
        //create pieces only if receiving is required
        return piecesService.createPieces(compPOL, titleId, piecesWithItemId, true, requestContext);
      });
  }

  private boolean isReceiptNotRequired(CompositePoLine.ReceiptStatus receiptStatus) {
    return receiptStatus == CompositePoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED;
  }

  private CompletableFuture<Void> openOrderUpdateInventory(Map<String, List<Title>> lineIdsTitles, CompositePurchaseOrder compPO,
                                                           RequestContext requestContext) {
    return CompletableFuture.allOf(
      compPO.getCompositePoLines()
        .stream()
        .map(poLine -> openOrderUpdateInventory(poLine, getFirstTitleIdIfExist(lineIdsTitles, poLine), requestContext))
        .toArray(CompletableFuture[]::new)
    );
  }

  private CompositePurchaseOrder validateMaterialTypes(CompositePurchaseOrder purchaseOrder){
    if (purchaseOrder.getWorkflowStatus() != PENDING) {
      List<Error> errors = CompositePoLineValidationUtil.checkMaterialsAvailability(purchaseOrder.getCompositePoLines());
      if (!errors.isEmpty()) {
        throw new HttpException(422, errors.get(0));
      }
    }
    return purchaseOrder;
  }

  private CompletableFuture<Map<String, List<Title>>> fetchNonPackageTitles(CompositePurchaseOrder compPO, RequestContext requestContext) {
    List<String> lineIds = getNonPackageLineIds(compPO.getCompositePoLines());
    return titlesService.getTitlesByPoLineIds(lineIds, requestContext);
  }

  private List<String> getNonPackageLineIds(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).map(CompositePoLine::getId).collect(toList());
  }

  private void populateInstanceId(Map<String, List<Title>> lineIdsTitles, List<CompositePoLine> lines) {
    getNonPackageLines(lines).forEach(line -> {
      if (lineIdsTitles.containsKey(line.getId())) {
        line.setInstanceId(lineIdsTitles.get(line.getId()).get(0).getInstanceId());
      }
    });
  }

  private List<CompositePoLine> getNonPackageLines(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().filter(line -> !line.getIsPackage()).collect(toList());
  }

  private String getFirstTitleIdIfExist(Map<String, List<Title>> lineIdsTitles, CompositePoLine poLine) {
    return Optional.ofNullable(lineIdsTitles.get(poLine.getId()))
      .map(titles -> titles.get(0))
      .map(Title::getId)
      .orElse(null);
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

}
