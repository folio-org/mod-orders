package org.folio.service.orders;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.ROLLOVER_NOT_COMPLETED;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import javax.money.Monetary;
import javax.money.MonetaryAmount;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.ReEncumbranceHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.service.finance.BudgetRestrictionService;
import org.folio.service.finance.RolloverErrorService;
import org.folio.service.finance.RolloverRetrieveService;
import org.folio.service.finance.TransactionService;
import org.folio.service.finance.TransactionSummariesService;
import org.javamoney.moneta.Money;

import io.vertx.core.json.JsonObject;

public class OrderReEncumberService {

  protected final Logger logger = LogManager.getLogger();

  private final CompositePurchaseOrderService compositePurchaseOrderService;
  private final ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder;
  private final RolloverErrorService rolloverErrorService;
  private final RolloverRetrieveService rolloverRetrieveService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TransactionService transactionService;
  private final TransactionSummariesService transactionSummaryService;
  private final BudgetRestrictionService budgetRestrictionService;

  public OrderReEncumberService(CompositePurchaseOrderService compositePurchaseOrderService,
                                ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder,
                                RolloverErrorService rolloverErrorService,
                                RolloverRetrieveService rolloverRetrieveService,
                                PurchaseOrderLineService purchaseOrderLineService,
                                TransactionService transactionService,
                                TransactionSummariesService transactionSummaryService,
                                BudgetRestrictionService budgetRestrictionService) {
    this.compositePurchaseOrderService = compositePurchaseOrderService;
    this.reEncumbranceHoldersBuilder = reEncumbranceHoldersBuilder;
    this.rolloverErrorService = rolloverErrorService;
    this.rolloverRetrieveService = rolloverRetrieveService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionService = transactionService;
    this.transactionSummaryService = transactionSummaryService;
    this.budgetRestrictionService = budgetRestrictionService;
  }

  public CompletableFuture<CompositePurchaseOrder> populateNeedReEncumberFlag(CompositePurchaseOrder compPO,
      RequestContext requestContext) {
    compPO.setNeedReEncumber(false);
    List<ReEncumbranceHolder> reEncumbranceHolders = reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(compPO);
    if (reEncumbranceHolders.isEmpty()) {
      return CompletableFuture.completedFuture(compPO);
    }
    return reEncumbranceHoldersBuilder.withFunds(reEncumbranceHolders, requestContext)
      .thenCompose(holderList -> {
        if (holderList.stream()
          .anyMatch(holder -> Objects.isNull(holder.getFund()))) {
          return CompletableFuture.completedFuture(compPO.withNeedReEncumber(true));
        }
        return reEncumbranceHoldersBuilder.withCurrentFiscalYear(holderList, requestContext)
          .thenCompose(holders -> reEncumbranceHoldersBuilder.withRollovers(holders, requestContext))
          .thenCompose(holders -> getLedgersIdsRolloverNotCompleted(holders, requestContext).thenCompose(ledgerIds -> {
            if (isRolloversPartiallyCompleted(holders, ledgerIds)) {
              return CompletableFuture.completedFuture(compPO.withNeedReEncumber(true));
            }
            return rolloverErrorService.getLedgerFyRolloverErrors(compPO.getId(), requestContext)
              .thenApply(LedgerFiscalYearRolloverErrorCollection::getLedgerFiscalYearRolloverErrors)
              .thenApply(ledgerFyRolloverErrors -> compPO.withNeedReEncumber(!ledgerFyRolloverErrors.isEmpty()));
          }));
      });
  }

  public CompletableFuture<Void> reEncumber(String orderId, RequestContext requestContext) {
    return compositePurchaseOrderService.getCompositeOrderById(orderId, requestContext)
      .thenApply(reEncumbranceHoldersBuilder::buildReEncumbranceHoldersWithOrdersData)
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withFunds(holders, requestContext))
      .thenApply(this::checkAllFundsFound)
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withLedgers(holders, requestContext))
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withCurrentFiscalYear(holders, requestContext))
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withRollovers(holders, requestContext))
      .thenApply(reEncumbranceHoldersBuilder::withEncumbranceRollover)
      .thenCompose(holders -> checkRolloverHappensForAllLedgers(holders, requestContext))
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withBudgets(holders, requestContext))
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withConversion(holders, requestContext))
      .thenApply(this::filterNeedReEncumbranceHolders)
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withEncumbrances(holders, requestContext))
      .thenApply(this::adjustPoLinesCost)
      .thenApply(this::verifyBudgets)
      .thenCompose(holders -> createEncumbrances(holders, requestContext))
      .thenApply(this::updateLinkToEncumbrances)
      .thenCompose(holders -> deleteRolloverErrors(orderId, requestContext).thenApply(aVoid -> holders))
      .thenCompose(holders -> updatePoLines(holders, requestContext));
  }

  private List<ReEncumbranceHolder> checkAllFundsFound(List<ReEncumbranceHolder> holders) {
    List<Parameter> parameters =  holders.stream()
            .filter(holder -> Objects.isNull(holder.getFund()))
            .map(ReEncumbranceHolder::getFundId)
            .map(id -> new Parameter().withValue(id).withKey("fund"))
            .collect(toList());
    if (parameters.isEmpty()) {
      return holders;
    }
    throw new HttpException(404, FUNDS_NOT_FOUND.toError().withParameters(parameters));
  }

  private CompletableFuture<List<ReEncumbranceHolder>> checkRolloverHappensForAllLedgers(List<ReEncumbranceHolder> holders,
                                                                                         RequestContext requestContext) {
    return getLedgersIdsRolloverNotCompleted(holders, requestContext)
            .thenAccept(ledgerIds -> {
              if (isRolloversPartiallyCompleted(holders, ledgerIds)) {
                Parameter parameter = new Parameter().withKey("ledgerIds")
                        .withValue(String.join(", ", ledgerIds));
                throw new HttpException(400, ROLLOVER_NOT_COMPLETED.toError()
                        .withParameters(Collections.singletonList(parameter)));
              }
            })
            .thenApply(aVoid -> holders);
  }

  private boolean isRolloversPartiallyCompleted(List<ReEncumbranceHolder> holders, List<String> ledgerIds) {
    return !ledgerIds.isEmpty()
            && ledgerIds.size() != holders.stream().map(ReEncumbranceHolder::getFund).map(Fund::getLedgerId).distinct().count();
  }

  private CompletableFuture<List<String>> getLedgersIdsRolloverNotCompleted(List<ReEncumbranceHolder> holders, RequestContext requestContext) {
    List<String> ledgerIds = holders.stream()
            .filter(holder -> Objects.isNull(holder.getRollover()))
            .map(ReEncumbranceHolder::getFund)
            .map(Fund::getLedgerId)
            .distinct()
            .collect(toList());

    return CompletableFuture.allOf(holders.stream()
            .filter(holder -> Objects.nonNull(holder.getRollover()))
            .map(ReEncumbranceHolder::getRollover)
            .map(rollover -> rolloverRetrieveService.getRolloversProgress(rollover.getId(), requestContext)
                    .thenAccept(progresses -> {
                      if (isRolloverNotCompleted(progresses)) {
                        ledgerIds.add(rollover.getLedgerId());
                      }
                    }))
            .toArray(CompletableFuture[]::new))
            .thenApply(aVoid -> ledgerIds);
  }

  private boolean isRolloverNotCompleted(List<LedgerFiscalYearRolloverProgress> progresses) {
    return progresses.isEmpty() || progresses.stream()
            .anyMatch(progress -> progress.getOverallRolloverStatus() == LedgerFiscalYearRolloverProgress.RolloverStatus.IN_PROGRESS
                    || progress.getOverallRolloverStatus() == LedgerFiscalYearRolloverProgress.RolloverStatus.NOT_STARTED);
  }

  private List<ReEncumbranceHolder> filterNeedReEncumbranceHolders(List<ReEncumbranceHolder> reEncumbranceHolders) {
    return reEncumbranceHolders.stream()
            .filter(this::isRolloverRequired)
            .collect(toList());
  }

  private boolean isRolloverRequired(ReEncumbranceHolder reEncumbranceHolder) {
    return Objects.nonNull(reEncumbranceHolder.getEncumbranceRollover());
  }

  private CompletableFuture<Void> deleteRolloverErrors(String orderId, RequestContext requestContext) {
    return rolloverErrorService.getLedgerFyRolloverErrors(orderId, requestContext)
            .thenApply(LedgerFiscalYearRolloverErrorCollection::getLedgerFiscalYearRolloverErrors)
            .thenCompose(errors -> rolloverErrorService.deleteRolloverErrors(errors, requestContext));
  }

  private CompletableFuture<Void> updatePoLines(List<ReEncumbranceHolder> holders, RequestContext requestContext) {
    List<PoLine> lines = holders.stream().map(ReEncumbranceHolder::getPoLine).map(poLine -> {
      List<String> alertIds = poLine.getAlerts().stream().map(Alert::getId).collect(toList());
      List<String> codeIds = poLine.getReportingCodes().stream().map(ReportingCode::getId).collect(toList());
      JsonObject jsonPoLine = JsonObject.mapFrom(poLine);
      jsonPoLine.remove(ALERTS);
      jsonPoLine.remove(REPORTING_CODES);
      return jsonPoLine.mapTo(PoLine.class).withAlerts(alertIds).withReportingCodes(codeIds);
    }).collect(toList());
    return purchaseOrderLineService.updateOrderLines(lines, requestContext);
  }

  private List<ReEncumbranceHolder> verifyBudgets(List<ReEncumbranceHolder> holders) {
    Map<Budget, List<Transaction>> budgetTransactionsMap = holders.stream()
            .filter(holder -> StringUtils.isEmpty(holder.getToFYEncumbrance().getId()))
            .filter(ReEncumbranceHolder::getRestrictEncumbrance)
            .collect(groupingBy(ReEncumbranceHolder::getBudget,
                    HashMap::new,
                    Collectors.mapping(ReEncumbranceHolder::getToFYEncumbrance, toList())));

    budgetRestrictionService.checkEnoughMoneyInBudgets(budgetTransactionsMap);

    return holders;
  }


  private List<ReEncumbranceHolder> updateLinkToEncumbrances(List<ReEncumbranceHolder> holders) {
    holders.forEach(holder -> holder.getFundDistribution().setEncumbrance(holder.getToFYEncumbrance().getId()));
    return holders;
  }


  private List<ReEncumbranceHolder> adjustPoLinesCost(List<ReEncumbranceHolder> reEncumbranceHolders) {
    Map<CompositePoLine, List<ReEncumbranceHolder>> poLineHoldersMap = reEncumbranceHolders.stream()
      .collect(groupingBy(ReEncumbranceHolder::getPoLine));
      poLineHoldersMap.forEach(this::adjustPoLineCost);

    return reEncumbranceHolders;

  }

  private void adjustPoLineCost(CompositePoLine poLine, List<ReEncumbranceHolder> holders) {
    holders.stream()
            .map(ReEncumbranceHolder::getPoLineToFyConversion)
            .filter(Objects::nonNull)
            .findFirst()
            .ifPresent(conversion -> {
              MonetaryAmount encumbranceTotalAmount = holders
                      .stream()
                      .map(ReEncumbranceHolder::getToFYEncumbrance)
                      .map(transaction -> Money.of(transaction.getAmount(), transaction.getCurrency()))
                      .reduce(Money::add)
                      .orElseGet(() -> Money.zero(Monetary.getCurrency(poLine
                              .getCost()
                              .getCurrency())));
              MonetaryAmount poLineEstimatedPrice = HelperUtils.calculateEstimatedPrice(poLine
                      .getCost()).with(conversion);
              poLine.getCost().setFyroAdjustmentAmount(encumbranceTotalAmount.subtract(poLineEstimatedPrice).getNumber().doubleValue());
            });
  }


  private CompletableFuture<List<ReEncumbranceHolder>> createEncumbrances(List<ReEncumbranceHolder> holders,
      RequestContext requestContext) {
    List<ReEncumbranceHolder> holderForCreateEncumbrances = holders.stream()
      .filter(holder -> StringUtils.isEmpty(holder.getToFYEncumbrance().getId()))
      .collect(toList());

    return holders.stream()
      .findFirst()
      .map(ReEncumbranceHolder::getPurchaseOrder)
      .map(purchaseOrder -> {

        String orderId = purchaseOrder.getId();
        return transactionSummaryService.updateOrderTransactionSummary(orderId, holderForCreateEncumbrances.size(), requestContext)
          .thenCompose(aVoid -> CompletableFuture.allOf(holderForCreateEncumbrances.stream()
            .map(holder -> transactionService.createTransaction(holder.getToFYEncumbrance(), requestContext)
              .thenAccept(holder::withToFYEncumbrance))
            .toArray(CompletableFuture[]::new)))
          .thenApply(aVoid -> holders);
      })
      .orElseGet(() -> CompletableFuture.completedFuture(holders));

  }
  
}
