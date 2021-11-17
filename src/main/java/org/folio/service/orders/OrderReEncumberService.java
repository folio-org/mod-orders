package org.folio.service.orders;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.rest.core.exceptions.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.ROLLOVER_NOT_COMPLETED;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.MonetaryOperator;
import javax.money.convert.CurrencyConversion;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.models.ReEncumbranceHolder;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.finance.rollover.RolloverErrorService;
import org.folio.service.finance.rollover.RolloverRetrieveService;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.finance.transaction.TransactionSummariesService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.json.JsonObject;

public class OrderReEncumberService implements CompositeOrderDynamicDataPopulateService {

  protected final Logger logger = LogManager.getLogger();

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder;
  private final RolloverErrorService rolloverErrorService;
  private final RolloverRetrieveService rolloverRetrieveService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TransactionService transactionService;
  private final TransactionSummariesService transactionSummaryService;
  private final BudgetRestrictionService budgetRestrictionService;

  public OrderReEncumberService(PurchaseOrderStorageService purchaseOrderStorageService,
                                ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder,
                                RolloverErrorService rolloverErrorService,
                                RolloverRetrieveService rolloverRetrieveService,
                                PurchaseOrderLineService purchaseOrderLineService,
                                TransactionService transactionService,
                                TransactionSummariesService transactionSummaryService,
                                BudgetRestrictionService budgetRestrictionService) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.reEncumbranceHoldersBuilder = reEncumbranceHoldersBuilder;
    this.rolloverErrorService = rolloverErrorService;
    this.rolloverRetrieveService = rolloverRetrieveService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionService = transactionService;
    this.transactionSummaryService = transactionSummaryService;
    this.budgetRestrictionService = budgetRestrictionService;
  }

  public CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder orderRetrieveHolder,
                                                            RequestContext requestContext) {
    orderRetrieveHolder.withNeedReEncumber(false);
    List<ReEncumbranceHolder> reEncumbranceHolders = reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(orderRetrieveHolder.getOrder());
    if (reEncumbranceHolders.isEmpty()) {
      return CompletableFuture.completedFuture(orderRetrieveHolder);
    }
    return reEncumbranceHoldersBuilder.withFundsData(reEncumbranceHolders, requestContext)
      .thenCompose(holderList -> {
        if (holderList.stream()
          .anyMatch(holder -> Objects.isNull(holder.getLedgerId()))) {
          return CompletableFuture.completedFuture(orderRetrieveHolder.withNeedReEncumber(true));
        }

        Optional.ofNullable(orderRetrieveHolder.getFiscalYear())
            .ifPresent(fiscalYear ->  reEncumbranceHolders
                .forEach(holder -> holder.withCurrentFiscalYearId(fiscalYear.getId()).withCurrency(fiscalYear.getCurrency())));

        return reEncumbranceHoldersBuilder.withRollovers(reEncumbranceHolders, requestContext)
          .thenCompose(holders -> getLedgersIdsRolloverNotCompleted(holders, requestContext).thenCompose(ledgerIds -> {
            if (isRolloversPartiallyCompleted(holders, ledgerIds)) {
              return CompletableFuture.completedFuture(orderRetrieveHolder.withNeedReEncumber(true));
            }
            return rolloverErrorService.getLedgerFyRolloverErrors(orderRetrieveHolder.getOrderId(), requestContext)
              .thenApply(LedgerFiscalYearRolloverErrorCollection::getLedgerFiscalYearRolloverErrors)
              .thenApply(ledgerFyRolloverErrors -> orderRetrieveHolder.withNeedReEncumber(!ledgerFyRolloverErrors.isEmpty()));
          }));
      });
  }

  public CompletableFuture<Void> reEncumber(String orderId, RequestContext requestContext) {
    return purchaseOrderStorageService.getCompositeOrderById(orderId, requestContext)
      .thenApply(reEncumbranceHoldersBuilder::buildReEncumbranceHoldersWithOrdersData)
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withFundsData(holders, requestContext))
      .thenApply(this::checkAllFundsFound)
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withLedgersData(holders, requestContext))
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withCurrentFiscalYearData(holders, requestContext))
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withRollovers(holders, requestContext))
      .thenApply(reEncumbranceHoldersBuilder::withEncumbranceRollover)
      .thenCompose(holders -> checkRolloverHappensForAllLedgers(holders, requestContext))
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withBudgets(holders, requestContext))
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withConversions(holders, requestContext))
      .thenApply(this::filterNeedReEncumbranceHolders)
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withPreviousFyEncumbrances(holders, requestContext))
      .thenApply(this::adjustPoLinesCost)
      .thenCompose(holders -> reEncumbranceHoldersBuilder.withToEncumbrances(holders, requestContext))
      .thenCompose(holders -> validateAndCreateEncumbrances(holders, requestContext))
      .thenApply(this::updateLinkToEncumbrances)
      .thenCompose(holders -> deleteRolloverErrors(orderId, requestContext).thenApply(aVoid -> holders))
      .thenCompose(holders -> updatePoLines(holders, requestContext));
  }

  private List<ReEncumbranceHolder> checkAllFundsFound(List<ReEncumbranceHolder> holders) {
    List<Parameter> parameters =  holders.stream()
            .filter(holder -> Objects.isNull(holder.getLedgerId()))
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
            && ledgerIds.size() != holders.stream().map(ReEncumbranceHolder::getLedgerId).filter(Objects::nonNull).distinct().count();
  }

  private CompletableFuture<List<String>> getLedgersIdsRolloverNotCompleted(List<ReEncumbranceHolder> holders, RequestContext requestContext) {
    List<String> ledgerIds = holders.stream()
            .filter(holder -> Objects.isNull(holder.getRollover()))
            .map(ReEncumbranceHolder::getLedgerId)
            .filter(Objects::nonNull)
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
    return purchaseOrderLineService.saveOrderLines(lines, requestContext);
  }

  private List<ReEncumbranceHolder> updateLinkToEncumbrances(List<ReEncumbranceHolder> holders) {
    holders.forEach(holder -> holder.getFundDistribution().setEncumbrance(holder.getNewEncumbrance().getId()));
    return holders;
  }


  private List<ReEncumbranceHolder> adjustPoLinesCost(List<ReEncumbranceHolder> reEncumbranceHolders) {
    Map<CompositePoLine, List<ReEncumbranceHolder>> poLineHoldersMap = reEncumbranceHolders.stream()
      .collect(groupingBy(ReEncumbranceHolder::getPoLine));
      poLineHoldersMap.forEach(this::adjustPoLineCost);
    return reEncumbranceHolders;
  }

  private void adjustPoLineCost(CompositePoLine poLine, List<ReEncumbranceHolder> holders) {
    MonetaryAmount poLineEstimatedPriceAfterRollover = Money.zero(Monetary.getCurrency(poLine.getCost().getCurrency()));
    for (ReEncumbranceHolder holder : holders) {
      if (holder.getEncumbranceRollover() != null && holder.getPoLineToFyConversion() != null) {
        CurrencyConversion fyToPoLineConversion = holder.getFyToPoLineConversion();
        Transaction fromEncumbrance = holder.getPreviousFyEncumbrance();
        EncumbranceRollover encumbranceRollover = holder.getEncumbranceRollover();
        MonetaryOperator percentOperator = MonetaryOperators.percent(encumbranceRollover.getIncreaseBy());
        MonetaryAmount amount;
        if (encumbranceRollover.getBasedOn() == EncumbranceRollover.BasedOn.REMAINING) {
          amount = Money.of(fromEncumbrance.getAmount(), fromEncumbrance.getCurrency());
        } else if (encumbranceRollover.getBasedOn() == EncumbranceRollover.BasedOn.EXPENDED){
          amount = Money.of(fromEncumbrance.getEncumbrance().getAmountExpended(), fromEncumbrance.getCurrency());
        } else {
          amount = Money.of(fromEncumbrance.getEncumbrance().getInitialAmountEncumbered(), fromEncumbrance.getCurrency());
        }
        amount = amount.add(amount.with(percentOperator)).with(fyToPoLineConversion);
        poLineEstimatedPriceAfterRollover = poLineEstimatedPriceAfterRollover.add(amount);
      }
    }
    MonetaryAmount poLineEstimatedPriceBeforeRollover = HelperUtils.calculateEstimatedPrice(poLine.getCost());
    poLine.getCost().setFyroAdjustmentAmount(poLineEstimatedPriceAfterRollover.subtract(poLineEstimatedPriceBeforeRollover).getNumber().doubleValue());

    for (ReEncumbranceHolder holder : holders) {
      FundDistribution fundDistr = holder.getFundDistribution();
      MonetaryAmount newFundAmount;
      if (fundDistr.getDistributionType().equals(FundDistribution.DistributionType.AMOUNT)) {
        MonetaryAmount fdAmount = Money.of(fundDistr.getValue(), poLine.getCost().getCurrency());
        newFundAmount = fdAmount.divide(poLineEstimatedPriceBeforeRollover.getNumber().doubleValue())
                                .multiply(poLineEstimatedPriceAfterRollover.getNumber().doubleValue());
        fundDistr.setValue(newFundAmount.getNumber().doubleValue());
      }
    }
  }

  private CompletableFuture<List<ReEncumbranceHolder>> validateAndCreateEncumbrances(List<ReEncumbranceHolder> holders,
                                                                                     RequestContext requestContext) {
    List<ReEncumbranceHolder> holderForCreateEncumbrances = holders.stream()
      .filter(holder -> StringUtils.isEmpty(holder.getNewEncumbrance().getId()))
      .collect(toList());

    budgetRestrictionService.checkEncumbranceRestrictions(holderForCreateEncumbrances);
    return holders.stream()
      .findFirst()
      .map(ReEncumbranceHolder::getPurchaseOrder)
      .map(purchaseOrder -> {

        String orderId = purchaseOrder.getId();
        return transactionSummaryService.updateOrderTransactionSummary(orderId, holderForCreateEncumbrances.size(), requestContext)
          .thenCompose(aVoid -> CompletableFuture.allOf(holderForCreateEncumbrances.stream()
            .map(holder -> transactionService.createTransaction(holder.getNewEncumbrance(), requestContext)
              .thenAccept(holder::withNewEncumbrance))
            .toArray(CompletableFuture[]::new)))
          .thenApply(aVoid -> holders);
      })
      .orElseGet(() -> CompletableFuture.completedFuture(holders));

  }

}
