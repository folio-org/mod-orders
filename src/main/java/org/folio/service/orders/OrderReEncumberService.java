package org.folio.service.orders;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.rest.core.exceptions.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.rest.core.exceptions.ErrorCodes.ROLLOVER_NOT_COMPLETED;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.MonetaryOperator;
import javax.money.convert.CurrencyConversion;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.models.ReEncumbranceHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.ReportingCode;
import org.folio.rest.jaxrs.model.RolloverStatus;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.finance.rollover.RolloverErrorService;
import org.folio.service.finance.rollover.RolloverRetrieveService;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.finance.transaction.summary.OrderTransactionSummariesService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class OrderReEncumberService implements CompositeOrderDynamicDataPopulateService {

  protected final Logger logger = LogManager.getLogger();

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder;
  private final RolloverErrorService rolloverErrorService;
  private final RolloverRetrieveService rolloverRetrieveService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TransactionService transactionService;
  private final OrderTransactionSummariesService orderTransactionSummariesService;
  private final BudgetRestrictionService budgetRestrictionService;

  public OrderReEncumberService(PurchaseOrderStorageService purchaseOrderStorageService,
                                ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder,
                                RolloverErrorService rolloverErrorService,
                                RolloverRetrieveService rolloverRetrieveService,
                                PurchaseOrderLineService purchaseOrderLineService,
                                TransactionService transactionService,
                                OrderTransactionSummariesService orderTransactionSummariesService,
                                BudgetRestrictionService budgetRestrictionService) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.reEncumbranceHoldersBuilder = reEncumbranceHoldersBuilder;
    this.rolloverErrorService = rolloverErrorService;
    this.rolloverRetrieveService = rolloverRetrieveService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionService = transactionService;
    this.orderTransactionSummariesService = orderTransactionSummariesService;
    this.budgetRestrictionService = budgetRestrictionService;
  }

  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder orderRetrieveHolder,
                                                            RequestContext requestContext) {
    orderRetrieveHolder.withNeedReEncumber(false);
    List<ReEncumbranceHolder> reEncumbranceHolders = reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(orderRetrieveHolder.getOrder());
    if (reEncumbranceHolders.isEmpty()) {
      return Future.succeededFuture(orderRetrieveHolder);
    }
    return reEncumbranceHoldersBuilder.withFundsData(reEncumbranceHolders, requestContext)
      .compose(holderList -> {
        if (holderList.stream()
          .anyMatch(holder -> Objects.isNull(holder.getLedgerId()))) {
          return Future.succeededFuture(orderRetrieveHolder.withNeedReEncumber(true));
        }

        Optional.ofNullable(orderRetrieveHolder.getFiscalYear())
            .ifPresent(fiscalYear ->  reEncumbranceHolders
                .forEach(holder -> holder.withCurrentFiscalYearId(fiscalYear.getId()).withCurrency(fiscalYear.getCurrency())));

        return reEncumbranceHoldersBuilder.withRollovers(reEncumbranceHolders, requestContext)
          .compose(holders -> getLedgersIdsRolloverNotCompleted(holders, requestContext).compose(ledgerIds -> {
            if (isRolloversPartiallyCompleted(holders, ledgerIds)) {
              return Future.succeededFuture(orderRetrieveHolder.withNeedReEncumber(true));
            }
            return rolloverErrorService.getLedgerFyRolloverErrors(orderRetrieveHolder.getOrderId(), requestContext)
              .map(LedgerFiscalYearRolloverErrorCollection::getLedgerFiscalYearRolloverErrors)
              .map(ledgerFyRolloverErrors -> orderRetrieveHolder.withNeedReEncumber(!ledgerFyRolloverErrors.isEmpty()));
          }));
      });
  }

  public Future<Void> reEncumber(String orderId, RequestContext requestContext) {
    return purchaseOrderStorageService.getCompositeOrderById(orderId, requestContext)
      .map(reEncumbranceHoldersBuilder::buildReEncumbranceHoldersWithOrdersData)
      .compose(holders -> reEncumbranceHoldersBuilder.withFundsData(holders, requestContext))
      .map(this::checkAllFundsFound)
      .compose(holders -> reEncumbranceHoldersBuilder.withLedgersData(holders, requestContext))
      .compose(holders -> reEncumbranceHoldersBuilder.withCurrentFiscalYearData(holders, requestContext))
      .compose(holders -> reEncumbranceHoldersBuilder.withRollovers(holders, requestContext))
      .map(reEncumbranceHoldersBuilder::withEncumbranceRollover)
      .compose(holders -> checkRolloverHappensForAllLedgers(holders, requestContext))
      .compose(holders -> reEncumbranceHoldersBuilder.withBudgets(holders, requestContext))
      .compose(holders -> reEncumbranceHoldersBuilder.withConversions(holders, requestContext))
      .map(this::filterNeedReEncumbranceHolders)
      .compose(holders -> reEncumbranceHoldersBuilder.withPreviousFyEncumbrances(holders, requestContext))
      .map(this::adjustPoLinesCost)
      .compose(holders -> reEncumbranceHoldersBuilder.withToEncumbrances(holders, requestContext))
      .compose(holders -> validateAndCreateEncumbrances(holders, requestContext))
      .map(this::updateLinkToEncumbrances)
      .compose(holders -> deleteRolloverErrors(orderId, requestContext).map(aVoid -> holders))
      .compose(holders -> updatePoLines(holders, requestContext));
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

  private Future<List<ReEncumbranceHolder>> checkRolloverHappensForAllLedgers(List<ReEncumbranceHolder> holders,
                                                                                         RequestContext requestContext) {
    return getLedgersIdsRolloverNotCompleted(holders, requestContext)
            .map(ledgerIds -> {
              if (isRolloversPartiallyCompleted(holders, ledgerIds)) {
                Parameter parameter = new Parameter().withKey("ledgerIds")
                        .withValue(String.join(", ", ledgerIds));
                throw new HttpException(400, ROLLOVER_NOT_COMPLETED.toError()
                        .withParameters(Collections.singletonList(parameter)));
              }
              return holders;
            });
  }

  private boolean isRolloversPartiallyCompleted(List<ReEncumbranceHolder> holders, List<String> ledgerIds) {
    return !ledgerIds.isEmpty()
            && ledgerIds.size() != holders.stream().map(ReEncumbranceHolder::getLedgerId).filter(Objects::nonNull).distinct().count();
  }

  private Future<List<String>> getLedgersIdsRolloverNotCompleted(List<ReEncumbranceHolder> holders, RequestContext requestContext) {
    List<String> ledgerIds = holders.stream()
            .filter(holder -> Objects.isNull(holder.getRollover()))
            .map(ReEncumbranceHolder::getLedgerId)
            .filter(Objects::nonNull)
            .distinct()
            .collect(toList());

    return GenericCompositeFuture.join(holders.stream()
            .filter(holder -> Objects.nonNull(holder.getRollover()))
            .map(ReEncumbranceHolder::getRollover)
            .map(rollover -> rolloverRetrieveService.getRolloversProgress(rollover.getId(), requestContext)
                    .onSuccess(progresses -> {
                      if (isRolloverNotCompleted(progresses)) {
                        ledgerIds.add(rollover.getLedgerId());
                      }
                    }))
            .collect(toList()))
            .map(aVoid -> ledgerIds);
  }

  private boolean isRolloverNotCompleted(List<LedgerFiscalYearRolloverProgress> progresses) {
    return progresses.isEmpty() || progresses.stream()
            .anyMatch(progress -> progress.getOverallRolloverStatus() == RolloverStatus.IN_PROGRESS
                    || progress.getOverallRolloverStatus() == RolloverStatus.NOT_STARTED);
  }

  private List<ReEncumbranceHolder> filterNeedReEncumbranceHolders(List<ReEncumbranceHolder> reEncumbranceHolders) {
    return reEncumbranceHolders.stream()
            .filter(this::isRolloverRequired)
            .collect(toList());
  }

  private boolean isRolloverRequired(ReEncumbranceHolder reEncumbranceHolder) {
    return Objects.nonNull(reEncumbranceHolder.getEncumbranceRollover());
  }

  private Future<Void> deleteRolloverErrors(String orderId, RequestContext requestContext) {
    return rolloverErrorService.getLedgerFyRolloverErrors(orderId, requestContext)
            .map(LedgerFiscalYearRolloverErrorCollection::getLedgerFiscalYearRolloverErrors)
            .compose(errors -> rolloverErrorService.deleteRolloverErrors(errors, requestContext));
  }

  private Future<Void> updatePoLines(List<ReEncumbranceHolder> holders, RequestContext requestContext) {
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

  private Future<List<ReEncumbranceHolder>> validateAndCreateEncumbrances(List<ReEncumbranceHolder> holders,
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
        return orderTransactionSummariesService.updateTransactionSummary(orderId, holderForCreateEncumbrances.size(), requestContext)
          .compose(aVoid -> GenericCompositeFuture.join(holderForCreateEncumbrances.stream()
            .map(holder -> transactionService.createTransaction(holder.getNewEncumbrance(), requestContext)
              .onSuccess(holder::withNewEncumbrance))
            .collect(toList())))
          .map(aVoid -> holders);
      })
      .orElseGet(() ->  Future.succeededFuture(holders));

  }

}
