package org.folio.service.orders;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.rest.core.exceptions.ErrorCodes.ROLLOVER_NOT_COMPLETED;
import static org.folio.rest.core.exceptions.ErrorCodes.ENCUMBRANCES_FOR_RE_ENCUMBER_NOT_FOUND;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import javax.money.Monetary;
import javax.money.MonetaryAmount;
import javax.money.MonetaryOperator;
import javax.money.convert.CurrencyConversion;

import io.netty.handler.codec.http.HttpResponseStatus;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.models.ReEncumbranceHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.EncumbranceRollover;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.RolloverStatus;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.finance.rollover.LedgerRolloverErrorService;
import org.folio.service.finance.rollover.LedgerRolloverProgressService;
import org.folio.service.finance.transaction.FinanceUtils;
import org.folio.service.finance.transaction.TransactionService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Future;

public class OrderReEncumberService implements CompositeOrderDynamicDataPopulateService {

  protected final Logger logger = LogManager.getLogger();

  private final PurchaseOrderStorageService purchaseOrderStorageService;
  private final ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder;
  private final LedgerRolloverErrorService ledgerRolloverErrorService;
  private final LedgerRolloverProgressService ledgerRolloverProgressService;
  private final PurchaseOrderLineService purchaseOrderLineService;
  private final TransactionService transactionService;
  private final BudgetRestrictionService budgetRestrictionService;

  public OrderReEncumberService(PurchaseOrderStorageService purchaseOrderStorageService,
                                ReEncumbranceHoldersBuilder reEncumbranceHoldersBuilder,
                                LedgerRolloverErrorService ledgerRolloverErrorService,
                                LedgerRolloverProgressService ledgerRolloverProgressService, PurchaseOrderLineService purchaseOrderLineService,
                                TransactionService transactionService,
                                BudgetRestrictionService budgetRestrictionService) {
    this.purchaseOrderStorageService = purchaseOrderStorageService;
    this.reEncumbranceHoldersBuilder = reEncumbranceHoldersBuilder;
    this.ledgerRolloverErrorService = ledgerRolloverErrorService;
    this.ledgerRolloverProgressService = ledgerRolloverProgressService;
    this.purchaseOrderLineService = purchaseOrderLineService;
    this.transactionService = transactionService;
    this.budgetRestrictionService = budgetRestrictionService;
  }

  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder orderRetrieveHolder,
                                                            RequestContext requestContext) {
    orderRetrieveHolder.withNeedReEncumber(false);
    List<ReEncumbranceHolder> reEncumbranceHolders = reEncumbranceHoldersBuilder.buildReEncumbranceHoldersWithOrdersData(orderRetrieveHolder.getOrder());
    if (reEncumbranceHolders.isEmpty()) {
      return Future.succeededFuture(orderRetrieveHolder);
    }
    return reEncumbranceHoldersBuilder.getLedgerIds(reEncumbranceHolders, requestContext)
      .compose(lIds -> {
        if (reEncumbranceHolders.stream().anyMatch(holder -> Objects.isNull(holder.getLedgerId()))) {
          return Future.succeededFuture(orderRetrieveHolder.withNeedReEncumber(true));
        }

        Optional.ofNullable(orderRetrieveHolder.getFiscalYearId())
            .ifPresent(fiscalYearId ->  reEncumbranceHolders
                .forEach(holder -> holder.withCurrentFiscalYearId(fiscalYearId).withCurrency(orderRetrieveHolder.getFiscalYearCurrency())));

        return reEncumbranceHoldersBuilder.withRollovers(reEncumbranceHolders, requestContext)
          .compose(holders -> getLedgersIdsRolloverNotCompleted(holders, requestContext).compose(ledgerIds -> {
            if (isRolloversPartiallyCompleted(holders, ledgerIds)) {
              return Future.succeededFuture(orderRetrieveHolder.withNeedReEncumber(true));
            }
            return ledgerRolloverErrorService.getLedgerFyRolloverErrors(orderRetrieveHolder.getOrderId(), requestContext)
              .map(LedgerFiscalYearRolloverErrorCollection::getLedgerFiscalYearRolloverErrors)
              .map(ledgerFyRolloverErrors -> orderRetrieveHolder.withNeedReEncumber(!ledgerFyRolloverErrors.isEmpty()));
          }));
      });
  }

  public Future<Void> reEncumber(String orderId, RequestContext requestContext) {
    return purchaseOrderStorageService.getCompositeOrderById(orderId, requestContext)
      .map(reEncumbranceHoldersBuilder::buildReEncumbranceHoldersWithOrdersData)
      .compose(holders -> reEncumbranceHoldersBuilder.withFinances(holders, requestContext)
        .map(v -> holders))
      .compose(holders -> reEncumbranceHoldersBuilder.withRollovers(holders, requestContext))
      .map(reEncumbranceHoldersBuilder::withEncumbranceRollover)
      .compose(holders -> checkRolloverHappensForAllLedgers(holders, requestContext))
      .compose(holders -> reEncumbranceHoldersBuilder.withPreviousFyEncumbrances(holders, requestContext))
      .map(this::filterNeedReEncumbranceHolders)
      .map(this::ensureReEncumbranceHoldersExist)
      .map(this::adjustPoLinesCost)
      .compose(holders -> reEncumbranceHoldersBuilder.withToEncumbrances(holders, requestContext))
      .compose(holders -> validateAndCreateEncumbrances(holders, requestContext))
      .map(this::updateLinkToEncumbrances)
      .compose(holders -> deleteRolloverErrors(orderId, requestContext).map(aVoid -> holders))
      .compose(holders -> updatePoLines(holders, requestContext));
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

    return Future.join(holders.stream()
            .map(ReEncumbranceHolder::getRollover)
            .filter(Objects::nonNull)
            .map(rollover -> ledgerRolloverProgressService.getRolloversProgress(rollover.getId(), requestContext)
              .map(progresses -> {
                if (isRolloverNotCompleted(progresses)) {
                  ledgerIds.add(rollover.getLedgerId());
                }
                return null;
              })
            )
            .toList())
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
            .toList();
  }

  private boolean isRolloverRequired(ReEncumbranceHolder reEncumbranceHolder) {
    return ObjectUtils.allNotNull(reEncumbranceHolder.getEncumbranceRollover(),
      reEncumbranceHolder.getFyToPoLineConversion(), reEncumbranceHolder.getPreviousFyEncumbrance());
  }

  private List<ReEncumbranceHolder> ensureReEncumbranceHoldersExist(List<ReEncumbranceHolder> holders) {
    return Optional.of(holders)
      .filter(CollectionUtils::isNotEmpty)
      .orElseThrow(() -> new HttpException(HttpResponseStatus.CONFLICT.code(), ENCUMBRANCES_FOR_RE_ENCUMBER_NOT_FOUND.toError()));
  }

  private Future<Void> deleteRolloverErrors(String orderId, RequestContext requestContext) {
    return ledgerRolloverErrorService.getLedgerFyRolloverErrors(orderId, requestContext)
            .map(LedgerFiscalYearRolloverErrorCollection::getLedgerFiscalYearRolloverErrors)
            .compose(errors -> ledgerRolloverErrorService.deleteRolloverErrors(errors, requestContext));
  }

  private Future<Void> updatePoLines(List<ReEncumbranceHolder> holders, RequestContext requestContext) {
    List<PoLine> lines = holders.stream().map(ReEncumbranceHolder::getPoLine).toList();
    return purchaseOrderLineService.saveOrderLinesWithoutSearchLocationsUpdate(lines, requestContext);
  }

  private List<ReEncumbranceHolder> updateLinkToEncumbrances(List<ReEncumbranceHolder> holders) {
    holders.forEach(holder -> holder.getFundDistribution().setEncumbrance(holder.getNewEncumbrance().getId()));
    return holders;
  }


  private List<ReEncumbranceHolder> adjustPoLinesCost(List<ReEncumbranceHolder> reEncumbranceHolders) {
    Map<PoLine, List<ReEncumbranceHolder>> poLineHoldersMap = reEncumbranceHolders.stream()
      .collect(groupingBy(ReEncumbranceHolder::getPoLine));
      poLineHoldersMap.forEach(this::adjustPoLineCost);
    return reEncumbranceHolders;
  }

  private void adjustPoLineCost(PoLine poLine, List<ReEncumbranceHolder> holders) {
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
          amount = Money.of(FinanceUtils.calculateEncumbranceTotalAmount(fromEncumbrance), fromEncumbrance.getCurrency());
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
        newFundAmount = (!poLineEstimatedPriceBeforeRollover.isZero()) ? fdAmount.divide(poLineEstimatedPriceBeforeRollover.getNumber().doubleValue())
                                .multiply(poLineEstimatedPriceAfterRollover.getNumber().doubleValue()) : Money.of(0, poLine.getCost().getCurrency());
        fundDistr.setValue(newFundAmount.getNumber().doubleValue());
      }
    }
  }

  private Future<List<ReEncumbranceHolder>> validateAndCreateEncumbrances(List<ReEncumbranceHolder> holders,
                                                                                     RequestContext requestContext) {
    List<ReEncumbranceHolder> holderForCreateEncumbrances = holders.stream()
      .filter(holder -> StringUtils.isEmpty(holder.getNewEncumbrance().getId()))
      .toList();

    budgetRestrictionService.checkEncumbranceRestrictions(holderForCreateEncumbrances);
    List<Transaction> encumbrances = holderForCreateEncumbrances.stream().map(ReEncumbranceHolder::getNewEncumbrance).toList();
    if (encumbrances.isEmpty()) {
      return Future.succeededFuture(holders);
    }
    return transactionService.batchCreate(encumbrances, requestContext)
      .map(aVoid -> holders);
  }

}
