package org.folio.service.finance;

import static io.vertx.core.Future.succeededFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toMap;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.buildConversionQuery;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.convertFieldListToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_FISCAL_YEAR;
import static org.folio.rest.core.exceptions.ErrorCodes.MULTIPLE_FISCAL_YEARS;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.EncumbranceConversionHolder;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.exchange.CustomExchangeRateProvider;
import org.folio.service.finance.budget.BudgetService;

import io.vertx.core.Future;

public class FinanceHoldersBuilder {

  protected final Logger logger = LogManager.getLogger();

  private final FundService fundService;
  private final FiscalYearService fiscalYearService;
  private final BudgetService budgetService;
  private final LedgerService ledgerService;
  protected final CacheableExchangeRateService cacheableExchangeRateService;

  public FinanceHoldersBuilder(FundService fundService, FiscalYearService fiscalYearService,
                               BudgetService budgetService, LedgerService ledgerService,
                               CacheableExchangeRateService cacheableExchangeRateService) {
    this.fundService = fundService;
    this.fiscalYearService = fiscalYearService;
    this.budgetService = budgetService;
    this.ledgerService = ledgerService;
    this.cacheableExchangeRateService = cacheableExchangeRateService;
  }

  /**
   * Populate the encumbrance holders with the following data based on the fund ids in the holders:
   * ledger ids, ledgers, fiscal year, budgets, currency conversion.
   */
  public Future<Void> withFinances(List<? extends EncumbranceRelationsHolder> encumbranceHolders,
                                   RequestContext requestContext) {
    if (encumbranceHolders.stream().noneMatch(h -> h.getFundDistribution() != null && h.getFundId() != null)) {
      return succeededFuture();
    }
    return getLedgerIds(encumbranceHolders, requestContext)
      .compose(ledgerIds -> getLedgers(ledgerIds, encumbranceHolders, requestContext))
      .compose(ledgers -> getFiscalYear(ledgers, encumbranceHolders, requestContext))
      .compose(fiscalYear -> getBudgets(fiscalYear, encumbranceHolders, requestContext))
      .compose(v -> getExchangeRatesPerCurrencyHolder(encumbranceHolders, requestContext))
      .compose(this::withConversion)
      .onSuccess(v -> logger.info("withFinances :: success retrieving finance data"))
      .onFailure(t -> logger.error("withFinances :: error retrieving finance data", t));
  }

  /**
   * Populate the ledger ids in the encumbrance holders based on the fund ids in the holders.
   */
  public Future<List<String>> getLedgerIds(List<? extends EncumbranceRelationsHolder> encumbranceHolders,
                                           RequestContext requestContext) {
    List<String> fundIds = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getFundId)
      .filter(Objects::nonNull)
      .distinct()
      .toList();
    if (fundIds.isEmpty()) {
      return succeededFuture(List.of());
    }
    return fundService.getAllFunds(fundIds, requestContext)
      .map(funds -> {
        populateLedgerIds(funds, encumbranceHolders);
        return funds.stream()
          .map(Fund::getLedgerId)
          .distinct()
          .toList();
      })
      .onSuccess(funds -> logger.info("getLedgerIds :: success"))
      .onFailure(t -> logger.error("getLedgerIds :: error - fundIds: {}", fundIds, t));
  }

  protected Future<List<EncumbranceConversionHolder>> getExchangeRatesPerCurrencyHolder(List<? extends EncumbranceRelationsHolder> encumbranceHolders,
                                                                                        RequestContext requestContext) {
    var fyCurrency = getFyCurrency(encumbranceHolders);
    if (fyCurrency.isEmpty()) {
      return succeededFuture();
    }
    var currencyHoldersMap = encumbranceHolders.stream()
      .filter(holder -> Objects.nonNull(holder.getPoLine()))
      .collect(groupingBy(holder -> holder.getPoLine().getCost().getCurrency()));
    var encumbranceConversionHolderFutures = new ArrayList<Future<EncumbranceConversionHolder>>();
    currencyHoldersMap.forEach((poLineCurrency, holders) -> {
      var fixedExchangeRate = getFirstFixedPoLineExchangeRateFromCost(holders);
      logger.info("getExchangeRatesPerCurrencyHolder:: Preparing to retrieve exchange rate for encumbrance, {} -> {}, fixed exchange rate: {}",
        poLineCurrency, fyCurrency.get(), fixedExchangeRate);
      encumbranceConversionHolderFutures.add(cacheableExchangeRateService.getExchangeRate(poLineCurrency, fyCurrency.get(), fixedExchangeRate, requestContext)
        .compose(exchangeRate -> {
          var provider = new CustomExchangeRateProvider();
          var query = buildConversionQuery(poLineCurrency, fyCurrency.get(), exchangeRate.getExchangeRate());
          var conversion = provider.getCurrencyConversion(query);
          return Future.succeededFuture(new EncumbranceConversionHolder().withHolders(holders).withConversion(conversion));
        }));
    });

    return collectResultsOnSuccess(encumbranceConversionHolderFutures);
  }

  protected Double getFirstFixedPoLineExchangeRateFromCost(List<? extends EncumbranceRelationsHolder> holders) {
    return holders.stream()
      .map(EncumbranceRelationsHolder::getPoLine)
      .map(PoLine::getCost)
      .map(Cost::getExchangeRate)
      .filter(Objects::nonNull)
      .findFirst()
      .orElse(null);
  }

  protected Optional<String> getFyCurrency(List<? extends EncumbranceRelationsHolder> encumbranceHolders) {
    return encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getCurrency)
      .filter(Objects::nonNull)
      .findFirst();
  }

  protected Future<Void> withConversion(List<EncumbranceConversionHolder> encumbranceConversionHolders) {
    encumbranceConversionHolders.forEach(encumbranceConversionHolder -> {
      var encumbranceRelationsHolders = encumbranceConversionHolder.getEncumbranceRelationsHolders();
      encumbranceRelationsHolders.forEach(holder ->
        holder.withPoLineToFyConversion(encumbranceConversionHolder.getConversion()));
    });
    return Future.succeededFuture();
  }

  private void populateLedgerIds(List<Fund> funds,
                                 List<? extends EncumbranceRelationsHolder> encumbranceHolders) {
    Map<String, String> idFundMap = funds.stream()
      .collect(toMap(Fund::getId, Fund::getLedgerId));
    encumbranceHolders.stream()
      .filter(holder -> Objects.nonNull(holder.getFundId()))
      .forEach(holder -> holder.withLedgerId(idFundMap.get(holder.getFundId())));
  }

  private Future<List<Ledger>> getLedgers(List<String> ledgerIds,
                                          List<? extends EncumbranceRelationsHolder> encumbranceHolders, RequestContext requestContext) {
    return ledgerService.getLedgersByIds(ledgerIds, requestContext)
      .map(ledgers -> {
        mapRestrictEncumbranceToHolders(ledgers, encumbranceHolders);
        return ledgers;
      });
  }

  private Future<FiscalYear> getFiscalYear(List<Ledger> ledgers,
                                           List<? extends EncumbranceRelationsHolder> encumbranceHolders, RequestContext requestContext) {
    return getLedgersFiscalYears(ledgers, requestContext)
      .map(fiscalYears -> {
        List<String> fiscalYearIds = fiscalYears.stream()
          .map(FiscalYear::getId)
          .distinct()
          .toList();
        if (fiscalYearIds.size() > 1) {
          List<Parameter> parameters = List.of(
            new Parameter().withKey("fiscalYearIds").withValue(fiscalYearIds.toString()),
            new Parameter().withKey("poId").withValue(encumbranceHolders.getFirst().getPurchaseOrder().getId())
          );
          throw new HttpException(422, MULTIPLE_FISCAL_YEARS.toError().withParameters(parameters));
        }
        FiscalYear fiscalYear = fiscalYears.getFirst();
        encumbranceHolders.forEach(holder -> holder.withCurrentFiscalYearId(fiscalYear.getId())
          .withCurrency(fiscalYear.getCurrency()));
        return fiscalYear;
      });
  }

  private Future<List<FiscalYear>> getLedgersFiscalYears(List<Ledger> ledgers, RequestContext requestContext) {
    List<String> ledgerIds = ledgers.stream()
      .map(Ledger::getId)
      .distinct()
      .toList();

    List<Future<FiscalYear>> futures = ledgerIds.stream()
      .map(ledgerId -> fiscalYearService.getCurrentFiscalYear(ledgerId, requestContext))
      .toList();

    return collectResultsOnSuccess(futures);
  }

  private Future<Void> getBudgets(FiscalYear fiscalYear, List<? extends EncumbranceRelationsHolder> encumbranceHolders,
                                  RequestContext requestContext) {
    List<String> fundIds = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getFundId)
      .filter(Objects::nonNull)
      .distinct()
      .toList();
    var futures = ofSubLists(new ArrayList<>(fundIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getBudgetsChunk(ids, fiscalYear, requestContext))
      .toList();
    return collectResultsOnSuccess(futures)
      .map(lists -> lists.stream().flatMap(Collection::stream).toList())
      .map(budgets -> {
        if (budgets.size() < fundIds.size()) {
          List<? extends EncumbranceRelationsHolder> holdersOfMissingBudgets = encumbranceHolders.stream()
            .filter(h -> budgets.stream().noneMatch(b -> h.getFundId().equals(b.getFundId())))
            .distinct()
            .toList();
          List<String> fundIdsOfMissingBudgets = holdersOfMissingBudgets.stream()
            .map(EncumbranceRelationsHolder::getFundId)
            .toList();
          List<String> fundCodesOfMissingBudgets = holdersOfMissingBudgets.stream()
            .map(h -> h.getFundDistribution().getCode())
            .distinct()
            .toList();
          List<String> poLineIdsOfMissingBudgets = holdersOfMissingBudgets.stream()
            .map(EncumbranceRelationsHolder::getPoLineId)
            .distinct()
            .toList();
          List<String> poLineNumbersOfMissingBudgets = holdersOfMissingBudgets.stream()
            .map(h -> h.getPoLine().getPoLineNumber())
            .distinct()
            .filter(Objects::nonNull)
            .toList();
          throw new HttpException(422, BUDGET_NOT_FOUND_FOR_FISCAL_YEAR.toError()
            .withParameters(List.of(
              new Parameter().withKey("fundIds").withValue(fundIdsOfMissingBudgets.toString()),
              new Parameter().withKey("fundCodes").withValue(fundCodesOfMissingBudgets.toString()),
              new Parameter().withKey("poLineIds").withValue(poLineIdsOfMissingBudgets.toString()),
              new Parameter().withKey("poLineNumbers").withValue(poLineNumbersOfMissingBudgets.toString()),
              new Parameter().withKey("fiscalYearId").withValue(fiscalYear.getId()),
              new Parameter().withKey("fiscalYearCode").withValue(fiscalYear.getCode())
            )));
        }
        mapHoldersToBudgets(budgets, encumbranceHolders);
        return null;
      });
  }

  private Future<List<Budget>> getBudgetsChunk(List<String> fundIds, FiscalYear fiscalYear, RequestContext requestContext) {
    String query = convertFieldListToCqlQuery(fundIds, "fundId", true) +
      String.format(" AND fiscalYearId == %s AND budgetStatus == Active", fiscalYear.getId());
    return budgetService.getBudgetsByQuery(query, requestContext);
  }

  private void mapRestrictEncumbranceToHolders(List<Ledger> ledgers,
                                               List<? extends EncumbranceRelationsHolder> encumbranceHolders) {
    Map<String, Ledger> idLedgerMap = ledgers.stream()
      .collect(toMap(Ledger::getId, Function.identity()));
    encumbranceHolders.stream()
      .filter(holder -> Objects.nonNull(holder.getLedgerId()))
      .forEach(holder -> {
        Ledger ledger = idLedgerMap.get(holder.getLedgerId());
        holder.withRestrictEncumbrances(Optional.ofNullable(ledger)
          .map(Ledger::getRestrictEncumbrance)
          .orElse(false));
      });
  }

  private void mapHoldersToBudgets(List<Budget> budgets, List<? extends EncumbranceRelationsHolder> encumbranceHolders) {
    Map<String, Budget> fundIdBudgetMap = budgets.stream()
      .collect(toMap(Budget::getFundId, Function.identity()));
    encumbranceHolders.forEach(holder -> holder.withBudget(fundIdBudgetMap.get(holder.getFundId())));
  }
}
