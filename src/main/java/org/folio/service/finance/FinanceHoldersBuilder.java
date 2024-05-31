package org.folio.service.finance;

import static io.vertx.core.Future.succeededFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toMap;
import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertFieldListToCqlQuery;
import static org.folio.orders.utils.HelperUtils.getConversionQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_FISCAL_YEAR;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;
import javax.money.convert.ExchangeRateProvider;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.folio.service.finance.budget.BudgetService;

import io.vertx.core.Future;

public class FinanceHoldersBuilder {
  protected final Logger logger = LogManager.getLogger();

  private final FundService fundService;
  private final FiscalYearService fiscalYearService;
  protected final ExchangeRateProviderResolver exchangeRateProviderResolver;
  private final BudgetService budgetService;
  private final LedgerService ledgerService;

  public FinanceHoldersBuilder(FundService fundService, FiscalYearService fiscalYearService,
      ExchangeRateProviderResolver exchangeRateProviderResolver, BudgetService budgetService,
      LedgerService ledgerService) {
    this.fundService = fundService;
    this.fiscalYearService = fiscalYearService;
    this.exchangeRateProviderResolver = exchangeRateProviderResolver;
    this.budgetService = budgetService;
    this.ledgerService = ledgerService;
  }

  public Future<Void> withFinances(List<? extends EncumbranceRelationsHolder> encumbranceHolders,
      RequestContext requestContext) {
    if (encumbranceHolders.stream().noneMatch(h -> h.getFundDistribution() != null && h.getFundId() != null)) {
      return succeededFuture();
    }
    return getLedgerIds(encumbranceHolders, requestContext)
      .compose(ledgerIds -> getLedgers(ledgerIds, encumbranceHolders, requestContext))
      .compose(ledgers -> getFiscalYear(ledgers, encumbranceHolders, requestContext))
      .compose(fiscalYear -> getBudgets(fiscalYear, encumbranceHolders, requestContext))
      .compose(v -> withConversion(encumbranceHolders, requestContext))
      .onSuccess(v -> logger.info("withFinances :: success retrieving finance data"))
      .onFailure(t -> logger.error("withFinances :: error retrieving finance data", t));
  }

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

  protected Future<Void> withConversion(List<? extends EncumbranceRelationsHolder> encumbranceHolders,
      RequestContext requestContext) {
    String fyCurrency = encumbranceHolders.stream()
      .map(EncumbranceRelationsHolder::getCurrency)
      .filter(Objects::nonNull)
      .findFirst()
      .orElse(null);
    if (fyCurrency == null) {
      return succeededFuture();
    }
    return requestContext.getContext().executeBlocking(() -> {
      Map<String, List<EncumbranceRelationsHolder>> currencyHolderMap = encumbranceHolders.stream()
        .filter(holder -> Objects.nonNull(holder.getPoLine()))
        .collect(groupingBy(holder -> holder.getPoLine().getCost().getCurrency()));

      currencyHolderMap.forEach((poLineCurrency, encumbranceRelationsHolders) -> {
        Double exchangeRate = encumbranceRelationsHolders.stream()
          .map(EncumbranceRelationsHolder::getPoLine)
          .map(CompositePoLine::getCost)
          .map(Cost::getExchangeRate)
          .filter(Objects::nonNull)
          .findFirst()
          .orElse(null);

        ConversionQuery conversionQuery = getConversionQuery(exchangeRate, poLineCurrency, fyCurrency);
        ExchangeRateProvider exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
        CurrencyConversion conversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
        encumbranceRelationsHolders.forEach(holder -> holder.withPoLineToFyConversion(conversion));
      });
      return null;
    })
    .mapEmpty();
  }

  private void populateLedgerIds(List<Fund> funds, List<? extends EncumbranceRelationsHolder> encumbranceHolders) {
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
    return fiscalYearService.getCurrentFiscalYear(ledgers.get(0).getId(), requestContext)
      .map(fiscalYear -> {
        encumbranceHolders.forEach(holder -> holder.withCurrentFiscalYearId(fiscalYear.getId())
          .withCurrency(fiscalYear.getCurrency()));
        return fiscalYear;
      });
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
          List<? extends EncumbranceRelationsHolder> holdersOfFundsNotFound = encumbranceHolders.stream()
            .filter(h -> budgets.stream().noneMatch(b -> h.getFundId().equals(b.getFundId())))
            .distinct()
            .toList();
          List<String> idsOfFundsNotFound = holdersOfFundsNotFound.stream()
            .map(EncumbranceRelationsHolder::getFundId)
            .toList();
          List<String> codesOfFundsNotFound = holdersOfFundsNotFound.stream()
            .map(h -> h.getFundDistribution().getCode())
            .distinct()
            .toList();
          List<String> poLineIdsOfFundsNotFound = holdersOfFundsNotFound.stream()
            .map(EncumbranceRelationsHolder::getPoLineId)
            .distinct()
            .toList();
          List<String> poLineNumbersOfFundsNotFound = holdersOfFundsNotFound.stream()
            .map(h -> h.getPoLine().getPoLineNumber())
            .distinct()
            .filter(Objects::nonNull)
            .toList();
          throw new HttpException(422, BUDGET_NOT_FOUND_FOR_FISCAL_YEAR.toError()
            .withParameters(List.of(
              new Parameter().withKey("fundIds").withValue(idsOfFundsNotFound.toString()),
              new Parameter().withKey("fundCodes").withValue(codesOfFundsNotFound.toString()),
              new Parameter().withKey("poLineIds").withValue(poLineIdsOfFundsNotFound.toString()),
              new Parameter().withKey("poLineNumbers").withValue(poLineNumbersOfFundsNotFound.toString()),
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
