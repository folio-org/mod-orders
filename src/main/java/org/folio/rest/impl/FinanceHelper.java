package org.folio.rest.impl;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.BUDGET_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.orders.utils.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.orders.utils.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.ResourcePathResolver.ENCUMBRANCES;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TRANSACTION_SUMMARIES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.function.BiConsumer;

import javax.money.MonetaryAmount;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import one.util.streamex.StreamEx;

public class FinanceHelper extends AbstractHelper {
  private static final String ENCUMBRANCE_POST_ENDPOINT = resourcesPath(ENCUMBRANCES) + "?lang=%s";
  private static final String GET_CURRENT_FISCAL_YEAR_BY_ID = "/finance/ledgers/%s/current-fiscal-year?lang=%s";
  private static final String GET_FUNDS_WITH_SEARCH_PARAMS = resourcesPath(FUNDS) + SEARCH_PARAMS;

  FinanceHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * Creates Encumbrance records associated with given PO line and updates PO line with corresponding links.
   *
   * @return CompletableFuture with void on success.
   */
  CompletableFuture<Void> handleEncumbrances(CompositePurchaseOrder compPo) {
    List<Pair<Transaction, FundDistribution>> encumbranceDistributionPairs = buildEncumbrances(compPo);
    List<Transaction> encumbrances = encumbranceDistributionPairs.stream()
      .map(Pair::getLeft)
      .collect(toList());
    return setCurrentFiscalYear(encumbrances)
      .thenCompose(aVoid -> createSummary(compPo.getId(), encumbrances.size()))
      .thenCompose(vVoid -> createEncumbrances(encumbranceDistributionPairs));
  }

  private CompletableFuture<String> createSummary(String id, int number) {
    OrderTransactionSummary summary = new OrderTransactionSummary()
      .withId(id)
      .withNumTransactions(number);
    return createRecordInStorage(JsonObject.mapFrom(summary), resourcesPath(ORDER_TRANSACTION_SUMMARIES));
  }

  private CompletableFuture<Void> createEncumbrances(List<Pair<Transaction, FundDistribution>> encumbranceDistributionPairs) {
    return VertxCompletableFuture.allOf(ctx, encumbranceDistributionPairs.stream()
      .map(pair -> createRecordInStorage(JsonObject.mapFrom(pair.getLeft()), String.format(ENCUMBRANCE_POST_ENDPOINT, lang))
        .thenAccept(id -> pair.getValue().setEncumbrance(id))
        .exceptionally(fail -> {
          checkForCustomTransactionError(fail);
          return null;
        }))
      .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> setCurrentFiscalYear(List<Transaction> encumbrances) {
    Map<String, List<Transaction>> groupedByFund = encumbrances.stream()
      .collect(groupingBy(Transaction::getFromFundId));
    return groupByLedgerIds(groupedByFund).thenCompose(groupedByLedgerId -> VertxCompletableFuture.allOf(ctx,
        groupedByLedgerId.entrySet()
          .stream()
          .map(entry -> getCurrentFiscalYear(entry.getKey()).thenAccept(fiscalYear -> updateEncumbrances(entry, fiscalYear)))
          .collect(toList())
          .toArray(new CompletableFuture[0])));
  }

  private void updateEncumbrances(Map.Entry<String, List<Transaction>> entry, FiscalYear fiscalYear) {
    entry.getValue()
      .forEach(transaction -> transaction.withFiscalYearId(fiscalYear.getId())
        .withCurrency(fiscalYear.getCurrency()));
  }

  private CompletableFuture<Map<String, List<Transaction>>> groupByLedgerIds(Map<String, List<Transaction>> groupedByFund) {
    return getFunds(groupedByFund).thenApply(lists -> lists.stream()
      .flatMap(Collection::stream)
      .collect(HashMap::new, accumulator(groupedByFund), Map::putAll));
  }

  private CompletableFuture<List<List<Fund>>> getFunds(Map<String, List<Transaction>> groupedByFund) {
    return collectResultsOnSuccess(StreamEx.ofSubLists(new ArrayList<>(groupedByFund.entrySet()), MAX_IDS_FOR_GET_RQ)
      .map(entries -> entries.stream()
        .map(Map.Entry::getKey)
        .distinct()
        .collect(toList()))
      .map(this::getFundsByIds)
      .toList());
  }


  private BiConsumer<HashMap<String, List<Transaction>>, Fund> accumulator(Map<String, List<Transaction>> groupedByFund) {
    return (map, fund) -> map.merge(fund.getLedgerId(), groupedByFund.get(fund.getId()), (transactions, transactions2) -> {
      transactions.addAll(transactions2);
      return transactions;
    });
  }

  private CompletableFuture<List<Fund>> getFundsByIds(List<String> ids) {
    String query = convertIdsToCqlQuery(ids);
    String queryParam = "&query=" + encodeQuery(query, logger);
    String endpoint = String.format(GET_FUNDS_WITH_SEARCH_PARAMS, MAX_IDS_FOR_GET_RQ, 0, queryParam, lang);

    return HelperUtils.handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(entries -> entries.mapTo(FundCollection.class))
      .thenApply(fundCollection -> {
        if (ids.size() == fundCollection.getFunds().size()) {
          return fundCollection.getFunds();
        }
        String missingIds = String.join(", ", CollectionUtils.subtract(ids, fundCollection.getFunds().stream().map(Fund::getId).collect(toList())));
        throw new HttpException(400, FUNDS_NOT_FOUND.toError().withParameters(Collections.singletonList(new Parameter().withKey("funds").withValue(missingIds))));
      });
  }

  private List<Pair<Transaction, FundDistribution>>  buildEncumbrances(CompositePurchaseOrder compPo) {

    return compPo.getCompositePoLines()
      .stream()
      .flatMap(poLine -> poLine.getFundDistribution()
        .stream()
        .map(fundDistribution -> new MutablePair<>(buildEncumbrance(fundDistribution, poLine, compPo), fundDistribution)))
      .collect(toList());
  }

  private CompletableFuture<FiscalYear> getCurrentFiscalYear(String ledgerId) {
    String endpoint = String.format(GET_CURRENT_FISCAL_YEAR_BY_ID, ledgerId, lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger).thenApply(entry -> entry.mapTo(FiscalYear.class))
      .exceptionally(t -> {
        if (isFiscalYearNotFound(t)) {
          List<Parameter> parameters = Collections.singletonList(new Parameter().withValue(ledgerId)
            .withKey("ledgerId"));
          throw new HttpException(400, CURRENT_FISCAL_YEAR_NOT_FOUND.toError()
            .withParameters(parameters));
        }
        throw new CompletionException(t.getCause());
      });
  }

  private boolean isFiscalYearNotFound(Throwable t) {
    return t.getCause() instanceof HttpException && ((HttpException) t.getCause()).getCode() == 404;
  }

  private Transaction buildEncumbrance(FundDistribution distribution, CompositePoLine poLine, CompositePurchaseOrder compPo) {
    MonetaryAmount estimatedPrice = calculateEstimatedPrice(poLine.getCost());
    Transaction transaction = new Transaction();
    transaction.setTransactionType(Transaction.TransactionType.ENCUMBRANCE);
    transaction.setFromFundId(distribution.getFundId());
    transaction.setSource(Transaction.Source.USER);
    transaction.setAmount(calculateAmountEncumbered(distribution, estimatedPrice));

    transaction.setEncumbrance(new Encumbrance());
    transaction.getEncumbrance()
      .setSourcePoLineId(poLine.getId());
    transaction.getEncumbrance()
      .setSourcePurchaseOrderId(compPo.getId());
    transaction.getEncumbrance()
      .setReEncumber(compPo.getReEncumber());
    transaction.getEncumbrance()
      .setSubscription(compPo.getOrderType() == CompositePurchaseOrder.OrderType.ONGOING);
    transaction.getEncumbrance()
      .setStatus(Encumbrance.Status.UNRELEASED);
    transaction.getEncumbrance()
      .setOrderType(Encumbrance.OrderType.fromValue(compPo.getOrderType()
        .value()));
    transaction.getEncumbrance()
      .setInitialAmountEncumbered(transaction.getAmount());

    return transaction;
  }

  private double calculateAmountEncumbered(FundDistribution distribution, MonetaryAmount estimatedPrice) {
    if (distribution.getDistributionType() == FundDistribution.DistributionType.PERCENTAGE) {
      return estimatedPrice.with(MonetaryOperators.percent(distribution.getValue()))
        .with(MonetaryOperators.rounding())
        .getNumber()
        .doubleValue();
    }
    return distribution.getValue();
  }

  public void checkForCustomTransactionError(Throwable fail) {
    if (fail.getCause().getMessage().contains(BUDGET_NOT_FOUND_FOR_TRANSACTION.getDescription())) {
      throw new CompletionException(new HttpException(422, BUDGET_NOT_FOUND_FOR_TRANSACTION));
    } else if (fail.getCause().getMessage().contains(LEDGER_NOT_FOUND_FOR_TRANSACTION.getDescription())) {
      throw new CompletionException(new HttpException(422, LEDGER_NOT_FOUND_FOR_TRANSACTION));
    } else if (fail.getCause().getMessage().contains(BUDGET_IS_INACTIVE.getDescription())) {
      throw new CompletionException(new HttpException(422, BUDGET_IS_INACTIVE));
    } else if (fail.getCause().getMessage().contains(FUND_CANNOT_BE_PAID.getDescription())) {
      throw new CompletionException(new HttpException(422, FUND_CANNOT_BE_PAID));
    } else {
      throw new CompletionException(fail.getCause());
    }
  }
}
