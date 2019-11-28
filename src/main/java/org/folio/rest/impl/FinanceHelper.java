package org.folio.rest.impl;

import static java.util.concurrent.CompletableFuture.allOf;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.ResourcePathResolver.ENCUMBRANCES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import javax.money.MonetaryAmount;

import org.apache.commons.lang3.StringUtils;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;

public class FinanceHelper extends AbstractHelper {
  private static final String ENCUMBRANCE_POST_ENDPOINT = resourcesPath(ENCUMBRANCES) + "?lang=%s";
  private static final String ENCUMBRANCE_GET_ENDPOINT = resourcesPath(ENCUMBRANCES) + "?limit=%d&query=%s&lang=%s";

  FinanceHelper(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  /**
   * Creates Encumbrance records associated with given PO line and updates PO line with corresponding links.
   *
   * @param poLine Composite PO line to create encumbrances for
   * @return CompletableFuture with void on success.
   */
  CompletableFuture<Void> handleEncumbrances(CompositePoLine poLine) {
    return hasMissingEncumbrances(poLine) ?
      getExistingEncumbrances(poLine).thenCompose(encumbrances -> createOrLinkEncumbrances(poLine, encumbrances)) :
      CompletableFuture.completedFuture(null);
  }

  private CompletableFuture<Void> createOrLinkEncumbrances(CompositePoLine poLine, List<Transaction> encumbrances) {
    return allOf(poLine.getFundDistribution()
      .stream()
      .filter(distribution -> Objects.isNull(distribution.getEncumbrance()))
      .map(distribution -> handleEncumbrance(distribution, poLine, encumbrances))
      .toArray(CompletableFuture[]::new));
  }

  /**
   * @param poLine Composite PO line to check if any encumbrance is missing
   * @return {@code true} if there it at least one {@link FundDistribution} without linked encumbrance
   */
  private boolean hasMissingEncumbrances(CompositePoLine poLine) {
    return Optional.ofNullable(poLine.getFundDistribution())
      .map(distributions -> distributions.stream()
        .map(FundDistribution::getEncumbrance)
        .anyMatch(Objects::isNull))
      .orElse(false);
  }

  /**
   * @param poLine Composite PO line to search encumbrances for
   * @return list of encumbrance records which are not linked to any {@link FundDistribution}
   */
  private CompletableFuture<List<Transaction>> getExistingEncumbrances(CompositePoLine poLine) {
    int limit = poLine.getFundDistribution().size();
    String endpoint = String.format(ENCUMBRANCE_GET_ENDPOINT, limit, "poLineId==" + poLine.getId(), lang);
    return handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
      .thenApply(json -> json.mapTo(TransactionCollection.class).getTransactions())
      .thenApply(encumbrances -> filterLinkedEncumbrances(poLine.getFundDistribution(), encumbrances));
  }

  private List<Transaction> filterLinkedEncumbrances(List<FundDistribution> fundDistributions, List<Transaction> encumbrances) {
    encumbrances.removeIf(encumbrance -> fundDistributions.stream()
      .anyMatch(fundDistribution -> encumbrance.getId()
        .equals(fundDistribution.getEncumbrance())));
    return encumbrances;
  }

  private CompletableFuture<Void> handleEncumbrance(FundDistribution distribution, CompositePoLine poLine,
                                                    List<Transaction> encumbrances) {
    Transaction encumbrance = buildEncumbrance(distribution, poLine);
    String existingEncumbranceId = matchExistingEncumbrance(encumbrances, encumbrance);
    if (existingEncumbranceId != null) {
      distribution.setEncumbrance(existingEncumbranceId);
      return CompletableFuture.completedFuture(null);
    } else {
      return createRecordInStorage(JsonObject.mapFrom(encumbrance), String.format(ENCUMBRANCE_POST_ENDPOINT, lang))
        .thenAccept(distribution::setEncumbrance);
    }
  }

  /**
   * The method tries to match existing encumbrance record with the one build based on fund distribution without linked encumbrance.
   * The matching logic is based on fund id and amount encumbered.
   * If matched, the encumbrance is removed from the list and its id returned as a result.
   *
   * @param transactions list of existing encumbrances not linked to any fund distribution
   * @param transaction new encumbrance record build based on particular fund distribution
   * @return {@code null} or id of the existing encumbrance record if matched
   */
  private String matchExistingEncumbrance(List<Transaction> transactions, Transaction transaction) {
    final Iterator<Transaction> each = transactions.iterator();
    while (each.hasNext()) {
      Transaction next = each.next();
      if (StringUtils.equals(transaction.getFromFundId(), next.getFromFundId())
        && Double.compare(transaction.getAmount(), next.getAmount()) == 0) {
        each.remove();
        return next.getId();
      }
    }
    return null;
  }

  private Transaction buildEncumbrance(FundDistribution distribution, CompositePoLine poLine) {
    MonetaryAmount estimatedPrice = calculateEstimatedPrice(poLine.getCost());
    Transaction encumbrance = new Transaction();
//    encumbrance.getEncumbrance().se(poLine.getId());
//    encumbrance.setAmountEncumbered(calculateAmountEncumbered(distribution, estimatedPrice));
//    encumbrance.setFundId(distribution.getFundId());
    return encumbrance;
  }

  private double calculateAmountEncumbered(FundDistribution distribution, MonetaryAmount estimatedPrice) {
    return estimatedPrice.with(MonetaryOperators.percent(distribution.getValue()))
      .with(MonetaryOperators.rounding())
      .getNumber()
      .doubleValue();
  }
}
