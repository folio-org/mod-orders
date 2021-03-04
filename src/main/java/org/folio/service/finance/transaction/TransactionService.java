package org.folio.service.finance.transaction;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class TransactionService {

  private static final String ENDPOINT = "/finance/transactions";
  private static final String ENCUMBRANCE_ENDPOINT = "/finance/encumbrances";
  private static final String ENCUMBRANCE_BY_ID_ENDPOINT = "/finance/encumbrances/{id}";
  private static final String ENCUMBRANCE_STORAGE_BY_ID_ENDPOINT = "/finance-storage/transactions/{id}";

  private final RestClient restClient;

  public TransactionService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<TransactionCollection> getTransactions(String query, int offset, int limit,
                                                                  RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withOffset(offset)
      .withLimit(limit);

    return restClient.get(requestEntry, requestContext, TransactionCollection.class);
  }

  public CompletableFuture<List<Transaction>> getTransactionsByPoLinesIds(List<String> trIds, RequestContext requestContext) {
    return collectResultsOnSuccess(
        ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ).map(ids -> getTransactionsChunksByPoLineIds(ids, requestContext))
          .toList()).thenApply(
              lists -> lists.stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toList()));
  }

  public CompletableFuture<List<Transaction>> getCurrentPaymentsByEncumbranceIds(List<String> trIds, String fiscalYearId, RequestContext requestContext) {
    return collectResultsOnSuccess(
            ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ).map(ids -> getPaymentsChunksByEncumbranceIds(ids, fiscalYearId, requestContext))
                    .toList()).thenApply(
            lists -> lists.stream()
                    .flatMap(Collection::stream)
                    .collect(Collectors.toList()));
  }

  private CompletableFuture<List<Transaction>> getTransactionsChunksByPoLineIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids, "encumbrance.sourcePoLineId");
    return getTransactionsChunksByIds(query, requestContext);
  }

  private CompletableFuture<List<Transaction>> getPaymentsChunksByEncumbranceIds(Collection<String> ids, String fiscalYearId, RequestContext requestContext) {
    String query = String.format("transactionType==(Payment OR Credit) AND fiscalYearId==%s AND %s", fiscalYearId, convertIdsToCqlQuery(ids, "paymentEncumbranceId"));
    return getTransactionsChunksByIds(query, requestContext);
  }

  private CompletableFuture<List<Transaction>> getTransactionsChunksByIds(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, requestContext, TransactionCollection.class)
      .thenApply(TransactionCollection::getTransactions);
  }

  public CompletableFuture<Transaction> createTransaction(Transaction transaction, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENCUMBRANCE_ENDPOINT);
    return restClient.post(requestEntry, transaction, requestContext, Transaction.class);
  }

  public CompletableFuture<Void> updateTransaction(Transaction transaction, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENCUMBRANCE_BY_ID_ENDPOINT).withId(transaction.getId());
    return restClient.put(requestEntry, transaction, requestContext);
  }

  public CompletableFuture<Void> updateTransactions(List<Transaction> transactions, RequestContext requestContext) {
    return CompletableFuture.allOf(transactions.stream()
      .map(transaction -> updateTransaction(transaction, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  public CompletableFuture<Void> deleteTransactions(List<Transaction> transactions, RequestContext requestContext) {
    return CompletableFuture.allOf(transactions.stream()
      .map(transaction -> deleteTransaction(transaction, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> deleteTransaction(Transaction transaction, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENCUMBRANCE_STORAGE_BY_ID_ENDPOINT).withId(transaction.getId());
    return restClient.delete(requestEntry, requestContext);
  }
}
