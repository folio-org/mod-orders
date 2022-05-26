package org.folio.service.finance.transaction;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.rest.core.exceptions.ErrorCodes.ERROR_RETRIEVING_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

public class TransactionService {

  private static final String ENDPOINT = "/finance/transactions";
  private static final String ENCUMBRANCE_ENDPOINT = "/finance/encumbrances";
  private static final String ENCUMBRANCE_BY_ID_ENDPOINT = "/finance/encumbrances/{id}";

  private final RestClient restClient;

  public TransactionService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<List<Transaction>> getTransactions(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, requestContext, TransactionCollection.class)
      .thenApply(TransactionCollection::getTransactions);
  }

  public CompletableFuture<List<Transaction>> getTransactionsByPoLinesIds(List<String> trIds, String searchCriteria, RequestContext requestContext) {
    return collectResultsOnSuccess(
        ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ).map(ids -> getTransactionsChunksByPoLineIds(ids, searchCriteria, requestContext))
          .toList()).thenApply(
              lists -> lists.stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toList()));
  }

  public CompletableFuture<List<Transaction>> getTransactionsByIds(List<String> trIds, RequestContext requestContext) {
    return collectResultsOnSuccess(ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ)
        .map(ids -> getTransactionsChunksByIds(ids, requestContext)).toList())
      .thenApply(lists -> lists.stream().flatMap(Collection::stream).collect(Collectors.toList()))
      .thenApply(trList -> {
        if (trList.size() != trIds.size()) {
          List<Parameter> parameters = new ArrayList<>();
          parameters.add(new Parameter().withKey("trIds").withValue(trIds.toString()));
          throw new HttpException(500, ERROR_RETRIEVING_TRANSACTION.toError().withParameters(parameters));
        }
        return trList;
      });
  }


  private CompletableFuture<List<Transaction>> getTransactionsChunksByPoLineIds(Collection<String> ids, String criteria, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids, "encumbrance.sourcePoLineId") + " AND " + criteria;
    return getTransactionsChunksByIds(query, requestContext);
  }

  private CompletableFuture<List<Transaction>> getTransactionsChunksByIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids) ;
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
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), transactions.stream()
      .map(transaction -> updateTransaction(transaction, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  public CompletableFuture<Void> deleteTransactions(List<Transaction> transactions, RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(),transactions.stream()
      .map(transaction -> deleteTransactionById(transaction.getId(), requestContext))
      .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> deleteTransactionById(String transactionId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENCUMBRANCE_BY_ID_ENDPOINT).withId(transactionId);
    return restClient.delete(requestEntry, requestContext);
  }
}
