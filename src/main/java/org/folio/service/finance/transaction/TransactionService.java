package org.folio.service.finance.transaction;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.ERROR_RETRIEVING_TRANSACTION;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

import io.vertx.core.Future;
import one.util.streamex.StreamEx;

public class TransactionService {
  private static final String ENDPOINT = "/finance/transactions";
  private static final String ENCUMBRANCE_ENDPOINT = "/finance/encumbrances";
  private static final String ENCUMBRANCE_BY_ID_ENDPOINT = "/finance/encumbrances/{id}";

  private final RestClient restClient;

  public TransactionService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<List<Transaction>> getTransactions(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, TransactionCollection.class, requestContext)
      .map(TransactionCollection::getTransactions);
  }

  public Future<List<Transaction>> getTransactionsByPoLinesIds(List<String> trIds, String searchCriteria, RequestContext requestContext) {
    return collectResultsOnSuccess(
        ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ_15).map(ids -> getTransactionsChunksByPoLineIds(ids, searchCriteria, requestContext))
          .toList()).map(
              lists -> lists.stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toList()));
  }

  public Future<List<Transaction>> getTransactionsByIds(List<String> trIds, RequestContext requestContext) {
    Set<String> uniqueTrIds = new HashSet<>(trIds);
    return collectResultsOnSuccess(StreamEx.ofSubLists(new ArrayList<>(uniqueTrIds), MAX_IDS_FOR_GET_RQ_15)
        .map(ids -> getTransactionsChunksByIds(ids, requestContext))
      .toList())
      .map(lists -> lists.stream().flatMap(Collection::stream).collect(Collectors.toList()))
      .map(trList -> {
        if (trList.size() != uniqueTrIds.size()) {
          List<Parameter> parameters = new ArrayList<>();
          parameters.add(new Parameter().withKey("trIds").withValue(trIds.toString()));
          throw new HttpException(500, ERROR_RETRIEVING_TRANSACTION.toError().withParameters(parameters));
        }
        return trList;
      });
  }


  private Future<List<Transaction>> getTransactionsChunksByPoLineIds(Collection<String> ids, String criteria, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids, "encumbrance.sourcePoLineId") + " AND " + criteria;
    return getTransactions(query, requestContext);
  }

  private Future<List<Transaction>> getTransactionsChunksByIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids) ;
    return getTransactions(query, requestContext);
  }

  public Future<Transaction> createTransaction(Transaction transaction, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENCUMBRANCE_ENDPOINT);
    return restClient.post(requestEntry, transaction, Transaction.class, requestContext);
  }

  public Future<Void> updateTransaction(Transaction transaction, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENCUMBRANCE_BY_ID_ENDPOINT).withId(transaction.getId());
    return restClient.put(requestEntry, transaction, requestContext);
  }

  public Future<Void> updateTransactions(List<Transaction> transactions, RequestContext requestContext) {
    return GenericCompositeFuture.join(transactions.stream()
      .map(transaction -> updateTransaction(transaction, requestContext))
      .collect(Collectors.toList()))
      .mapEmpty();
  }

  public Future<Void> deleteTransactions(List<Transaction> transactions, RequestContext requestContext) {
    // TODO: add semaphores
    return GenericCompositeFuture.join(transactions.stream()
      .map(transaction -> deleteTransactionById(transaction.getId(), requestContext))
        .collect(Collectors.toList()))
      .mapEmpty();
  }

  private Future<Void> deleteTransactionById(String transactionId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENCUMBRANCE_BY_ID_ENDPOINT).withId(transactionId);
    return restClient.delete(requestEntry, requestContext);
  }
}
