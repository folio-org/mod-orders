package org.folio.service.finance.transaction;

import io.vertx.core.Future;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

public abstract class AbstractTransactionManagingService {

  private static final Map<Transaction.TransactionType, String> TRANSACTION_ENDPOINTS = Map.of(
    Transaction.TransactionType.PAYMENT, "/finance/payments",
    Transaction.TransactionType.CREDIT, "/finance/credits",
    Transaction.TransactionType.PENDING_PAYMENT, "/finance/pending-payments",
    Transaction.TransactionType.ENCUMBRANCE, "/finance/encumbrances"
  );

  protected final TransactionService transactionService;
  protected final RestClient restClient;

  protected AbstractTransactionManagingService(TransactionService transactionService, RestClient restClient) {
    this.transactionService = transactionService;
    this.restClient = restClient;
  }

  public Future<List<Transaction>> getTransactionsByEncumbranceIds(List<String> trIds, RequestContext requestContext) {
    return collectResultsOnSuccess(
      ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ_15).map(ids -> getTransactionsChunksByEncumbranceIds(ids, requestContext))
        .toList()).map(
      lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(Collectors.toList()));
  }

  public Future<Void> updateTransactions(List<Transaction> transactions, RequestContext requestContext) {
    return GenericCompositeFuture.join(transactions.stream()
        .map(transaction -> updateTransaction(transaction, requestContext))
        .collect(Collectors.toList()))
      .mapEmpty();
  }

  private Future<Void> updateTransaction(Transaction transaction, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(getByIdEndpoint(transaction)).withId(transaction.getId());
    return restClient.put(requestEntry, transaction, requestContext);
  }

  private Future<List<Transaction>> getTransactionsChunksByEncumbranceIds(List<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids, getEncumbranceIdentifierQuery()) ;
    return transactionService.getTransactions(query, requestContext);
  }

  private String getEndpoint(Transaction transaction) {
    return TRANSACTION_ENDPOINTS.get(transaction.getTransactionType());
  }

  private String getByIdEndpoint(Transaction transaction) {
    return getEndpoint(transaction) + "/{id}";
  }

  protected abstract String getEncumbranceIdentifierQuery();

}
