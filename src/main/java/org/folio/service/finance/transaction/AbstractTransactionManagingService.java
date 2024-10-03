package org.folio.service.finance.transaction;

import io.vertx.core.Future;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

public abstract class AbstractTransactionManagingService {

  protected final TransactionService transactionService;

  protected AbstractTransactionManagingService(TransactionService transactionService) {
    this.transactionService = transactionService;
  }

  public Future<List<Transaction>> getTransactionsByEncumbranceIds(List<String> trIds, RequestContext requestContext) {
    return collectResultsOnSuccess(
      ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ_15).map(ids -> getTransactionsChunksByEncumbranceIds(ids, requestContext))
        .toList()).map(
      lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(Collectors.toList()));
  }

  private Future<List<Transaction>> getTransactionsChunksByEncumbranceIds(List<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids, getEncumbranceIdentifierQuery()) ;
    return transactionService.getTransactions(query, requestContext);
  }

  protected abstract String getEncumbranceIdentifierQuery();

}
