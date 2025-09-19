package org.folio.service.finance.transaction;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.FINANCE_BATCH_TRANSACTIONS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.ERROR_RETRIEVING_TRANSACTION;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import lombok.extern.log4j.Log4j2;
import org.folio.rest.acq.model.finance.Batch;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.acq.model.finance.TransactionPatch;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

import io.vertx.core.Future;

@Log4j2
public class TransactionService {

  private static final String ENDPOINT = "/finance/transactions";

  private static final String ENCUMBRANCE_SOURCE_PO_LINE_ID = "encumbrance.sourcePoLineId";
  private static final String AWAITING_PAYMENT_ENCUMBRANCE_ID = "awaitingPayment.encumbranceId";
  private static final String PAYMENT_ENCUMBRANCE_ID = "paymentEncumbranceId";

  private static final String PENDING_PAYMENT_TYPE = "transactionType == \"Pending payment\"";
  private static final String PAYMENT_TYPE = "transactionType == \"Payment\"";

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

  public Future<List<Transaction>> getTransactionsByIds(List<String> trIds, RequestContext requestContext) {
    Set<String> uniqueTrIds = new LinkedHashSet<>(trIds);
    return collectResultsOnSuccess(ofSubLists(new ArrayList<>(uniqueTrIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getTransactionsChunksByIds(ids, requestContext))
      .toList())
      .map(lists -> lists.stream().flatMap(Collection::stream).toList())
      .map(trList -> {
        if (trList.size() != uniqueTrIds.size()) {
          List<Parameter> parameters = new ArrayList<>();
          parameters.add(new Parameter().withKey("trIds").withValue(trIds.toString()));
          throw new HttpException(500, ERROR_RETRIEVING_TRANSACTION.toError().withParameters(parameters));
        }
        return trList;
      });
  }

  public Future<List<Transaction>> getTransactionsByPoLinesIds(List<String> trIds, String searchCriteria, RequestContext requestContext) {
    return collectResultsOnSuccess(ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getTransactionsChunksByFieldNameAndCriteria(ids, ENCUMBRANCE_SOURCE_PO_LINE_ID, searchCriteria, requestContext))
      .toList())
      .map(lists -> lists.stream().flatMap(Collection::stream).toList());
  }

  public Future<List<Transaction>> getPendingPaymentsByEncumbranceIds(List<String> trIds, RequestContext requestContext) {
    return collectResultsOnSuccess(ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getTransactionsChunksByFieldNameAndCriteria(ids, AWAITING_PAYMENT_ENCUMBRANCE_ID, PENDING_PAYMENT_TYPE, requestContext))
      .toList())
      .map(lists -> lists.stream().flatMap(Collection::stream).toList());
  }

  public Future<List<Transaction>> getPaymentsByEncumbranceIds(List<String> trIds, RequestContext requestContext) {
    return collectResultsOnSuccess(ofSubLists(new ArrayList<>(trIds), MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getTransactionsChunksByFieldNameAndCriteria(ids, PAYMENT_ENCUMBRANCE_ID, PAYMENT_TYPE, requestContext))
      .toList())
      .map(lists -> lists.stream().flatMap(Collection::stream).toList());
  }

  private Future<List<Transaction>> getTransactionsChunksByFieldNameAndCriteria(Collection<String> ids, String fieldName,
                                                                                String criteria, RequestContext requestContext) {
    String query = "%s AND %s".formatted(convertIdsToCqlQuery(ids, fieldName), criteria);
    return getTransactions(query, requestContext);
  }

  private Future<List<Transaction>> getTransactionsChunksByIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    return getTransactions(query, requestContext);
  }

  public Future<Void> batchAllOrNothing(List<Transaction> transactionsToCreate, List<Transaction> transactionsToUpdate,
                                        List<String> idsOfTransactionsToDelete, List<TransactionPatch> transactionPatches, RequestContext requestContext) {
    Batch batch = new Batch();
    if (transactionsToCreate != null) {
      transactionsToCreate.forEach(tr -> {
        if (tr.getId() == null) {
          tr.setId(UUID.randomUUID().toString());
        }
      });
      batch.setTransactionsToCreate(transactionsToCreate);
    }
    if (transactionsToUpdate != null) {
      batch.setTransactionsToUpdate(transactionsToUpdate);
    }
    if (idsOfTransactionsToDelete != null) {
      batch.setIdsOfTransactionsToDelete(idsOfTransactionsToDelete);
    }
    if (transactionPatches != null) {
      batch.setTransactionPatches(transactionPatches);
    }
    return restClient.postEmptyResponse(resourcesPath(FINANCE_BATCH_TRANSACTIONS), batch, requestContext)
      .onSuccess(v -> log.info("batchAllOrNothing completed successfully"))
      .onFailure(t -> log.error("batchAllOrNothing failed", t));
  }

  public Future<Void> batchCreate(List<Transaction> transactions, RequestContext requestContext) {
    return batchAllOrNothing(transactions, null, null, null, requestContext);
  }

  public Future<Void> batchUpdate(List<Transaction> transactions, RequestContext requestContext) {
    return batchAllOrNothing(null, transactions, null, null, requestContext);
  }

  public Future<Void> batchRelease(List<Transaction> transactions, RequestContext requestContext) {
    // NOTE: we will have to use transactionPatches when it is available (see MODORDERS-1008)
    transactions.forEach(tr -> tr.getEncumbrance().setStatus(Encumbrance.Status.RELEASED));
    return batchUpdate(transactions, requestContext);
  }

  public Future<Void> batchUnrelease(List<Transaction> transactions, RequestContext requestContext) {
    // NOTE: we will have to use transactionPatches when it is available (see MODORDERS-1008)
    transactions.forEach(tr -> tr.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED));
    return batchUpdate(transactions, requestContext);
  }

  public Future<Void> batchDelete(List<String> transactionIds, RequestContext requestContext) {
    return batchAllOrNothing(null, null, transactionIds, null, requestContext);
  }

  public Future<Void> batchReleaseAndDelete(List<Transaction> transactions, RequestContext requestContext) {
    // Transactions are not automatically released in mod-finance-storage before they are deleted.
    // They need to be released for the budgets to be updated correctly.
    // 0-amount encumbrances are not released before deletion to allow deleting pending order lines (MODORDERS-1253)
    // NOTE: we will have to use transactionPatches when it is available (see MODORDERS-1008)
    List<String> allIds = transactions.stream().map(Transaction::getId).toList();
    List<Transaction> transactionsToRelease = transactions.stream()
      .filter(tr -> tr.getEncumbrance().getStatus() != Encumbrance.Status.RELEASED && tr.getAmount() != 0d)
      .toList();
    transactionsToRelease.forEach(tr -> tr.getEncumbrance().setStatus(Encumbrance.Status.RELEASED));
    return batchAllOrNothing(null, transactionsToRelease, allIds, null, requestContext);
  }
}
