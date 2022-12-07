package org.folio.service.finance.transaction;

import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;
import io.vertx.core.Promise;

public class TransactionSummariesService {

    private static final String ENDPOINT = "/finance/order-transaction-summaries";
    private static final String GET_BY_ID_STORAGE_ENDPOINT = "/finance-storage/order-transaction-summaries/{id}";
    private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

    private final RestClient restClient;

    public TransactionSummariesService(RestClient restClient) {
        this.restClient = restClient;
    }

    public Future<OrderTransactionSummary> createOrderTransactionSummary(String id, int number, RequestContext requestContext) {
        OrderTransactionSummary summary = new OrderTransactionSummary()
                .withId(id)
                .withNumTransactions(number);
        RequestEntry requestEntry = new RequestEntry(ENDPOINT);
        return restClient.post(requestEntry, summary, OrderTransactionSummary.class, requestContext);
    }

    public Future<OrderTransactionSummary> getOrderTransactionSummary(String orderId, RequestContext requestContext) {
        RequestEntry requestEntry = new RequestEntry(GET_BY_ID_STORAGE_ENDPOINT).withId(orderId);
        return restClient.get(requestEntry.buildEndpoint(), true, OrderTransactionSummary.class, requestContext);
    }

    public Future<Void> updateOrderTransactionSummary(String orderId, int number, RequestContext requestContext) {
        if (number > 0) {
            OrderTransactionSummary summary = new OrderTransactionSummary()
                    .withId(orderId)
                    .withNumTransactions(number);
            RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(orderId);
            return restClient.put(requestEntry, summary, requestContext);
        } else {
            return Future.succeededFuture();
        }
    }

    public Future<Void> createOrUpdateOrderTransactionSummary(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
      Promise<Void> promise = Promise.promise();

      Stream<String> orderIdFromCreate = holder.getEncumbrancesForCreate()
        .stream()
        .map(EncumbranceRelationsHolder::getOrderId);
      Stream<String> orderIdFromStorage = holder.getEncumbrancesFromStorage()
        .stream()
        .map(Transaction::getEncumbrance)
        .map(Encumbrance::getSourcePurchaseOrderId);
      String orderId = Stream.concat(orderIdFromCreate, orderIdFromStorage)
        .findFirst()
        .orElse(null);

      // update or create summary if not exists
      if (CollectionUtils.isEmpty(holder.getEncumbrancesFromStorage())) {
        getOrderTransactionSummary(orderId, requestContext).onComplete(result -> {
          if (result.succeeded() && result.result() != null) {
            updateOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
              .onSuccess(a -> promise.complete())
              .onFailure(promise::fail);
          } else if (result.result() == null) {
            createOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
              .onSuccess(a -> promise.complete())
              .onFailure(promise::fail);
          } else {
            promise.fail(result.cause());
          }
        });

      } else if (holder.getAllEncumbrancesQuantity() == 0) {
        promise.complete();
      } else {
        updateOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
          .onSuccess(a -> promise.complete())
          .onFailure(promise::fail);
      }
      return promise.future();
    }

}
