package org.folio.service.finance.transaction;

import java.util.concurrent.CompletableFuture;
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

public class TransactionSummariesService {

    private static final String ENDPOINT = "/finance/order-transaction-summaries";
    private static final String GET_BY_ID_STORAGE_ENDPOINT = "/finance-storage/order-transaction-summaries/{id}";
    private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

    private final RestClient restClient;

    public TransactionSummariesService(RestClient restClient) {
        this.restClient = restClient;
    }

    public CompletableFuture<OrderTransactionSummary> createOrderTransactionSummary(String id, int number, RequestContext requestContext) {
        OrderTransactionSummary summary = new OrderTransactionSummary()
                .withId(id)
                .withNumTransactions(number);
        RequestEntry requestEntry = new RequestEntry(ENDPOINT);
        return restClient.post(requestEntry, summary, requestContext, OrderTransactionSummary.class);
    }

    public CompletableFuture<OrderTransactionSummary> getOrderTransactionSummary(String orderId, RequestContext requestContext) {
        RequestEntry requestEntry = new RequestEntry(GET_BY_ID_STORAGE_ENDPOINT).withId(orderId);
        return restClient.get(requestEntry, true, requestContext, OrderTransactionSummary.class);
    }

    public CompletableFuture<Void> updateOrderTransactionSummary(String orderId, int number, RequestContext requestContext) {
        if (number > 0) {
            OrderTransactionSummary summary = new OrderTransactionSummary()
                    .withId(orderId)
                    .withNumTransactions(number);
            RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(orderId);
            return restClient.put(requestEntry, summary, requestContext);
        } else {
            return CompletableFuture.completedFuture(null);
        }
    }

    public CompletableFuture<Void> createOrUpdateOrderTransactionSummary(EncumbrancesProcessingHolder holder,
        RequestContext requestContext) {
      CompletableFuture<Void> future = new CompletableFuture<>();

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
        getOrderTransactionSummary(orderId, requestContext).handle((ok, error) -> {
          if (error == null && ok != null) {
            updateOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
              .thenAccept(a -> future.complete(null))
              .exceptionally(t -> {
                future.completeExceptionally(t);
                return null;
              });
          } else if (ok == null) {
            createOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
              .thenAccept(a -> future.complete(null))
              .exceptionally(t -> {
                future.completeExceptionally(t);
                return null;
              });
          } else {
            future.completeExceptionally(error);
          }
          return null;
        });

      } else if (holder.getAllEncumbrancesQuantity() == 0) {
        future.complete(null);
      } else {
        updateOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
          .thenAccept(a -> future.complete(null))
          .exceptionally(t -> {
            future.completeExceptionally(t);
            return null;
          });
      }
      return future;
    }

}
