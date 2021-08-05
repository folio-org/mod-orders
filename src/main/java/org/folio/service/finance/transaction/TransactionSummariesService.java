package org.folio.service.finance.transaction;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class TransactionSummariesService {

    private static final String ENDPOINT = "/finance/order-transaction-summaries";
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
        RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(orderId);
        return restClient.get(requestEntry, requestContext, OrderTransactionSummary.class);
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
        return updateOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
          .handle((ok, error) -> {
            if (error == null) {
              return CompletableFuture.completedFuture(null);
            } else if (error instanceof HttpException && ((HttpException) error).getCode() == 404) {
              return createOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext);
            } else {
              throw new CompletionException(error);
            }
          })
          .thenApply(id -> null);
      } else if (holder.getAllEncumbrancesQuantity() == 0) {
        return CompletableFuture.completedFuture(null);
      } else {
        return updateOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext);
      }
    }

}
