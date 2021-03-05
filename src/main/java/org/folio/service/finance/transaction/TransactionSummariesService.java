package org.folio.service.finance.transaction;

import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
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

    public CompletableFuture<Void> createOrUpdateOrderTransactionSummary(String orderId, EncumbrancesProcessingHolder holder, RequestContext requestContext) {
        if (CollectionUtils.isEmpty(holder.getEncumbrancesFromStorage())) {
            return createOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
                    .thenApply(id -> null);
        }
        else if (holder.getAllEncumbrancesQuantity() == 0) {
            return CompletableFuture.completedFuture(null);
        }
        else {
            return updateOrderTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext);
        }
    }

}
