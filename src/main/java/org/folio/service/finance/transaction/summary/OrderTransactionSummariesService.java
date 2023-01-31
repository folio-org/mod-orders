package org.folio.service.finance.transaction.summary;

import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;

import io.vertx.core.Future;
import io.vertx.core.Promise;

public class OrderTransactionSummariesService extends AbstractTransactionSummariesService<OrderTransactionSummary> {

    private static final String SUMMARY_NAME = "order";

    public OrderTransactionSummariesService(RestClient restClient) {
        super(restClient);
    }

    public Future<Void> updateTransactionSummary(String orderId, int number, RequestContext requestContext) {
      OrderTransactionSummary summary = new OrderTransactionSummary()
        .withId(orderId)
        .withNumTransactions(number);
      return updateTransactionSummary(summary, requestContext);
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
        getTransactionSummary(orderId, requestContext).onComplete(result -> {
          if (result.succeeded() && result.result() != null) {
            updateTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
              .onSuccess(a -> promise.complete())
              .onFailure(promise::fail);
          } else if (result.result() == null) {
            createTransactionSummary(new OrderTransactionSummary().withId(orderId).withNumTransactions(holder.getAllEncumbrancesQuantity()), requestContext)
              .onSuccess(a -> promise.complete())
              .onFailure(promise::fail);
          } else {
            promise.fail(result.cause());
          }
        });

      } else if (holder.getAllEncumbrancesQuantity() == 0) {
        promise.complete();
      } else {
        updateTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext)
          .onSuccess(a -> promise.complete())
          .onFailure(promise::fail);
      }
      return promise.future();
    }

  @Override
  protected String getId(OrderTransactionSummary summary) {
    return summary.getId();
  }

  @Override
  protected int getTransactionNumber(OrderTransactionSummary summary) {
    return summary.getNumTransactions();
  }

  @Override
  protected String getSummaryName() {
    return SUMMARY_NAME;
  }

  @Override
  protected Class<OrderTransactionSummary> getClassT() {
    return OrderTransactionSummary.class;
  }

}
