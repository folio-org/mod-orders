package org.folio.service.finance.transaction.summary;

import java.util.concurrent.CompletionException;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;

import io.vertx.core.Future;

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



  public Future<Void> updateOrCreateTransactionSummary(String orderId, int number, RequestContext requestContext) {
    return getTransactionSummary(orderId, requestContext)
      .recover(t -> {
        if (HelperUtils.isNotFound(t))
          return null;
        throw new CompletionException(t);
      })
      .compose(summary -> {
        if (summary != null) {
          return updateTransactionSummary(orderId, number, requestContext);
        }
        return createTransactionSummary(new OrderTransactionSummary().withId(orderId).withNumTransactions(number),
          requestContext)
          .mapEmpty();
      });
  }

  public Future<Void> createOrUpdateOrderTransactionSummary(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
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
      return updateOrCreateTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext);
    } else if (holder.getAllEncumbrancesQuantity() == 0) {
      return Future.succeededFuture();
    } else {
      return updateTransactionSummary(orderId, holder.getAllEncumbrancesQuantity(), requestContext);
    }
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
