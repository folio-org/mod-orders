package org.folio.service;

import org.folio.rest.acq.model.finance.OrderTransactionSummary;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.impl.AbstractHelper;
import org.folio.rest.tools.client.interfaces.HttpClientInterface;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import me.escoffier.vertx.completablefuture.VertxCompletableFuture;
import static org.folio.orders.utils.HelperUtils.URL_WITH_LANG_PARAM;
import static org.folio.orders.utils.HelperUtils.getEndpointWithQuery;
import static org.folio.orders.utils.HelperUtils.handleGetRequest;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TRANSACTION_SUMMARIES;
import static org.folio.orders.utils.ResourcePathResolver.TRANSACTIONS_ENDPOINT;
import static org.folio.orders.utils.ResourcePathResolver.TRANSACTIONS_STORAGE_ENDPOINT;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class TransactionService extends AbstractHelper {
  private static final String TRANSACTION_ENDPOINT_BY_QUERY = resourcesPath(TRANSACTIONS_ENDPOINT) + SEARCH_PARAMS;
  public static final String TRANSACTION_STORAGE_ENDPOINT_BYID = resourceByIdPath(TRANSACTIONS_STORAGE_ENDPOINT) + URL_WITH_LANG_PARAM;

  public TransactionService(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(getHttpClient(okapiHeaders), okapiHeaders, ctx, lang);
  }

  public TransactionService(HttpClientInterface httpClient, Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(httpClient, okapiHeaders, ctx, lang);
  }

  public CompletableFuture<TransactionCollection> getTransactions(int limit, int offset, String query) {
    CompletableFuture<TransactionCollection> future = new VertxCompletableFuture<>(ctx);
    try {
      String queryParam = getEndpointWithQuery(query, logger);
      String endpoint = String.format(TRANSACTION_ENDPOINT_BY_QUERY, limit, offset, queryParam, lang);
      handleGetRequest(endpoint, httpClient, ctx, okapiHeaders, logger)
        .thenAccept(jsonTransactions -> {
          logger.info("Successfully retrieved voucher lines: " + jsonTransactions.encodePrettily());
          future.complete(jsonTransactions.mapTo(TransactionCollection.class));
        })
        .exceptionally(t -> {
          logger.error("Error getting voucher lines", t);
          future.completeExceptionally(t);
          return null;
        });
    } catch (Exception e) {
      future.completeExceptionally(e);
    }
    return future;
  }

  public CompletableFuture<String> createOrderTransactionSummary(String id, int number) {
    OrderTransactionSummary summary = new OrderTransactionSummary()
      .withId(id)
      .withNumTransactions(number);
    return createRecordInStorage(JsonObject.mapFrom(summary), resourcesPath(ORDER_TRANSACTION_SUMMARIES));
  }

  public CompletableFuture<Void> updateTransaction(Transaction transaction) {
    String endpoint = String.format(TRANSACTION_STORAGE_ENDPOINT_BYID, transaction.getId(), lang);
    return handleUpdateRequest(endpoint, transaction);
  }

  public CompletableFuture<Void> updateTransactions(List<Transaction> transactions) {
    return VertxCompletableFuture.allOf(ctx, transactions.stream()
      .map(this::updateTransaction)
      .toArray(CompletableFuture[]::new));
  }
}
