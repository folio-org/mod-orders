package org.folio.service.finance.transaction.summary;

import io.vertx.core.Future;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public abstract class AbstractTransactionSummariesService<T> {

  private static final String ENDPOINT = "/finance/%s-transaction-summaries";

  private static final String GET_BY_ID_STORAGE_ENDPOINT = "/finance-storage/%s-transaction-summaries/{id}";

  protected RestClient restClient;

  protected AbstractTransactionSummariesService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<T> createTransactionSummary(T summary, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(getEndpoint());
    return restClient.post(requestEntry, summary, getClassT(), requestContext);

  }

  public Future<T> getTransactionSummary(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(getStorageEndpointById()).withId(id);
    return restClient.get(requestEntry.buildEndpoint(), true, getClassT(), requestContext);
  }

  public Future<Void> updateTransactionSummary(T summary, RequestContext requestContext) {
    int transactionNumber = getTransactionNumber(summary);
    if (transactionNumber > 0) {
      RequestEntry requestEntry = new RequestEntry(getEndpointById()).withId(getId(summary));
      return restClient.put(requestEntry, summary, requestContext);
    } else {
      return Future.succeededFuture();
    }
  }

  private String getEndpointById() {
    return getEndpoint() + "/{id}";
  }

  private String getEndpoint() {
    return String.format(ENDPOINT, getSummaryName());
  }

  private String getStorageEndpointById() {
    return String.format(GET_BY_ID_STORAGE_ENDPOINT, getSummaryName());
  }

  protected abstract String getId(T summary);

  protected abstract int getTransactionNumber(T summary);

  protected abstract String getSummaryName();

  protected abstract Class<T> getClassT();
}
