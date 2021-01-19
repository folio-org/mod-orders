package org.folio.service;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;

import java.util.concurrent.CompletableFuture;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;

public class ReasonForClosureService {

  private static final String ENDPOINT = "/orders-storage/configuration/reasons-for-closure";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private final RestClient restClient;

  public ReasonForClosureService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<ReasonForClosureCollection> getReasonsForClosure(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, requestContext, ReasonForClosureCollection.class);
  }

  public CompletableFuture<ReasonForClosure> getReasonForClosureById(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.get(requestEntry, requestContext, ReasonForClosure.class);
  }

  public CompletableFuture<ReasonForClosure> createReasonForClosure(ReasonForClosure reasonForClosure, RequestContext requestContext) {
    // Set Source.USER according to requirement. Source.SYSTEM was populated by storage module.
    reasonForClosure.setSource(ReasonForClosure.Source.USER);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT);
    return restClient.post(requestEntry, reasonForClosure, requestContext, ReasonForClosure.class);
  }

  public CompletableFuture<Void> updateReasonForClosure(String id, ReasonForClosure reasonForClosure, RequestContext requestContext) {

    if (isEmpty(reasonForClosure.getId())) {
      reasonForClosure.setId(id);
    } else if (!id.equals(reasonForClosure.getId())) {
      CompletableFuture<Void> future = new CompletableFuture<>();
      future.completeExceptionally(new HttpException(422, MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY));
      return future;
    }

    // Set Source.USER according to requirement. Source.SYSTEM was populated by storage module.
    reasonForClosure.setSource(ReasonForClosure.Source.USER);
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.put(requestEntry, reasonForClosure, requestContext);
  }

  public CompletableFuture<Void> deleteReasonForClosure(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.delete(requestEntry, requestContext);
  }

}
