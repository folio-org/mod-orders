package org.folio.service;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;

import io.vertx.core.Future;

public class ReasonForClosureService {

  private static final String ENDPOINT = "/orders-storage/configuration/reasons-for-closure";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private final RestClient restClient;

  public ReasonForClosureService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<ReasonForClosureCollection> getReasonsForClosure(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, ReasonForClosureCollection.class, requestContext);
  }

  public Future<ReasonForClosure> getReasonForClosureById(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.get(requestEntry, ReasonForClosure.class, requestContext);
  }

  public Future<ReasonForClosure> createReasonForClosure(ReasonForClosure reasonForClosure, RequestContext requestContext) {
    // Set Source.USER according to requirement. Source.SYSTEM was populated by storage module.
    reasonForClosure.setSource(ReasonForClosure.Source.USER);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT);
    return restClient.post(requestEntry, reasonForClosure, ReasonForClosure.class, requestContext);
  }

  public Future<Void> updateReasonForClosure(String id, ReasonForClosure reasonForClosure, RequestContext requestContext) {

    if (isEmpty(reasonForClosure.getId())) {
      reasonForClosure.setId(id);
    } else if (!id.equals(reasonForClosure.getId())) {
      return Future.failedFuture(new HttpException(422, MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY));
    }

    // Set Source.USER according to requirement. Source.SYSTEM was populated by storage module.
    reasonForClosure.setSource(ReasonForClosure.Source.USER);
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.put(requestEntry, reasonForClosure, requestContext);
  }

  public Future<Void> deleteReasonForClosure(String id, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(id);
    return restClient.delete(requestEntry, requestContext);
  }

}
