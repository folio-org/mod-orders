package org.folio.service;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.folio.dao.ReasonForClosureDAO;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.Context;

public class ReasonForClosureService {

  @Autowired
  private ReasonForClosureDAO reasonForClosureDAO;

  public CompletableFuture<ReasonForClosureCollection> getReasonsForClosure(String query, int limit, int offset, Context context, Map<String, String> okapiHeaders) {
    return reasonForClosureDAO.get(query, limit, offset, context, okapiHeaders);
  }

  public CompletableFuture<ReasonForClosure> getReasonForClosureById(String id, Context context, Map<String, String> okapiHeaders) {
    return reasonForClosureDAO.getById(id, context, okapiHeaders);
  }

  public CompletableFuture<ReasonForClosure> createReasonForClosure(ReasonForClosure reasonForClosure, Context context, Map<String, String> okapiHeaders) {
    // Set Source.USER according to requirement. Source.SYSTEM was populated by storage module.
    reasonForClosure.setSource(ReasonForClosure.Source.USER);
    return reasonForClosureDAO.save(reasonForClosure, context, okapiHeaders);
  }

  public CompletableFuture<Void> updateReasonForClosure(String id, ReasonForClosure reasonForClosure, Context context, Map<String, String> okapiHeaders) {

    if (isEmpty(reasonForClosure.getId())) {
      reasonForClosure.setId(id);
    } else if (!id.equals(reasonForClosure.getId())) {
      CompletableFuture<Void> future = new CompletableFuture<>();
      future.completeExceptionally(new HttpException(422, MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY));
      return future;
    }

    // Set Source.USER according to requirement. Source.SYSTEM was populated by storage module.
    reasonForClosure.setSource(ReasonForClosure.Source.USER);
    return reasonForClosureDAO.update(id, reasonForClosure, context, okapiHeaders);
  }

  public CompletableFuture<Void> deleteReasonForClosure(String id, Context context, Map<String, String> okapiHeaders) {
    return reasonForClosureDAO.delete(id, context, okapiHeaders);
  }

}
