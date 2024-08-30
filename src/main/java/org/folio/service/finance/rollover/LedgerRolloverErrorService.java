package org.folio.service.finance.rollover;

import java.util.List;
import java.util.stream.Collectors;

import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import static org.folio.orders.utils.ResourcePathResolver.LEDGER_FY_ROLLOVER_ERRORS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class LedgerRolloverErrorService {

    // use only for get by query requests
    private static final String ENDPOINT = resourcesPath(LEDGER_FY_ROLLOVER_ERRORS);
    private static final String ENDPOINT_BY_ID = resourceByIdPath(LEDGER_FY_ROLLOVER_ERRORS) + "{id}";

    private final RestClient restClient;

    public LedgerRolloverErrorService(RestClient restClient) {
        this.restClient = restClient;
    }

    public Future<LedgerFiscalYearRolloverErrorCollection> getRolloverErrorsByRolloverId(String rolloverId, RequestContext requestContext) {
      String query = "ledgerRolloverId==" + rolloverId ;
      RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
      return restClient.get(requestEntry, LedgerFiscalYearRolloverErrorCollection.class, requestContext);
    }

    public Future<LedgerFiscalYearRolloverErrorCollection> getLedgerFyRolloverErrors(String orderId, RequestContext requestContext) {
        String query = "details.purchaseOrderId==" + orderId;
        RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
        return restClient.get(requestEntry, LedgerFiscalYearRolloverErrorCollection.class, requestContext);
    }

    public Future<LedgerFiscalYearRolloverError> saveRolloverError(String rolloverId, Throwable t, LedgerFiscalYearRolloverError.ErrorType errorType,
                                         String failedAction, RequestContext requestContext) {
      String message;
      try {
        JsonObject errorObj = new JsonObject(t.getMessage());
        message = errorObj.getJsonArray("errors").getJsonObject(0).getString("message");
        if (message == null)
          message = t.getMessage();
      } catch (Exception ex) {
        message = t.getMessage();
      }
      LedgerFiscalYearRolloverError error = new LedgerFiscalYearRolloverError()
        .withLedgerRolloverId(rolloverId)
        .withErrorType(errorType)
        .withFailedAction(failedAction)
        .withErrorMessage(message);
      RequestEntry requestEntry = new RequestEntry(ENDPOINT);
      return restClient.post(requestEntry, error, LedgerFiscalYearRolloverError.class, requestContext);
  }

    public Future<Void> deleteRolloverErrors(List<LedgerFiscalYearRolloverError> errors, RequestContext requestContext) {
      return GenericCompositeFuture.join(errors.stream()
        .map(error -> deleteRolloverError(error.getId(), requestContext))
        .collect(Collectors.toList()))
        .mapEmpty();
    }

    public Future<Void> deleteRolloverError(String id, RequestContext requestContext) {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT_BY_ID).withId(id);
        return restClient.delete(requestEntry, requestContext);
    }
}
