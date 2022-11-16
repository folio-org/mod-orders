package org.folio.service.finance.rollover;

import static org.folio.service.finance.transaction.EncumbranceService.AND;

import java.util.List;
import java.util.stream.Collectors;

import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;

public class RolloverErrorService {
    private static final String ENDPOINT = "/finance/ledger-rollovers-errors";
    private static final String ENDPOINT_BY_ID = "/finance-storage/ledger-rollovers-errors/{id}";

    private final RestClient restClient;

    public RolloverErrorService(RestClient restClient) {
        this.restClient = restClient;
    }

    public Future<LedgerFiscalYearRolloverErrorCollection> getLedgerFyRolloverErrors(String orderId, String rolloverId, RequestContext requestContext) {
        String query = "details.purchaseOrderId==" + orderId + AND + "ledgerRolloverId==" + rolloverId ;
        RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
        return restClient.get(requestEntry, LedgerFiscalYearRolloverErrorCollection.class, requestContext);
    }

    public Future<LedgerFiscalYearRolloverErrorCollection> getLedgerFyRolloverErrors(String orderId, RequestContext requestContext) {
        String query = "details.purchaseOrderId==" + orderId;
        RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
        return restClient.get(requestEntry, LedgerFiscalYearRolloverErrorCollection.class, requestContext);
    }

    public Future<Void> deleteRolloverErrors(List<LedgerFiscalYearRolloverError> errors, RequestContext requestContext) {
      return GenericCompositeFuture.all(errors.stream()
        .map(error -> deleteRolloverError(error.getId(), requestContext))
        .collect(Collectors.toList()))
        .mapEmpty();
    }

    public Future<Void> deleteRolloverError(String id, RequestContext requestContext) {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT_BY_ID).withId(id);
        return restClient.delete(requestEntry, requestContext);
    }
}
