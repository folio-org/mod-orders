package org.folio.service.finance;

import static org.folio.service.finance.EncumbranceService.AND;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class RolloverErrorService {
    private static final String ENDPOINT = "/finance/ledger-rollovers-errors";

    private final RestClient restClient;

    public RolloverErrorService(RestClient restClient) {
        this.restClient = restClient;
    }

    public CompletableFuture<LedgerFiscalYearRolloverErrorCollection> getLedgerFyRolloverErrors(String orderId, String rolloverId, RequestContext requestContext) {
        String query = "details.purchaseOrderId==" + orderId + AND + "ledgerRolloverId==" + rolloverId ;
        RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
        return restClient.get(requestEntry, requestContext, LedgerFiscalYearRolloverErrorCollection.class);
    }
}
