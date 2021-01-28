package org.folio.service.finance;

import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgressCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRolloverCollection;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.service.finance.EncumbranceService.AND;

public class RolloverRetrieveService {

    private static final String LEDGER_ROLLOVERS_ENDPOINT = "/finance/ledger-rollovers";
    private static final String LEDGER_ROLLOVERS_PROGRESS_ENDPOINT = "/finance/ledger-rollovers-progress";

    private final RestClient restClient;

    public RolloverRetrieveService(RestClient restClient) {
        this.restClient = restClient;
    }


    public CompletableFuture<LedgerFiscalYearRolloverCollection> getLedgerFyRollovers(String fyId, String ledgerId, RequestContext requestContext) {
        String query = "toFiscalYearId==" + fyId + AND + "ledgerId==" + ledgerId ;
        RequestEntry requestEntry = new RequestEntry(LEDGER_ROLLOVERS_ENDPOINT)
                .withQuery(query).withOffset(0).withLimit(1);
        return restClient.get(requestEntry, requestContext, LedgerFiscalYearRolloverCollection.class);
    }

    public CompletableFuture<List<LedgerFiscalYearRollover>> getLedgerFyRollovers(String fiscalYeaId, List<String> ledgerIds, RequestContext requestContext) {
        String query = "fromFiscalYearId==" + fiscalYeaId + AND + convertIdsToCqlQuery(ledgerIds, "ledgerId");
        RequestEntry requestEntry = new RequestEntry(LEDGER_ROLLOVERS_ENDPOINT)
                .withQuery(query).withOffset(0).withLimit(ledgerIds.size());
        return restClient.get(requestEntry, requestContext, LedgerFiscalYearRolloverCollection.class)
                .thenApply(LedgerFiscalYearRolloverCollection::getLedgerFiscalYearRollovers);
    }

    public CompletableFuture<List<LedgerFiscalYearRolloverProgress>> getRolloversProgress(String rolloverId, RequestContext requestContext) {
        String query = "ledgerRolloverId==" + rolloverId;
        RequestEntry requestEntry = new RequestEntry(LEDGER_ROLLOVERS_PROGRESS_ENDPOINT)
                .withQuery(query)
                .withOffset(0)
                .withLimit(1);
        return restClient.get(requestEntry, requestContext, LedgerFiscalYearRolloverProgressCollection.class)
                .thenApply(LedgerFiscalYearRolloverProgressCollection::getLedgerFiscalYearRolloverProgresses);
    }


}
