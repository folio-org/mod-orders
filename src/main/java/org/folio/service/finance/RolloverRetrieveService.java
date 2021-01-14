package org.folio.service.finance;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRolloverCollection;

import java.util.concurrent.CompletableFuture;

import static org.folio.service.finance.EncumbranceService.AND;

public class RolloverRetrieveService {

    private static final String ENDPOINT = "/finance/ledger-rollovers";

    private final RestClient restClient;

    public RolloverRetrieveService(RestClient restClient) {
        this.restClient = restClient;
    }


    public CompletableFuture<LedgerFiscalYearRolloverCollection> getLedgerFyRollovers(String fyId, String ledgerId, RequestContext requestContext) {
        String query = "fromFiscalYearId==" + fyId + AND + "ledgerId==" + ledgerId ;
        RequestEntry requestEntry = new RequestEntry(ENDPOINT)
                .withQuery(query).withOffset(0).withLimit(1);
        return restClient.get(requestEntry, requestContext, LedgerFiscalYearRolloverCollection.class);
    }


}
