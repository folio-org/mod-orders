package org.folio.service.finance.rollover;

import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.rest.jaxrs.model.LedgerFiscalYearRollover.RolloverType.COMMIT;
import static org.folio.service.finance.transaction.EncumbranceService.AND;

import java.util.List;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRolloverCollection;

import io.vertx.core.Future;

public class LedgerRolloverService {

    private static final String LEDGER_ROLLOVERS_ENDPOINT = "/finance/ledger-rollovers";

    private final RestClient restClient;

    public LedgerRolloverService(RestClient restClient) {
        this.restClient = restClient;
    }


    public Future<LedgerFiscalYearRolloverCollection> getLedgerFyRollovers(String fyId, String ledgerId, RequestContext requestContext) {
        String query = "toFiscalYearId==" + fyId + AND + "ledgerId==" + ledgerId ;
        RequestEntry requestEntry = new RequestEntry(LEDGER_ROLLOVERS_ENDPOINT)
                .withQuery(query).withOffset(0).withLimit(1);
        return restClient.get(requestEntry, LedgerFiscalYearRolloverCollection.class, requestContext);
    }

    public Future<List<LedgerFiscalYearRollover>> getLedgerFyRollovers(String fiscalYeaId, List<String> ledgerIds, RequestContext requestContext) {
        String query = String.format("rolloverType==%s AND toFiscalYearId==%s AND %s",
          COMMIT, fiscalYeaId, convertIdsToCqlQuery(ledgerIds, "ledgerId"));
        RequestEntry requestEntry = new RequestEntry(LEDGER_ROLLOVERS_ENDPOINT)
                .withQuery(query).withOffset(0).withLimit(ledgerIds.size());
        return restClient.get(requestEntry, LedgerFiscalYearRolloverCollection.class, requestContext)
                .map(LedgerFiscalYearRolloverCollection::getLedgerFiscalYearRollovers);
    }

}
