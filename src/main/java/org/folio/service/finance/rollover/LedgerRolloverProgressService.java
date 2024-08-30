package org.folio.service.finance.rollover;

import io.vertx.core.Future;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgress;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverProgressCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import java.util.List;

public class LedgerRolloverProgressService {

  private static final String ENDPOINT = "/finance/ledger-rollovers-progress";

  private static final String ENDPOINT_BY_ID = ENDPOINT + "/{id}";

  private final RestClient restClient;

  public LedgerRolloverProgressService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<List<LedgerFiscalYearRolloverProgress>> getRolloversProgress(String rolloverId, RequestContext requestContext) {
    String query = "ledgerRolloverId==" + rolloverId;
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(0).withLimit(1);
    return restClient.get(requestEntry, LedgerFiscalYearRolloverProgressCollection.class, requestContext)
      .map(LedgerFiscalYearRolloverProgressCollection::getLedgerFiscalYearRolloverProgresses);
  }

  public Future<LedgerFiscalYearRolloverProgress> getRolloversProgressByRolloverId(String rolloverId, RequestContext requestContext) {
    return getRolloversProgress(rolloverId, requestContext)
      .map(progresses -> progresses.stream().findFirst()
        .orElseThrow(() -> new HttpException(404, "Can't retrieve rollover progress by rolloverId")));
  }

  public Future<Void> updateRolloverProgress(LedgerFiscalYearRolloverProgress ledgerFiscalYearRolloverProgress, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT_BY_ID).withId(ledgerFiscalYearRolloverProgress.getId());
    return restClient.put(requestEntry, ledgerFiscalYearRolloverProgress, requestContext);
  }

}
