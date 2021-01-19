package org.folio.service.finance;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.ID;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.LedgerCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

public class LedgerService {

  private static final String ENDPOINT = "/finance/ledgers";

  private final RestClient restClient;

  public LedgerService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<List<Ledger>> getLedgersByIds(Collection<String> ledgerIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ledgerIds, ID);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
              .withLimit(MAX_IDS_FOR_GET_RQ).withOffset(0);
    return restClient.get(requestEntry, requestContext, LedgerCollection.class)
      .thenApply(ledgerCollection -> {
        if (ledgerIds.size() == ledgerCollection.getLedgers()
          .size()) {
          return ledgerCollection.getLedgers();
        }
        String missingIds = String.join(", ", CollectionUtils.subtract(ledgerIds, ledgerCollection.getLedgers()
          .stream()
          .map(Ledger::getId)
          .collect(toList())));
        throw new HttpException(404, LEDGER_NOT_FOUND_FOR_TRANSACTION.toError()
          .withParameters(Collections.singletonList(new Parameter().withKey("ledgers")
            .withValue(missingIds))));
      });
  }
}
