package org.folio.service.finance;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.ErrorCodes.FUNDS_NOT_FOUND;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.CompositeFund;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Parameter;

public class FundService {
  private static final String FUNDS_BY_LEDGER_ID_QUERY = "ledgerId==%s";
  private final RestClient fundRestClient;

  public FundService(RestClient fundRestClient) {
    this.fundRestClient = fundRestClient;
  }

  public CompletableFuture<List<Fund>> getFunds(Collection<String> fundIds, RequestContext requestContext) {
    return collectResultsOnSuccess(ofSubLists(new ArrayList<>(fundIds), MAX_IDS_FOR_GET_RQ)
      .map(ids -> getFundsByIds(ids, requestContext))
      .toList())
      .thenApply(lists -> lists.stream().flatMap(Collection::stream)
      .collect(Collectors.toList()));
  }

  public CompletableFuture<Fund> retrieveFundById(String fundId, RequestContext requestContext) {
    return fundRestClient.getById(fundId, requestContext, CompositeFund.class)
      .thenApply(CompositeFund::getFund)
      .exceptionally(t -> {
        Throwable cause = t.getCause() == null ? t : t.getCause();
        if (HelperUtils.isNotFound(cause)) {
          List<Parameter> parameters = Collections.singletonList(new Parameter().withValue(fundId).withKey("funds"));
          throw new HttpException(404, FUNDS_NOT_FOUND.toError().withParameters(parameters));
        }
        throw new CompletionException(cause);
      });
  }

  public CompletableFuture<List<Fund>> getFundsByLedgerId(String ledgerId, RequestContext requestContext) {
    String query = String.format(FUNDS_BY_LEDGER_ID_QUERY, ledgerId);
    return fundRestClient.get(query, 0, Integer.MAX_VALUE, requestContext, FundCollection.class)
                         .thenApply(FundCollection::getFunds);
  }

  private CompletableFuture<List<Fund>> getFundsByIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    return fundRestClient.get(query, 0, MAX_IDS_FOR_GET_RQ, requestContext, FundCollection.class)
                         .thenApply(FundCollection::getFunds);
  }
}
