package org.folio.service.finance;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.rest.core.exceptions.ErrorCodes.FUNDS_NOT_FOUND;
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

import org.folio.rest.core.exceptions.HttpException;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.CompositeFund;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

public class FundService {

  private static final String ENDPOINT = "/finance/funds";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private static final String FUNDS_BY_LEDGER_ID_QUERY = "ledgerId==%s";
  private final RestClient restClient;

  public FundService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<List<Fund>> getAllFunds(Collection<String> fundIds, RequestContext requestContext) {
    return collectResultsOnSuccess(
        ofSubLists(new ArrayList<>(fundIds), MAX_IDS_FOR_GET_RQ).map(ids -> getAllFundsByIds(ids, requestContext))
          .toList()).thenApply(
              lists -> lists.stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toList()));
  }

  public CompletableFuture<List<Fund>> getFunds(Collection<String> fundIds, RequestContext requestContext) {
    return collectResultsOnSuccess(
        ofSubLists(new ArrayList<>(fundIds), MAX_IDS_FOR_GET_RQ).map(ids -> getFundsByIds(ids, requestContext))
          .toList()).thenApply(
              lists -> lists.stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toList()));
  }

  private CompletableFuture<List<Fund>> getFundsByIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withLimit(MAX_IDS_FOR_GET_RQ)
      .withOffset(0);
    return restClient.get(requestEntry, requestContext, FundCollection.class)
      .thenApply(FundCollection::getFunds);
  }

  public CompletableFuture<Fund> retrieveFundById(String fundId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(fundId);
    return restClient.get(requestEntry, requestContext, CompositeFund.class)
      .thenApply(CompositeFund::getFund)
      .exceptionally(t -> {
        Throwable cause = t.getCause() == null ? t : t.getCause();
        if (HelperUtils.isNotFound(cause)) {
          List<Parameter> parameters = Collections.singletonList(new Parameter().withValue(fundId)
            .withKey("funds"));
          throw new HttpException(404, FUNDS_NOT_FOUND.toError()
            .withParameters(parameters));
        }
        throw new CompletionException(cause);
      });
  }

  public CompletableFuture<List<Fund>> getFundsByLedgerId(String ledgerId, RequestContext requestContext) {
    String query = String.format(FUNDS_BY_LEDGER_ID_QUERY, ledgerId);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withLimit(Integer.MAX_VALUE)
      .withOffset(0);
    return restClient.get(requestEntry, requestContext, FundCollection.class)
      .thenApply(FundCollection::getFunds);
  }

  private CompletableFuture<List<Fund>> getAllFundsByIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withLimit(MAX_IDS_FOR_GET_RQ)
      .withOffset(0);
    return restClient.get(requestEntry, requestContext, FundCollection.class)
      .thenApply(FundCollection::getFunds)
      .thenApply(funds -> {
        if (funds.size() == ids.size()) {
          return funds;
        }
        List<Parameter> parameters = ids.stream()
          .filter(id -> funds.stream()
            .noneMatch(fund -> fund.getId()
              .equals(id)))
          .map(id -> new Parameter().withValue(id)
            .withKey("funds"))
          .collect(Collectors.toList());
        throw new HttpException(404, FUNDS_NOT_FOUND.toError()
          .withParameters(parameters));
      });
  }
}
