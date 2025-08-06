package org.folio.service.finance;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.FUNDS_NOT_FOUND;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.CompositeFund;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

import io.vertx.core.Future;

public class FundService {

  private static final String ENDPOINT = "/finance/funds";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private static final String FUNDS_BY_LEDGER_ID_QUERY = "ledgerId==%s";
  private final RestClient restClient;

  public FundService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<List<Fund>> getAllFunds(Collection<String> fundIds, RequestContext requestContext) {
    Set<String> uniqueFundIds = new LinkedHashSet<>(fundIds);
    return collectResultsOnSuccess(
        ofSubLists(new ArrayList<>(uniqueFundIds), MAX_IDS_FOR_GET_RQ_15).map(ids-> getAllFundsByIds(ids, requestContext))
          .toList()).map(
              lists -> lists.stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toList()));
  }

  public Future<Fund> retrieveFundById(String fundId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(fundId);
    return restClient.get(requestEntry, CompositeFund.class, requestContext)
      .map(CompositeFund::getFund)
      .recover(cause -> {
        if (HelperUtils.isNotFound(cause)) {
          List<Parameter> parameters = Collections.singletonList(new Parameter().withValue(fundId).withKey("funds"));
          throw new HttpException(404, FUNDS_NOT_FOUND.toError().withParameters(parameters));
        }
        throw new CompletionException(cause);
      });
  }

  public Future<List<Fund>> getFundsByLedgerId(String ledgerId, RequestContext requestContext) {
    String query = String.format(FUNDS_BY_LEDGER_ID_QUERY, ledgerId);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withLimit(Integer.MAX_VALUE)
      .withOffset(0);
    return restClient.get(requestEntry, FundCollection.class, requestContext)
      .map(FundCollection::getFunds);
  }

  private Future<List<Fund>> getAllFundsByIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withLimit(MAX_IDS_FOR_GET_RQ_15)
      .withOffset(0);
    return restClient.get(requestEntry, FundCollection.class, requestContext)
      .map(FundCollection::getFunds)
      .map(funds -> {
        if (funds.size() == ids.size()) {
          return funds;
        }
        List<Parameter> parameters = ids.stream()
          .filter(id -> funds.stream().noneMatch(fund -> fund.getId().equals(id)))
          .map(id -> new Parameter().withValue(id).withKey("funds"))
          .collect(Collectors.toList());
        throw new HttpException(404, FUNDS_NOT_FOUND.toError().withParameters(parameters));
      });
  }
}
