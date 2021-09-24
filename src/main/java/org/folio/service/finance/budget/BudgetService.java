package org.folio.service.finance.budget;

import static java.util.stream.Collectors.toList;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

import one.util.streamex.StreamEx;

public class BudgetService {

  private static final String ENDPOINT = "/finance/funds/{id}/budget";

  private final RestClient restClient;

  public BudgetService(RestClient restClient) {
    this.restClient = restClient;
  }

  private CompletableFuture<List<List<Budget>>> getBudgetsByChunks(Collection<String> fundIds, RequestContext requestContext) {
    return collectResultsOnSuccess(StreamEx.ofSubLists(new ArrayList<>(fundIds), MAX_IDS_FOR_GET_RQ)
      .map(fIds -> fetchBudgetsByFundIds(fIds, requestContext))
      .toList());
  }

  public CompletableFuture<List<Budget>> fetchBudgetsByFundIds(List<String> fundIds, RequestContext requestContext) {
    List<CompletableFuture<Budget>> futureList = fundIds.stream()
      .distinct()
      .map(fundId -> getActiveBudgetByFundId(fundId, requestContext))
      .collect(toList());

    return CompletableFuture.allOf(futureList.toArray(new CompletableFuture[0]))
      .thenApply(v -> futureList.stream()
        .map(CompletableFuture::join)
        .collect(Collectors.toList()));
  }

  public CompletableFuture<List<Budget>> getBudgets(Collection<String> fundIds, RequestContext requestContext) {
    return getBudgetsByChunks(fundIds, requestContext).thenApply(lists -> lists.stream()
      .flatMap(Collection::stream)
      .collect(Collectors.toList()));
  }

  public CompletableFuture<Budget> getActiveBudgetByFundId(String fundId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withId(fundId).withQueryParameter("status", "Active");
    return restClient.get(requestEntry, requestContext, Budget.class)
      .exceptionally(t -> {
        Throwable cause = Objects.nonNull(t.getCause()) ? t.getCause() : t;
        if (cause instanceof HttpException) {
          throw new HttpException(404, BUDGET_NOT_FOUND_FOR_TRANSACTION.toError()
            .withParameters(Collections.singletonList(new Parameter().withKey("fund")
              .withValue(fundId))));
        }
        throw new CompletionException(cause);
      });
  }
}
