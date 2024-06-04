package org.folio.service.finance.budget;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.BudgetCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;

public class BudgetService {
  private static final Logger logger = LogManager.getLogger();

  private static final String BUDGETS_ENDPOINT = "/finance/budgets";

  private final RestClient restClient;

  public BudgetService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<List<Budget>> getBudgetsByQuery(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BUDGETS_ENDPOINT)
      .withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, BudgetCollection.class, requestContext)
      .map(BudgetCollection::getBudgets)
      .onSuccess(budgets -> logger.info("getBudgetsByQuery :: Successfully retrieved budgets"))
      .onFailure(t -> logger.error("getBudgetsByQuery :: Failed to retrieve budgets", t));
  }
}
