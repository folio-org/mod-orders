package org.folio.service.finance.expenceclass;

import org.folio.rest.acq.model.finance.BudgetExpenseClassCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class BudgetExpenseClassService {

    private static final String ENDPOINT = "/finance-storage/budget-expense-classes";

    private final RestClient restClient;

    public BudgetExpenseClassService(RestClient restClient) {
        this.restClient = restClient;
    }

    public CompletableFuture<BudgetExpenseClassCollection> getBudgetExpenseClasses(String query, int offset, int limit, RequestContext requestContext) {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT)
                .withQuery(query)
                .withLimit(limit)
                .withOffset(offset);
        return restClient.get(requestEntry, requestContext, BudgetExpenseClassCollection.class);
    }
}
