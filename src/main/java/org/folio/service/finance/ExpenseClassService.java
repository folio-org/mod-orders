package org.folio.service.finance;

import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.ExpenseClassCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class ExpenseClassService {

    private static final String ENDPOINT = "/finance-storage/budget-expense-classes";
    private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

    private final RestClient restClient;

    public ExpenseClassService(RestClient restClient) {
        this.restClient = restClient;
    }

    public CompletableFuture<ExpenseClassCollection> getExpenseClasses(String query, int offset, int limit, RequestContext requestContext) {
        RequestEntry requestEntry = new RequestEntry(resourcesPath(ENDPOINT))
                .withQuery(query)
                .withLimit(limit)
                .withOffset(offset);
        return restClient.get(requestEntry, requestContext, ExpenseClassCollection.class);
    }
}
