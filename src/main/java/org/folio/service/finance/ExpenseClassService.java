package org.folio.service.finance;

import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.acq.model.finance.ExpenseClassCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

public class ExpenseClassService {

    private static final String ENDPOINT = "/finance/expense-classes";


    private final RestClient restClient;

    public ExpenseClassService(RestClient restClient) {
        this.restClient = restClient;
    }

    public CompletableFuture<ExpenseClassCollection> getExpenseClasses(String query, int offset, int limit, RequestContext requestContext) {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT)
                .withQuery(query)
                .withLimit(limit)
                .withOffset(offset);
        return restClient.get(requestEntry, requestContext, ExpenseClassCollection.class);
    }
}
