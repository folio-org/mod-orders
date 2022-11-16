package org.folio.service.finance.expenceclass;

import org.folio.rest.acq.model.finance.ExpenseClassCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;

public class ExpenseClassService {

    private static final String ENDPOINT = "/finance/expense-classes";


    private final RestClient restClient;

    public ExpenseClassService(RestClient restClient) {
        this.restClient = restClient;
    }

    public Future<ExpenseClassCollection> getExpenseClasses(String query, int offset, int limit, RequestContext requestContext) {
        RequestEntry requestEntry = new RequestEntry(ENDPOINT)
                .withQuery(query)
                .withLimit(limit)
                .withOffset(offset);
        return restClient.get(requestEntry, ExpenseClassCollection.class, requestContext);
    }
}
