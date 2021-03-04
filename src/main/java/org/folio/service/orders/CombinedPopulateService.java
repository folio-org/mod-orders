package org.folio.service.orders;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.FiscalYearService;

import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

public class CombinedPopulateService implements CompositeOrderDynamicDataPopulateService {

    private final FiscalYearService fiscalYearService;
    private final Set<CompositeOrderDynamicDataPopulateService> populateServices;

    public CombinedPopulateService(FiscalYearService fiscalYearService, Set<CompositeOrderDynamicDataPopulateService> populateServices) {
        this.fiscalYearService = fiscalYearService;
        this.populateServices = populateServices;
    }

    @Override
    public CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
        return holder.getOrder().getCompositePoLines().stream().flatMap(poLine -> poLine.getFundDistribution().stream())
                .map(FundDistribution::getFundId).findFirst()
                .map(fundId -> fiscalYearService.getCurrentFiscalYearByFundId(fundId, requestContext)
                    .thenApply(holder::withFiscalYear)
                    .exceptionally(t -> {
                        Throwable cause = Objects.nonNull(t.getCause()) ? t.getCause() : t;
                        if (cause instanceof HttpException && ((HttpException) cause).getCode() == 404) {
                          return holder;
                        }
                        throw new CompletionException(cause);
                    }))
                .orElseGet(() -> CompletableFuture.completedFuture(holder))
                .thenCompose(fiscalYear -> populateAllDynamicData(holder, requestContext));
    }

    private CompletableFuture<CompositeOrderRetrieveHolder> populateAllDynamicData(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
        return CompletableFuture.allOf(populateServices.stream().map(service -> service.populate(holder, requestContext)).toArray(CompletableFuture[]::new))
                .thenApply(aVoid -> holder);
    }
}
