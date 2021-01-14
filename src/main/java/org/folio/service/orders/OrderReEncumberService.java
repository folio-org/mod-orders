package org.folio.service.orders;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.RolloverErrorService;
import org.folio.service.finance.RolloverRetrieveService;

import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.toList;

public class OrderReEncumberService {

    private final CompositePurchaseOrderService compositePurchaseOrderService;
    private final FundService fundService;
    private final RolloverErrorService rolloverErrorService;
    private final RolloverRetrieveService rolloverRetrieveService;
    private final FiscalYearService fiscalYearService;


    public OrderReEncumberService(CompositePurchaseOrderService compositePurchaseOrderService, FundService fundService, RolloverErrorService rolloverErrorService, RolloverRetrieveService rolloverRetrieveService, FiscalYearService fiscalYearService) {
        this.compositePurchaseOrderService = compositePurchaseOrderService;
        this.fundService = fundService;
        this.rolloverErrorService = rolloverErrorService;
        this.rolloverRetrieveService = rolloverRetrieveService;
        this.fiscalYearService = fiscalYearService;
    }

    public CompletableFuture<Void> reEncumber(String orderId, RequestContext requestContext) {
        return compositePurchaseOrderService.getCompositeOrderById(orderId, requestContext)
                .thenAccept(compositePurchaseOrder -> compositePurchaseOrder.getAcqUnitIds());
    }


    public CompletableFuture<CompositePurchaseOrder> populateNeedReEncumberFlag(CompositePurchaseOrder compPO, RequestContext requestContext) {
        try {
            var fundIds = compPO.getCompositePoLines()
                    .stream()
                    .flatMap(op -> op.getFundDistribution().stream())
                    .map(FundDistribution::getFundId)
                    .collect(toList());

            if (fundIds.isEmpty()) {
                compPO.setNeedReEncumber(false);
                return CompletableFuture.completedFuture(compPO);
            }

            return fundService.getFunds(fundIds, requestContext)
                    .thenCompose(funds -> {
                        if (funds.isEmpty()) {
                            compPO.setNeedReEncumber(false);
                            return CompletableFuture.completedFuture(compPO);
                        }
                        return fiscalYearService.getCurrentFiscalYear(funds.get(0).getLedgerId(), requestContext)
                                .thenCompose(currentFY -> rolloverRetrieveService.getLedgerFyRollovers(currentFY.getId(), funds.get(0).getLedgerId(), requestContext))
                                .thenCompose(ledgerFyRollovers -> {
                                    if (ledgerFyRollovers.getLedgerFiscalYearRollovers().isEmpty()) {
                                        compPO.setNeedReEncumber(false);
                                        return CompletableFuture.completedFuture(compPO);
                                    } else {
                                        return rolloverErrorService.getLedgerFyRolloverErrors(compPO.getId(), ledgerFyRollovers.getLedgerFiscalYearRollovers().get(0).getId(), requestContext)
                                                .thenApply(ledgerFyRolloverErrors -> compPO
                                                        .withNeedReEncumber(!ledgerFyRolloverErrors.getLedgerFiscalYearRolloverErrors().isEmpty()));
                                    }
                                });
                    });

        } catch (Exception e) {
          //  logger.error(compPO, e);
            return CompletableFuture.failedFuture(e);
        }
    }
}
