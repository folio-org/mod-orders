package org.folio.service.orders;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.convertToCompositePurchaseOrder;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.PoLine;

import io.vertx.core.json.JsonObject;
import org.folio.rest.jaxrs.model.ReportingCode;

public class CompositePurchaseOrderService {

    private final PurchaseOrderService purchaseOrderService;
    private final PurchaseOrderLineService purchaseOrderLineService;

    public CompositePurchaseOrderService(PurchaseOrderService purchaseOrderService, PurchaseOrderLineService purchaseOrderLineService) {
        this.purchaseOrderService = purchaseOrderService;
        this.purchaseOrderLineService = purchaseOrderLineService;
    }

    public CompletableFuture<CompositePurchaseOrder> getCompositeOrderById(String orderId, RequestContext requestContext) {
        return purchaseOrderService.getPurchaseOrderById(orderId, requestContext)
                .thenCompose(purchaseOrder -> purchaseOrderLineService.getOrderLines("purchaseOrderId==" + orderId, 0, Integer.MAX_VALUE, requestContext)
                    .thenApply(poLines -> poLines.stream().map(this::toCompositePoLine).collect(toList()))
                    .thenApply(poLines -> convertToCompositePurchaseOrder(JsonObject.mapFrom(purchaseOrder)).withCompositePoLines(poLines)));
    }

    private CompositePoLine toCompositePoLine(PoLine poLine) {
        List<Alert> alerts = poLine.getAlerts().stream().map(alertId -> new Alert().withId(alertId)).collect(toList());
        List<ReportingCode> reportingCodes = poLine.getReportingCodes().stream().map(codeId -> new ReportingCode().withId(codeId)).collect(toList());
        JsonObject jsonLine = JsonObject.mapFrom(poLine);
        jsonLine.remove(ALERTS);
        jsonLine.remove(REPORTING_CODES);
        return jsonLine.mapTo(CompositePoLine.class).withAlerts(alerts).withReportingCodes(reportingCodes);
    }
}
