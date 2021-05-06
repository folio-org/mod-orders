package org.folio.service.orders;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Alert;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.ReportingCode;

import io.vertx.core.json.JsonObject;

public class PurchaseOrderLineService {
  private static final Logger logger = LogManager.getLogger(PurchaseOrderLineService.class);
  private static final String ENDPOINT = "/orders-storage/po-lines";
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";

  private final RestClient restClient;

  public PurchaseOrderLineService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<List<PoLine>> getOrderLines(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, requestContext, PoLineCollection.class)
      .thenApply(PoLineCollection::getPoLines);
  }

  public CompletableFuture<PoLine> getOrderLineById(String orderLineId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(orderLineId);
    return restClient.get(requestEntry, requestContext, PoLine.class);
  }

  public CompletableFuture<Void> updateOrderLine(PoLine poLine, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(poLine.getId());
    return restClient.put(requestEntry, poLine, requestContext);
  }

  public CompletableFuture<Void> updateOrderLines(List<PoLine> orderLines, RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), orderLines.stream()
      .map(poLine -> updateOrderLine(poLine, requestContext)
                    .exceptionally(t -> {
                      throw new HttpException(400, ErrorCodes.POL_LINES_LIMIT_EXCEEDED.toError());
                    }))
      .toArray(CompletableFuture[]::new));
  }

  public CompletableFuture<List<CompositePoLine>> getCompositePoLinesByOrderId(String orderId, RequestContext requestContext) {
    return getOrderLines("purchaseOrderId==" + orderId, 0, Integer.MAX_VALUE, requestContext)
                .thenApply(poLines -> poLines.stream().map(this::toCompositePoLine).collect(toList()));
  }

  public CompletableFuture<CompositePoLine> getCompositePoLinesById(String lineId, RequestContext requestContext) {
    return getOrderLineById(lineId, requestContext).thenApply(this::toCompositePoLine);
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

