package org.folio.service.orders;

import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_RELATES_TO_INVOICE;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.invoice.InvoiceLineService;

import io.vertx.core.Future;

public class OrderInvoiceRelationService {

  private static final Logger logger = LogManager.getLogger();
  private static final String ENDPOINT = "/orders-storage/order-invoice-relns";

  private final RestClient restClient;

  private final InvoiceLineService invoiceLineService;

  public OrderInvoiceRelationService(RestClient restClient, InvoiceLineService invoiceLineService) {
    this.restClient = restClient;
    this.invoiceLineService = invoiceLineService;
  }

  public Future<OrderInvoiceRelationshipCollection> getOrderInvoiceRelationshipCollection(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, OrderInvoiceRelationshipCollection.class, requestContext);
  }

  public Future<Boolean> isOrderLinkedToAnInvoice(String orderId, RequestContext requestContext) {
    String query = "purchaseOrderId==" + orderId;
    return getOrderInvoiceRelationshipCollection(query, 0, 0, requestContext)
      .map(oirs -> oirs.getTotalRecords() > 0);
  }

  public Future<Void> checkOrderInvoiceRelationship(String id, RequestContext requestContext) {
    String query = "purchaseOrderId==" + id;

    return getOrderInvoiceRelationshipCollection(query, 0, 0, requestContext)
      .map(oirs -> {
        if (oirs.getTotalRecords() > 0) {
          logger.error("Order or order line {} is linked to the invoice and can not be deleted", id);
          throw new HttpException(400, ORDER_RELATES_TO_INVOICE);
        }
        return null;
      });
  }

  public Future<Void> checkOrderPOLineLinkedToInvoiceLine(PoLine line, RequestContext requestContext) {
    String query = "purchaseOrderId==" + line.getPurchaseOrderId();

    return getOrderInvoiceRelationshipCollection(query, 0, 0, requestContext)
      .compose(oirs -> {
        if (oirs.getTotalRecords() > 0) {
          return invoiceLineService.getInvoiceLinesByOrderLineId(line.getId(), requestContext)
            .map(invoiceLines -> {
                boolean notAllowedDeletePOLine = invoiceLines.stream()
                  .filter(invoiceLine -> invoiceLine.getPoLineId() != null)
                  .anyMatch(invoiceLine -> invoiceLine.getPoLineId().equals(line.getId()));
                if (notAllowedDeletePOLine) {
                  logger.error("Order or order line {} is linked to the invoice and can not be deleted", line.getId());
                  throw new HttpException(400, ORDER_RELATES_TO_INVOICE);
                }
                return null;
              }
            );
        }
        return Future.succeededFuture();
      });
  }
}
