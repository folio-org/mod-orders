package org.folio.service.orders;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.OrderInvoiceRelationship;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.services.invoice.InvoiceLineService;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.ErrorCodes.ORDER_RELATES_TO_INVOICE;

public class OrderInvoiceRelationService {

  private static final Logger logger = LogManager.getLogger();
  private static final String ENDPOINT = "/orders-storage/order-invoice-relns";

  private final RestClient restClient;

  @Autowired
  private InvoiceLineService invoiceLineService;

  public OrderInvoiceRelationService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<OrderInvoiceRelationshipCollection> getOrderInvoiceRelationshipCollection(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query).withOffset(offset).withLimit(limit);
    return restClient.get(requestEntry, requestContext, OrderInvoiceRelationshipCollection.class);
  }

  public CompletableFuture<Void> checkOrderInvoiceRelationship(String id, RequestContext requestContext) {
    String query = "purchaseOrderId==" + id;

    return getOrderInvoiceRelationshipCollection(query, 0, 0, requestContext)
      .thenApply(oirs -> {
        if (oirs.getTotalRecords() > 0) {
          logger.error("Order or order line {} is linked to the invoice and can not be deleted", id);
          throw new HttpException(400, ORDER_RELATES_TO_INVOICE);
        }
        return null;
      });
  }

  public CompletableFuture<Void> checkOrderPOLineLinkedToInvoiceLine(PoLine line, RequestContext requestContext) {
    String query = "purchaseOrderId==" + line.getPurchaseOrderId();
    return getOrderInvoiceRelationshipCollection(query, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(oirsc -> retrieveInvoiceIdList(oirsc.getOrderInvoiceRelationships()))
      .thenCompose(invoiceIds -> invoiceLineService.getInvoiceLinesByIds(invoiceIds, requestContext))
      .thenAccept(invoiceLineList -> {
        boolean notAllowedDeletePOLine = invoiceLineList.stream()
          .filter(invoiceLine -> invoiceLine.getPoLineId() != null)
          .anyMatch(invoiceLine -> invoiceLine.getPoLineId().equals(line.getId()));
        if (notAllowedDeletePOLine) {
          logger.error("Order or order line {} is linked to the invoice and can not be deleted", line.getId());
          throw new HttpException(400, ORDER_RELATES_TO_INVOICE);
        }
      });
  }

  private List<String> retrieveInvoiceIdList(List<OrderInvoiceRelationship> orderInvoiceRelationshipList) {
    List<String> invoiceIdList = new ArrayList();
    orderInvoiceRelationshipList.forEach(orderInvoiceRelationship -> invoiceIdList.add(orderInvoiceRelationship.getInvoiceId()));
    return invoiceIdList;
  }
}
