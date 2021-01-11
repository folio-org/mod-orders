package org.folio.service.orders;

import static org.folio.orders.utils.ErrorCodes.ORDER_RELATES_TO_INVOICE;

import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;

public class OrderInvoiceRelationService {

  private static final Logger logger = LogManager.getLogger();

  private final RestClient restClient;

  public OrderInvoiceRelationService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<OrderInvoiceRelationshipCollection> getOrderInvoiceRelationshipCollection(String query, int offset, int limit, RequestContext requestContext) {
    return restClient.get(query, offset, limit, requestContext, OrderInvoiceRelationshipCollection.class);
  }

  public CompletableFuture<Void> checkOrderInvoiceRelationship(String id, RequestContext requestContext) {
    String query = "purchaseOrderId==" + id;

    return getOrderInvoiceRelationshipCollection(query, 0,0, requestContext)
      .thenApply(oirs -> {
        if (oirs.getTotalRecords() > 0) {
          logger.error("Order {} is linked to invoice and can not be deleted", id);
          throw new HttpException(400, ORDER_RELATES_TO_INVOICE);
        }
        return null;
      });
  }
}
