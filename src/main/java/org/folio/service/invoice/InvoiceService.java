package org.folio.service.invoice;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertIdsToCqlQuery;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.rest.acq.model.OrderInvoiceRelationship;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.orders.OrderInvoiceRelationService;

public class InvoiceService {
  private static final String INVOICE_ENDPOINT = "/invoice/invoices";
  private static final String GET_ORDER_INVOICE_REL_BY_PO_ID_RQ = "purchaseOrderId==%s";

  private final RestClient restClient;
  private final OrderInvoiceRelationService orderInvoiceRelationService;

  public InvoiceService(RestClient restClient, OrderInvoiceRelationService orderInvoiceRelationService) {
    this.restClient = restClient;
    this.orderInvoiceRelationService = orderInvoiceRelationService;
  }

  public CompletableFuture<List<Invoice>> getInvoicesByOrderId(String orderId, RequestContext requestContext) {
    String query = String.format(GET_ORDER_INVOICE_REL_BY_PO_ID_RQ, orderId);
    return orderInvoiceRelationService.getOrderInvoiceRelationshipCollection(query, 0, Integer.MAX_VALUE, requestContext)
                  .thenApply(OrderInvoiceRelationshipCollection::getOrderInvoiceRelationships)
                  .thenCompose(orderInvoiceRel -> {
                    if (CollectionUtils.isNotEmpty(orderInvoiceRel)) {
                      List<String> invoiceIds = orderInvoiceRel.stream().map(OrderInvoiceRelationship::getInvoiceId).collect(Collectors.toList());
                      return getInvoicesByIds(invoiceIds, requestContext);
                    }
                    return CompletableFuture.completedFuture(Collections.emptyList());
                  });
  }

  public CompletableFuture<List<Invoice>> getInvoicesByIds(Collection<String> invoiceIds, RequestContext requestContext) {
    return collectResultsOnSuccess(
      ofSubLists(new ArrayList<>(invoiceIds), MAX_IDS_FOR_GET_RQ)
                  .map(ids -> getInvoicesChunkByInvoiceIds(ids, requestContext)).toList())
      .thenApply(invoiceCollections -> invoiceCollections.stream()
                                            .flatMap(invoiceCol -> invoiceCol.getInvoices().stream())
                                            .collect(Collectors.toList()));
  }

  private CompletableFuture<InvoiceCollection> getInvoicesChunkByInvoiceIds(Collection<String> invoiceIds, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(invoiceIds);
    RequestEntry requestEntry = new RequestEntry(INVOICE_ENDPOINT).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, requestContext, InvoiceCollection.class);
  }
}
