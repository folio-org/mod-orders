package org.folio.service.invoice;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public class InvoiceLineService {

  private static final String INVOICE_LINES_ENDPOINT = "/invoice/invoice-lines";
  private static final String GET_INVOICE_LINE_BY_PO_LINE_ID_QUERY =  "poLineId==%s";
  private static final Logger logger = LogManager.getLogger();
  private final RestClient restClient;

  public InvoiceLineService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<InvoiceLineCollection> getInvoiceLines(RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.get(requestEntry, requestContext, InvoiceLineCollection.class);
  }

  public CompletableFuture<List<InvoiceLine>> getInvoiceLinesByOrderLineId(String orderPoLineId, RequestContext requestContext) {
     String query = String.format(GET_INVOICE_LINE_BY_PO_LINE_ID_QUERY, orderPoLineId);
    RequestEntry requestEntry = new RequestEntry(INVOICE_LINES_ENDPOINT)
      .withQuery(query)
      .withLimit(Integer.MAX_VALUE)
      .withOffset(0);
     return getInvoiceLines(requestEntry, requestContext)
       .thenApply(InvoiceLineCollection::getInvoiceLines);
  }

}
