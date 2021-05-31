package org.folio.service.invoice;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.folio.HttpStatus;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;

public class InvoiceService {

  private static final String INVOICE_LINES_ENDPOINT = "/invoice/invoice-lines";
  private static final String GET_INVOICE_LINES_ERROR = "Error when retrieving invoice lines";
  private static final Logger logger = LogManager.getLogger();
  private final RestClient restClient;

  public InvoiceService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<List<InvoiceLine>> retrieveInvoiceLines(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVOICE_LINES_ENDPOINT).withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, requestContext, InvoiceLineCollection.class)
      .thenApply(InvoiceLineCollection::getInvoiceLines)
      .exceptionally(t -> {
        logger.error(GET_INVOICE_LINES_ERROR + ". Query: " + query, t);
        throw new HttpException(t.getCause() instanceof HttpException ? ((HttpException)t.getCause()).getCode() :
          HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), GET_INVOICE_LINES_ERROR);
      });
  }
}
