package org.folio.service.invoice;

import one.util.streamex.StreamEx;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static java.util.stream.Collectors.toList;
import static org.folio.helper.AbstractHelper.MAX_IDS_FOR_GET_RQ;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.HelperUtils.convertFieldListToCqlQuery;

public class InvoiceLineService {

  private static final String INVOICE_LINES_ENDPOINT = "/invoice/invoice-lines";
  private static final String INVOICE_LINE_BY_PO_LINE_ID_QUERY =  "poLineId==%s";
  private static final Logger logger = LogManager.getLogger();
  private final RestClient restClient;

  public InvoiceLineService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<InvoiceLineCollection> getInvoiceLines(String query, int offset, int limit, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVOICE_LINES_ENDPOINT)
      .withQuery(query)
      .withLimit(Integer.MAX_VALUE)
      .withOffset(0);
    return restClient.get(requestEntry, requestContext, InvoiceLineCollection.class);
  }

  public CompletableFuture<List<InvoiceLine>> getInvoiceLinesByIds(List<String> invoiceIds, RequestContext requestContext) {
    List<CompletableFuture<List<InvoiceLine>>> futures = StreamEx
      .ofSubLists(invoiceIds, MAX_IDS_FOR_GET_RQ)
      .map(ids -> getInvoiceLinesChunk(ids, requestContext))
      .collect(toList());
    return collectResultsOnSuccess(futures)
      .thenApply(listList -> listList.stream().flatMap(Collection::stream).distinct().collect(toList()));
  }

  private CompletableFuture<List<InvoiceLine>> getInvoiceLinesChunk(List<String> ids, RequestContext requestContext) {
    String query = convertFieldListToCqlQuery(ids, "invoiceId", true);
    return getInvoiceLines(query, 0, Integer.MAX_VALUE, requestContext)
      .thenApply(InvoiceLineCollection::getInvoiceLines);
  }

  public CompletableFuture<List<InvoiceLine>> getInvoiceLinesByOrderLineId(String orderPoLineId, RequestContext requestContext) {
    String query = String.format(INVOICE_LINE_BY_PO_LINE_ID_QUERY, orderPoLineId);
    RequestEntry requestEntry = new RequestEntry(INVOICE_LINES_ENDPOINT)
      .withQuery(query)
      .withLimit(Integer.MAX_VALUE)
      .withOffset(0);
    return restClient.get(requestEntry, requestContext, InvoiceLineCollection.class)
      .thenApply(InvoiceLineCollection::getInvoiceLines);
  }

}
