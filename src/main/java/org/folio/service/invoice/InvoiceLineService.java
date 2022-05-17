package org.folio.service.invoice;

import static java.lang.String.format;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.folio.completablefuture.FolioVertxCompletableFuture.allOf;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.rest.acq.model.invoice.Adjustment;
import org.folio.rest.acq.model.invoice.FundDistribution;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import one.util.streamex.StreamEx;

public class InvoiceLineService {

  private static final String INVOICE_LINES_ENDPOINT = "/invoice/invoice-lines";
  private static final String INVOICE_LINE_BY_ID_ENDPOINT = INVOICE_LINES_ENDPOINT + "/{id}";
  private static final String GET_INVOICE_LINES_ERROR = "Error when retrieving invoice lines";
  private static final String GET_INVOICE_LINE_BY_PO_LINE_ID_QUERY =  "poLineId==%s";
  private static final Logger logger = LogManager.getLogger();

  private final RestClient restClient;

  public InvoiceLineService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<List<InvoiceLine>> getInvoiceLines(RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.get(requestEntry, requestContext, InvoiceLineCollection.class)
     .thenApply(InvoiceLineCollection::getInvoiceLines);
  }

  public CompletableFuture<List<InvoiceLine>> retrieveInvoiceLines(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVOICE_LINES_ENDPOINT).withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return getInvoiceLines(requestEntry, requestContext)
      .exceptionally(t -> {
        logger.error(GET_INVOICE_LINES_ERROR + ". Query: " + query, t);
        throw new HttpException(t.getCause() instanceof HttpException ? ((HttpException)t.getCause()).getCode() :
          HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), GET_INVOICE_LINES_ERROR);
      });
  }

  public CompletableFuture<List<InvoiceLine>> getInvoiceLinesByOrderLineId(String orderPoLineId,
      RequestContext requestContext) {
     String query = String.format(GET_INVOICE_LINE_BY_PO_LINE_ID_QUERY, orderPoLineId);
     return retrieveInvoiceLines(query, requestContext);
  }

  public CompletableFuture<List<InvoiceLine>> getInvoiceLinesByOrderLineIds(List<String> poLineIds, RequestContext requestContext) {
    List<CompletableFuture<List<InvoiceLine>>> futures = StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ)
      .map(ids -> getInvoiceLineByOrderLineIdsChunk(ids, requestContext))
      .collect(toList());
    return collectResultsOnSuccess(futures)
      .thenApply(listList -> listList.stream().flatMap(Collection::stream).collect(toList()))
      .thenApply(invoiceLines -> invoiceLines.stream().distinct().collect(Collectors.toList()));
  }

  public CompletableFuture<Void> removeEncumbranceLinks(List<InvoiceLine> invoiceLines, List<String> transactionIds,
      RequestContext requestContext) {
    List<InvoiceLine> invoiceLinesToUpdate = new ArrayList<>();
    for (InvoiceLine invoiceLine : invoiceLines) {
      for (FundDistribution fd : invoiceLine.getFundDistributions()) {
        if (transactionIds.contains(fd.getEncumbrance())) {
          fd.setEncumbrance(null);
          if (!invoiceLinesToUpdate.contains(invoiceLine))
            invoiceLinesToUpdate.add(invoiceLine);
        }
      }
      for (Adjustment adj : invoiceLine.getAdjustments()) {
        for (FundDistribution fd : adj.getFundDistributions()) {
          if (transactionIds.contains(fd.getEncumbrance())) {
            fd.setEncumbrance(null);
            if (!invoiceLinesToUpdate.contains(invoiceLine))
              invoiceLinesToUpdate.add(invoiceLine);
          }
        }
      }
    }
    return saveInvoiceLines(invoiceLinesToUpdate, requestContext);
  }

  private CompletableFuture<Void> saveInvoiceLines(List<InvoiceLine> invoiceLines, RequestContext requestContext) {
    return allOf(requestContext.getContext(), invoiceLines.stream()
      .map(invoiceLine -> saveInvoiceLine(invoiceLine, requestContext))
      .toArray(CompletableFuture[]::new));
  }

  private CompletableFuture<Void> saveInvoiceLine(InvoiceLine invoiceLine,  RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVOICE_LINE_BY_ID_ENDPOINT).withId(invoiceLine.getId());
    return restClient.put(requestEntry, invoiceLine, requestContext);
  }

  private CompletableFuture<List<InvoiceLine>> getInvoiceLineByOrderLineIdsChunk(List<String> poLineIds, RequestContext requestContext) {
    String query = buildInvoiceLineWithPoLineQuery(poLineIds);
    return retrieveInvoiceLines(query, requestContext);
  }

  private String buildInvoiceLineWithPoLineQuery(List<String> poLineIds) {
    String values = poLineIds.stream()
      .map(id -> format("\"%s\"", id))
      .collect(joining(" OR "));
    return String.format("poLineId == (%s)", values);
  }

}
