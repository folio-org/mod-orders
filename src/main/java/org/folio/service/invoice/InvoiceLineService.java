package org.folio.service.invoice;

import static java.lang.String.format;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.InvoiceUtil.filterInvoiceLinesByStatuses;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.acq.model.invoice.Adjustment;
import org.folio.rest.acq.model.invoice.FundDistribution;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLine.InvoiceLineStatus;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;
import one.util.streamex.StreamEx;

public class InvoiceLineService {

  private static final String INVOICE_LINES_ENDPOINT = "/invoice/invoice-lines";
  private static final String INVOICE_LINE_BY_ID_ENDPOINT = INVOICE_LINES_ENDPOINT + "/{id}?skipPoNumbers=true";
  private static final String GET_INVOICE_LINES_ERROR = "Error when retrieving invoice lines";
  private static final String GET_INVOICE_LINE_BY_INVOICE_ID_AND_STATUS_QUERY =  "invoiceId==%s and invoiceLineStatus==%s";
  private static final String GET_INVOICE_LINE_BY_PO_LINE_ID_QUERY =  "poLineId==%s";
  private static final Logger logger = LogManager.getLogger();

  private final RestClient restClient;

  public InvoiceLineService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<List<InvoiceLine>> retrieveInvoiceLines(String query, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVOICE_LINES_ENDPOINT).withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, InvoiceLineCollection.class, requestContext)
      .map(InvoiceLineCollection::getInvoiceLines)
      .recover(t -> {
        logger.error("{}. Query: {}", GET_INVOICE_LINES_ERROR, query, t);
        throw new HttpException(t instanceof HttpException ? ((HttpException) t).getCode() :
          HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), GET_INVOICE_LINES_ERROR);
      });
  }

  public Future<List<InvoiceLine>> getInvoiceLinesByInvoiceIdAndStatus(String invoiceId, InvoiceLine.InvoiceLineStatus status, RequestContext requestContext) {
    var query = GET_INVOICE_LINE_BY_INVOICE_ID_AND_STATUS_QUERY.formatted(invoiceId, status.name());
    return retrieveInvoiceLines(query, requestContext);
  }

  public Future<List<InvoiceLine>> getInvoiceLinesByOrderLineId(String orderPoLineId, RequestContext requestContext) {
     String query = String.format(GET_INVOICE_LINE_BY_PO_LINE_ID_QUERY, orderPoLineId);
     return retrieveInvoiceLines(query, requestContext);
  }

  public Future<List<InvoiceLine>> getInvoiceLinesByOrderLineIds(List<String> poLineIds, RequestContext requestContext) {
    List<Future<List<InvoiceLine>>> futures = StreamEx
      .ofSubLists(poLineIds, MAX_IDS_FOR_GET_RQ_15)
      .map(ids -> getInvoiceLineByOrderLineIdsChunk(ids, requestContext))
      .collect(toList());
    return collectResultsOnSuccess(futures)
      .map(listList -> listList.stream().flatMap(Collection::stream).collect(toList()))
      .map(invoiceLines -> invoiceLines.stream().distinct().collect(Collectors.toList()));
  }

  public Future<Void> removeEncumbranceLinks(List<InvoiceLine> invoiceLines, List<String> transactionIds, RequestContext requestContext) {
    List<InvoiceLine> editableOrCancelledInvoiceLines = filterInvoiceLinesByStatuses(invoiceLines,
      List.of(InvoiceLineStatus.OPEN, InvoiceLineStatus.REVIEWED, InvoiceLineStatus.CANCELLED));
    List<InvoiceLine> invoiceLinesToUpdate = new ArrayList<>();
    for (InvoiceLine invoiceLine : editableOrCancelledInvoiceLines) {
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
    if (CollectionUtils.isEmpty(invoiceLinesToUpdate)) {
      logger.info("removeEncumbranceLinks:: No invoice lines to update");
      return Future.succeededFuture();
    }
    var invoiceIds = invoiceLinesToUpdate.stream()
      .map(InvoiceLine::getId)
      .collect(joining(", "));
    logger.info("removeEncumbranceLinks:: Updating invoice lines with removed encumbrance links={} ", invoiceIds);
    return saveInvoiceLines(invoiceLinesToUpdate, requestContext);
  }

  public Future<Void> saveInvoiceLines(List<InvoiceLine> invoiceLines, RequestContext requestContext) {
    return GenericCompositeFuture.join(invoiceLines.stream()
        .map(invLine -> saveInvoiceLine(invLine, requestContext))
        .collect(toList()))
      .mapEmpty();
  }

  private Future<Void> saveInvoiceLine(InvoiceLine invoiceLine, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVOICE_LINE_BY_ID_ENDPOINT).withId(invoiceLine.getId());
    return restClient.put(requestEntry, invoiceLine, requestContext);
  }

  private Future<List<InvoiceLine>> getInvoiceLineByOrderLineIdsChunk(List<String> poLineIds, RequestContext requestContext) {
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
