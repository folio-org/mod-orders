package org.folio.service.orders.flows.update.reopen;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.orders.flows.update.reopen.ReOpenCompositeOrderHolder;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.json.JsonObject;

public class ReOpenCompositeOrderManager {
  private final EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory;
  private final PieceStorageService pieceStorageService;
  private final InvoiceLineService invoiceLineService;
  private final InvoiceService invoiceService;

  public ReOpenCompositeOrderManager(EncumbranceWorkflowStrategyFactory encumbranceWorkflowStrategyFactory,
    PieceStorageService pieceStorageService, InvoiceLineService invoiceLineService, InvoiceService invoiceService) {
    this.encumbranceWorkflowStrategyFactory = encumbranceWorkflowStrategyFactory;
    this.pieceStorageService = pieceStorageService;
    this.invoiceLineService = invoiceLineService;
    this.invoiceService = invoiceService;
  }

  public CompletableFuture<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
                                       RequestContext requestContext) {
    addPoLinesIfNeeded(compPO, poFromStorage);
    return processEncumbrances(compPO, poFromStorage, requestContext)
            .thenCompose(v -> updatePoLineStatuses(compPO, requestContext));
  }

  private void addPoLinesIfNeeded(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage) {
    if (CollectionUtils.isEmpty(compPO.getCompositePoLines())) {
      List<CompositePoLine> clonedLines = poFromStorage.getCompositePoLines()
        .stream()
        .map(line -> JsonObject.mapFrom(line).mapTo(CompositePoLine.class))
        .collect(toList());
      compPO.setCompositePoLines(clonedLines);
    }
  }

  private CompletableFuture<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
                                                      RequestContext requestContext) {
    EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.CLOSED_TO_OPEN);
    return strategy.processEncumbrances(compPO, poFromStorage, requestContext);
  }

  private CompletableFuture<Void> updatePoLineStatuses(CompositePurchaseOrder compPO, RequestContext requestContext) {
    return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(compPO.getOrderType()))
              .thenCompose(orderTypeP -> {
                if (CompositePurchaseOrder.OrderType.ONE_TIME == compPO.getOrderType()) {
                  return oneTimeUpdatePoLineStatuses(compPO.getId(), compPO.getCompositePoLines(), requestContext);
                } else if (CompositePurchaseOrder.OrderType.ONGOING == compPO.getOrderType()) {
                  return ongoingUpdatePoLineStatuses(compPO.getCompositePoLines(), requestContext);
                }
                return CompletableFuture.completedFuture(null);
              });
  }

  /**
   * Ongoing orders always stay with Payment and Receipt status
   * @param poLines
   */
  private CompletableFuture<Void> ongoingUpdatePoLineStatuses(List<CompositePoLine> poLines, RequestContext requestContext) {
   return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(poLines))
                   .thenAccept(poLinesP -> {
                     poLinesP.forEach(poLine -> {
                       poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.ONGOING);
                       poLine.setPaymentStatus(CompositePoLine.PaymentStatus.ONGOING);
                     });
                   });
  }

  private CompletableFuture<Void> oneTimeUpdatePoLineStatuses(String orderId, List<CompositePoLine> poLines, RequestContext requestContext) {
    return buildHolder(orderId, poLines, requestContext)
            .thenAccept(holder -> {
              var poLineInvoicesMap = getInvoicesByPoLineId(holder.getOrderInvoices(), holder.getOrderLineInvoiceLines());
              poLines.forEach(poLine -> {
                updatePoLineReceiptStatus(poLine, holder);
                updatePoLinePaymentStatus(poLine, poLineInvoicesMap);
               });
            });
  }

  private void updatePoLinePaymentStatus(CompositePoLine poLine, Map<String, List<Invoice>> poLineInvoicesMap) {
    List<Invoice> poLineInvoices = poLineInvoicesMap.get(poLine.getId());
    if (CollectionUtils.isNotEmpty(poLineInvoices) && isAnyInvoicesHasTransactions(poLineInvoices)) {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.PARTIALLY_PAID);
    } else {
      poLine.setPaymentStatus(CompositePoLine.PaymentStatus.AWAITING_PAYMENT);
    }
  }

  private boolean isAnyInvoicesHasTransactions(List<Invoice> poLineInvoices) {
    return poLineInvoices.stream().anyMatch(invoice -> Invoice.Status.APPROVED.equals(invoice.getStatus()) ||
                                                            Invoice.Status.PAID.equals(invoice.getStatus()));
  }

  private void updatePoLineReceiptStatus(CompositePoLine poLine, ReOpenCompositeOrderHolder holder) {
    Map<String, List<Piece>> orderLineIdPieceMap = groupPiecesByOrderId(holder.getOrderLinePieces());
    List<Piece> pieces = orderLineIdPieceMap.get(poLine.getId());
    if (CollectionUtils.isNotEmpty(pieces) && isReceivedPiecePresent(pieces)) {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.PARTIALLY_RECEIVED);
    } else {
      poLine.setReceiptStatus(CompositePoLine.ReceiptStatus.AWAITING_RECEIPT);
    }
  }

  private CompletableFuture<ReOpenCompositeOrderHolder> buildHolder(String orderId, List<CompositePoLine> poLines, RequestContext requestContext) {
    ReOpenCompositeOrderHolder holder = new ReOpenCompositeOrderHolder(orderId);
    return getPieces(poLines, requestContext)
              .thenAccept(holder::withPieces)
              .thenApply(v -> poLines.stream().map(CompositePoLine::getId).distinct().collect(toList()))
              .thenCompose(poLineIds -> invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext))
              .thenAccept(holder::withInvoiceLines)
              .thenCompose(v -> invoiceService.getInvoicesByOrderId(orderId, requestContext))
              .thenApply(holder::withOrderInvoices)
              .thenApply(v -> holder);
  }

  private CompletableFuture<List<Piece>> getPieces(List<CompositePoLine> poLines, RequestContext requestContext) {
    List<String> poLineIds = poLines.stream().map(CompositePoLine::getId).distinct().collect(toList());
    return pieceStorageService.getPiecesByLineIdsByChunks(poLineIds, requestContext);
  }

  private boolean isReceivedPiecePresent(List<Piece> pieces) {
    return pieces.stream().anyMatch(piece -> Piece.ReceivingStatus.RECEIVED.equals(piece.getReceivingStatus()));
  }

  private Map<String, List<Piece>> groupPiecesByOrderId(List<Piece> pieces) {
    return pieces.stream().collect(groupingBy(Piece::getPoLineId));
  }

  private Map<String, List<Invoice>> getInvoicesByPoLineId(List<Invoice> invoices, List<InvoiceLine> invoiceLines) {
    Map<String, List<Invoice>> invoiceByIdMap = invoices.stream().collect(groupingBy(Invoice::getId));
    Map<String, List<InvoiceLine>> invoiceLineByPoLineIdMap = invoiceLines.stream().collect(groupingBy(InvoiceLine::getPoLineId));
    Map<String, List<Invoice>> poLineVsInvoices = new HashMap<>();
    for (Map.Entry<String, List<InvoiceLine>> invoiceLineVsPoLineId : invoiceLineByPoLineIdMap.entrySet()) {
      List<Invoice> resPoLineInvoices = new ArrayList<>();
      List<InvoiceLine> poLineInvoiceLines = invoiceLineVsPoLineId.getValue();
      Map<String, List<InvoiceLine>> invoiceLinesByInvoiceIdMap = poLineInvoiceLines.stream().collect(groupingBy(InvoiceLine::getInvoiceId));
      for (Map.Entry<String, List<InvoiceLine>> invoiceLinesByInvoiceIdEntry : invoiceLinesByInvoiceIdMap.entrySet()) {
        List<Invoice> invoiceList = invoiceByIdMap.get(invoiceLinesByInvoiceIdEntry.getKey());
        if (CollectionUtils.isNotEmpty(invoiceList)) {
          resPoLineInvoices.addAll(invoiceList);
        }
      }
      poLineVsInvoices.put(invoiceLineVsPoLineId.getKey(), resPoLineInvoices);
    }
    return poLineVsInvoices;
  }

}
