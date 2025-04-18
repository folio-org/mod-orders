package org.folio.service.orders.flows.update.reopen;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.helper.CheckinReceivePiecesHelper;
import org.folio.models.orders.flows.update.reopen.ReOpenCompositeOrderHolder;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategy;
import org.folio.service.finance.transaction.EncumbranceWorkflowStrategyFactory;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.folio.service.orders.OrderWorkflowType;
import org.folio.service.pieces.PieceStorageService;

import io.vertx.core.Future;
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

  public Future<Void> process(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
                                       RequestContext requestContext) {
    return Future.succeededFuture()
      .map(v -> {
        addPoLinesIfNeeded(compPO, poFromStorage);
        return null;
      })
      .compose(v -> processEncumbrances(compPO, poFromStorage, requestContext))
      .compose(v -> updatePoLineStatuses(compPO, requestContext));
  }

  private void addPoLinesIfNeeded(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage) {
    if (CollectionUtils.isEmpty(compPO.getPoLines())) {
      List<PoLine> clonedLines = poFromStorage.getPoLines()
        .stream()
        .map(line -> JsonObject.mapFrom(line).mapTo(PoLine.class))
        .collect(toList());
      compPO.setPoLines(clonedLines);
    }
  }

  private Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poFromStorage,
                                                      RequestContext requestContext) {
    EncumbranceWorkflowStrategy strategy = encumbranceWorkflowStrategyFactory.getStrategy(OrderWorkflowType.CLOSED_TO_OPEN);
    return strategy.processEncumbrances(compPO, poFromStorage, requestContext);
  }

  private Future<Void> updatePoLineStatuses(CompositePurchaseOrder compPO, RequestContext requestContext) {
    if (CompositePurchaseOrder.OrderType.ONE_TIME == compPO.getOrderType()) {
      return oneTimeUpdatePoLineStatuses(compPO.getId(), compPO.getPoLines(), requestContext);
    } else if (CompositePurchaseOrder.OrderType.ONGOING == compPO.getOrderType()) {
      return ongoingUpdatePoLineStatuses(compPO.getPoLines());
    }
    return Future.succeededFuture();
  }

  /**
   * Ongoing orders always stay with Payment and Receipt status
   *
   * @param poLines
   * @return
   */
  private Future<Void> ongoingUpdatePoLineStatuses(List<PoLine> poLines) {
    poLines.forEach(poLine -> {
      poLine.setReceiptStatus(PoLine.ReceiptStatus.ONGOING);
      poLine.setPaymentStatus(PoLine.PaymentStatus.ONGOING);
    });
    return Future.succeededFuture();
  }

  private Future<Void> oneTimeUpdatePoLineStatuses(String orderId, List<PoLine> poLines, RequestContext requestContext) {
    return buildHolder(orderId, poLines, requestContext).map(holder -> {
      poLines.forEach(poLine -> {
        updatePoLineReceiptStatus(poLine, holder);
        updatePoLinePaymentStatus(poLine, holder);
      });
      return null;
    });
  }

  private void updatePoLinePaymentStatus(PoLine poLine, ReOpenCompositeOrderHolder holder) {
    var poLineInvoicesMap = getInvoicesByPoLineId(holder.getOrderInvoices(), holder.getOrderLineInvoiceLines());
    var poLineInvoiceLinesMap = holder.getOrderLineInvoiceLines().stream().collect(groupingBy(InvoiceLine::getPoLineId));

    List<Invoice> poLineInvoices = poLineInvoicesMap.get(poLine.getId());
    if (CollectionUtils.isNotEmpty(poLineInvoices) && isAnyInvoicePaid(poLineInvoices)) {
      var invoiceLines = poLineInvoiceLinesMap.get(poLine.getId());
      if (isAnyInvoiceLineReleaseEncumbrance(invoiceLines)) {
        poLine.setPaymentStatus(PoLine.PaymentStatus.FULLY_PAID);
      } else {
        poLine.setPaymentStatus(PoLine.PaymentStatus.PARTIALLY_PAID);
      }
    } else {
      poLine.setPaymentStatus(PoLine.PaymentStatus.AWAITING_PAYMENT);
    }
  }

  private boolean isAnyInvoiceLineReleaseEncumbrance(List<InvoiceLine> invoiceLines) {
    return invoiceLines.stream().anyMatch(InvoiceLine::getReleaseEncumbrance);
  }

  private boolean isAnyInvoicePaid(List<Invoice> poLineInvoices) {
    return poLineInvoices.stream().anyMatch(invoice -> Invoice.Status.PAID.equals(invoice.getStatus()));
  }

  private void updatePoLineReceiptStatus(PoLine poLine, ReOpenCompositeOrderHolder holder) {
    Map<String, List<Piece>> orderLineIdPieceMap = groupPiecesByOrderId(holder.getOrderLinePieces());
    List<Piece> pieces = orderLineIdPieceMap.get(poLine.getId());
    if (CollectionUtils.isNotEmpty(pieces) && isReceivedPiecePresent(pieces)) {
      poLine.setReceiptStatus(PoLine.ReceiptStatus.PARTIALLY_RECEIVED);
    } else {
      poLine.setReceiptStatus(PoLine.ReceiptStatus.AWAITING_RECEIPT);
    }
  }

  private Future<ReOpenCompositeOrderHolder> buildHolder(String orderId, List<PoLine> poLines, RequestContext requestContext) {
    ReOpenCompositeOrderHolder holder = new ReOpenCompositeOrderHolder(orderId);
    return getPieces(poLines, requestContext)
              .map(holder::withPieces)
              .map(aHolder -> poLines.stream().map(PoLine::getId).distinct().collect(toList()))
              .compose(poLineIds -> invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext))
              .map(holder::withInvoiceLines)
              .compose(aHolder -> invoiceService.getInvoicesByOrderId(orderId, requestContext))
              .map(holder::withOrderInvoices)
              .map(v -> holder);
  }

  private Future<List<Piece>> getPieces(List<PoLine> poLines, RequestContext requestContext) {
    List<String> poLineIds = poLines.stream().map(PoLine::getId).distinct().collect(toList());
    return pieceStorageService.getPiecesByLineIdsByChunks(poLineIds, requestContext);
  }

  private boolean isReceivedPiecePresent(List<Piece> pieces) {
    return pieces.stream().anyMatch(piece ->
      CheckinReceivePiecesHelper.RECEIVED_STATUSES.contains(piece.getReceivingStatus()));
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
