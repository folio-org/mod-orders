package org.folio.service.invoice;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.http.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.models.PoLineInvoiceLineHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;

import io.vertx.core.Future;
import org.folio.service.finance.transaction.PendingPaymentService;
import org.folio.service.finance.transaction.summary.InvoiceTransactionSummariesService;

import javax.money.Monetary;
import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.groupingBy;
import static org.folio.orders.utils.HelperUtils.calculateEncumbranceEffectiveAmount;
import static org.folio.service.finance.transaction.summary.InvoiceTransactionSummariesService.buildInvoiceTransactionsSummary;
import static org.folio.service.invoice.PoLineInvoiceLineHolderBuilder.filterInvoiceLinesByStatuses;

public class POLInvoiceLineRelationService {
  private final InvoiceLineService invoiceLineService;
  private final PendingPaymentService pendingPaymentService;
  private final InvoiceTransactionSummariesService invoiceTransactionSummariesService;
  private final PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder;

  public POLInvoiceLineRelationService(InvoiceLineService invoiceLineService, PendingPaymentService pendingPaymentService, InvoiceTransactionSummariesService invoiceTransactionSummariesService, PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder) {
    this.invoiceLineService = invoiceLineService;
    this.pendingPaymentService = pendingPaymentService;
    this.invoiceTransactionSummariesService = invoiceTransactionSummariesService;
    this.poLineInvoiceLineHolderBuilder = poLineInvoiceLineHolderBuilder;
  }

  public Future<Void> prepareRelatedInvoiceLines(PoLineInvoiceLineHolder holder, RequestContext requestContext) {
    if (isFundDistributionNotChanged(holder)) {
      return Future.succeededFuture();
    } else {
      return poLineInvoiceLineHolderBuilder.buildHolder(holder.getPoLineFromRequest(), holder.getPoLineFromStorage(), requestContext)
        .map(poLineInvoiceLineHolder -> holder.withOpenOrReviewedInvoiceLines(poLineInvoiceLineHolder.getOpenOrReviewedInvoiceLines())
          .withPaidOrCancelledInvoiceLines(poLineInvoiceLineHolder.getPaidOrCancelledInvoiceLines())).mapEmpty();
    }
  }

  public Future<EncumbrancesProcessingHolder> manageInvoiceRelation(EncumbrancesProcessingHolder encumbrancesProcessingHolder, RequestContext requestContext) {
    List<Transaction> forDelete = encumbrancesProcessingHolder.getEncumbrancesForDelete().stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance).collect(Collectors.toList());
    List<Transaction> forCreate = encumbrancesProcessingHolder.getEncumbrancesForCreate().stream()
      .map(EncumbranceRelationsHolder::getNewEncumbrance).collect(Collectors.toList());
    List<String> poLineIds = forDelete.stream().map(transaction -> transaction.getEncumbrance().getSourcePoLineId()).distinct().collect(Collectors.toList());

    return invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext)
      .compose(invoiceLines -> {
        validateInvoiceLineStatuses(invoiceLines);

        if (CollectionUtils.isNotEmpty(forCreate) && CollectionUtils.isNotEmpty(forDelete)) {
          List<String> encumbranceForDeleteIds = forDelete.stream().map(Transaction::getId).distinct().collect(Collectors.toList());
          String currency = encumbrancesProcessingHolder.getEncumbrancesForCreate().stream()
            .map(EncumbranceRelationsHolder::getCurrency).findFirst().orElseThrow(() -> new IllegalArgumentException("Idk why"));

          copyAmountsAndRecalculateNewEncumbrance(forCreate, forDelete, currency);
          return removeEncumbranceReferenceFromTransactions(encumbranceForDeleteIds, requestContext)
            .map(encumbrancesProcessingHolder);
        } else {
          return Future.succeededFuture(encumbrancesProcessingHolder);
        }
      });
  }


  public Future<Void> updateInvoiceLineReference(PoLineInvoiceLineHolder holder, RequestContext requestContext) {
    if (isFundDistributionNotChanged(holder)) {
      return Future.succeededFuture();
    }

    List<org.folio.rest.acq.model.invoice.FundDistribution> newFundDistribution = holder.getPoLineFromRequest().getFundDistribution()
      .stream().map(POLInvoiceLineRelationService::convertToInvoiceFundDistribution).collect(Collectors.toList());
    List<InvoiceLine> invoiceLines = holder.getOpenOrReviewedInvoiceLines()
      .stream().map(invoiceLine -> invoiceLine.withFundDistributions(newFundDistribution)).collect(Collectors.toList());
    return invoiceLineService.saveInvoiceLines(invoiceLines, requestContext);
  }

  private Future<Void> removeEncumbranceReferenceFromTransactions(List<String> encumbranceIds, RequestContext requestContext) {
    return pendingPaymentService.getTransactionsByEncumbranceIds(encumbranceIds, requestContext)
      .map(pendingPayments -> pendingPayments.stream().map(transaction -> {
        transaction.getAwaitingPayment().setEncumbranceId(null);
        return transaction;
      }).collect(groupingBy(Transaction::getSourceInvoiceId)))
      .map(pendingPayments -> pendingPayments.entrySet().stream().map(entry ->
        invoiceTransactionSummariesService.updateTransactionSummary(buildInvoiceTransactionsSummary(entry.getKey(), entry.getValue().size()), requestContext)
          .compose(aVoid -> pendingPaymentService.updateTransactions(entry.getValue(),requestContext))).collect(Collectors.toList()))
      .compose(GenericCompositeFuture::join)
      .mapEmpty();
  }

  private static org.folio.rest.acq.model.invoice.FundDistribution convertToInvoiceFundDistribution(FundDistribution fundDistribution) {
    return new org.folio.rest.acq.model.invoice.FundDistribution().withCode(fundDistribution.getCode()).withEncumbrance(fundDistribution.getEncumbrance())
      .withFundId(fundDistribution.getFundId()).withValue(fundDistribution.getValue()).withExpenseClassId(fundDistribution.getExpenseClassId())
      .withDistributionType(org.folio.rest.acq.model.invoice.FundDistribution.DistributionType.fromValue(fundDistribution.getDistributionType().value()));
  }

  private void copyAmountsAndRecalculateNewEncumbrance(List<Transaction> forCreate, List<Transaction> forDelete, String currency) {
    double amountExpended = forDelete.stream().map(encumbrance -> encumbrance.getEncumbrance().getAmountExpended())
      .map(BigDecimal::valueOf).reduce(BigDecimal.ZERO, BigDecimal::add).doubleValue();
    double amountAwaitingPayment = forDelete.stream().map(encumbrance -> encumbrance.getEncumbrance().getAmountAwaitingPayment())
      .map(BigDecimal::valueOf).reduce(BigDecimal.ZERO, BigDecimal::add).doubleValue();

    forCreate.stream().findFirst().ifPresent(transaction -> {
      Encumbrance encumbrance = transaction.getEncumbrance();
      double encumbranceAmount = HelperUtils.calculateEncumbranceEffectiveAmount(encumbrance.getInitialAmountEncumbered(),
        amountExpended, amountAwaitingPayment, Monetary.getCurrency(currency));
      transaction.withAmount(encumbranceAmount).withEncumbrance(encumbrance.withAmountExpended(amountExpended)
        .withAmountAwaitingPayment(amountAwaitingPayment).withStatus(encumbranceAmount == 0d ? Encumbrance.Status.RELEASED : Encumbrance.Status.UNRELEASED));
    });
  }

  private boolean isFundDistributionNotChanged(PoLineInvoiceLineHolder holder) {
    return CollectionUtils.isEqualCollection(getFundIdsFromFundDistribution(holder.getPoLineFromRequest().getFundDistribution()),
      getFundIdsFromFundDistribution(holder.getPoLineFromStorage().getFundDistribution()));
  }

  private List<String> getFundIdsFromFundDistribution(List<FundDistribution> fundDistributions) {
    return fundDistributions.stream().map(FundDistribution::getFundId).collect(Collectors.toList());
  }

  public static void validateInvoiceLineStatuses(List<InvoiceLine> invoiceLines) {
    List<InvoiceLine> approvedInvoices = filterInvoiceLinesByStatuses(invoiceLines, List.of(InvoiceLine.InvoiceLineStatus.APPROVED));
    if (CollectionUtils.isNotEmpty(approvedInvoices)) {
      String invoiceLineIds = approvedInvoices.stream().map(InvoiceLine::getId).collect(Collectors.joining(", "));
      String message = String.format(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getDescription(), invoiceLineIds);
      throw new HttpException(HttpStatus.SC_FORBIDDEN, new Error().withCode(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getCode()).withMessage(message));
    }
  }

}
