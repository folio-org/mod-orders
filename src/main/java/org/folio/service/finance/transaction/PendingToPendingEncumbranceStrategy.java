package org.folio.service.finance.transaction;

import static java.lang.Boolean.TRUE;
import static org.folio.orders.utils.FundDistributionUtils.validateFundDistributionTotal;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.http.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.orders.utils.validators.TransactionValidator;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

public class PendingToPendingEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private final EncumbranceService encumbranceService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final PendingPaymentService pendingPaymentService;

  public PendingToPendingEncumbranceStrategy(EncumbranceService encumbranceService,
    EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder, PendingPaymentService pendingPaymentService) {
    this.encumbranceService = encumbranceService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    this.pendingPaymentService = pendingPaymentService;
  }

  @Override
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
      RequestContext requestContext) {

    validateFundDistributionTotal(compPO.getPoLines());
    List<EncumbranceRelationsHolder> encumbranceRelationsHolders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return encumbranceRelationsHoldersBuilder.withExistingTransactions(encumbranceRelationsHolders, poAndLinesFromStorage, requestContext)
      .compose(v -> distributeHoldersByOperation(encumbranceRelationsHolders, requestContext))
      .compose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.PENDING_TO_PENDING;
  }

  private Future<EncumbrancesProcessingHolder> distributeHoldersByOperation(List<EncumbranceRelationsHolder> encumbranceRelationsHolders,
      RequestContext requestContext) {
    EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
    List<EncumbranceRelationsHolder> toUpdate = getTransactionsToUpdate(encumbranceRelationsHolders);
    List<EncumbranceRelationsHolder> toDelete = getTransactionsToDelete(encumbranceRelationsHolders);
    // 0-amount encumbrances are not released before deletion to avoid blocking pending order fund distributions (MODORDERS-1253)
    List<Transaction> toRelease = toDelete.stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .filter(tr -> tr.getAmount() != 0d)
      .toList();
    holder.withEncumbrancesForUpdate(toUpdate);
    holder.withEncumbrancesForRelease(toRelease);
    holder.withEncumbrancesForDelete(toDelete);
    validateTransactionsToDelete(toDelete);
    return addPendingPaymentsToUpdate(holder, requestContext);
  }

  private List<EncumbranceRelationsHolder> getTransactionsToDelete(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    return encumbranceRelationsHolders.stream()
      .filter(holder -> holder.getOldEncumbrance() != null && holder.getNewEncumbrance() == null)
      .toList();
  }

  private List<EncumbranceRelationsHolder> getTransactionsToUpdate(List<EncumbranceRelationsHolder> encumbranceRelationsHolders) {
    // Return a list of holders with transactions that need to have an expense class update.
    // We want to avoid loading all financial data like in pending->open, so we copy some fields
    // from the old encumbrance (amount is 0 with pending orders).
    List<EncumbranceRelationsHolder> toUpdate = encumbranceRelationsHolders.stream()
      .filter(holder -> holder.getNewEncumbrance() != null && holder.getOldEncumbrance() != null &&
        !Objects.equals(holder.getNewEncumbrance().getExpenseClassId(), holder.getOldEncumbrance().getExpenseClassId()))
      .toList();
    toUpdate.forEach(holder -> {
      Transaction oldEncumbrance = holder.getOldEncumbrance();
      Transaction newEncumbrance = holder.getNewEncumbrance();
      newEncumbrance.withFiscalYearId(oldEncumbrance.getFiscalYearId())
        .withCurrency(oldEncumbrance.getCurrency())
        .withAmount(oldEncumbrance.getAmount())
        .getEncumbrance()
          .withInitialAmountEncumbered(oldEncumbrance.getEncumbrance().getInitialAmountEncumbered())
          .withOrderStatus(Encumbrance.OrderStatus.PENDING)
          .withStatus(oldEncumbrance.getEncumbrance().getStatus())
          .withSubscription(oldEncumbrance.getEncumbrance().getSubscription())
          .withReEncumber(oldEncumbrance.getEncumbrance().getReEncumber());
    });
    return toUpdate;
  }

  private void validateTransactionsToDelete(List<EncumbranceRelationsHolder> toDelete) {
    toDelete.stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .forEach(TransactionValidator::validateEncumbranceForDeletion);
  }


  public Future<EncumbrancesProcessingHolder> addPendingPaymentsToUpdate(EncumbrancesProcessingHolder holder,
      RequestContext requestContext) {
    if (holder.getEncumbrancesForDelete().isEmpty()) {
      return Future.succeededFuture(holder);
    }
    List<String> idsOfEncumbrancesToDelete = holder.getEncumbrancesForDelete().stream()
      .map(EncumbranceRelationsHolder::getOldEncumbrance)
      .map(Transaction::getId)
      .toList();

    return pendingPaymentService.getTransactionsByEncumbranceIds(idsOfEncumbrancesToDelete, requestContext)
      .map(pendingPayments -> {
        if (pendingPayments.isEmpty()) {
          return holder;
        }
        if (pendingPayments.stream().anyMatch(pp -> !TRUE.equals(pp.getInvoiceCancelled()))) {
          // Do not allow removing encumbrances if they are linked to an invoice that is not cancelled
          String invoiceLineIds = pendingPayments.stream()
            .filter(pp -> !TRUE.equals(pp.getInvoiceCancelled()))
            .map(Transaction::getSourceInvoiceLineId)
            .collect(Collectors.joining(", "));
          String message = String.format(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getDescription(), invoiceLineIds);
          throw new HttpException(HttpStatus.SC_FORBIDDEN, new Error()
            .withCode(ErrorCodes.PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR.getCode())
            .withMessage(message));
        }
        pendingPayments.forEach(pp -> pp.getAwaitingPayment().setEncumbranceId(null));
        holder.withPendingPaymentsToUpdate(pendingPayments);
        return holder;
      });
  }
}
