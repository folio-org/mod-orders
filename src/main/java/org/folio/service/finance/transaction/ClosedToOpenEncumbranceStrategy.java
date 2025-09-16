package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;

import java.util.List;
import java.util.stream.Collectors;

import io.vertx.core.json.JsonObject;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.tuple.Pair;
import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.EncumbranceUtils;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

@Log4j2
public class ClosedToOpenEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private static final String PROCESS_ENCUMBRANCES_ERROR = "Error when processing encumbrances to reopen an order";
  private static final String TRANSACTION_TYPE_PAYMENT = "transactionType == \"Payment\"";

  private final EncumbranceService encumbranceService;
  private final FundsDistributionService fundsDistributionService;
  private final BudgetRestrictionService budgetRestrictionService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final InvoiceLineService invoiceLineService;
  private final TransactionService transactionService;

  public ClosedToOpenEncumbranceStrategy(EncumbranceService encumbranceService,
                                         FundsDistributionService fundsDistributionService,
                                         BudgetRestrictionService budgetRestrictionService,
                                         EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
                                         InvoiceLineService invoiceLineService,
                                         TransactionService transactionService) {
    this.encumbranceService = encumbranceService;
    this.fundsDistributionService = fundsDistributionService;
    this.budgetRestrictionService = budgetRestrictionService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    this.invoiceLineService = invoiceLineService;
    this.transactionService = transactionService;
  }

  @Override
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
                                          RequestContext requestContext) {
    if (isFundDistributionsPresent(compPO.getPoLines())) {
      // get the encumbrances to unrelease
      return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithPoLines(compPO, poAndLinesFromStorage, requestContext)
        .compose(mapFiscalYearIdsWithPoLines -> encumbranceService.getOrderEncumbrancesToUnrelease(compPO, mapFiscalYearIdsWithPoLines, requestContext))
        .compose(transactions -> {
          var orderLineIds = transactions.stream()
            .map(transaction -> transaction.getEncumbrance().getSourcePoLineId())
            .toList();
          return invoiceLineService.getInvoiceLinesByOrderLineIds(orderLineIds, requestContext)
            .map(invoiceLines -> Pair.of(transactions, invoiceLines));
        })
        .compose(transactionsVsInvoiceLines -> {
          var encumbranceIds = transactionsVsInvoiceLines.getLeft().stream().map(Transaction::getId).toList();
          // TODO Change Pairs to a proper DTO
          return transactionService.getTransactionsByEncumbranceIds(encumbranceIds, TRANSACTION_TYPE_PAYMENT, requestContext)
            .map(paymentTransactions -> Pair.of(transactionsVsInvoiceLines, paymentTransactions));
        })
        .compose(encumbrancesVsInvoiceLinesVsPayments -> {
          var encumbranceTransactions = encumbrancesVsInvoiceLinesVsPayments.getLeft().getLeft();
          var paymentTransactions = encumbrancesVsInvoiceLinesVsPayments.getRight();
          var invoiceLines = encumbrancesVsInvoiceLinesVsPayments.getLeft().getRight();

          // TODO Remove
          encumbranceTransactions.forEach(t -> log.info("processEncumbrances:: Encumbrances: {}", JsonObject.mapFrom(t).encode()));
          paymentTransactions.forEach(p -> log.info("processEncumbrances:: Payments: {}", JsonObject.mapFrom(p).encode()));
          invoiceLines.forEach(il -> log.info("processEncumbrances:: InvoiceLines: {}", JsonObject.mapFrom(il).encode()));

          // stop if nothing needs to be done
          if (encumbranceTransactions.isEmpty() && compPO.getPoLines().stream().noneMatch(
              pol -> pol.getFundDistribution().stream().anyMatch(f -> f.getEncumbrance() == null))) {
            return Future.succeededFuture();
          }

          // check encumbrance restrictions as in PendingToOpenEncumbranceStrategy
          // (except we use a different list of poLines/encumbranceTransactions)
          List<EncumbranceRelationsHolder> holders = encumbranceRelationsHoldersBuilder
            .buildBaseHolders(compPO)
            // only keep holders with a missing encumbrance or a matching selected transaction
            .stream()
            .filter(h -> h.getFundDistribution().getEncumbrance() == null || encumbranceTransactions.stream()
              .anyMatch(t -> t.getEncumbrance().getSourcePoLineId().equals(h.getPoLineId())))
            .collect(Collectors.toList());
          return encumbranceRelationsHoldersBuilder.withFinances(holders, requestContext)
            // use given encumbranceTransactions (withKnownTransactions) instead of retrieving them (withExistingTransactions)
            .map(v -> {
              encumbranceRelationsHoldersBuilder.withKnownTransactions(holders, encumbranceTransactions);
              return holders;
            })
            .map(fundsDistributionService::distributeFunds)
            .map(dataHolders -> {
              budgetRestrictionService.checkEncumbranceRestrictions(dataHolders);
              return null;
            })
            .compose(v -> {
              // create missing encumbrances and unrelease existing ones
              EncumbrancesProcessingHolder holder = new EncumbrancesProcessingHolder();
              List<EncumbranceRelationsHolder> toBeCreatedHolders = holders.stream()
                .filter(h -> h.getOldEncumbrance() == null)
                .collect(toList());
              holder.withEncumbrancesForCreate(toBeCreatedHolders);
              // only unrelease encumbrances with expended + credited + awaiting payment = 0
              List<Transaction> encToUnrelease = EncumbranceUtils.collectAllowedTransactionsForUnrelease(encumbranceTransactions, paymentTransactions, invoiceLines);
              holder.withEncumbrancesForUnrelease(encToUnrelease);
              return encumbranceService.createOrUpdateEncumbrances(holder, requestContext);
            });
        })
         .recover(t -> {
          log.error(PROCESS_ENCUMBRANCES_ERROR, t);
          throw new HttpException(HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), PROCESS_ENCUMBRANCES_ERROR +
            (t.getMessage() != null ? ": " + t.getMessage() : ""));
        });
    }
    return Future.succeededFuture();
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.CLOSED_TO_OPEN;
  }
}
