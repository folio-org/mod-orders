package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;
import static org.folio.service.finance.EncumbranceUtils.collectAllowedEncumbrancesForUnrelease;

import java.util.List;
import java.util.stream.Collectors;

import org.folio.HttpStatus;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbranceUnreleaseHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;

@Log4j2
public class ClosedToOpenEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private static final String PROCESS_ENCUMBRANCES_ERROR = "Error when processing encumbrances to reopen an order";

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
    log.debug("processEncumbrances: start for order {}", compPO.getId());
    if (isFundDistributionsPresent(compPO.getPoLines())) {
      log.info("processEncumbrances: Fund distributions present for order {}", compPO.getId());
      // get the encumbrances to unrelease
      return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithPoLines(compPO, poAndLinesFromStorage, requestContext)
        .compose(mapFiscalYearIdsWithPoLines -> {
          log.info("processEncumbrances: Retrieved map of fiscal years with PO lines for order {}", compPO.getId());
          return encumbranceService.getOrderEncumbrancesToUnrelease(compPO, mapFiscalYearIdsWithPoLines, requestContext);
        })
        .compose(encumbrances -> {
          log.info("processEncumbrances: Order encumbrances to unrelease for order {}: {}", compPO.getId(), encumbrances.stream().map(Transaction::getId).collect(Collectors.toList()));
          var unreleaseHolder = new EncumbranceUnreleaseHolder().withEncumbrances(encumbrances);
          return invoiceLineService.getInvoiceLinesByOrderLineIds(unreleaseHolder.getPoLineIds(), requestContext)
            .map(unreleaseHolder::withInvoiceLines);
        })
        .compose(unreleaseHolder -> {
          log.info("processEncumbrances: Invoice lines for order {}: {}", compPO.getId(), unreleaseHolder.getInvoiceLines().stream().map(il -> il.getId() + " polId=" + il.getPoLineId()).collect(Collectors.toList()));
          return transactionService.getPendingPaymentsByEncumbranceIds(unreleaseHolder.getEncumbranceIds(), requestContext)
            .map(unreleaseHolder::withPendingPayments);
        })
        .compose(unreleaseHolder -> {
          log.info("processEncumbrances: Pending payments for order {}: {}", compPO.getId(), unreleaseHolder.getPendingPayments().stream().map(Transaction::getId).collect(Collectors.toList()));
          return transactionService.getPaymentsByEncumbranceIds(unreleaseHolder.getEncumbranceIds(), requestContext)
            .map(unreleaseHolder::withPayments);
        })
        .compose(unreleaseHolder -> {
          log.info("processEncumbrances: Payments for order {}: {}", compPO.getId(), unreleaseHolder.getPayments().stream().map(Transaction::getId).collect(Collectors.toList()));
          var encumbrances = unreleaseHolder.getEncumbrances();
          // stop if nothing needs to be done
          if (checkIfMatchingEncumbrances(compPO, encumbrances)) {
            log.info("processEncumbrances: Matching encumbrances found for order {}, returning", compPO.getId());
            return Future.succeededFuture();
          }
          // check encumbrance restrictions as in PendingToOpenEncumbranceStrategy
          // (except we use a different list of poLines/encumbrances)
          var holders = createEncumbranceRelationHolders(compPO, encumbrances);
          log.info("processEncumbrances: Created encumbrance relation holders for order {}: {}", compPO.getId(), holders.size());
          return encumbranceRelationsHoldersBuilder.withFinances(holders, requestContext)
            // use given encumbrances (withKnownTransactions) instead of retrieving them (withExistingTransactions)
            .map(v -> {
              encumbranceRelationsHoldersBuilder.withKnownTransactions(holders, encumbrances);
              log.info("processEncumbrances: Holders with known transactions for order {}: {}", compPO.getId(), holders.stream().map(EncumbranceRelationsHolder::getOldEncumbrance).map(Transaction::getId).collect(Collectors.toList()));
              return holders;
            })
            .map(fundsDistributionService::distributeFunds)
            .map(dataHolders -> {
              log.info("processEncumbrances: Funds distributed for order {}", compPO.getId());
              budgetRestrictionService.checkEncumbranceRestrictions(dataHolders);
              log.info("processEncumbrances: Encumbrance restrictions checked for order {}", compPO.getId());
              return null;
            })
            .compose(v -> {
              // create missing encumbrances and unrelease existing ones
              var holder = new EncumbrancesProcessingHolder();
              var toBeCreatedHolders = createToBeCreatedHolders(holders);
              holder.withEncumbrancesForCreate(toBeCreatedHolders);
              // only unrelease encumbrances with expended + credited + awaiting payment = 0
              var encumbrancesToUnrelease = collectAllowedEncumbrancesForUnrelease(unreleaseHolder);
              holder.withEncumbrancesForUnrelease(encumbrancesToUnrelease);
              log.info("processEncumbrances: Encumbrances to be created for order {}: {}", compPO.getId(), toBeCreatedHolders.stream().map(h -> h.getPoLine().getPoLineNumber()).collect(Collectors.toList()));
              log.info("processEncumbrances: Encumbrances to be unreleased for order {}: {}", compPO.getId(), encumbrancesToUnrelease.stream().map(Transaction::getId).collect(Collectors.toList()));
              return encumbranceService.createOrUpdateEncumbrances(holder, requestContext);
            });
        })
         .recover(t -> {
          log.error("processEncumbrances: " + PROCESS_ENCUMBRANCES_ERROR + " for order " + compPO.getId(), t);
          throw new HttpException(HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), PROCESS_ENCUMBRANCES_ERROR +
            (t.getMessage() != null ? ": " + t.getMessage() : ""));
        });
    }
    log.debug("processEncumbrances: No fund distributions present for order {}", compPO.getId());
    return Future.succeededFuture();
  }

  private static boolean checkIfMatchingEncumbrances(CompositePurchaseOrder compPO, List<Transaction> encumbrances) {
    return encumbrances.isEmpty() && compPO.getPoLines()
      .stream()
      .noneMatch(pol -> pol.getFundDistribution().stream().anyMatch(f -> f.getEncumbrance() == null));
  }

  private static List<EncumbranceRelationsHolder> createToBeCreatedHolders(List<EncumbranceRelationsHolder> holders) {
    return holders.stream()
      .filter(h -> h.getOldEncumbrance() == null)
      .collect(toList());
  }

  private List<EncumbranceRelationsHolder> createEncumbranceRelationHolders(CompositePurchaseOrder compPO, List<Transaction> encumbrances) {
    return encumbranceRelationsHoldersBuilder
      .buildBaseHolders(compPO)
      // only keep holders with a missing encumbrance or a matching selected transaction
      .stream()
      .filter(h -> h.getFundDistribution().getEncumbrance() == null || encumbrances.stream()
        .anyMatch(t -> t.getEncumbrance().getSourcePoLineId().equals(h.getPoLineId())))
      .collect(Collectors.toList());
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.CLOSED_TO_OPEN;
  }
}
