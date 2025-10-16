package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.FundDistributionUtils.isFundDistributionsPresent;
import static org.folio.service.finance.EncumbranceUtils.collectAllowedEncumbrancesForUnrelease;

import java.util.List;
import java.util.stream.Collectors;

import lombok.extern.log4j.Log4j2;
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
    String poLineNumbers = compPO.getPoLines().stream().map(pol -> pol.getPoLineNumber()).collect(Collectors.joining(", "));
    log.info("processEncumbrances:: START - orderId={}, poNumber={}, poLineCount={}, poLineNumbers=[{}]",
      compPO.getId(), compPO.getPoNumber(), compPO.getPoLines().size(), poLineNumbers);

    if (isFundDistributionsPresent(compPO.getPoLines())) {
      log.info("processEncumbrances:: Fund distributions present for orderId={}", compPO.getId());

      // get the encumbrances to unrelease
      return encumbranceRelationsHoldersBuilder.retrieveMapFiscalYearsWithPoLines(compPO, poAndLinesFromStorage, requestContext)
        .onSuccess(mapFiscalYearIdsWithPoLines -> {
          int totalPoLines = mapFiscalYearIdsWithPoLines.values().stream().mapToInt(List::size).sum();
          log.info("processEncumbrances:: STEP 1 - Retrieved fiscal years mapping for orderId={}, fiscalYearCount={}, totalPoLines={}",
            compPO.getId(), mapFiscalYearIdsWithPoLines.size(), totalPoLines);
        })
        .compose(mapFiscalYearIdsWithPoLines -> {
          log.info("processEncumbrances:: STEP 2 - Fetching encumbrances to unrelease for orderId={}", compPO.getId());
          return encumbranceService.getOrderEncumbrancesToUnrelease(compPO, mapFiscalYearIdsWithPoLines, requestContext);
        })
        .onSuccess(encumbrances -> {
          String encIds = encumbrances.stream().map(Transaction::getId).collect(Collectors.joining(", "));
          String poLineIds = encumbrances.stream().map(e -> e.getEncumbrance().getSourcePoLineId()).distinct().collect(Collectors.joining(", "));
          log.info("processEncumbrances:: STEP 2 RESULT - orderId={}, encumbranceCount={}, encumbranceIds=[{}], poLineIds=[{}]",
            compPO.getId(), encumbrances.size(), encIds, poLineIds);

          // Log detailed info about each encumbrance
          encumbrances.forEach(enc -> {
            var e = enc.getEncumbrance();
            log.info("processEncumbrances:: Encumbrance detail - id={}, poLineId={}, status={}, amountExpended={}, amountCredited={}, amountAwaitingPayment={}",
              enc.getId(), e.getSourcePoLineId(), e.getStatus(), e.getAmountExpended(), e.getAmountCredited(), e.getAmountAwaitingPayment());
          });
        })
        .compose(encumbrances -> {
          var unreleaseHolder = new EncumbranceUnreleaseHolder().withEncumbrances(encumbrances);
          var poLineIds = unreleaseHolder.getPoLineIds();
          log.info("processEncumbrances:: STEP 3 - Fetching invoice lines for orderId={}, poLineIdCount={}, poLineIds=[{}]",
            compPO.getId(), poLineIds.size(), String.join(", ", poLineIds));

          return invoiceLineService.getInvoiceLinesByOrderLineIds(unreleaseHolder.getPoLineIds(), requestContext)
            .onSuccess(invoiceLines -> {
              log.info("processEncumbrances:: STEP 3 RESULT - orderId={}, invoiceLineCount={}", compPO.getId(), invoiceLines.size());
              if (!invoiceLines.isEmpty()) {
                invoiceLines.forEach(il -> log.info("processEncumbrances:: InvoiceLine - id={}, poLineId={}, releaseEncumbrance={}, status={}",
                  il.getId(), il.getPoLineId(), il.getReleaseEncumbrance(), il.getInvoiceLineStatus()));
              }
            })
            .map(unreleaseHolder::withInvoiceLines);
        })
        .compose(unreleaseHolder -> {
          var encumbranceIds = unreleaseHolder.getEncumbranceIds();
          log.info("processEncumbrances:: STEP 4 - Fetching pending payments for orderId={}, encumbranceIdCount={}, encumbranceIds=[{}]",
            compPO.getId(), encumbranceIds.size(), String.join(", ", encumbranceIds));

          return transactionService.getPendingPaymentsByEncumbranceIds(unreleaseHolder.getEncumbranceIds(), requestContext)
            .onSuccess(pendingPayments -> {
              log.info("processEncumbrances:: STEP 4 RESULT - orderId={}, pendingPaymentCount={}", compPO.getId(), pendingPayments.size());
            })
            .map(unreleaseHolder::withPendingPayments);
        })
        .compose(unreleaseHolder -> {
          var encumbranceIds = unreleaseHolder.getEncumbranceIds();
          log.info("processEncumbrances:: STEP 5 - Fetching payments for orderId={}, encumbranceIdCount={}, encumbranceIds=[{}]",
            compPO.getId(), encumbranceIds.size(), String.join(", ", encumbranceIds));

          return transactionService.getPaymentsByEncumbranceIds(unreleaseHolder.getEncumbranceIds(), requestContext)
            .onSuccess(payments -> {
              log.info("processEncumbrances:: STEP 5 RESULT - orderId={}, paymentCount={}", compPO.getId(), payments.size());
            })
            .map(unreleaseHolder::withPayments);
        })
        .compose(unreleaseHolder -> {
          var encumbrances = unreleaseHolder.getEncumbrances();
          log.info("processEncumbrances:: STEP 6 - Checking if matching encumbrances for orderId={}, encumbranceCount={}",
            compPO.getId(), encumbrances.size());

          // stop if nothing needs to be done
          if (checkIfMatchingEncumbrances(compPO, encumbrances)) {
            log.info("processEncumbrances:: STEP 6 RESULT - Matching encumbrances found, EARLY EXIT for orderId={}", compPO.getId());
            return Future.succeededFuture();
          }

          log.info("processEncumbrances:: STEP 6 RESULT - No matching encumbrances, continuing for orderId={}", compPO.getId());

          // check encumbrance restrictions as in PendingToOpenEncumbranceStrategy
          // (except we use a different list of poLines/encumbrances)
          var holders = createEncumbranceRelationHolders(compPO, encumbrances);
          String holderPoLineIds = holders.stream().map(EncumbranceRelationsHolder::getPoLineId).distinct().collect(Collectors.joining(", "));
          log.info("processEncumbrances:: STEP 7 - Created encumbrance relation holders for orderId={}, holderCount={}, poLineIds=[{}]",
            compPO.getId(), holders.size(), holderPoLineIds);

          if (holders.isEmpty()) {
            log.warn("processEncumbrances:: WARNING - No holders created for orderId={}. This might indicate an issue!", compPO.getId());
          }

          return encumbranceRelationsHoldersBuilder.withFinances(holders, requestContext)
            .onSuccess(v -> log.info("processEncumbrances:: STEP 8 - Retrieved finances for holders, orderId={}", compPO.getId()))
            // use given encumbrances (withKnownTransactions) instead of retrieving them (withExistingTransactions)
            .map(v -> {
              encumbranceRelationsHoldersBuilder.withKnownTransactions(holders, encumbrances);
              log.info("processEncumbrances:: STEP 9 - Set known transactions for holders, orderId={}", compPO.getId());
              return holders;
            })
            .map(fundsDistributionService::distributeFunds)
            .onSuccess(dataHolders -> log.info("processEncumbrances:: STEP 10 - Distributed funds for orderId={}, holderCount={}",
              compPO.getId(), dataHolders.size()))
            .map(dataHolders -> {
              log.info("processEncumbrances:: STEP 11 - Checking encumbrance restrictions for orderId={}", compPO.getId());
              budgetRestrictionService.checkEncumbranceRestrictions(dataHolders);
              log.info("processEncumbrances:: STEP 11 RESULT - Restrictions passed for orderId={}", compPO.getId());
              return null;
            })
            .compose(v -> {
              log.info("processEncumbrances:: STEP 12 - Preparing encumbrances for create/update/unrelease for orderId={}", compPO.getId());

              // create missing encumbrances and unrelease existing ones
              var holder = new EncumbrancesProcessingHolder();
              var toBeCreatedHolders = createToBeCreatedHolders(holders);
              holder.withEncumbrancesForCreate(toBeCreatedHolders);

              String createPoLineIds = toBeCreatedHolders.stream()
                .map(EncumbranceRelationsHolder::getPoLineId)
                .collect(Collectors.joining(", "));
              log.info("processEncumbrances:: STEP 12a - Encumbrances to CREATE: count={}, poLineIds=[{}]",
                toBeCreatedHolders.size(), createPoLineIds);

              // only unrelease encumbrances with expended + credited + awaiting payment = 0
              var encumbrancesToUnrelease = collectAllowedEncumbrancesForUnrelease(unreleaseHolder);
              holder.withEncumbrancesForUnrelease(encumbrancesToUnrelease);

              String unreleaseEncIds = encumbrancesToUnrelease.stream().map(Transaction::getId).collect(Collectors.joining(", "));
              String unreleasePoLineIds = encumbrancesToUnrelease.stream()
                .map(e -> e.getEncumbrance().getSourcePoLineId())
                .distinct()
                .collect(Collectors.joining(", "));
              log.info("processEncumbrances:: STEP 12b - Encumbrances to UNRELEASE: count={}, encumbranceIds=[{}], poLineIds=[{}]",
                encumbrancesToUnrelease.size(), unreleaseEncIds, unreleasePoLineIds);

              // Log warning if nothing to do
              if (encumbrancesToUnrelease.isEmpty() && toBeCreatedHolders.isEmpty()) {
                log.warn("processEncumbrances:: WARNING - No encumbrances to create or unrelease for orderId={}. THIS IS THE FLOATING ISSUE!",
                  compPO.getId());
                log.warn("processEncumbrances:: Debug info - originalEncumbranceCount={}, holderCount={}, unreleaseHolderEncumbranceCount={}",
                  encumbrances.size(), holders.size(), unreleaseHolder.getEncumbrances().size());
              }

              log.info("processEncumbrances:: STEP 13 - Calling createOrUpdateEncumbrances for orderId={}", compPO.getId());
              return encumbranceService.createOrUpdateEncumbrances(holder, requestContext)
                .onSuccess(v2 -> log.info("processEncumbrances:: STEP 13 RESULT - Successfully created/updated encumbrances for orderId={}", compPO.getId()));
            });
        })
         .recover(t -> {
          log.error("processEncumbrances:: ERROR - {} for orderId={}, poNumber={}", PROCESS_ENCUMBRANCES_ERROR, compPO.getId(), compPO.getPoNumber(), t);
          throw new HttpException(HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), PROCESS_ENCUMBRANCES_ERROR +
            (t.getMessage() != null ? ": " + t.getMessage() : ""));
        })
        .onSuccess(v -> log.info("processEncumbrances:: SUCCESS - Completed for orderId={}, poNumber={}", compPO.getId(), compPO.getPoNumber()));
    }

    log.info("processEncumbrances:: SKIPPED - No fund distributions present for orderId={}", compPO.getId());
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

