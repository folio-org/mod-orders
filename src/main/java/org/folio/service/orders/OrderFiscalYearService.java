package org.folio.service.orders;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.FiscalYearsHolder;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.transaction.TransactionService;

import io.vertx.core.Future;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class OrderFiscalYearService {

  private final TransactionService transactionService;
  private final FiscalYearService fiscalYearService;
  private final FundService fundService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;

  public OrderFiscalYearService(TransactionService transactionService, FiscalYearService fiscalYearService,
                               FundService fundService, PurchaseOrderStorageService purchaseOrderStorageService) {
    this.transactionService = transactionService;
    this.fiscalYearService = fiscalYearService;
    this.fundService = fundService;
    this.purchaseOrderStorageService = purchaseOrderStorageService;
  }

  public Future<FiscalYearsHolder> getAvailableFiscalYears(String orderId, RequestContext requestContext) {
    return purchaseOrderStorageService.getCompositeOrderById(orderId, requestContext)
      .compose(order -> getTransactionsAndProcessFiscalYears(orderId, requestContext));
  }

  private Future<FiscalYearsHolder> getTransactionsAndProcessFiscalYears(String orderId, RequestContext requestContext) {
    String query = "transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s".formatted(orderId);
    return transactionService.getTransactions(query, requestContext)
      .compose(transactions -> {
        List<String> fiscalYearIds = transactions.stream()
          .map(Transaction::getFiscalYearId)
          .filter(Objects::nonNull)
          .distinct()
          .toList();

        if (fiscalYearIds.isEmpty()) {
          return Future.succeededFuture(new FiscalYearsHolder());
        }

        return fiscalYearService.getAllFiscalYears(fiscalYearIds, requestContext)
          .compose(fiscalYears -> getCurrentFiscalYearsFromFunds(transactions, fiscalYears, requestContext));
      });
  }

  private Future<FiscalYearsHolder> getCurrentFiscalYearsFromFunds(List<Transaction> transactions, List<FiscalYear> allFiscalYears, RequestContext requestContext) {
    Set<String> distinctFundIds = transactions.stream()
      .map(Transaction::getFromFundId)
      .filter(Objects::nonNull)
      .collect(Collectors.toSet());

    if (distinctFundIds.isEmpty()) {
      return Future.succeededFuture(buildResultHolder(allFiscalYears, List.of(), List.of()));
    }

    return fundService.getAllFunds(distinctFundIds, requestContext)
      .compose(funds -> {
        Set<String> distinctLedgerIds = funds.stream()
          .map(Fund::getLedgerId)
          .filter(Objects::nonNull)
          .collect(Collectors.toSet());

        if (distinctLedgerIds.isEmpty()) {
          return Future.succeededFuture(buildResultHolder(allFiscalYears, List.of(), List.of()));
        }

        List<Future<FiscalYear>> currentFiscalYearFutures = distinctLedgerIds.stream()
          .map(ledgerId -> fiscalYearService.getCurrentFiscalYear(ledgerId, requestContext)
            .recover(throwable -> Future.succeededFuture(null)))
          .toList();

        return collectResultsOnSuccess(currentFiscalYearFutures)
          .compose(currentResults -> {
            List<FiscalYear> currentFiscalYears = currentResults.stream()
              .filter(Objects::nonNull)
              .toList();

            if (CollectionUtils.isEqualCollection(allFiscalYears, currentFiscalYears)) {
              return Future.succeededFuture(buildResultHolder(allFiscalYears, currentFiscalYears, List.of()));
            }

            List<Future<FiscalYear>> plannedFiscalYearFutures = distinctLedgerIds.stream()
              .map(ledgerId -> fiscalYearService.getPlannedFiscalYear(ledgerId, requestContext))
              .toList();
            return collectResultsOnSuccess(plannedFiscalYearFutures)
              .compose(plannedResults -> {
                List<FiscalYear> plannedFiscalYears = plannedResults.stream()
                  .filter(Objects::nonNull)
                  .toList();

                return Future.succeededFuture(buildResultHolder(allFiscalYears, currentFiscalYears, plannedFiscalYears));
              });
          });
      });
  }

  private FiscalYearsHolder buildResultHolder(List<FiscalYear> availableFiscalYears, List<FiscalYear> currentFiscalYears, List<FiscalYear> plannedFiscalYears) {
    Comparator<FiscalYear> nameComparator = Comparator.comparing(FiscalYear::getName).reversed();

    Set<String> plannedFiscalYearIds = plannedFiscalYears.stream()
      .map(FiscalYear::getId)
      .collect(Collectors.toSet());

    List<FiscalYear> mutableCurrentFiscalYears = new ArrayList<>(currentFiscalYears);

    // Add planned fiscal years that exist in available fiscal years to current
    for (FiscalYear availableFy : availableFiscalYears) {
      if (plannedFiscalYearIds.contains(availableFy.getId())) {
        mutableCurrentFiscalYears.add(availableFy);
      }
    }

    Set<String> currentFiscalYearIds = mutableCurrentFiscalYears.stream()
      .map(FiscalYear::getId)
      .collect(Collectors.toSet());

    List<FiscalYear> filteredAvailableFiscalYears = availableFiscalYears.stream()
      .filter(fy -> !currentFiscalYearIds.contains(fy.getId()))
      .toList();

    return new FiscalYearsHolder()
      .withCurrent(mutableCurrentFiscalYears.stream()
        .sorted(nameComparator)
        .toList())
      .withPrevious(filteredAvailableFiscalYears.stream()
        .sorted(nameComparator)
        .toList());
  }
}
