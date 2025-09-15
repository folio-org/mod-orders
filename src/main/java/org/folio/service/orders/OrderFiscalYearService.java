package org.folio.service.orders;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

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
      return Future.succeededFuture(buildResultHolder(allFiscalYears, List.of()));
    }

    return fundService.getAllFunds(distinctFundIds, requestContext)
      .compose(funds -> {
        Set<String> distinctLedgerIds = funds.stream()
          .map(Fund::getLedgerId)
          .filter(Objects::nonNull)
          .collect(Collectors.toSet());

        if (distinctLedgerIds.isEmpty()) {
          return Future.succeededFuture(buildResultHolder(allFiscalYears, List.of()));
        }

        List<Future<FiscalYear>> currentFiscalYearFutures = distinctLedgerIds.stream()
          .map(ledgerId -> fiscalYearService.getCurrentFiscalYear(ledgerId, requestContext)
            .recover(throwable -> Future.succeededFuture(null)))
          .toList();

        return collectResultsOnSuccess(currentFiscalYearFutures)
          .compose(currentFiscalYears -> Future.succeededFuture(buildResultHolder(allFiscalYears, currentFiscalYears)));
      });
  }

  private FiscalYearsHolder buildResultHolder(List<FiscalYear> availableFiscalYears, List<FiscalYear> currentFiscalYears) {
    Comparator<FiscalYear> nameComparator = Comparator.comparing(FiscalYear::getName).reversed();

    Set<String> currentFiscalYearIds = currentFiscalYears.stream()
      .filter(Objects::nonNull)
      .map(FiscalYear::getId)
      .collect(Collectors.toSet());

    List<FiscalYear> filteredAvailableFiscalYears = availableFiscalYears.stream()
      .filter(fy -> !currentFiscalYearIds.contains(fy.getId()))
      .toList();

    return new FiscalYearsHolder()
      .withCurrent(currentFiscalYears.stream()
        .filter(Objects::nonNull)
        .sorted(nameComparator)
        .toList())
      .withPrevious(filteredAvailableFiscalYears.stream()
        .filter(Objects::nonNull)
        .sorted(nameComparator)
        .toList());
  }
}
