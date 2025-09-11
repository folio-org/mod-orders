package org.folio.service.orders;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.FiscalYearsHolder;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.TransactionService;

import io.vertx.core.Future;

import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class OrderFiscalYearService {

  private final TransactionService transactionService;
  private final FiscalYearService fiscalYearService;
  private final PurchaseOrderStorageService purchaseOrderStorageService;

  public OrderFiscalYearService(TransactionService transactionService, FiscalYearService fiscalYearService,
                               PurchaseOrderStorageService purchaseOrderStorageService) {
    this.transactionService = transactionService;
    this.fiscalYearService = fiscalYearService;
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
      return Future.succeededFuture(buildResultHolder(allFiscalYears, Set.of()));
    }

    List<Future<FiscalYear>> currentFiscalYearFutures = distinctFundIds.stream()
      .map(fundId -> fiscalYearService.getCurrentFiscalYearByFundId(fundId, requestContext)
        .recover(throwable -> Future.succeededFuture(null)))
      .toList();

    return collectResultsOnSuccess(currentFiscalYearFutures)
      .compose(currentFiscalYears -> {
        Set<String> currentFiscalYearIds = currentFiscalYears.stream()
          .filter(Objects::nonNull)
          .map(FiscalYear::getId)
          .collect(Collectors.toSet());

        // If no current fiscal years found from funds, try to add current fiscal year from series
        if (currentFiscalYearIds.isEmpty() && CollectionUtils.isNotEmpty(allFiscalYears)) {
          return addCurrentFiscalYearIfMissing(allFiscalYears.getFirst().getId(), allFiscalYears, requestContext);
        }

        return Future.succeededFuture(buildResultHolder(allFiscalYears, currentFiscalYearIds));
      });
  }

  private Future<FiscalYearsHolder> addCurrentFiscalYearIfMissing(String fiscalYearId, List<FiscalYear> fiscalYears,
                                                                  RequestContext requestContext) {
    return fiscalYearService.getCurrentFYForSeriesByFYId(fiscalYearId, requestContext)
      .compose(currentFiscalYearId -> {
        if (currentFiscalYearId == null) {
          return Future.succeededFuture(buildResultHolder(fiscalYears, Set.of()));
        }
        boolean isAlreadyPresent = fiscalYears.stream()
          .anyMatch(fy -> currentFiscalYearId.equals(fy.getId()));
        if (isAlreadyPresent) {
          return Future.succeededFuture(buildResultHolder(fiscalYears, Set.of(currentFiscalYearId)));
        }

        // Add the missing current fiscal year and mark it as current
        return fiscalYearService.getFiscalYearById(currentFiscalYearId, requestContext)
          .map(currentFiscalYear -> {
            List<FiscalYear> updatedFiscalYears = new ArrayList<>(fiscalYears);
            updatedFiscalYears.add(currentFiscalYear);
            return buildResultHolder(updatedFiscalYears, Set.of(currentFiscalYearId));
          });
      });
  }

  private FiscalYearsHolder buildResultHolder(List<FiscalYear> availableFiscalYears, Set<String> currentFiscalYearIds) {
    Comparator<FiscalYear> nameComparator = Comparator.comparing(FiscalYear::getName).reversed();

    List<FiscalYear> currentFiscalYears = availableFiscalYears.stream()
      .filter(fy -> currentFiscalYearIds.contains(fy.getId()))
      .sorted(nameComparator)
      .toList();

    List<FiscalYear> previousFiscalYears = availableFiscalYears.stream()
      .filter(fy -> !currentFiscalYearIds.contains(fy.getId()))
      .sorted(nameComparator)
      .toList();

    return new FiscalYearsHolder()
      .withCurrent(currentFiscalYears)
      .withPrevious(previousFiscalYears);
  }
}
