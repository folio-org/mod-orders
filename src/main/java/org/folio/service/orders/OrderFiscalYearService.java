package org.folio.service.orders;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.FiscalYearsHolder;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.TransactionService;

import io.vertx.core.Future;

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
      .compose(order -> {
        Set<String> orderFundIds = extractOrderFundIds(order);

        var query = "transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s"
          .formatted(orderId);
        return transactionService.getTransactions(query, requestContext)
          .compose(transactions -> {
            Set<String> currentFiscalYearIds = orderFundIds.isEmpty() ?
              Set.of() : getCurrentFiscalYearIds(orderFundIds, transactions);

            List<String> poFiscalYearIds = transactions.stream()
              .map(Transaction::getFiscalYearId)
              .filter(Objects::nonNull)
              .distinct()
              .toList();

            if (poFiscalYearIds.isEmpty()) {
              return Future.succeededFuture(new FiscalYearsHolder());
            }

            return fiscalYearService.getAllFiscalYears(poFiscalYearIds, requestContext)
              .compose(poFiscalYears -> {
                if (currentFiscalYearIds.isEmpty()) {
                  return addCurrentFiscalYearIfMissing(poFiscalYearIds.getFirst(), poFiscalYears, requestContext)
                    .map(availableFiscalYears -> {
                      Set<String> allCurrentFiscalYearIds = new HashSet<>();

                      // Any fiscal year added by addCurrentFiscalYearIfMissing should go to current
                      for (FiscalYear fy : availableFiscalYears) {
                        if (!poFiscalYears.contains(fy)) {
                          allCurrentFiscalYearIds.add(fy.getId());
                        }
                      }

                      return buildResultHolder(availableFiscalYears, allCurrentFiscalYearIds);
                    });
                } else {
                  return Future.succeededFuture(buildResultHolder(poFiscalYears, currentFiscalYearIds));
                }
              });
          });
      });
  }

  private Set<String> extractOrderFundIds(CompositePurchaseOrder order) {
    return order.getPoLines().stream()
      .flatMap(poLine -> poLine.getFundDistribution().stream())
      .map(FundDistribution::getFundId)
      .filter(Objects::nonNull)
      .collect(Collectors.toSet());
  }

  private Set<String> getCurrentFiscalYearIds(Set<String> orderFundIds, List<Transaction> allTransactions) {
    List<Transaction> currentTransactions = allTransactions.stream()
      .filter(transaction -> orderFundIds.contains(transaction.getToFundId()))
      .toList();

    return currentTransactions.stream()
      .map(Transaction::getFiscalYearId)
      .filter(Objects::nonNull)
      .collect(Collectors.toSet());
  }

  private FiscalYearsHolder buildResultHolder(List<FiscalYear> availableFiscalYears, Set<String> currentFiscalYearIds) {
    List<FiscalYear> currentFiscalYears = availableFiscalYears.stream()
      .filter(fy -> currentFiscalYearIds.contains(fy.getId()))
      .sorted(Comparator.comparing(FiscalYear::getName).reversed())
      .toList();

    List<FiscalYear> previousFiscalYears = availableFiscalYears.stream()
      .filter(fy -> !currentFiscalYearIds.contains(fy.getId()))
      .sorted(Comparator.comparing(FiscalYear::getName).reversed())
      .toList();

    FiscalYearsHolder result = new FiscalYearsHolder();
    result.setCurrent(currentFiscalYears);
    result.setPrevious(previousFiscalYears);
    return result;
  }

  private Future<List<FiscalYear>> addCurrentFiscalYearIfMissing(String fiscalYearId, List<FiscalYear> fiscalYears, RequestContext requestContext) {
    return fiscalYearService.getCurrentFYForSeriesByFYId(fiscalYearId, requestContext)
      .compose(currentFiscalYearId -> {
        if (currentFiscalYearId == null || isCurrentFiscalYearPresent(fiscalYears, currentFiscalYearId)) {
          return Future.succeededFuture(fiscalYears);
        }

        return fiscalYearService.getFiscalYearById(currentFiscalYearId, requestContext)
          .map(currentFiscalYear -> {
            var updatedFiscalYears = new ArrayList<>(fiscalYears);
            updatedFiscalYears.add(currentFiscalYear);
            return updatedFiscalYears;
          });
      });
  }

  private boolean isCurrentFiscalYearPresent(List<FiscalYear> fiscalYears, String currentFiscalYearId) {
    return fiscalYears.stream()
      .anyMatch(fy -> currentFiscalYearId.equals(fy.getId()));
  }
}
