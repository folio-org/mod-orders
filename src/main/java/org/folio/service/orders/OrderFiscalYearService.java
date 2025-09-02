package org.folio.service.orders;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.FiscalYearCollection;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.TransactionService;

import io.vertx.core.Future;

public class OrderFiscalYearService {

  private final TransactionService transactionService;
  private final FiscalYearService fiscalYearService;

  public OrderFiscalYearService(TransactionService transactionService, FiscalYearService fiscalYearService) {
    this.transactionService = transactionService;
    this.fiscalYearService = fiscalYearService;
  }

  public Future<FiscalYearCollection> getAvailableFiscalYears(String orderId, RequestContext requestContext) {
    var query = "transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s"
      .formatted(orderId);

    return transactionService.getTransactions(query, requestContext)
      .map(transactions -> transactions.stream()
        .map(Transaction::getFiscalYearId)
        .filter(Objects::nonNull)
        .distinct()
        .toList())
      .compose(fiscalYearIds -> {
        if (fiscalYearIds.isEmpty()) {
          return Future.succeededFuture(new FiscalYearCollection()
            .withFiscalYears(java.util.List.of())
            .withTotalRecords(0));
        }

        return fiscalYearService.getAllFiscalYears(fiscalYearIds, requestContext)
          .compose(fiscalYears -> addCurrentFiscalYearIfMissing(fiscalYearIds.getFirst(), fiscalYears, requestContext));
      });
  }

  private Future<FiscalYearCollection> addCurrentFiscalYearIfMissing(String fiscalYearId, List<FiscalYear> fiscalYears, RequestContext requestContext) {
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
      })
      .map(this::sortAndCreateCollection);
  }

  private FiscalYearCollection sortAndCreateCollection(List<FiscalYear> fiscalYears) {
    var sortedFiscalYears = fiscalYears.stream()
      .sorted(java.util.Comparator.comparing(FiscalYear::getName))
      .toList();

    return new FiscalYearCollection()
      .withFiscalYears(sortedFiscalYears)
      .withTotalRecords(sortedFiscalYears.size());
  }

  private boolean isCurrentFiscalYearPresent(List<FiscalYear> fiscalYears, String currentFiscalYearId) {
    return fiscalYears.stream()
      .anyMatch(fy -> currentFiscalYearId.equals(fy.getId()));
  }
}
