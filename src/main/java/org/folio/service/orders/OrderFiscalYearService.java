package org.folio.service.orders;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.google.common.collect.Sets;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.FiscalYearsHolder;
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
      .compose(order -> getFiscalYearsFromTransactions(orderId, requestContext))
      .compose(fiscalYearIds -> processFiscalYears(fiscalYearIds, requestContext));
  }

  private Future<List<String>> getFiscalYearsFromTransactions(String orderId, RequestContext requestContext) {
    String query = "transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s".formatted(orderId);
    return transactionService.getTransactions(query, requestContext)
      .map(transactions -> {
        return transactions.stream()
          .map(Transaction::getFiscalYearId)
          .filter(Objects::nonNull)
          .distinct()
          .toList();
      });
  }

  private Future<FiscalYearsHolder> processFiscalYears(List<String> fiscalYearIds, RequestContext requestContext) {
    if (fiscalYearIds.isEmpty()) {
      return Future.succeededFuture(new FiscalYearsHolder());
    }

    return fiscalYearService.getAllFiscalYears(fiscalYearIds, requestContext)
      .compose(fiscalYears -> {
        String currentFiscalYearId = fiscalYearService.extractCurrentFiscalYearId(fiscalYears);

        if (StringUtils.isNotBlank(currentFiscalYearId)) {
          return Future.succeededFuture(buildResultHolder(fiscalYears, Sets.newHashSet(currentFiscalYearId)));
        }

        return addCurrentFiscalYearIfMissing(fiscalYearIds.getFirst(), fiscalYears, requestContext);
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
