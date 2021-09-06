package org.folio.service.finance.transaction;

import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public abstract class BaseEncumbranceWorkflowStrategy implements EncumbranceWorkflowStrategy {
  private final EncumbranceService encumbranceService;

  protected BaseEncumbranceWorkflowStrategy(EncumbranceService encumbranceService) {
    this.encumbranceService = encumbranceService;
  }

  public CompletableFuture<List<Transaction>> getOrderEncumbrancesForCurrentFy(List<EncumbranceRelationsHolder> encumbranceRelationsHolders,
                                                                    RequestContext requestContext) {
    return FolioVertxCompletableFuture.from(requestContext.getContext(), completedFuture(encumbranceRelationsHolders))
                      .thenApply(ehList -> ehList.stream().collect(groupingBy(EncumbranceRelationsHolder::getCurrentFiscalYearId,
                        mapping(EncumbranceRelationsHolder::getPoLine, toList()))))
                      .thenCompose(poLinesByCurrentFy -> getEncumbrancesByPoLinesFromCurrentFy(poLinesByCurrentFy, requestContext))
                      .thenApply(trs -> trs.stream().flatMap(Collection::stream).collect(Collectors.toList()));
  }

  public CompletableFuture<List<List<Transaction>>> getEncumbrancesByPoLinesFromCurrentFy(
    Map<String, List<CompositePoLine>> poLinesByFy, RequestContext requestContext) {
    return collectResultsOnSuccess(poLinesByFy.entrySet()
      .stream()
      .map(entry -> encumbranceService.getCurrentPoLinesEncumbrances(entry.getValue(), entry.getKey(), requestContext))
      .collect(toList()));
  }
}
