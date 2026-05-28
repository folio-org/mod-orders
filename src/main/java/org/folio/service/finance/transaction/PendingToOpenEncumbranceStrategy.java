package org.folio.service.finance.transaction;

import static java.util.stream.Collectors.groupingBy;
import static org.folio.orders.utils.FundDistributionUtils.validateFundDistributionTotal;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import lombok.extern.log4j.Log4j2;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.FundsDistributionService;
import org.folio.service.finance.budget.BudgetRestrictionService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.POLInvoiceLineRelationService;
import org.folio.service.orders.OrderWorkflowType;

import io.vertx.core.Future;

@Log4j2
public class PendingToOpenEncumbranceStrategy implements EncumbranceWorkflowStrategy {

  private static final String AND = " AND ";

  private final EncumbranceService encumbranceService;
  private final FundsDistributionService fundsDistributionService;
  private final BudgetRestrictionService budgetRestrictionService;
  private final EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder;
  private final EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder;
  private final POLInvoiceLineRelationService polInvoiceLineRelationService;
  private final InvoiceLineService invoiceLineService;

  public PendingToOpenEncumbranceStrategy(EncumbranceService encumbranceService, FundsDistributionService fundsDistributionService,
      BudgetRestrictionService budgetRestrictionService, EncumbranceRelationsHoldersBuilder encumbranceRelationsHoldersBuilder,
      EncumbrancesProcessingHolderBuilder encumbrancesProcessingHolderBuilder, POLInvoiceLineRelationService polInvoiceLineRelationService,
      InvoiceLineService invoiceLineService) {
    this.encumbranceService = encumbranceService;
    this.fundsDistributionService = fundsDistributionService;
    this.budgetRestrictionService = budgetRestrictionService;
    this.encumbranceRelationsHoldersBuilder = encumbranceRelationsHoldersBuilder;
    this.encumbrancesProcessingHolderBuilder = encumbrancesProcessingHolderBuilder;
    this.polInvoiceLineRelationService = polInvoiceLineRelationService;
    this.invoiceLineService = invoiceLineService;
  }

  @Override
  public Future<Void> processEncumbrances(CompositePurchaseOrder compPO, CompositePurchaseOrder poAndLinesFromStorage,
                                          RequestContext requestContext) {
    return prepareProcessEncumbrancesAndValidate(compPO, poAndLinesFromStorage, requestContext)
      .map(encumbrancesProcessingHolderBuilder::distributeHoldersByOperation)
      .compose(holder -> polInvoiceLineRelationService.manageInvoiceRelation(holder, requestContext))
      .compose(holder -> encumbranceService.createOrUpdateEncumbrances(holder, requestContext));
  }

  @Override
  public Future<List<EncumbranceRelationsHolder>> prepareProcessEncumbrancesAndValidate(CompositePurchaseOrder compPO,
                                                                                        CompositePurchaseOrder poAndLinesFromStorage, RequestContext requestContext) {
    validateFundDistributionTotal(compPO.getPoLines());
    List<EncumbranceRelationsHolder> holders = encumbranceRelationsHoldersBuilder.buildBaseHolders(compPO);
    return encumbranceRelationsHoldersBuilder.withFinances(holders, requestContext)
      .map(v -> getFiscalYearId(holders, compPO))
      .compose(fiscalYearId -> withInvoiceWithReleaseEncumbrance(fiscalYearId, holders, requestContext))
      .compose(v -> encumbranceRelationsHoldersBuilder.withExistingTransactions(holders, poAndLinesFromStorage, requestContext))
      .map(v -> fundsDistributionService.distributeFunds(holders))
      .map(dataHolders -> {
        budgetRestrictionService.checkEncumbranceRestrictions(dataHolders);
        return null;
      })
      .map(v -> holders);
  }

  @Override
  public OrderWorkflowType getStrategyName() {
    return OrderWorkflowType.PENDING_TO_OPEN;
  }

  private String getFiscalYearId(List<EncumbranceRelationsHolder> holders, CompositePurchaseOrder compPO) {
    String fiscalYearId = holders.stream()
      .map(EncumbranceRelationsHolder::getCurrentFiscalYearId)
      .filter(Objects::nonNull)
      .findFirst()
      .orElse(null);
    if (fiscalYearId != null) {
      compPO.setFiscalYearId(fiscalYearId);
    }
    return fiscalYearId;
  }

  private Future<Void> withInvoiceWithReleaseEncumbrance(String fiscalYearId, List<EncumbranceRelationsHolder> holders, RequestContext requestContext) {
    if (holders.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> polIds = holders.stream()
      .map(EncumbranceRelationsHolder::getPoLineId)
      .distinct()
      .toList();
    return invoiceLineService.getInvoiceLinesByIdsAndQuery(polIds,
        ids -> queryToGetRelatedInvoiceLinesByPoLineIds(fiscalYearId, polIds), requestContext)
      .map(invoiceLines -> {
        Map<String, List<InvoiceLine>> polIdToInvoiceLine = invoiceLines.stream()
          .collect(groupingBy(InvoiceLine::getPoLineId));
        holders.forEach(holder -> {
          List<InvoiceLine> relatedInvoiceLines = polIdToInvoiceLine.get(holder.getPoLineId());
          if (relatedInvoiceLines == null) {
            holder.withInvoiceWithReleaseEncumbrance(false);
          } else {
            holder.withInvoiceWithReleaseEncumbrance(relatedInvoiceLines.stream().anyMatch(InvoiceLine::getReleaseEncumbrance));
          }
        });
        return null;
      });
  }

  private String queryToGetRelatedInvoiceLinesByPoLineIds(String fiscalYearId, List<String> poLineIds) {
    return "invoiceLineStatus == (Approved OR Paid)" + AND +
      "releaseEncumbrance == true" + AND +
      (fiscalYearId == null ? "" : "invoices.fiscalYearId == " + fiscalYearId + AND) +
      convertIdsToCqlQuery(poLineIds, "poLineId");
  }
}
