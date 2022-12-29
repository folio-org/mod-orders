package org.folio.service.invoice;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.models.PoLineInvoiceLineHolder;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.FundDistribution;

import io.vertx.core.Future;

import java.util.List;
import java.util.stream.Collectors;

public class POLInvoiceLineRelationService {
  private final InvoiceLineService invoiceLineService;
  private final PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder;

  public POLInvoiceLineRelationService(InvoiceLineService invoiceLineService, PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder) {
    this.invoiceLineService = invoiceLineService;
    this.poLineInvoiceLineHolderBuilder = poLineInvoiceLineHolderBuilder;
  }

  public Future<Void> prepareRelatedInvoiceLines(PoLineInvoiceLineHolder holder, RequestContext requestContext) {
    if (isFundDistributionNotChanged(holder)) {
      return Future.succeededFuture();
    } else {
      return poLineInvoiceLineHolderBuilder.buildHolder(holder.getPoLineFromRequest(), holder.getPoLineFromStorage(), requestContext)
        .map(poLineInvoiceLineHolder -> {
          holder.withOpenOrReviewedInvoiceLines(poLineInvoiceLineHolder.getOpenOrReviewedInvoiceLines());
          holder.withPaidOrCancelledInvoiceLines(poLineInvoiceLineHolder.getPaidOrCancelledInvoiceLines());
          return null;
        });
    }
  }

  public Future<EncumbrancesProcessingHolder> removePaidOrCancelledInvoiceEncumbrancesFromDeletion(EncumbrancesProcessingHolder encumbrancesProcessingHolder, RequestContext requestContext) {
    List<EncumbranceRelationsHolder> holders = encumbrancesProcessingHolder.getEncumbrancesForDelete();
    List<String> poLineIds = holders.stream().map(EncumbranceRelationsHolder::getOldEncumbrance)
      .map(Transaction::getEncumbrance).map(Encumbrance::getSourcePoLineId).distinct().collect(Collectors.toList());
    return invoiceLineService.getInvoiceLinesByOrderLineIds(poLineIds, requestContext)
      .map(PoLineInvoiceLineHolderBuilder::getInvoiceLinesWithTransactions)
      .map(invoiceLines -> invoiceLines.stream().flatMap(invoiceLine -> invoiceLine.getFundDistributions().stream())
        .map(org.folio.rest.acq.model.invoice.FundDistribution::getEncumbrance).collect(Collectors.toSet()))
      .map(encumbranceIds -> holders.stream().filter(holder -> !encumbranceIds.contains(holder.getOldEncumbrance().getId())).collect(Collectors.toList()))
      .map(encumbrancesProcessingHolder::withEncumbrancesForDelete);
  }


  public Future<Void> updateInvoiceLineReference(PoLineInvoiceLineHolder holder, RequestContext requestContext) {
    if (isFundDistributionNotChanged(holder)) {
      return Future.succeededFuture();
    }

    List<org.folio.rest.acq.model.invoice.FundDistribution> newFundDistribution = holder.getPoLineFromRequest().getFundDistribution()
      .stream().map(POLInvoiceLineRelationService::convertToInvoiceFundDistribution).collect(Collectors.toList());
    List<InvoiceLine> invoiceLines = holder.getOpenOrReviewedInvoiceLines()
      .stream().map(invoiceLine -> invoiceLine.withFundDistributions(newFundDistribution)).collect(Collectors.toList());
    return invoiceLineService.saveInvoiceLines(invoiceLines, requestContext);
  }

  private static org.folio.rest.acq.model.invoice.FundDistribution convertToInvoiceFundDistribution(FundDistribution fundDistribution) {
    return new org.folio.rest.acq.model.invoice.FundDistribution().withCode(fundDistribution.getCode()).withEncumbrance(fundDistribution.getEncumbrance())
      .withFundId(fundDistribution.getFundId()).withValue(fundDistribution.getValue()).withExpenseClassId(fundDistribution.getExpenseClassId())
      .withDistributionType(org.folio.rest.acq.model.invoice.FundDistribution.DistributionType.fromValue(fundDistribution.getDistributionType().value()));
  }

  private boolean isFundDistributionNotChanged(PoLineInvoiceLineHolder holder) {
    return CollectionUtils.isEqualCollection(getFundIdsFromFundDistribution(holder.getPoLineFromRequest().getFundDistribution()),
      getFundIdsFromFundDistribution(holder.getPoLineFromStorage().getFundDistribution()));
  }

  private List<String> getFundIdsFromFundDistribution(List<FundDistribution> fundDistributions) {
    return fundDistributions.stream().map(FundDistribution::getFundId).collect(Collectors.toList());
  }

}
