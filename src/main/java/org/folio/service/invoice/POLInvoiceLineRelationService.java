package org.folio.service.invoice;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.PoLineInvoiceLineHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.PoLine;

import io.vertx.core.Future;

import java.util.List;
import java.util.stream.Collectors;

public class POLInvoiceLineRelationService {
  private final PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder;

  public POLInvoiceLineRelationService(PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder) {
    this.poLineInvoiceLineHolderBuilder = poLineInvoiceLineHolderBuilder;
  }

  public Future<Void> prepareRelatedInvoiceLines(PoLineInvoiceLineHolder holder, RequestContext requestContext) {
    CompositePoLine compositePoLine = holder.getPoLineFromRequest();
    PoLine poLineFromStorage = holder.getPoLineFromStorage();
    if (CollectionUtils.isEqualCollection(getFundIdFromFundDistribution(compositePoLine.getFundDistribution()),
      getFundIdFromFundDistribution(poLineFromStorage.getFundDistribution()))) {
      return Future.succeededFuture();
    } else {
      return poLineInvoiceLineHolderBuilder.buildHolder(compositePoLine, poLineFromStorage, requestContext)
        .map(poLineInvoiceLineHolder -> {
          holder.withOpenOrReviewedInvoiceLines(poLineInvoiceLineHolder.getOpenOrReviewedInvoiceLines());
          holder.withCurrentYearPaidOrCancelledInvoiceLines(poLineInvoiceLineHolder.getCurrentYearPaidOrCancelledInvoiceLines());
          return null;
        });
    }
  }

  private List<String> getFundIdFromFundDistribution(List<FundDistribution> fundDistributions) {
    return fundDistributions.stream().map(FundDistribution::getFundId).collect(Collectors.toList());
  }

}
