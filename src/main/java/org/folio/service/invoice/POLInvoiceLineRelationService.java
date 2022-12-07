package org.folio.service.invoice;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.PoLineInvoiceLineHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.PoLine;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class POLInvoiceLineRelationService {
  private final PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder;

  public POLInvoiceLineRelationService(PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder) {
    this.poLineInvoiceLineHolderBuilder = poLineInvoiceLineHolderBuilder;
  }

  public Future<Void> prepareRelatedInvoiceLines(PoLineInvoiceLineHolder holder, RequestContext requestContext) {
    CompositePoLine compositePoLine = holder.getPoLine();
    JsonObject poLineFromStorage = holder.getPoLineFromStorage();
    if (CollectionUtils.isEqualCollection(compositePoLine.getFundDistribution(), poLineFromStorage.mapTo(PoLine.class).getFundDistribution())) {
      return Future.succeededFuture();
    } else {
      return poLineInvoiceLineHolderBuilder.buildHolder(compositePoLine, poLineFromStorage, requestContext)
        .map(poLineInvoiceLineHolder -> {
          holder.withOpenOrReviewedInvoiceLines(poLineInvoiceLineHolder.getOpenOrReviewedInvoiceLines());
          holder.withCurrentYearPaidInvoiceLines(poLineInvoiceLineHolder.getCurrentYearPaidInvoiceLines());
          return null;
        });
    }
  }

}
