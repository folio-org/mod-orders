package org.folio.service.invoice;

import io.vertx.core.json.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.models.PoLineInvoiceLineHolder;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.concurrent.CompletableFuture;

public class POLInvoiceLineRelationService {
  private final PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder;

  public POLInvoiceLineRelationService(PoLineInvoiceLineHolderBuilder poLineInvoiceLineHolderBuilder) {
    this.poLineInvoiceLineHolderBuilder = poLineInvoiceLineHolderBuilder;
  }

  public CompletableFuture<Void> prepareRelatedInvoiceLines(PoLineInvoiceLineHolder holder, RequestContext requestContext) {
    CompositePoLine compositePoLine = holder.getPoLine();
    JsonObject poLineFromStorage = holder.getPoLineFromStorage();
    if (CollectionUtils.isEqualCollection(compositePoLine.getFundDistribution(), poLineFromStorage.mapTo(PoLine.class).getFundDistribution())) {
      return CompletableFuture.completedFuture(null);
    } else {
      return poLineInvoiceLineHolderBuilder.buildHolder(compositePoLine, poLineFromStorage, requestContext)
        .thenAccept(poLineInvoiceLineHolder -> {
          holder.withOpenOrReviewedInvoiceLines(poLineInvoiceLineHolder.getOpenOrReviewedInvoiceLines());
          holder.withCurrentYearPaidInvoiceLines(poLineInvoiceLineHolder.getCurrentYearPaidInvoiceLines());
        });
    }
  }

}
