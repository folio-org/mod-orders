package org.folio.service;

import static org.folio.orders.utils.ResourcePathResolver.ACQUISITION_METHODS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.AcquisitionMethod;
import org.folio.rest.jaxrs.model.AcquisitionMethodCollection;
import org.folio.service.orders.PurchaseOrderLineService;
import org.springframework.util.CollectionUtils;

public class AcquisitionMethodsService {
  private static final String ENDPOINT = resourcesPath(ACQUISITION_METHODS);
  private static final String BY_ID_ENDPOINT = ENDPOINT + "/{id}";
  private static final String PO_LINE_BY_ACQUISITION_METHOD_QUERY = "acquisitionMethod==%s";

  private final RestClient restClient;
  private final PurchaseOrderLineService purchaseOrderLineService;

  public AcquisitionMethodsService(RestClient restClient, PurchaseOrderLineService purchaseOrderLineService) {
    this.restClient = restClient;
    this.purchaseOrderLineService = purchaseOrderLineService;
  }

  public CompletableFuture<AcquisitionMethodCollection> getAcquisitionMethods(int limit, int offset, String query,
      RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withOffset(offset)
      .withLimit(limit);
    return restClient.get(requestEntry, requestContext, AcquisitionMethodCollection.class);
  }

  public CompletableFuture<AcquisitionMethod> getAcquisitionMethodById(String acquisitionMethodId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(acquisitionMethodId);
    return restClient.get(requestEntry, requestContext, AcquisitionMethod.class);
  }

  public CompletableFuture<Void> saveAcquisitionMethod(AcquisitionMethod acquisitionMethod, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(acquisitionMethod.getId());
    return restClient.put(requestEntry, acquisitionMethod, requestContext);
  }

  public CompletableFuture<AcquisitionMethod> createAcquisitionMethod(AcquisitionMethod acquisitionMethod,
      RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT);
    return restClient.post(requestEntry, acquisitionMethod, requestContext, AcquisitionMethod.class);
  }

  public CompletableFuture<Void> deleteAcquisitionMethod(String acquisitionMethodId, RequestContext requestContext) {

    return isDeletePossible(acquisitionMethodId, requestContext).thenCompose(poLines -> {
      RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(acquisitionMethodId);
      return restClient.delete(requestEntry, requestContext);
    });
  }

  private CompletableFuture<Boolean> isDeletePossible(String acquisitionMethodId, RequestContext requestContext) {
    return getAcquisitionMethodById(acquisitionMethodId, requestContext).thenCompose(acquisitionMethod -> {
      if (!acquisitionMethod.getSource().equals(AcquisitionMethod.Source.SYSTEM)) {
        String query = String.format(PO_LINE_BY_ACQUISITION_METHOD_QUERY, acquisitionMethod.getValue());
        return purchaseOrderLineService.getOrderLines(query, 0, Integer.MAX_VALUE, requestContext)
                                       .thenApply(CollectionUtils::isEmpty);
      }
      return CompletableFuture.completedFuture(false);
    });
  }

}
