package org.folio.service;

import static org.folio.orders.utils.ResourcePathResolver.ACQUISITION_METHODS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.FORBIDDEN_DELETE_SYSTEM_VALUE;
import static org.folio.rest.core.exceptions.ErrorCodes.FORBIDDEN_DELETE_USED_VALUE;

import java.util.concurrent.CompletableFuture;

import org.folio.HttpStatus;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.AcquisitionMethod;
import org.folio.rest.jaxrs.model.AcquisitionMethodCollection;
import org.folio.service.orders.PurchaseOrderLineService;

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

    return isDeletePossible(acquisitionMethodId, requestContext).thenCompose(aVoid -> {
      RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(acquisitionMethodId);
      return restClient.delete(requestEntry, requestContext);
    });
  }

  private CompletableFuture<Void> isDeletePossible(String acquisitionMethodId, RequestContext requestContext) {
    return getAcquisitionMethodById(acquisitionMethodId, requestContext).thenCompose(acquisitionMethod -> {
      boolean isSystem = AcquisitionMethod.Source.SYSTEM.equals(acquisitionMethod.getSource());
      if (!isSystem) {
        String query = String.format(PO_LINE_BY_ACQUISITION_METHOD_QUERY, acquisitionMethod.getId());
        return purchaseOrderLineService.getOrderLines(query, 0, Integer.MAX_VALUE, requestContext)
           .thenAccept(poLines -> {
             if (!poLines.isEmpty()) {
               throw new HttpException(HttpStatus.HTTP_BAD_REQUEST.toInt(), FORBIDDEN_DELETE_USED_VALUE);
             }
           });
      } else {
        throw new HttpException(HttpStatus.HTTP_BAD_REQUEST.toInt(), FORBIDDEN_DELETE_SYSTEM_VALUE);
      }
    });
  }

}
