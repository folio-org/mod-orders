package org.folio.service;

import static org.folio.orders.utils.ResourcePathResolver.ACQUISITION_METHODS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.FORBIDDEN_DELETE_SYSTEM_VALUE;
import static org.folio.rest.core.exceptions.ErrorCodes.FORBIDDEN_DELETE_USED_VALUE;

import org.folio.HttpStatus;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.AcquisitionMethod;
import org.folio.rest.jaxrs.model.AcquisitionMethodCollection;
import org.folio.service.orders.PurchaseOrderLineService;

import io.vertx.core.Future;

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

  public Future<AcquisitionMethodCollection> getAcquisitionMethods(int limit, int offset, String query,
      RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(query)
      .withOffset(offset)
      .withLimit(limit);
    return restClient.get(requestEntry, AcquisitionMethodCollection.class, requestContext);
  }

  public Future<AcquisitionMethod> getAcquisitionMethodById(String acquisitionMethodId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(acquisitionMethodId);
    return restClient.get(requestEntry, AcquisitionMethod.class, requestContext);
  }

  public Future<Void> saveAcquisitionMethod(AcquisitionMethod acquisitionMethod, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(acquisitionMethod.getId());
    return restClient.put(requestEntry, acquisitionMethod, requestContext);
  }

  public Future<AcquisitionMethod> createAcquisitionMethod(AcquisitionMethod acquisitionMethod,
      RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT);
    return restClient.post(requestEntry, acquisitionMethod, AcquisitionMethod.class, requestContext);
  }

  public Future<Void> deleteAcquisitionMethod(String acquisitionMethodId, RequestContext requestContext) {

    return validateDeleteOperation(acquisitionMethodId, requestContext)
      .compose(v -> {
      RequestEntry requestEntry = new RequestEntry(BY_ID_ENDPOINT).withId(acquisitionMethodId);
      return restClient.delete(requestEntry, requestContext);
    });
  }

  private Future<Void> validateDeleteOperation(String acquisitionMethodId, RequestContext requestContext) {
    return getAcquisitionMethodById(acquisitionMethodId, requestContext).compose(acquisitionMethod -> {
      boolean isSystem = AcquisitionMethod.Source.SYSTEM.equals(acquisitionMethod.getSource());
      if (!isSystem) {
        String query = String.format(PO_LINE_BY_ACQUISITION_METHOD_QUERY, acquisitionMethod.getId());
        return purchaseOrderLineService.getOrderLines(query, 0, Integer.MAX_VALUE, requestContext)
          .map(poLines -> {
            if (!poLines.isEmpty()) {
              throw new HttpException(HttpStatus.HTTP_BAD_REQUEST.toInt(), FORBIDDEN_DELETE_USED_VALUE);
            }
            return null;
          });
      } else {
        throw new HttpException(HttpStatus.HTTP_BAD_REQUEST.toInt(), FORBIDDEN_DELETE_SYSTEM_VALUE);
      }
    })
      .mapEmpty();
  }

}
