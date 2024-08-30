package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITION_METHODS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.AcquisitionMethod;
import org.folio.rest.jaxrs.resource.OrdersAcquisitionMethods;
import org.folio.service.AcquisitionMethodsService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;

public class AcquisitionMethodAPI extends BaseApi implements OrdersAcquisitionMethods {

  @Autowired
  private AcquisitionMethodsService acquisitionMethodsService;
  public AcquisitionMethodAPI() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  public void getOrdersAcquisitionMethods(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    acquisitionMethodsService.getAcquisitionMethods(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(acquisitionMethods -> asyncResultHandler.handle(succeededFuture(buildOkResponse(acquisitionMethods))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  public void postOrdersAcquisitionMethods(AcquisitionMethod entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    acquisitionMethodsService.createAcquisitionMethod(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(acquisitionMethod -> asyncResultHandler.handle(
        succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(ACQUISITION_METHODS, acquisitionMethod.getId()), acquisitionMethod))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  public void getOrdersAcquisitionMethodsById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    acquisitionMethodsService.getAcquisitionMethodById(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(acquisitionMethod -> asyncResultHandler.handle(succeededFuture(buildOkResponse(acquisitionMethod))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  public void deleteOrdersAcquisitionMethodsById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    acquisitionMethodsService.deleteAcquisitionMethod(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  public void putOrdersAcquisitionMethodsById(String id, AcquisitionMethod entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    // Set id if this is available only in path
    if (isEmpty(entity.getId())) {
      entity.setId(id);
    } else if (!id.equals(entity.getId())) {
      addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
      return;
    }

    acquisitionMethodsService.saveAcquisitionMethod(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
