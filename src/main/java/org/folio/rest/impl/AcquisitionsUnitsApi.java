package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembership;
import org.folio.rest.jaxrs.resource.AcquisitionsUnits;
import org.folio.service.AcquisitionsUnitsService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;

public class AcquisitionsUnitsApi extends BaseApi implements AcquisitionsUnits {

  private static final Logger logger = LogManager.getLogger();

  @Autowired
  private AcquisitionsUnitsService acquisitionsUnitsService;

  public AcquisitionsUnitsApi() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }


  @Override
  @Validate
  public void postAcquisitionsUnitsUnits(AcquisitionsUnit entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.createAcquisitionsUnit(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(unit -> asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(
        okapiHeaders.get(OKAPI_URL), resourceByIdPath(ACQUISITIONS_UNITS, unit.getId()), unit))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsUnits(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.getAcquisitionsUnits(query, offset, limit, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(units -> asyncResultHandler.handle(succeededFuture(buildOkResponse(units))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void putAcquisitionsUnitsUnitsById(String id, AcquisitionsUnit entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    if (entity.getId() != null && !entity.getId().equals(id)) {
      addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
    } else {
      acquisitionsUnitsService.updateAcquisitionsUnit(entity.withId(id), new RequestContext(vertxContext, okapiHeaders))
        .onSuccess(units -> {
          logger.debug("Successfully updated acquisitions unit with id={}", id);
          asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
        })
        .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
    }
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsUnitsById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.getAcquisitionsUnit(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(unit -> asyncResultHandler.handle(succeededFuture(buildOkResponse(unit))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void deleteAcquisitionsUnitsUnitsById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.deleteAcquisitionsUnit(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(ok -> {
        if (logger.isInfoEnabled()) {
          logger.debug("Successfully deleted acquisitions unit with id={}", id);
        }
        asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postAcquisitionsUnitsMemberships(AcquisitionsUnitMembership entity, Map<String, String> okapiHeaders,
                                               Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.createAcquisitionsUnitsMembership(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(membership -> asyncResultHandler.handle(
        succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(ACQUISITIONS_MEMBERSHIPS, membership.getId()), membership))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsMemberships(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders,
                                              Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    acquisitionsUnitsService.getAcquisitionsUnitsMemberships(query, offset, limit, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(memberships -> asyncResultHandler.handle(succeededFuture(buildOkResponse(memberships))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void putAcquisitionsUnitsMembershipsById(String id, AcquisitionsUnitMembership entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (entity.getId() != null && !entity.getId().equals(id)) {
      addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
    } else {
      acquisitionsUnitsService.updateAcquisitionsUnitsMembership(entity.withId(id), new RequestContext(vertxContext, okapiHeaders))
        .onSuccess(membership -> {
          logger.debug("Successfully updated acquisitions units membership with id={}", id);
          asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
        })
        .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
    }
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsMembershipsById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    acquisitionsUnitsService.getAcquisitionsUnitsMembership(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(membership -> asyncResultHandler.handle(succeededFuture(buildOkResponse(membership))))
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void deleteAcquisitionsUnitsMembershipsById(String id, Map<String, String> okapiHeaders,
                                                     Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
   acquisitionsUnitsService.deleteAcquisitionsUnitsMembership(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(ok -> {
        if (logger.isInfoEnabled()) {
          logger.debug("Successfully deleted acquisitions units membership with id={}", id);
        }
        asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
      })
      .onFailure(t -> handleErrorResponse(asyncResultHandler, t));
  }

}
