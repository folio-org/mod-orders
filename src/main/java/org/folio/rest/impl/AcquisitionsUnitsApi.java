package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;

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
import io.vertx.core.json.JsonObject;

public class AcquisitionsUnitsApi extends BaseApi implements AcquisitionsUnits {

  private static final Logger logger = LogManager.getLogger();

  @Autowired
  private AcquisitionsUnitsService acquisitionsUnitsService;

  public AcquisitionsUnitsApi() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }


  @Override
  @Validate
  public void postAcquisitionsUnitsUnits(String lang, AcquisitionsUnit entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.createAcquisitionsUnit(entity, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(unit -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created new acquisitions unit: {} ", JsonObject.mapFrom(unit).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(
          okapiHeaders.get(OKAPI_URL), resourceByIdPath(ACQUISITIONS_UNITS, unit.getId()), unit)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsUnits(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.getAcquisitionsUnits(query, offset, limit, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(units -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created new acquisitions units: {}", JsonObject.mapFrom(units).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(buildOkResponse(units)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void putAcquisitionsUnitsUnitsById(String id, String lang, AcquisitionsUnit entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    if (entity.getId() != null && !entity.getId().equals(id)) {
      addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
    } else {
      acquisitionsUnitsService.updateAcquisitionsUnit(entity.withId(id), new RequestContext(vertxContext, okapiHeaders))
        .thenAccept(units -> {
          logger.info("Successfully updated acquisitions unit with id={}", id);
          asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
        })
        .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
    }
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsUnitsById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.getAcquisitionsUnit(id, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(unit -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved acquisitions unit: {}", JsonObject.mapFrom(unit).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(buildOkResponse(unit)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void deleteAcquisitionsUnitsUnitsById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.deleteAcquisitionsUnit(id, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(ok -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully deleted acquisitions unit with id={}", id);
        }
        asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void postAcquisitionsUnitsMemberships(String lang, AcquisitionsUnitMembership entity, Map<String, String> okapiHeaders,
                                               Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    acquisitionsUnitsService.createAcquisitionsUnitsMembership(entity, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(membership -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created new acquisitions units membership: {}", JsonObject.mapFrom(membership).encodePrettily());
        }
        asyncResultHandler.handle(
          succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(ACQUISITIONS_MEMBERSHIPS, membership.getId()), membership)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsMemberships(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders,
                                              Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    acquisitionsUnitsService.getAcquisitionsUnitsMemberships(query, offset, limit, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(memberships -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created new acquisitions units memberships: {}", JsonObject.mapFrom(memberships).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(buildOkResponse(memberships)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void putAcquisitionsUnitsMembershipsById(String id, String lang, AcquisitionsUnitMembership entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    if (entity.getId() != null && !entity.getId().equals(id)) {
      addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
    } else {
      acquisitionsUnitsService.updateAcquisitionsUnitsMembership(entity.withId(id), new RequestContext(vertxContext, okapiHeaders))
        .thenAccept(membership -> {
          logger.info("Successfully updated acquisitions units membership with id={}", id);
          asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
        })
        .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
    }
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsMembershipsById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    acquisitionsUnitsService.getAcquisitionsUnitsMembership(id, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(membership -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved acquisitions units membership: {}", JsonObject.mapFrom(membership).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(buildOkResponse(membership)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

  @Override
  @Validate
  public void deleteAcquisitionsUnitsMembershipsById(String id, String lang, Map<String, String> okapiHeaders,
                                                     Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
   acquisitionsUnitsService.deleteAcquisitionsUnitsMembership(id, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(ok -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully deleted acquisitions units membership with id={}", id);
        }
        asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, t));
  }

}
