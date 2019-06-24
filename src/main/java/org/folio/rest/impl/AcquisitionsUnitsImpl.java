package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.resource.AcquisitionsUnits;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class AcquisitionsUnitsImpl implements AcquisitionsUnits {

  private static final Logger logger = LoggerFactory.getLogger(AcquisitionsUnit.class);

  private static final String ACQUISITIONS_UNITS_LOCATION_PREFIX = "/acquisitions-units/units/";

  @Override
  @Validate
  public void postAcquisitionsUnitsUnits(String lang, AcquisitionsUnit entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    AcquisitionsUnitsHelper helper = new AcquisitionsUnitsHelper(okapiHeaders, vertxContext, lang);

    helper.createAcquisitionsUnit(entity)
      .thenAccept(unit -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created new acquisitions unit: " + JsonObject.mapFrom(unit).encodePrettily());
        }

        asyncResultHandler.handle(succeededFuture(buildResponseForPostUnit(unit)));
        helper.closeHttpClient();
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsUnits(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    AcquisitionsUnitsHelper helper = new AcquisitionsUnitsHelper(okapiHeaders, vertxContext, lang);

    helper.getAcquisitionsUnits(query, offset, limit)
      .thenAccept(units -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully created new acquisitions units: " + JsonObject.mapFrom(units).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(units)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void putAcquisitionsUnitsUnitsById(String id, String lang, AcquisitionsUnit entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    AcquisitionsUnitsHelper helper = new AcquisitionsUnitsHelper(okapiHeaders, vertxContext, lang);

    if (entity.getId() != null && !entity.getId().equals(id)) {
      helper.addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(422)));
    } else {
      helper.updateAcquisitionsUnit(entity.withId(id))
        .thenAccept(units -> {
          logger.info("Successfully updated acquisitions unit with id={}", id);
          asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse()));
        })
        .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
    }
  }

  @Override
  @Validate
  public void getAcquisitionsUnitsUnitsById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    AcquisitionsUnitsHelper helper = new AcquisitionsUnitsHelper(okapiHeaders, vertxContext, lang);

    helper.getAcquisitionsUnit(id)
      .thenAccept(unit -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully retrieved acquisitions unit: " + JsonObject.mapFrom(unit).encodePrettily());
        }
        asyncResultHandler.handle(succeededFuture(helper.buildOkResponse(unit)));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  @Override
  @Validate
  public void deleteAcquisitionsUnitsUnitsById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    AcquisitionsUnitsHelper helper = new AcquisitionsUnitsHelper(okapiHeaders, vertxContext, lang);

    helper.deleteAcquisitionsUnit(id)
      .thenAccept(ok -> {
        if (logger.isInfoEnabled()) {
          logger.info("Successfully deleted acquisitions unit with id={}", id);
        }
        asyncResultHandler.handle(succeededFuture(helper.buildNoContentResponse()));
      })
      .exceptionally(t -> handleErrorResponse(asyncResultHandler, helper, t));
  }

  private Response buildResponseForPostUnit(AcquisitionsUnit unit) {
    PostAcquisitionsUnitsUnitsResponse.HeadersFor201 headersFor201 = PostAcquisitionsUnitsUnitsResponse.headersFor201()
      .withLocation(ACQUISITIONS_UNITS_LOCATION_PREFIX + unit.getId());
    return PostAcquisitionsUnitsUnitsResponse.respond201WithApplicationJson(unit, headersFor201);
  }

  private Void handleErrorResponse(Handler<AsyncResult<Response>> asyncResultHandler, AbstractHelper helper, Throwable t) {
    asyncResultHandler.handle(succeededFuture(helper.buildErrorResponse(t)));
    return null;
  }
}
