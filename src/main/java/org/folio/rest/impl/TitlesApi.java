package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.HelperUtils.getEndpoint;
import static org.folio.orders.utils.HelperUtils.handleErrorResponse;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.helper.TitlesHelper;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.resource.OrdersTitles;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;

public class TitlesApi implements OrdersTitles {

  private static final String TITLES_LOCATION_PREFIX = getEndpoint(OrdersTitles.class) + "/%s";

  @Override
  @Validate
  public void getOrdersTitles(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    TitlesHelper titlesHelper = new TitlesHelper(okapiHeaders, vertxContext, lang);
    titlesHelper.getTitles(limit, offset, query)
      .thenAccept(titles -> asyncResultHandler.handle(succeededFuture(titlesHelper.buildOkResponse(titles))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, titlesHelper, fail));
  }

  @Override
  @Validate
  public void postOrdersTitles(String lang, Title entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    TitlesHelper titlesHelper = new TitlesHelper(okapiHeaders, vertxContext, lang);
    titlesHelper.createTitle(entity)
      .thenAccept(title -> asyncResultHandler.handle(
          succeededFuture(titlesHelper.buildResponseWithLocation(String.format(TITLES_LOCATION_PREFIX, title.getId()), title))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, titlesHelper, fail));
  }

  @Override
  @Validate
  public void getOrdersTitlesById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    TitlesHelper titlesHelper = new TitlesHelper(okapiHeaders, vertxContext, lang);
    titlesHelper.getTitle(id)
      .thenAccept(title -> asyncResultHandler.handle(succeededFuture(titlesHelper.buildOkResponse(title))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, titlesHelper, fail));
  }

  @Override
  @Validate
  public void deleteOrdersTitlesById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    TitlesHelper titlesHelper = new TitlesHelper(okapiHeaders, vertxContext, lang);
    titlesHelper.deleteTitle(id)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(titlesHelper.buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, titlesHelper, fail));
  }

  @Override
  @Validate
  public void putOrdersTitlesById(String id, String lang, Title entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    TitlesHelper titlesHelper = new TitlesHelper(okapiHeaders, vertxContext, lang);
    // Set id if this is available only in path
    if (isEmpty(entity.getId())) {
      entity.setId(id);
    } else if (!id.equals(entity.getId())) {
      titlesHelper.addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(titlesHelper.buildErrorResponse(422)));
      return;
    }

    titlesHelper.updateTitle(entity)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(titlesHelper.buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, titlesHelper, fail));
  }

}
