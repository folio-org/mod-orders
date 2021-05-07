package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.resource.OrdersTitles;
import org.folio.service.titles.TitlesService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;

public class TitlesApi extends BaseApi implements OrdersTitles {
  @Autowired
  private TitlesService titlesService;

  public TitlesApi() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }


  @Override
  @Validate
  public void getOrdersTitles(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    titlesService.getTitles(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(titles -> asyncResultHandler.handle(succeededFuture(buildOkResponse(titles))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersTitles(String lang, Title entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    titlesService.createTitle(entity, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(title -> asyncResultHandler.handle(
        succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(TITLES, title.getId()), title))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersTitlesById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    titlesService.getTitleById(id, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(title -> asyncResultHandler.handle(succeededFuture(buildOkResponse(title))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersTitlesById(String id, String lang, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    titlesService.deleteTitle(id, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersTitlesById(String id, String lang, Title entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    // Set id if this is available only in path
    if (isEmpty(entity.getId())) {
      entity.setId(id);
    } else if (!id.equals(entity.getId())) {
      addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
      return;
    }

    titlesService.updateTitle(entity, new RequestContext(vertxContext, okapiHeaders))
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

}
