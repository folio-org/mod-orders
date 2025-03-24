package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.rest.RestConstants.OKAPI_URL;
import static org.folio.rest.core.exceptions.ErrorCodes.MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY;

import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.jaxrs.resource.OrdersTitles;
import org.folio.service.titles.TitleValidationService;
import org.folio.service.titles.TitlesService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

public class TitlesApi extends BaseApi implements OrdersTitles {
  @Autowired
  private TitlesService titlesService;
  @Autowired
  private TitleValidationService titleValidationService;

  public TitlesApi() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }


  @Override
  @Validate
  public void getOrdersTitles(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders,
                              Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    titlesService.getTitles(limit, offset, query, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(titles -> asyncResultHandler.handle(succeededFuture(buildOkResponse(titles))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersTitles(Title entity, Map<String, String> okapiHeaders,
                               Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    List<Error> errors = titleValidationService.validateTitle(entity);
    if (CollectionUtils.isNotEmpty(errors)) {
      errors.forEach(this::addProcessingError);
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
      return;
    }
    titlesService.createTitle(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(title -> asyncResultHandler.handle(
        succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(TITLES, title.getId()), title))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersTitlesById(String id, Map<String, String> okapiHeaders,
                                  Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    titlesService.getTitleById(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(title -> asyncResultHandler.handle(succeededFuture(buildOkResponse(title))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersTitlesById(String id, Map<String, String> okapiHeaders,
                                     Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    titlesService.deleteTitle(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersTitlesById(String id, Title entity, Map<String, String> okapiHeaders,
                                  Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    // Set id if this is available only in path
    if (isEmpty(entity.getId())) {
      entity.setId(id);
    } else if (!id.equals(entity.getId())) {
      addProcessingError(MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY.toError());
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
      return;
    }
    List<Error> errors = titleValidationService.validateTitle(entity);
    if (CollectionUtils.isNotEmpty(errors)) {
      errors.forEach(this::addProcessingError);
      asyncResultHandler.handle(succeededFuture(buildErrorResponse(422)));
      return;
    }

    titlesService.saveTitleWithAcqUnitsCheck(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  public void deleteOrdersTitlesUnlinkById(String id, String deleteHolding, Map<String, String> okapiHeaders,
                                           Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    titlesService.unlinkTitleFromPackage(id, deleteHolding, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(response -> {
        if (response == null || response.isEmpty()) {
          asyncResultHandler.handle(succeededFuture(buildNoContentResponse()));
        } else {
          asyncResultHandler.handle(succeededFuture(buildOkResponse(response)));
        }
      })
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
