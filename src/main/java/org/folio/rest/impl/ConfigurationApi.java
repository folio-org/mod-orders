package org.folio.rest.impl;

import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.OKAPI_URL;
import static org.folio.orders.utils.ResourcePathResolver.PREFIXES;
import static org.folio.orders.utils.ResourcePathResolver.REASONS_FOR_CLOSURE;
import static org.folio.orders.utils.ResourcePathResolver.SUFFIXES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.resource.OrdersConfiguration;
import org.folio.service.PrefixService;
import org.folio.service.ReasonForClosureService;
import org.folio.service.SuffixService;
import org.folio.spring.SpringContextUtil;
import org.springframework.beans.factory.annotation.Autowired;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;


public class ConfigurationApi extends BaseApi implements OrdersConfiguration {

  @Autowired
  private SuffixService suffixService;
  @Autowired
  private PrefixService prefixService;
  @Autowired
  private ReasonForClosureService reasonForClosureService;

  public ConfigurationApi() {
    SpringContextUtil.autowireDependencies(this, Vertx.currentContext());
  }

  @Override
  @Validate
  public void getOrdersConfigurationReasonsForClosure(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    reasonForClosureService.getReasonsForClosure(query, offset, limit, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(reasonForClosureCollection -> asyncResultHandler.handle(succeededFuture(buildOkResponse(reasonForClosureCollection))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersConfigurationReasonsForClosureById(String id, ReasonForClosure entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    reasonForClosureService.updateReasonForClosure(id, entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersConfigurationReasonsForClosure(ReasonForClosure entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    reasonForClosureService.createReasonForClosure(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(obj -> asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(REASONS_FOR_CLOSURE, obj.getId()), obj))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationReasonsForClosureById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    reasonForClosureService.getReasonForClosureById(id, new RequestContext(vertxContext, okapiHeaders))
    .onSuccess(reasonForClosure -> asyncResultHandler.handle(succeededFuture(buildOkResponse(reasonForClosure))))
    .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationReasonsForClosureById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    reasonForClosureService.deleteReasonForClosure(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationSuffixes(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.getSuffixes(query, offset, limit, new RequestContext(vertxContext, okapiHeaders))
    .onSuccess(suffixCollection -> asyncResultHandler.handle(succeededFuture(buildOkResponse(suffixCollection))))
    .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersConfigurationSuffixesById(String id, Suffix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.updateSuffix(id, entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersConfigurationSuffixes(Suffix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.createSuffix(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(suffix -> asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(SUFFIXES, suffix.getId()), suffix))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationSuffixesById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.getSuffixById(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(suffix -> asyncResultHandler.handle(succeededFuture(buildOkResponse(suffix))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationSuffixesById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.deleteSuffix(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationPrefixes(String query, String totalRecords, int offset, int limit, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.getPrefixes(query, offset, limit, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(prefixCollection -> asyncResultHandler.handle(succeededFuture(buildOkResponse(prefixCollection))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersConfigurationPrefixesById(String id, Prefix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.updatePrefix(id, entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersConfigurationPrefixes(Prefix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.createPrefix(entity, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(prefix -> asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(PREFIXES, prefix.getId()), prefix))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationPrefixesById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.getPrefixById(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(prefix -> asyncResultHandler.handle(succeededFuture(buildOkResponse(prefix))))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationPrefixesById(String id, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.deletePrefix(id, new RequestContext(vertxContext, okapiHeaders))
      .onSuccess(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .onFailure(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
