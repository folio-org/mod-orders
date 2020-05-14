package org.folio.rest.impl;


import static io.vertx.core.Future.succeededFuture;
import static org.folio.orders.utils.HelperUtils.OKAPI_URL;
import static org.folio.orders.utils.HelperUtils.getEndpoint;
import static org.folio.orders.utils.ResourcePathResolver.PREFIXES;
import static org.folio.orders.utils.ResourcePathResolver.REASONS_FOR_CLOSURE;
import static org.folio.orders.utils.ResourcePathResolver.SUFFIXES;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;

import java.util.Map;

import javax.ws.rs.core.Response;

import io.vertx.core.Vertx;
import org.folio.rest.annotations.Validate;
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


public class ConfigurationApi extends BaseApi implements OrdersConfiguration {

  public static final String ORDERS_CONFIGURATION_PREFIX = getEndpoint(OrdersConfiguration.class) + "/%s";

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
  public void getOrdersConfigurationReasonsForClosure(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    reasonForClosureService.getReasonsForClosure(query, offset, limit, vertxContext, okapiHeaders)
      .thenAccept(reasonForClosureCollection -> asyncResultHandler.handle(succeededFuture(buildOkResponse(reasonForClosureCollection))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersConfigurationReasonsForClosureById(String id, String lang, ReasonForClosure entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    reasonForClosureService.updateReasonForClosure(id, entity, vertxContext, okapiHeaders)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersConfigurationReasonsForClosure(String lang, ReasonForClosure entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    reasonForClosureService.createReasonForClosure(entity, vertxContext, okapiHeaders)
      .thenAccept(obj -> asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(REASONS_FOR_CLOSURE, obj.getId()), obj))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationReasonsForClosureById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    reasonForClosureService.getReasonForClosureById(id, vertxContext, okapiHeaders)
    .thenAccept(reasonForClosure -> asyncResultHandler.handle(succeededFuture(buildOkResponse(reasonForClosure))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationReasonsForClosureById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    reasonForClosureService.deleteReasonForClosure(id, vertxContext, okapiHeaders)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationSuffixes(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.getSuffixes(query, limit, offset, vertxContext, okapiHeaders)
    .thenAccept(suffixCollection -> asyncResultHandler.handle(succeededFuture(buildOkResponse(suffixCollection))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersConfigurationSuffixesById(String id, String lang, Suffix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.updateSuffix(id, entity, vertxContext, okapiHeaders)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersConfigurationSuffixes(String lang, Suffix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.createSuffix(entity, vertxContext, okapiHeaders)
      .thenAccept(suffix -> asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(SUFFIXES, suffix.getId()), suffix))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationSuffixesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.getSuffixById(id, vertxContext, okapiHeaders)
      .thenAccept(suffix -> asyncResultHandler.handle(succeededFuture(buildOkResponse(suffix))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationSuffixesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    suffixService.deleteSuffix(id, vertxContext, okapiHeaders)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationPrefixes(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.getPrefixes(query, offset, limit, vertxContext, okapiHeaders)
      .thenAccept(prefixCollection -> asyncResultHandler.handle(succeededFuture(buildOkResponse(prefixCollection))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void putOrdersConfigurationPrefixesById(String id, String lang, Prefix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.updatePrefix(id, entity, vertxContext, okapiHeaders)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void postOrdersConfigurationPrefixes(String lang, Prefix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.createPrefix(entity, vertxContext, okapiHeaders)
      .thenAccept(prefix -> asyncResultHandler.handle(succeededFuture(buildResponseWithLocation(okapiHeaders.get(OKAPI_URL), resourceByIdPath(PREFIXES, prefix.getId()), prefix))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void getOrdersConfigurationPrefixesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.getPrefixById(id, vertxContext, okapiHeaders)
      .thenAccept(prefix -> asyncResultHandler.handle(succeededFuture(buildOkResponse(prefix))))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationPrefixesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    prefixService.deletePrefix(id, vertxContext, okapiHeaders)
      .thenAccept(v -> asyncResultHandler.handle(succeededFuture(buildNoContentResponse())))
      .exceptionally(fail -> handleErrorResponse(asyncResultHandler, fail));
  }
}
