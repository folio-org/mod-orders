package org.folio.rest.impl;


import static org.folio.orders.utils.HelperUtils.getEndpoint;

import java.util.Map;

import javax.ws.rs.core.Response;

import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;
import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.model.SuffixCollection;
import org.folio.rest.jaxrs.resource.OrdersConfiguration;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Handler;


public class ConfigurationImpl implements OrdersConfiguration {

  public static final String ORDERS_CONFIGURATION_PREFIX = getEndpoint(OrdersConfiguration.class) + "/%s";

  @Override
  @Validate
  public void getOrdersConfigurationReasonsForClosure(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<ReasonForClosure, ReasonForClosureCollection> helper
      = new BaseCrudHelper<>(ReasonForClosure.class, ReasonForClosureCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.get(query, offset, limit, asyncResultHandler);
  }

  @Override
  @Validate
  public void putOrdersConfigurationReasonsForClosureById(String id, String lang, ReasonForClosure entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    // Set Source.USER according to requirement. Source.SYSTEM was populated by storage module.
    entity.setSource(ReasonForClosure.Source.USER);
    BaseCrudHelper<ReasonForClosure, ReasonForClosureCollection> helper
      = new BaseCrudHelper<>(ReasonForClosure.class, ReasonForClosureCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.update(id, entity, asyncResultHandler);
  }

  @Override
  @Validate
  public void postOrdersConfigurationReasonsForClosure(String lang, ReasonForClosure entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    // Set Source.USER according to requirement. Source.SYSTEM was populated by storage module.
    entity.setSource(ReasonForClosure.Source.USER);
    BaseCrudHelper<ReasonForClosure, ReasonForClosureCollection> helper
      = new BaseCrudHelper<>(ReasonForClosure.class, ReasonForClosureCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.create(entity, asyncResultHandler);
  }

  @Override
  @Validate
  public void getOrdersConfigurationReasonsForClosureById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<ReasonForClosure, ReasonForClosureCollection> helper
      = new BaseCrudHelper<>(ReasonForClosure.class, ReasonForClosureCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.getById(id, asyncResultHandler);
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationReasonsForClosureById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<ReasonForClosure, ReasonForClosureCollection> helper
      = new BaseCrudHelper<>(ReasonForClosure.class, ReasonForClosureCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.delete(id, asyncResultHandler);
  }

  @Override
  @Validate
  public void getOrdersConfigurationSuffixes(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Suffix, SuffixCollection> helper
      = new BaseCrudHelper<>(Suffix.class, SuffixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.get(query, offset, limit, asyncResultHandler);
  }

  @Override
  @Validate
  public void putOrdersConfigurationSuffixesById(String id, String lang, Suffix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Suffix, SuffixCollection> helper
      = new BaseCrudHelper<>(Suffix.class, SuffixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.update(id, entity, asyncResultHandler);
  }

  @Override
  @Validate
  public void postOrdersConfigurationSuffixes(String lang, Suffix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Suffix, SuffixCollection> helper
      = new BaseCrudHelper<>(Suffix.class, SuffixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.create(entity, asyncResultHandler);
  }

  @Override
  @Validate
  public void getOrdersConfigurationSuffixesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Suffix, SuffixCollection> helper
      = new BaseCrudHelper<>(Suffix.class, SuffixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.getById(id, asyncResultHandler);
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationSuffixesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Suffix, SuffixCollection> helper
      = new BaseCrudHelper<>(Suffix.class, SuffixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.delete(id, asyncResultHandler);
  }

  @Override
  @Validate
  public void getOrdersConfigurationPrefixes(String query, int offset, int limit, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Prefix, PrefixCollection> helper
      = new BaseCrudHelper<>(Prefix.class, PrefixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.get(query, offset, limit, asyncResultHandler);
  }

  @Override
  @Validate
  public void putOrdersConfigurationPrefixesById(String id, String lang, Prefix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Prefix, PrefixCollection> helper
      = new BaseCrudHelper<>(Prefix.class, PrefixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.update(id, entity, asyncResultHandler);
  }

  @Override
  @Validate
  public void postOrdersConfigurationPrefixes(String lang, Prefix entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Prefix, PrefixCollection> helper
      = new BaseCrudHelper<>(Prefix.class, PrefixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.create(entity, asyncResultHandler);
  }

  @Override
  @Validate
  public void getOrdersConfigurationPrefixesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Prefix, PrefixCollection> helper
      = new BaseCrudHelper<>(Prefix.class, PrefixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.getById(id, asyncResultHandler);
  }

  @Override
  @Validate
  public void deleteOrdersConfigurationPrefixesById(String id, String lang, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    BaseCrudHelper<Prefix, PrefixCollection> helper
      = new BaseCrudHelper<>(Prefix.class, PrefixCollection.class, ORDERS_CONFIGURATION_PREFIX, okapiHeaders, vertxContext, lang);
    helper.delete(id, asyncResultHandler);
  }
}
