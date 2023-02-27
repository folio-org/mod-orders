package org.folio.service.configuration;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Configs;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class ConfigurationEntriesService {

  private static final Logger logger = LogManager.getLogger();
  public static final String TENANT_CONFIGURATION_ENTRIES = "/configurations/entries";
  public static final String CONFIG_QUERY = "module==%s";
  public static final String LOCALE_SETTINGS = "localeSettings";
  public static final String CURRENCY_USD = "USD";
  private final RestClient restClient;

  public ConfigurationEntriesService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<JsonObject> loadConfiguration(RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.get(requestEntry, Configs.class, requestContext)
      .map(configs -> {
        if (logger.isDebugEnabled()) {
          logger.debug("The response from mod-configuration: {}", JsonObject.mapFrom(configs).encodePrettily());
        }
        JsonObject config = new JsonObject();

        configs.getConfigs()
          .forEach(entry -> config.put(entry.getConfigName(), entry.getValue()));
        return config;
      });
  }


  public Future<String> getSystemCurrency(RequestEntry requestEntry, RequestContext requestContext) {


    return loadConfiguration(requestEntry, requestContext)
      .compose(config -> {
      String localeSettings = config.getString(LOCALE_SETTINGS);
      String systemCurrency;
      if (StringUtils.isEmpty(localeSettings)) {
        systemCurrency = CURRENCY_USD;
      } else {
        systemCurrency = new JsonObject(config.getString(LOCALE_SETTINGS)).getString("currency", "USD");
      }
      return Future.succeededFuture(systemCurrency);
    });
  }

}
