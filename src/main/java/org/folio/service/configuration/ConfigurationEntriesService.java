package org.folio.service.configuration;

import java.util.concurrent.CompletableFuture;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Configs;

import io.vertx.core.json.JsonObject;

import static org.folio.orders.utils.HelperUtils.SYSTEM_CONFIG_MODULE_NAME;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class ConfigurationEntriesService {

  private static final Logger logger = LogManager.getLogger();
  private static final String TENANT_CONFIGURATION_ENTRIES = "/configurations/entries";
  private static final String CONFIG_QUERY = "module==%s";
  public static final String LOCALE_SETTINGS = "localeSettings";
  public static final String CURRENCY_USD = "USD";

  private final RestClient restClient;

  public ConfigurationEntriesService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<JsonObject> loadConfiguration(String moduleConfig, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(TENANT_CONFIGURATION_ENTRIES).withQuery(String.format(CONFIG_QUERY, moduleConfig))
            .withOffset(0).withLimit(Integer.MAX_VALUE);
    logger.info("GET request: {}", CONFIG_QUERY);
    return restClient
      .get(requestEntry, requestContext, Configs.class)
      .thenApply(configs -> {
        if (logger.isDebugEnabled()) {
          logger.debug("The response from mod-configuration: {}", JsonObject.mapFrom(configs)
            .encodePrettily());
        }
        JsonObject config = new JsonObject();

        configs.getConfigs()
          .forEach(entry -> config.put(entry.getConfigName(), entry.getValue()));
        return config;
      });
  }

  /**
   * Retrieve configuration by moduleName and configName from mod-configuration.
   *
   * @param searchCriteria name of the module for which the configuration is to be retrieved
   * @return CompletableFuture with Configs
   */
  public CompletableFuture<Configs> getConfigurationsEntries(RequestContext requestContext, String... searchCriteria) {
    String query = buildSearchingQuery(searchCriteria);
    RequestEntry requestEntry = new RequestEntry(TENANT_CONFIGURATION_ENTRIES)
      .withQuery(query)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE);
    return restClient.get(requestEntry, requestContext, Configs.class);
  }

  public CompletableFuture<String> getSystemCurrency(RequestContext requestContext) {
    CompletableFuture<String> future = new CompletableFuture<>();
    loadConfiguration(SYSTEM_CONFIG_MODULE_NAME, requestContext).thenApply(config -> {
      String localeSettings = config.getString(LOCALE_SETTINGS);
      String systemCurrency;
      if (StringUtils.isEmpty(localeSettings)) {
        systemCurrency = CURRENCY_USD;
      } else {
        systemCurrency = new JsonObject(config.getString(LOCALE_SETTINGS)).getString("currency", "USD");
      }
      return future.complete(systemCurrency);
    })
      .exceptionally(future::completeExceptionally);
    return future;
  }

  private String extractLocalSettingConfigValueByName(JsonObject config, String name, String defaultValue) {
    String localeSettings = config.getString(LOCALE_SETTINGS);
    String confValue;
    if (StringUtils.isEmpty(localeSettings)) {
      confValue = defaultValue;
    } else {
      confValue = new JsonObject(config.getString(LOCALE_SETTINGS)).getString(name, defaultValue);
    }
    return confValue;
  }

  private String buildSearchingQuery(String[] searchCriteria) {
    return "(" + String.join(") OR (", searchCriteria) + ")";
  }

}
