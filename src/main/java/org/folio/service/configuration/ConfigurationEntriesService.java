package org.folio.service.configuration;

import java.util.concurrent.CompletableFuture;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Configs;

import io.vertx.core.json.JsonObject;

import static org.folio.orders.utils.HelperUtils.SYSTEM_CONFIG_MODULE_NAME;

public class ConfigurationEntriesService {

  private static final Logger LOGGER = LogManager.getLogger();
  private static final String ENDPOINT = "/configurations/entries";

  private static final String CONFIG_QUERY = "module==%s";
  public static final String LOCALE_SETTINGS = "localeSettings";
  public static final String CURRENCY_USD = "USD";

  private final RestClient restClient;

  public ConfigurationEntriesService(RestClient restClient) {
    this.restClient = restClient;
  }

  public CompletableFuture<JsonObject> loadConfiguration(String moduleConfig, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(ENDPOINT).withQuery(String.format(CONFIG_QUERY, moduleConfig))
            .withOffset(0).withLimit(Integer.MAX_VALUE);
    return restClient
      .get(requestEntry, requestContext, Configs.class)
      .thenApply(configs -> {
        if (LOGGER.isDebugEnabled()) {
          LOGGER.debug("The response from mod-configuration: {}", JsonObject.mapFrom(configs)
            .encodePrettily());
        }
        JsonObject config = new JsonObject();

        configs.getConfigs()
          .forEach(entry -> config.put(entry.getConfigName(), entry.getValue()));
        return config;
      });
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
}
