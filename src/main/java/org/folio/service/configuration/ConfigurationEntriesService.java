package org.folio.service.configuration;

import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Configs;

import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

public class ConfigurationEntriesService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ConfigurationEntriesService.class);

  private static final String CONFIG_QUERY = "module==%s";

  private final RestClient configurationEntriesRestClient;

  public ConfigurationEntriesService(RestClient configurationEntriesRestClient) {
    this.configurationEntriesRestClient = configurationEntriesRestClient;
  }


  public CompletableFuture<JsonObject> loadConfiguration(String moduleConfig, RequestContext requestContext) {
    return configurationEntriesRestClient.get(String.format(CONFIG_QUERY, moduleConfig), 0, Integer.MAX_VALUE, requestContext, Configs.class)
      .thenApply(configs -> {
        if (LOGGER.isDebugEnabled()) {
          LOGGER.debug("The response from mod-configuration: {}", JsonObject.mapFrom(configs).encodePrettily());
        }
        JsonObject config = new JsonObject();

        configs.getConfigs()
          .forEach(entry -> config.put(entry.getConfigName(), entry.getValue()));
        return config;
      });
  }
}
