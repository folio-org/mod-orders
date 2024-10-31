package org.folio.service.configuration;

import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Configs;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

@Log4j2
public class ConfigurationEntriesService {

  public static final String TENANT_CONFIGURATION_ENTRIES = "/configurations/entries";
  public static final String CONFIG_QUERY = "module==%s";
  public static final String LOCALE_SETTINGS = "localeSettings";

  public static final String CURRENCY_CONFIG = "currency";
  public static final String DEFAULT_CURRENCY = "USD";

  public static final String TZ_CONFIG = "timezone";
  public static final String TZ_UTC = "UTC";

  private final RestClient restClient;

  public ConfigurationEntriesService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<JsonObject> loadConfiguration(RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.get(requestEntry, Configs.class, requestContext)
      .map(configs -> {
        if (log.isDebugEnabled()) {
          log.debug("The response from mod-configuration: {}", JsonObject.mapFrom(configs).encodePrettily());
        }
        var config = new JsonObject();
        configs.getConfigs()
          .forEach(entry -> config.put(entry.getConfigName(), entry.getValue()));
        return config;
      });
  }

  public Future<String> getSystemCurrency(RequestEntry requestEntry, RequestContext requestContext) {
    return loadConfiguration(requestEntry, requestContext)
      .map(jsonConfig -> extractLocalSettingConfigValueByName(jsonConfig, CURRENCY_CONFIG, DEFAULT_CURRENCY));
  }

  public Future<String> getSystemTimeZone(RequestEntry requestEntry, RequestContext requestContext) {
    return loadConfiguration(requestEntry, requestContext)
      .map(jsonConfig -> extractLocalSettingConfigValueByName(jsonConfig, TZ_CONFIG, TZ_UTC));
  }

  private String extractLocalSettingConfigValueByName(JsonObject config, String name, String defaultValue) {
    String localeSettings = config.getString(LOCALE_SETTINGS);
    return StringUtils.isEmpty(localeSettings)
      ? defaultValue
      : new JsonObject(localeSettings).getString(name, defaultValue);
  }

}
