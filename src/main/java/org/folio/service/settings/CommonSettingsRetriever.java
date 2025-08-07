package org.folio.service.settings;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.acq.model.SettingCollection;
import org.folio.rest.acq.model.settings.CommonSetting;
import org.folio.rest.acq.model.settings.CommonSettingsCollection;
import org.folio.rest.acq.model.settings.Value;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Configs;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import java.util.List;
import java.util.Optional;

/**
 * This service class is used to fetch settings from <code>mod-settings</code>. Unlike {@link SettingsRetriever}, this class does not fetch
 * settings from the <code>mod-orders-storage</code> database. It is intended to be used for common settings that are not specific to orders.
 */
@Log4j2
@RequiredArgsConstructor
public class CommonSettingsRetriever {

  public static final String CURRENCY_KEY = "currency";
  public static final String CURRENCY_DEFAULT = "USD";

  public static final String TZ_KEY = "timezone";
  public static final String TZ_DEFAULT = "UTC";

  private final RestClient restClient;

  public Future<JsonObject> loadConfigurations(RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.get(requestEntry, Configs.class, requestContext)
      .map(configs -> {
        var config = new JsonObject();
        configs.getConfigs().forEach(entry -> config.put(entry.getConfigName(), entry.getValue()));
        return config;
      });
  }

  public Future<String> getSystemCurrency(RequestEntry requestEntry, RequestContext requestContext) {
    return getGlobalSetting(CURRENCY_KEY, CURRENCY_DEFAULT, requestEntry, requestContext);
  }

  public Future<String> getSystemTimeZone(RequestEntry requestEntry, RequestContext requestContext) {
    return getGlobalSetting(TZ_KEY, TZ_DEFAULT, requestEntry, requestContext);
  }

  private Future<String> getGlobalSetting(String key, String defaultValue, RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.get(requestEntry, CommonSettingsCollection.class, requestContext)
      .map(settingsCollection -> Optional.ofNullable(settingsCollection.getItems()).orElse(List.of()))
      .map(settings -> settings.stream()
        .findFirst()
        .map(CommonSetting::getValue)
        .map(Value::getAdditionalProperties)
        .map(properties -> properties.get(key))
        .map(Object::toString)
        .orElse(defaultValue));
  }

  public Future<JsonObject> getLocalSettings(RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.get(requestEntry, SettingCollection.class, requestContext)
      .map(settingsCollection -> {
        var settings = new JsonObject();
        settingsCollection.getSettings().forEach(setting -> settings.put(setting.getKey(), setting.getValue()));
        return settings;
      });
  }
}
