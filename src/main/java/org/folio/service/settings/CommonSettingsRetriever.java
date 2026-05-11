package org.folio.service.settings;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.acq.model.SettingCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

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

  public Future<String> getSystemCurrency(RequestEntry requestEntry, RequestContext requestContext) {
    return getLocaleSetting(CURRENCY_KEY, CURRENCY_DEFAULT, requestEntry, requestContext);
  }

  public Future<String> getSystemTimeZone(RequestEntry requestEntry, RequestContext requestContext) {
    return getLocaleSetting(TZ_KEY, TZ_DEFAULT, requestEntry, requestContext);
  }

  private Future<String> getLocaleSetting(String key, String defaultValue, RequestEntry requestEntry, RequestContext requestContext) {
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(jsonObject -> {
        if (jsonObject == null) {
          return defaultValue;
        }
        var value = jsonObject.getString(key);
        return StringUtils.isNotBlank(value) ? value : defaultValue;
      });
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
