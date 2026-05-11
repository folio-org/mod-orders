package org.folio.service.caches;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.settings.CommonSettingsRetriever;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.ResourcePathResolver.LOCALE;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_SETTINGS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

@Log4j2
@Component
@RequiredArgsConstructor
public class CommonSettingsCache extends AbstractConfigCache {

  private final CommonSettingsRetriever commonSettingsRetriever;
  private AsyncCache<String, JsonObject> settingsCache;
  private AsyncCache<String, String> systemCurrencyCache;
  private AsyncCache<String, String> systemTimezoneCache;

  @Value("${orders.cache.configuration-entries.expiration.time.seconds:30}")
  private long cacheExpirationTime;
  @Value("${orders.cache.configuration-entries.bypass-cache:false}")
  private boolean byPassCache;

  @PostConstruct
  void init() {
    var context = Vertx.currentContext();
    this.settingsCache = buildAsyncCache(context, cacheExpirationTime);
    this.systemCurrencyCache = buildAsyncCache(context, cacheExpirationTime);
    this.systemTimezoneCache = buildAsyncCache(context, cacheExpirationTime);
  }

  public Future<JsonObject> loadSettings(RequestContext requestContext) {
    return cacheData(resourcesPath(ORDER_SETTINGS), null, settingsCache,
      commonSettingsRetriever::getLocalSettings, byPassCache, requestContext);
  }

  public Future<String> getSystemCurrency(RequestContext requestContext) {
    return cacheData(resourcesPath(LOCALE), null, systemCurrencyCache,
      commonSettingsRetriever::getSystemCurrency, byPassCache, requestContext);
  }

  public Future<String> getSystemTimeZone(RequestContext requestContext) {
    return cacheData(resourcesPath(LOCALE), null, systemTimezoneCache,
      commonSettingsRetriever::getSystemTimeZone, byPassCache, requestContext);
  }

}
