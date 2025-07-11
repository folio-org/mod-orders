package org.folio.service.settings;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import jakarta.annotation.PostConstruct;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.rest.acq.model.Setting;
import org.folio.rest.acq.model.SettingCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.settings.util.SettingKey;
import org.springframework.beans.factory.annotation.Value;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_SETTINGS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

/**
 * This service class is used to fetch settings from <code>mod-orders-storage</code>, unlike {@link CommonSettingsRetriever}
 */
@Log4j2
public class SettingsRetriever {

  private static final String SETTINGS_ENDPOINT = resourcesPath(ORDER_SETTINGS);
  private static final String SETTINGS_BY_KEY_QUERY = "key==%s";
  private static final String SETTINGS_CACHE_KEY = "%s.%s";

  private final RestClient restClient;
  private AsyncCache<String, Optional<Setting>> asyncCache;

  @Value("${orders.cache.orders-settings.expiration.time.seconds:300}")
  private long cacheExpirationTime;

  public SettingsRetriever(RestClient restClient) {
    this.restClient = restClient;
  }

  @PostConstruct
  void init() {
    this.asyncCache = buildAsyncCache(Vertx.currentContext(), cacheExpirationTime);
  }

  public Future<Optional<Setting>> getSettingByKey(SettingKey settingKey, RequestContext requestContext) {
    try {
      var cacheKey = String.format(SETTINGS_CACHE_KEY, TenantTool.tenantId(requestContext.getHeaders()), settingKey.getName());
      return Future.fromCompletionStage(asyncCache.get(cacheKey, (key, executor) -> getSettingByKeyFromRemote(settingKey, requestContext)));
    } catch (Exception e) {
      log.error("Exception occurred when retrieving setting for key: {}", settingKey.getName(), e);
      return Future.failedFuture(e);
    }
  }

  private CompletableFuture<Optional<Setting>> getSettingByKeyFromRemote(SettingKey settingKey, RequestContext requestContext) {
    var requestEntry = new RequestEntry(SETTINGS_ENDPOINT).withLimit(1).withOffset(0)
      .withQuery(String.format(SETTINGS_BY_KEY_QUERY, settingKey.getName()));
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(settings -> settings.mapTo(SettingCollection.class))
      .map(settings -> settings.getTotalRecords() == null || settings.getTotalRecords() != 1 || CollectionUtils.isEmpty(settings.getSettings())
        ? Optional.<Setting>empty()
        : Optional.of(settings.getSettings().getFirst()))
      .toCompletionStage()
      .toCompletableFuture();
  }
}
