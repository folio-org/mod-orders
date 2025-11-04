
package org.folio.service.caches;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.dataexport.ExportConfigsRetriever;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CONFIGURATIONS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

@Log4j2
@Component
@RequiredArgsConstructor
public class ExportConfigsCache extends AbstractConfigCache {

  private static final String TYPE_QUERY = "type==%s";

  private final ExportConfigsRetriever exportConfigsRetriever;
  private AsyncCache<String, JsonObject> configsCache;

  @Value("${orders.cache.configuration-entries.expiration.time.seconds:30}")
  private long cacheExpirationTime;
  @Value("${orders.cache.configuration-entries.bypass-cache:false}")
  private boolean byPassCache;

  @PostConstruct
  void init() {
    var context = Vertx.currentContext();
    this.configsCache = buildAsyncCache(context, cacheExpirationTime);
  }

  public Future<JsonObject> loadExportConfigs(String exportType, RequestContext requestContext) {
    return cacheData(resourcesPath(DATA_EXPORT_SPRING_CONFIGURATIONS), TYPE_QUERY.formatted(exportType), configsCache,
      exportConfigsRetriever::getExportConfigs, byPassCache, requestContext);
  }

}
