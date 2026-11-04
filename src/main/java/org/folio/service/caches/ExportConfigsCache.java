
package org.folio.service.caches;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.dataexport.ExportConfigsRetriever;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CONFIGURATIONS;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_EFFECTIVE_CONFIGS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

@Log4j2
@Component
@RequiredArgsConstructor
public class ExportConfigsCache extends AbstractConfigCache {

  private static final String TYPE_QUERY = "type==%s";
  private static final String VENDOR_ID_PARAMETER = "vendorId";
  private static final String TYPE_PARAMETER = "type";
  private static final String ACQUISITION_METHOD_PARAMETER = "acquisitionMethod";
  private static final String EXPORT_TYPE_CLAIMS = "CLAIMS";

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

  /**
   * Loads the Claims integrations that apply to a PO line, as a map of config name to config.
   *
   * @param vendorId          vendor of the PO line's order
   * @param acquisitionMethod acquisition method of the PO line, null if the PO line has none
   * @param requestContext    Headers to make HTTP requests
   * @return Future of the matching configs, empty if no integration applies
   */
  public Future<JsonObject> loadClaimsConfigsForPoLine(String vendorId, String acquisitionMethod, RequestContext requestContext) {
    var requestEntry = new RequestEntry(resourcesPath(DATA_EXPORT_SPRING_EFFECTIVE_CONFIGS))
      .withQueryParameter(VENDOR_ID_PARAMETER, vendorId)
      .withQueryParameter(TYPE_PARAMETER, EXPORT_TYPE_CLAIMS);
    if (acquisitionMethod != null) {
      requestEntry.withQueryParameter(ACQUISITION_METHOD_PARAMETER, acquisitionMethod);
    }
    return cacheData(requestEntry, configsCache, exportConfigsRetriever::getExportConfigs, byPassCache, requestContext);
  }

}
