package org.folio.service.caches;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.UserService;
import org.springframework.stereotype.Component;

import java.util.function.BiFunction;

@Log4j2
@Component
@RequiredArgsConstructor
public class AbstractConfigCache {

  private static final String UNIQUE_CACHE_KEY_PATTERN = "%s_%s_%s";

  protected <T> Future<T> cacheData(String url, String query, AsyncCache<String, T> cache,
                                    BiFunction<RequestEntry, RequestContext, Future<T>> configExtractor,
                                    boolean byPassCache, RequestContext requestContext) {
    var requestEntry = new RequestEntry(url).withQuery(query).withOffset(0).withLimit(Integer.MAX_VALUE);
    var cacheKey = buildUniqueKey(requestEntry, requestContext);
    log.debug("loadSettingsData:: Loading setting data, url: '{}', query: '{}', bypass-cache mode: '{}'", url, query, byPassCache);
    if (byPassCache) {
      return extractData(configExtractor, requestContext, requestEntry);
    }
    return Future.fromCompletionStage(cache.get(cacheKey, (key, executor) ->
      extractData(configExtractor, requestContext, requestEntry).toCompletionStage().toCompletableFuture()));
  }

  private <T> Future<T> extractData(BiFunction<RequestEntry, RequestContext, Future<T>> extractor,
                                    RequestContext requestContext, RequestEntry requestEntry) {
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    return extractor.apply(requestEntry, requestContext)
      .onFailure(t -> log.error("Error loading configuration, tenantId: '{}'", tenantId, t));
  }

  private String buildUniqueKey(RequestEntry requestEntry, RequestContext requestContext) {
    var endpoint = requestEntry.buildEndpoint();
    var tenantId = TenantTool.tenantId(requestContext.getHeaders());
    var userId = UserService.getCurrentUserId(requestContext.getHeaders());
    return String.format(UNIQUE_CACHE_KEY_PATTERN, tenantId, userId, endpoint);
  }
}
