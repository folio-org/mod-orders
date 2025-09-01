package org.folio.service.finance;

import static one.util.streamex.StreamEx.ofSubLists;
import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.FISCAL_YEARS;
import static org.folio.orders.utils.ResourcePathResolver.LEDGER_CURRENT_FISCAL_YEAR;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.MAX_IDS_FOR_GET_RQ_15;
import static org.folio.rest.core.exceptions.ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND;

import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletionException;
import java.util.stream.Collectors;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Vertx;
import jakarta.annotation.PostConstruct;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.FiscalYearCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

import io.vertx.core.Future;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.caches.CommonSettingsCache;
import org.springframework.beans.factory.annotation.Value;

@Log4j2
public class FiscalYearService {

  private static final String LEDGER_CURRENT_FISCAL_YEAR_ENDPOINT = resourcesPath(LEDGER_CURRENT_FISCAL_YEAR);
  private static final String FISCAL_YEARS_ENDPOINT = resourcesPath(FISCAL_YEARS);
  private static final String FISCAL_YEAR_BY_ID_ENDPOINT = resourceByIdPath(FISCAL_YEARS, "{id}");
  private static final String FISCAL_YEAR_BY_SERIES_QUERY = "series==\"%s\" AND periodEnd>=%s sortBy periodStart";
  private static final String CACHE_KEY_TEMPLATE = "%s.%s";

  private final RestClient restClient;
  private final FundService fundService;
  private final CommonSettingsCache commonSettingsCache;
  private AsyncCache<String, String> currentFiscalYearCacheBySeries;
  private AsyncCache<String, String> seriesCacheByFiscalYearId;

  @Value("${orders.cache.fiscal-years.expiration.time.seconds:300}")
  private long cacheExpirationTime;

  public FiscalYearService(RestClient restClient, FundService fundService, CommonSettingsCache commonSettingsCache) {
    this.restClient = restClient;
    this.fundService = fundService;
    this.commonSettingsCache = commonSettingsCache;
  }

  @PostConstruct
  void init() {
    var context = Vertx.currentContext();
    this.currentFiscalYearCacheBySeries = buildAsyncCache(context, cacheExpirationTime);
    this.seriesCacheByFiscalYearId = buildAsyncCache(context, cacheExpirationTime);
  }

  public Future<FiscalYear> getCurrentFiscalYear(String ledgerId, RequestContext requestContext) {
    var requestEntry = new RequestEntry(LEDGER_CURRENT_FISCAL_YEAR_ENDPOINT).withId(ledgerId);
    return restClient.get(requestEntry, FiscalYear.class, requestContext)
      .recover(t -> {
        Throwable cause = Objects.nonNull(t.getCause()) ? t.getCause() : t;
        if (isFiscalYearNotFound(cause)) {
          List<Parameter> parameters = Collections.singletonList(new Parameter().withValue(ledgerId)
            .withKey("ledgerId"));
          throw new HttpException(404, CURRENT_FISCAL_YEAR_NOT_FOUND.toError()
            .withParameters(parameters));
        }
        throw new CompletionException(cause);
      });
  }

  public Future<FiscalYear> getCurrentFiscalYearByFundId(String fundId, RequestContext requestContext) {
    return fundService.retrieveFundById(fundId, requestContext)
      .compose(fund -> getCurrentFiscalYear(fund.getLedgerId(), requestContext));
  }

  /**
   * Retrieves the current fiscal year ID for the series related to the fiscal year corresponding to the given ID.
   *
   * @param fiscalYearId   fiscal year ID
   * @param requestContext {@link RequestContext} to be used for the request
   * @return future with the current fiscal year ID
   */
  public Future<String> getCurrentFYForSeriesByFYId(String fiscalYearId, RequestContext requestContext) {
    return getSeriesByFiscalYearId(fiscalYearId, requestContext)
      .compose(series -> getCurrentFiscalYearForSeries(series, requestContext));
  }

  private Future<String> getSeriesByFiscalYearId(String fiscalYearId, RequestContext requestContext) {
    var cacheKey = CACHE_KEY_TEMPLATE.formatted(fiscalYearId, TenantTool.tenantId(requestContext.getHeaders()));
    return Future.fromCompletionStage(seriesCacheByFiscalYearId.get(cacheKey, (key, executor) ->
      getFiscalYearById(fiscalYearId, requestContext)
        .map(FiscalYear::getSeries)
        .toCompletionStage().toCompletableFuture()));
  }

  public Future<FiscalYear> getFiscalYearById(String fiscalYearId, RequestContext requestContext) {
    var requestEntry = new RequestEntry(FISCAL_YEAR_BY_ID_ENDPOINT).withId(fiscalYearId);
    return restClient.get(requestEntry, FiscalYear.class, requestContext)
      .onFailure(t -> log.error("Unable to fetch fiscal year by id: {}", fiscalYearId, t));
  }

  public Future<List<FiscalYear>> getAllFiscalYears(Collection<String> fiscalYearIds, RequestContext requestContext) {
    Set<String> uniqueFiscalYearIds = new LinkedHashSet<>(fiscalYearIds);
    return collectResultsOnSuccess(
      ofSubLists(new ArrayList<>(uniqueFiscalYearIds), MAX_IDS_FOR_GET_RQ_15)
        .map(ids-> getAllFiscalYearsByIds(ids, requestContext))
        .toList())
      .map(lists -> lists.stream()
        .flatMap(Collection::stream)
        .collect(Collectors.toList()));
  }

  private Future<List<FiscalYear>> getAllFiscalYearsByIds(Collection<String> ids, RequestContext requestContext) {
    String query = convertIdsToCqlQuery(ids);
    RequestEntry requestEntry = new RequestEntry(FISCAL_YEARS_ENDPOINT).withQuery(query)
      .withLimit(MAX_IDS_FOR_GET_RQ_15)
      .withOffset(0);
    return restClient.get(requestEntry, FiscalYearCollection.class, requestContext)
      .map(FiscalYearCollection::getFiscalYears);
  }

  private Future<String> getCurrentFiscalYearForSeries(String series, RequestContext requestContext) {
    var cacheKey = CACHE_KEY_TEMPLATE.formatted(series, TenantTool.tenantId(requestContext.getHeaders()));
    return Future.fromCompletionStage(currentFiscalYearCacheBySeries.get(cacheKey, (key, executor) ->
      commonSettingsCache.getSystemTimeZone(requestContext)
        .map(timezone -> Instant.now().atZone(ZoneId.of(timezone)).toLocalDate())
        .map(now -> FISCAL_YEAR_BY_SERIES_QUERY.formatted(series, now))
        .map(query -> new RequestEntry(FISCAL_YEARS_ENDPOINT).withQuery(query).withLimit(3).withOffset(0))
        .compose(requestEntry -> restClient.get(requestEntry, FiscalYearCollection.class, requestContext))
        .map(fiscalYearCollection -> extractCurrentFiscalYearId(fiscalYearCollection.getFiscalYears()))
        .onFailure(t -> log.error("Unable to fetch current fiscal year for series: {}", series, t))
        .toCompletionStage().toCompletableFuture()));
  }

  private String extractCurrentFiscalYearId(List<FiscalYear> fiscalYears) {
    if (fiscalYears.isEmpty()) {
      return null;
    }
    if (fiscalYears.size() == 1) {
      return fiscalYears.getFirst().getId();
    }
    var now = new Date();
    var firstYear = fiscalYears.get(0);
    var secondYear = fiscalYears.get(1);
    if (firstYear.getPeriodStart().before(now) && firstYear.getPeriodEnd().after(now)
      && secondYear.getPeriodStart().before(now) && secondYear.getPeriodEnd().after(now)
      && firstYear.getPeriodEnd().after(secondYear.getPeriodStart())) {
      return secondYear.getId();
    }
    return firstYear.getId();
  }

  private boolean isFiscalYearNotFound(Throwable t) {
    return t instanceof HttpException ht && ht.getCode() == 404;
  }
}
