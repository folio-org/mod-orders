package org.folio.service.finance;

import static org.folio.orders.utils.ResourcePathResolver.FISCAL_YEARS;
import static org.folio.orders.utils.ResourcePathResolver.LEDGER_CURRENT_FISCAL_YEAR;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.core.exceptions.ErrorCodes.CURRENT_FISCAL_YEAR_NOT_FOUND;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletionException;
import java.util.concurrent.TimeUnit;

import com.github.benmanes.caffeine.cache.AsyncCache;
import com.github.benmanes.caffeine.cache.Caffeine;
import io.vertx.core.Vertx;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.FiscalYearCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

import io.vertx.core.Future;
import org.folio.rest.tools.utils.TenantTool;
import org.springframework.beans.factory.annotation.Value;

public class FiscalYearService {

  private static final String LEDGER_CURRENT_FISCAL_YEAR_ENDPOINT = resourcesPath(LEDGER_CURRENT_FISCAL_YEAR);
  private static final String FISCAL_YEARS_ENDPOINT = resourcesPath(FISCAL_YEARS);
  private static final String FISCAL_YEAR_BY_ID_ENDPOINT = resourceByIdPath(FISCAL_YEARS, "{id}");
  private static final String FISCAL_YEAR_BY_SERIES_QUERY = "series==\"%s\" sortBy periodStart";
  private static final String CACHE_KEY_TEMPLATE = "%s.%s";

  @Value("${orders.cache.fiscal-years.expiration.time.seconds:300}")
  private long cacheExpirationTime;
  private final AsyncCache<String, String> currentFiscalYearCacheBySeries;
  private final AsyncCache<String, String> seriesCacheByFiscalYearId;

  private final RestClient restClient;
  private final FundService fundService;


  public FiscalYearService(RestClient restClient, FundService fundService) {
    this.restClient = restClient;
    this.fundService = fundService;
    currentFiscalYearCacheBySeries = Caffeine.newBuilder()
      .expireAfterWrite(cacheExpirationTime, TimeUnit.SECONDS)
      .executor(task -> Vertx.currentContext().runOnContext(v -> task.run()))
      .buildAsync();
    seriesCacheByFiscalYearId = Caffeine.newBuilder()
      .expireAfterWrite(cacheExpirationTime, TimeUnit.SECONDS)
      .executor(task -> Vertx.currentContext().runOnContext(v -> task.run()))
      .buildAsync();
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
   * @param fiscalYearId fiscal year ID
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

  private Future<FiscalYear> getFiscalYearById(String fiscalYearId, RequestContext requestContext) {
    var requestEntry = new RequestEntry(FISCAL_YEAR_BY_ID_ENDPOINT).withId(fiscalYearId);
    return restClient.get(requestEntry, FiscalYear.class, requestContext);
  }

  private Future<String> getCurrentFiscalYearForSeries(String series, RequestContext requestContext) {
    var cacheKey = CACHE_KEY_TEMPLATE.formatted(series, TenantTool.tenantId(requestContext.getHeaders()));
    return Future.fromCompletionStage(currentFiscalYearCacheBySeries.get(cacheKey, (key, executor) -> {
      var query = FISCAL_YEAR_BY_SERIES_QUERY.formatted(series);
      var requestEntry = new RequestEntry(FISCAL_YEARS_ENDPOINT).withQuery(query);
      return restClient.get(requestEntry, FiscalYearCollection.class, requestContext)
          .map(fiscalYearCollection -> fiscalYearCollection.getFiscalYears().get(0).getId())
          .toCompletionStage().toCompletableFuture();
      }));
  }

  private boolean isFiscalYearNotFound(Throwable t) {
    return t instanceof HttpException && ((HttpException) t).getCode() == 404;
  }

}
