package org.folio.service.exchange;

import com.github.benmanes.caffeine.cache.AsyncCache;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import jakarta.annotation.PostConstruct;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.acq.model.finance.ExchangeRate.OperationMode;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.CacheUtils.buildAsyncCache;
import static org.folio.orders.utils.ResourcePathResolver.EXCHANGE_RATE;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

@Log4j2
@Service
public class CacheableExchangeRateService {

  private static final String FROM = "from";
  private static final String TO = "to";

  private final RestClient restClient;
  private AsyncCache<String, Optional<ExchangeRate>> asyncCache;

  @Value("${mod.orders.exchange-rate.expiration.time.seconds:360}")
  private long cacheExpirationTime;

  public CacheableExchangeRateService(RestClient restClient) {
    this.restClient = restClient;
  }

  @PostConstruct
  void init() {
    this.asyncCache = buildAsyncCache(Vertx.currentContext(), cacheExpirationTime);
  }

  public Future<ExchangeRate> getExchangeRate(String from, String to, Number customExchangeRate, RequestContext requestContext) {
    return getExchangeRate(from, to, customExchangeRate, OperationMode.MULTIPLY, requestContext);
  }

  public Future<ExchangeRate> getExchangeRate(String from, String to, Number customExchangeRate, OperationMode operationMode, RequestContext requestContext) {
    if (StringUtils.equals(from, to)) {
      return Future.succeededFuture(createDefaultExchangeRate(from, to, 1d));
    }
    if (Objects.nonNull(customExchangeRate)) {
      log.info("getExchangeRate:: Retrieving an exchange rate, {} -> {}, customExchangeRate: {}", from, to, customExchangeRate);
      return Future.succeededFuture(createDefaultExchangeRate(from, to, customExchangeRate).withOperationMode(operationMode));
    }
    try {
      var cacheKey = String.format("%s-%s", from, to);
      return Future.fromCompletionStage(asyncCache.get(cacheKey, (key, executor) -> getExchangeRateFromRemote(from, to, requestContext)))
        .compose(exchangeRateOptional -> exchangeRateOptional.map(Future::succeededFuture)
          .orElse(Future.failedFuture(new IllegalStateException("Cannot retrieve exchange rate from API using from=%s, and to=%s currencies".formatted(from, to))))
          .onSuccess(exchangeRate -> log.info("getExchangeRate:: Retrieving an exchange rate, {} -> {}, exchangeRate: {}, operationMode: {}",
            from, to, exchangeRate.getExchangeRate(), exchangeRate.getOperationMode())));
    } catch (Exception e) {
      log.error("Error when retrieving cacheable exchange rate", e);
      return Future.failedFuture(e);
    }
  }

  private ExchangeRate createDefaultExchangeRate(String from, String to, Number exchangeRateValue) {
    return new ExchangeRate().withFrom(from).withTo(to).withExchangeRate(exchangeRateValue.doubleValue());
  }

  private CompletableFuture<Optional<ExchangeRate>> getExchangeRateFromRemote(String from, String to, RequestContext requestContext) {
    var requestEntry = new RequestEntry(resourcesPath(EXCHANGE_RATE))
      .withQueryParameter(FROM, from)
      .withQueryParameter(TO, to);
    return restClient.get(requestEntry, ExchangeRate.class, requestContext)
      .map(Optional::ofNullable)
      .toCompletionStage().toCompletableFuture();
  }
}
