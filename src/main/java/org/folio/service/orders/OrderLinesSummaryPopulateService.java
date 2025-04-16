package org.folio.service.orders;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.exchange.CustomExchangeRateProvider;
import org.javamoney.moneta.Money;

import io.vertx.core.Future;

import static org.folio.orders.utils.HelperUtils.buildConversionQuery;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class OrderLinesSummaryPopulateService implements CompositeOrderDynamicDataPopulateService {

  protected final Logger logger = LogManager.getLogger(OrderLinesSummaryPopulateService.class);

  private final ConfigurationEntriesCache configurationEntriesCache;
  private final CacheableExchangeRateService cacheableExchangeRateService;

  public OrderLinesSummaryPopulateService(ConfigurationEntriesCache configurationEntriesCache,
                                          CacheableExchangeRateService cacheableExchangeRateService) {
    this.configurationEntriesCache = configurationEntriesCache;
    this.cacheableExchangeRateService = cacheableExchangeRateService;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    CompositePurchaseOrder compPO = holder.getOrder();
    List<CompositePoLine> compositePoLines = holder.getOrder().getCompositePoLines();
    return calculateTotalEstimatedPrice(compositePoLines, requestContext).map(totalAmount -> {
      compPO.setTotalEstimatedPrice(totalAmount);
      compPO.setTotalItems(calculateTotalItemsQuantity(compositePoLines));
      return holder;
    });
  }

  /**
   * Calculates PO's estimated price by summing the Estimated Price of the associated PO Lines. See MODORDERS-181 for more details.
   * At the moment assumption is that all prices could be in the different currency.
   *
   * @param compositePoLines list of composite PO Lines
   * @return estimated purchase order's total price
   */
  public Future<Double> calculateTotalEstimatedPrice(List<CompositePoLine> compositePoLines,
      RequestContext requestContext) {
    return configurationEntriesCache.getSystemCurrency(requestContext)
      .compose(toCurrency -> getExchangeRatesPerPoLine(compositePoLines, toCurrency, requestContext)
      .compose(poLineExchangeRate -> Future.succeededFuture(Pair.of(toCurrency, poLineExchangeRate)))
      .compose(toCurrencyPolExcRates -> Future.succeededFuture(convertEstimatedPrice(compositePoLines, toCurrencyPolExcRates))));
  }

  private Future<Map<String, ExchangeRate>> getExchangeRatesPerPoLine(List<CompositePoLine> poLines, String toCurrency, RequestContext requestContext) {
    var poLineExchangeRateFutures = new ArrayList<Future<Map<String, ExchangeRate>>>();
    poLines.forEach(poLine -> {
      var cost = poLine.getCost();
      poLineExchangeRateFutures.add(cacheableExchangeRateService.getExchangeRate(cost.getCurrency(), toCurrency, cost.getExchangeRate(), requestContext)
        .compose(exchangeRate -> Future.succeededFuture(Map.of(poLine.getId(), exchangeRate))));
    });
    return collectResultsOnSuccess(poLineExchangeRateFutures)
      .map(OrderLinesSummaryPopulateService::transformToSingleMap);
  }

  private static Map<String, ExchangeRate> transformToSingleMap(List<Map<String, ExchangeRate>> polExcRatePairList) {
    return polExcRatePairList.stream()
      .map(Map::entrySet).flatMap(Collection::stream)
      .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
  }

  private Double convertEstimatedPrice(List<CompositePoLine> compositePoLines,
                                       Pair<String, Map<String, ExchangeRate>> toCurrencyPoLineExchangeRates) {
    var toCurrency = toCurrencyPoLineExchangeRates.getLeft();
    var poLineExchangeRates = toCurrencyPoLineExchangeRates.getRight();
    return compositePoLines.stream()
      .map(poLine -> {
        var exchangeRate = poLineExchangeRates.get(poLine.getId());
        var amount = Money.of(poLine.getCost().getPoLineEstimatedPrice(), poLine.getCost().getCurrency());
        if (StringUtils.equals(exchangeRate.getFrom(), exchangeRate.getTo())) {
          return amount;
        } else {
          var provider = new CustomExchangeRateProvider();
          var query = buildConversionQuery(poLine.getCost().getCurrency(), toCurrency, exchangeRate.getExchangeRate());
          var conversion = provider.getCurrencyConversion(query);
          return amount.with(conversion);
        }
      })
      .reduce(Money.of(0, toCurrency), Money::add)
      .getNumber()
      .doubleValue();
  }

  private int calculateTotalItemsQuantity(List<CompositePoLine> poLines) {
    return poLines.stream()
      .mapToInt(HelperUtils::calculateTotalQuantity)
      .sum();
  }
}
