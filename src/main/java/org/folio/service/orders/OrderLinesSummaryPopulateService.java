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
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.exchange.CustomExchangeRateProvider;
import org.javamoney.moneta.Money;

import io.vertx.core.Future;

import static org.folio.orders.utils.HelperUtils.buildConversionQuery;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class OrderLinesSummaryPopulateService implements CompositeOrderDynamicDataPopulateService {

  protected final Logger logger = LogManager.getLogger(OrderLinesSummaryPopulateService.class);

  private final CommonSettingsCache commonSettingsCache;
  private final CacheableExchangeRateService cacheableExchangeRateService;

  public OrderLinesSummaryPopulateService(CommonSettingsCache commonSettingsCache,
                                          CacheableExchangeRateService cacheableExchangeRateService) {
    this.commonSettingsCache = commonSettingsCache;
    this.cacheableExchangeRateService = cacheableExchangeRateService;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder,
      RequestContext requestContext) {
    CompositePurchaseOrder compPO = holder.getOrder();
    List<PoLine> poLines = holder.getOrder().getPoLines();
    return calculateTotalEstimatedPrice(poLines, requestContext).map(totalAmount -> {
      compPO.setTotalEstimatedPrice(totalAmount);
      compPO.setTotalItems(calculateTotalItemsQuantity(poLines));
      return holder;
    });
  }

  /**
   * Calculates PO's estimated price by summing the Estimated Price of the associated PO Lines. See MODORDERS-181 for more details.
   * At the moment assumption is that all prices could be in the different currency.
   *
   * @param poLines list of PO Lines
   * @return estimated purchase order's total price
   */
  public Future<Double> calculateTotalEstimatedPrice(List<PoLine> poLines,
      RequestContext requestContext) {
    return commonSettingsCache.getSystemCurrency(requestContext)
      .compose(toCurrency -> getExchangeRatesPerPoLine(poLines, toCurrency, requestContext)
      .map(poLineExchangeRate -> Pair.of(toCurrency, poLineExchangeRate))
      .map(toCurrencyPolExcRates -> convertEstimatedPrice(poLines, toCurrencyPolExcRates)));
  }

  private Future<Map<String, ExchangeRate>> getExchangeRatesPerPoLine(List<PoLine> poLines, String toCurrency, RequestContext requestContext) {
    var poLineExchangeRateFutures = new ArrayList<Future<Map<String, ExchangeRate>>>();
    poLines.forEach(poLine -> {
      var cost = poLine.getCost();
      poLineExchangeRateFutures.add(cacheableExchangeRateService.getExchangeRate(cost.getCurrency(), toCurrency, cost.getExchangeRate(), requestContext)
        .map(exchangeRate -> Map.of(poLine.getId(), exchangeRate)));
    });
    return collectResultsOnSuccess(poLineExchangeRateFutures)
      .map(OrderLinesSummaryPopulateService::transformToSingleMap);
  }

  private static Map<String, ExchangeRate> transformToSingleMap(List<Map<String, ExchangeRate>> polExcRatePairList) {
    return polExcRatePairList.stream()
      .map(Map::entrySet).flatMap(Collection::stream)
      .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
  }

  private Double convertEstimatedPrice(List<PoLine> poLines,
                                       Pair<String, Map<String, ExchangeRate>> toCurrencyPoLineExchangeRates) {
    var toCurrency = toCurrencyPoLineExchangeRates.getLeft();
    var poLineExchangeRates = toCurrencyPoLineExchangeRates.getRight();
    return poLines.stream()
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

  private int calculateTotalItemsQuantity(List<PoLine> poLines) {
    return poLines.stream()
      .mapToInt(HelperUtils::calculateTotalQuantity)
      .sum();
  }
}
