package org.folio.service.orders;

import static org.folio.orders.utils.HelperUtils.getConversionQuery;

import java.util.List;
import java.util.stream.Collectors;

import javax.money.convert.ConversionQuery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.caches.ConfigurationEntriesCache;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.javamoney.moneta.Money;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;

public class OrderLinesSummaryPopulateService implements CompositeOrderDynamicDataPopulateService {
  protected final Logger logger = LogManager.getLogger(OrderLinesSummaryPopulateService.class);

  private final ConfigurationEntriesCache configurationEntriesCache;
  private final ExchangeRateProviderResolver exchangeRateProviderResolver;

  public OrderLinesSummaryPopulateService(ConfigurationEntriesCache configurationEntriesCache,
      ExchangeRateProviderResolver exchangeRateProviderResolver) {
    this.configurationEntriesCache = configurationEntriesCache;
    this.exchangeRateProviderResolver = exchangeRateProviderResolver;
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
   * @param poLines list of composite PO Lines
   * @return estimated purchase order's total price
   */
  public Future<Double> calculateTotalEstimatedPrice(List<PoLine> poLines,
      RequestContext requestContext) {
    return configurationEntriesCache.getSystemCurrency(requestContext)
      .compose(toCurrency -> getCollect(poLines, requestContext, toCurrency)
        .map(amounts -> amounts.stream()
        .reduce(Money.of(0, toCurrency), Money::add)
        .getNumber()
        .doubleValue()));
  }

  private Future<List<Money>> getCollect(List<PoLine> poLines, RequestContext requestContext, String toCurrency) {
    var futures = poLines.stream()
      .map(PoLine::getCost)
      .map(cost -> requestContext.getContext().<Money>executeBlocking(blockingFuture -> {
        Money money = Money.of(cost.getPoLineEstimatedPrice(), cost.getCurrency());
        if (money.getCurrency().getCurrencyCode().equals(toCurrency)) {
          blockingFuture.complete(money);
        } else {
          Double exchangeRate = cost.getExchangeRate();
          ConversionQuery conversionQuery = getConversionQuery(exchangeRate, cost.getCurrency(), toCurrency);
          var exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
          var conversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);
          blockingFuture.complete(money.with(conversion));
        }
      }))
      .collect(Collectors.toList());
    return GenericCompositeFuture.join(futures)
      .map(CompositeFuture::list);
  }

  private int calculateTotalItemsQuantity(List<PoLine> poLines) {
    return poLines.stream()
      .mapToInt(HelperUtils::calculateTotalQuantity)
      .sum();
  }
}
