package org.folio.service.orders;

import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.folio.service.exchange.ExchangeRateProviderResolver;
import org.javamoney.moneta.Money;

import javax.money.convert.ConversionQuery;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.HelperUtils.getConversionQuery;

public class OrderLinesSummaryPopulateService implements CompositeOrderDynamicDataPopulate {

    private final ConfigurationEntriesService configurationEntriesService;
    private final ExchangeRateProviderResolver exchangeRateProviderResolver;

    public OrderLinesSummaryPopulateService(ConfigurationEntriesService configurationEntriesService, ExchangeRateProviderResolver exchangeRateProviderResolver) {
        this.configurationEntriesService = configurationEntriesService;
        this.exchangeRateProviderResolver = exchangeRateProviderResolver;
    }

    @Override
    public CompletableFuture<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
        CompositePurchaseOrder compPO = holder.getOrder();
        List<CompositePoLine> compositePoLines = holder.getOrder().getCompositePoLines();
        return calculateTotalEstimatedPrice(compositePoLines, requestContext).thenApply(totalAmount -> {
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
    public CompletableFuture<Double> calculateTotalEstimatedPrice(List<CompositePoLine> compositePoLines, RequestContext requestContext) {
        return configurationEntriesService.getSystemCurrency(requestContext).thenApply(toCurrency -> compositePoLines.stream()
                .map(CompositePoLine::getCost)
                .map(cost -> {
                    Money money = Money.of(cost.getPoLineEstimatedPrice(), cost.getCurrency());
                    if (money.getCurrency().getCurrencyCode().equals(toCurrency)) {
                        return money;
                    }
                    Double exchangeRate = cost.getExchangeRate();
                    ConversionQuery conversionQuery = getConversionQuery(exchangeRate, cost.getCurrency(), toCurrency);
                    var exchangeRateProvider = exchangeRateProviderResolver.resolve(conversionQuery, requestContext);
                    var conversion = exchangeRateProvider.getCurrencyConversion(conversionQuery);

                    return money.with(conversion);
                })
                .reduce(Money.of(0, toCurrency), Money::add)
                .getNumber()
                .doubleValue());
    }

    private int calculateTotalItemsQuantity(List<CompositePoLine> poLines) {
        return poLines.stream().mapToInt(HelperUtils::calculateTotalQuantity).sum();
    }
}
