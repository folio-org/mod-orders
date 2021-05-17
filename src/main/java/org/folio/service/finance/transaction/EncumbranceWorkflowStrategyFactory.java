package org.folio.service.finance.transaction;

import org.folio.service.orders.OrderWorkflowType;

import java.util.EnumMap;
import java.util.Map;
import java.util.Set;

public class EncumbranceWorkflowStrategyFactory {

    private final Map<OrderWorkflowType, EncumbranceWorkflowStrategy> strategyMap =  new EnumMap<>(OrderWorkflowType.class);

    public EncumbranceWorkflowStrategyFactory(Set<EncumbranceWorkflowStrategy> strategies) {
        strategies.forEach(strategy -> this.strategyMap.put(strategy.getStrategyName(), strategy));
    }

    public EncumbranceWorkflowStrategy getStrategy(OrderWorkflowType name) {
        return strategyMap.get(name);
    }

}
