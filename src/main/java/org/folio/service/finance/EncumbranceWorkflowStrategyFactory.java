package org.folio.service.finance;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class EncumbranceWorkflowStrategyFactory {

    private final Map<WorkflowStatusName, EncumbranceWorkflowStrategy> strategyMap =  new HashMap<>();

    public EncumbranceWorkflowStrategyFactory(Set<EncumbranceWorkflowStrategy> strategies) {
        strategies.forEach(strategy -> this.strategyMap.put(strategy.getStrategyName(), strategy));
    }

    public EncumbranceWorkflowStrategy getStrategy(WorkflowStatusName name) {
        return strategyMap.get(name);
    }

}
