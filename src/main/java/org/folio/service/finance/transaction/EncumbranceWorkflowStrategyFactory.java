package org.folio.service.finance.transaction;

import org.folio.service.finance.WorkflowStatusName;

import java.util.EnumMap;
import java.util.Map;
import java.util.Set;

public class EncumbranceWorkflowStrategyFactory {

    private final Map<WorkflowStatusName, EncumbranceWorkflowStrategy> strategyMap =  new EnumMap<>(WorkflowStatusName.class);

    public EncumbranceWorkflowStrategyFactory(Set<EncumbranceWorkflowStrategy> strategies) {
        strategies.forEach(strategy -> this.strategyMap.put(strategy.getStrategyName(), strategy));
    }

    public EncumbranceWorkflowStrategy getStrategy(WorkflowStatusName name) {
        return strategyMap.get(name);
    }

}
