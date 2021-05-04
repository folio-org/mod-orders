package org.folio.service.orders.managers;

import java.util.Map;

import org.folio.service.orders.OrderWorkflowType;

public class OrderWorkflowManagerResolver {
  private final Map<OrderWorkflowType, OrderWorkflowManager> managerMap;

  public OrderWorkflowManagerResolver(Map<OrderWorkflowType, OrderWorkflowManager> managerMap) {
    this.managerMap = managerMap;
  }

  public OrderWorkflowManager resolveManager(OrderWorkflowType name) {
    return managerMap.get(name);
  }

}
