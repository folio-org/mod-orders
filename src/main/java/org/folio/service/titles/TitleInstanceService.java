package org.folio.service.titles;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Title;
import org.folio.service.inventory.InventoryInstanceManager;

import io.vertx.core.Future;

public class TitleInstanceService {

  private final InventoryInstanceManager inventoryInstanceManager;

  public TitleInstanceService(InventoryInstanceManager inventoryInstanceManager) {
    this.inventoryInstanceManager = inventoryInstanceManager;
  }

  public Future<String> getOrCreateInstance(Title title, RequestContext requestContext) {
    return getOrCreateInstance(title, false, false, requestContext);
  }

  public Future<String> getOrCreateInstance(Title title, boolean isInstanceMatchingDisabled,
                                            boolean suppressDiscovery, RequestContext requestContext) {
    return createShadowInstance(title.getInstanceId(), requestContext)
      .compose(shadowInstId -> createInventoryInstance(shadowInstId, title, isInstanceMatchingDisabled, suppressDiscovery, requestContext));
  }

  private Future<String> createShadowInstance(String instanceId, RequestContext requestContext) {
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, requestContext)
      .map(sharingInstance -> sharingInstance != null ? instanceId : null);
  }

  private Future<String> createInventoryInstance(String shadowInstId, Title title, boolean isInstanceMatchingDisabled,
                                                 boolean suppressDiscovery, RequestContext requestContext) {
    if (shadowInstId != null) {
      return Future.succeededFuture(shadowInstId);
    }
    if (title.getInstanceId() != null) {
      return Future.succeededFuture(title.getInstanceId());
    }
    return inventoryInstanceManager.getOrCreateInstanceRecord(title, isInstanceMatchingDisabled, suppressDiscovery, requestContext);
  }
}
