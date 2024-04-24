package org.folio.service.titles;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Title;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.service.inventory.InventoryInstanceManager;

import io.vertx.core.Future;

public class TitleInstanceService {

  private final TitlesService titlesService;
  private final InventoryInstanceManager inventoryInstanceManager;

  public TitleInstanceService(TitlesService titlesService, InventoryInstanceManager inventoryInstanceManager) {
    this.titlesService = titlesService;
    this.inventoryInstanceManager = inventoryInstanceManager;
  }

  public Future<String> createTitleInstance(Title title, RequestContext requestContext) {
    return createTitleInstance(title, false, requestContext);
  }

  public Future<String> createTitleInstance(Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    return createTitleInventoryInstance(title, isInstanceMatchingDisabled, requestContext)
      .compose(instId -> createTitleShadowInstance(instId, requestContext));
  }

  private Future<String> createTitleInventoryInstance(Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (title.getInstanceId() != null) {
      return Future.succeededFuture(title.getInstanceId());
    }
    return titlesService.saveTitleWithInstance(title, isInstanceMatchingDisabled, requestContext);
  }

  private Future<String> createTitleShadowInstance(String instanceId, RequestContext requestContext) {
    String targetTenantId = TenantTool.tenantId(requestContext.getHeaders());
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, targetTenantId, requestContext)
      .map(sharingInstance -> instanceId);
  }


}
