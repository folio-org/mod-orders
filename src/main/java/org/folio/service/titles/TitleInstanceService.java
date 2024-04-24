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

  public Future<String> updateTitleWithInstance(Title title, RequestContext requestContext) {
    return updateTitleWithInstance(title, false, requestContext);
  }

  public Future<String> updateTitleWithInstance(Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    return getOrCreateTitleInstance(title, isInstanceMatchingDisabled, requestContext)
      .map(title::withInstanceId)
      .compose(entity -> titlesService.saveTitle(entity, requestContext)
        .map(v -> entity.getInstanceId()));
  }

  public Future<String> getOrCreateTitleInstance(Title title, RequestContext requestContext) {
    return getOrCreateTitleInstance(title, false, requestContext);
  }

  public Future<String> getOrCreateTitleInstance(Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    return createTitleShadowInstance(title.getInstanceId(), requestContext)
      .compose(shadowInstId -> createTitleInventoryInstance(shadowInstId, title, isInstanceMatchingDisabled, requestContext));
  }


  private Future<String> createTitleInventoryInstance(String shadowInstId, Title title, boolean isInstanceMatchingDisabled, RequestContext requestContext) {
    if (shadowInstId != null) {
      return Future.succeededFuture(shadowInstId);
    } else if (title.getInstanceId() != null) {
      return Future.succeededFuture(title.getInstanceId());
    }
    return inventoryInstanceManager.getOrCreateInstanceRecord(title, isInstanceMatchingDisabled, requestContext);
  }

  private Future<String> createTitleShadowInstance(String instanceId, RequestContext requestContext) {
    String targetTenantId = TenantTool.tenantId(requestContext.getHeaders());
    return inventoryInstanceManager.createShadowInstanceIfNeeded(instanceId, targetTenantId, requestContext)
      .map(sharingInstance -> sharingInstance != null ? instanceId : null);
  }


}
