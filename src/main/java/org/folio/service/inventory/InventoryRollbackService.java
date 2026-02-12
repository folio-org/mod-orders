package org.folio.service.inventory;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.Set;

import static org.folio.service.inventory.InventoryUtils.HOLDINGS_RECORDS;
import static org.folio.service.inventory.InventoryUtils.INSTANCE_RECORDS_BY_ID_ENDPOINT;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;

@Log4j2
public class InventoryRollbackService {

  private final RestClient restClient;

  public InventoryRollbackService(RestClient restClient) {
    this.restClient = restClient;
  }

  /**
   * Deletes an orphaned instance (instance with no remaining holdings) if applicable.
   * Only deletes instances that were created during order opening (not matched to existing instances).
   * This prevents accumulation of unused instance records during order rollback scenarios.
   *
   * @param poLine              the purchase order line
   * @param createdInstanceIds  set of instance IDs that were created (not matched) during opening
   * @param requestContext      the request context
   * @return future that completes when instance deletion is attempted
   */
  public Future<Void> deleteOrphanedInstanceIfNeeded(PoLine poLine, Set<String> createdInstanceIds,
                                                      RequestContext requestContext) {
    if (StringUtils.isBlank(poLine.getInstanceId())) {
      log.debug("deleteOrphanedInstanceIfNeeded:: No instanceId on poLine, skipping instance deletion");
      return Future.succeededFuture();
    }

    String instanceId = poLine.getInstanceId();
    if (CollectionUtils.isEmpty(createdInstanceIds) || !createdInstanceIds.contains(instanceId)) {
      log.info("deleteOrphanedInstanceIfNeeded:: Instance {} was matched to existing instance, not created, will not delete", instanceId);
      return Future.succeededFuture();
    }

    return hasRemainingHoldings(instanceId, requestContext)
      .compose(hasHoldings -> {
        if (hasHoldings) {
          log.info("deleteOrphanedInstanceIfNeeded:: Instance {} still has holdings, will not delete", instanceId);
          return Future.succeededFuture();
        }
        return deleteInstance(instanceId, requestContext);
      })
      .recover(t -> {
        // Don't fail the entire rollback if instance deletion fails
        log.error("deleteOrphanedInstanceIfNeeded:: Failed to delete orphaned instance {}", instanceId, t);
        return Future.succeededFuture();
      });
  }

  private Future<Boolean> hasRemainingHoldings(String instanceId, RequestContext requestContext) {
    String query = String.format("instanceId==%s", instanceId);
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(HOLDINGS_RECORDS))
      .withQuery(query)
      .withOffset(0)
      .withLimit(1);

    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(response -> {
        int totalRecords = response.getInteger("totalRecords", 0);
        return totalRecords > 0;
      })
      .recover(t -> {
        log.error("hasRemainingHoldings:: Error checking holdings for instance {}", instanceId, t);
        return Future.succeededFuture(true);
      });
  }

  private Future<Void> deleteInstance(String instanceId, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(INVENTORY_LOOKUP_ENDPOINTS.get(INSTANCE_RECORDS_BY_ID_ENDPOINT))
      .withId(instanceId);

    return restClient.delete(requestEntry, requestContext)
      .onSuccess(v -> log.info("deleteInstance:: Successfully deleted instance {}", instanceId))
      .onFailure(t -> log.error("deleteInstance:: Failed to delete instance {}", instanceId, t))
      .mapEmpty();
  }
}
