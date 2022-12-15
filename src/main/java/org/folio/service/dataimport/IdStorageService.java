package org.folio.service.dataimport;

import io.vertx.core.Future;

/**
 * Service that manages associations between record id and entity id
 */
public interface IdStorageService {

  /**
   * Stores association between specified record id and entity id and returns specified {@code entityId}.
   * If the specified {@code record id} already associated with entity id, then returns
   * that existing (previously associated) entity id without saving association with the specified {@code entityId}.
   *
   * @param recordId - record id
   * @param entityId - entity id
   * @param tenantId - tenant id
   * @return future with entity id that associated with specified {@code recordId}
   */
  Future<String> store(String recordId, String entityId, String tenantId);
}
