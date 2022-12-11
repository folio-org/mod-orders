package org.folio.dao;

import io.vertx.core.Future;

public interface IdStorageDao {

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
