package org.folio.service.dataimport;

import io.vertx.core.Future;

/**
 * Service that manages associations between record id and entity id
 */
public interface IdStorageService {

  /**
   * Stores specified record id and created date for this store operation and returns specified saved {@code recordId}.
   *
   * @param recordId - record id
   * @param tenantId - tenant id
   * @return future with entity id that associated with specified {@code recordId}
   */
  Future<String> store(String recordId, String tenantId);

  /**
   * Retrieves specific recordId if exists inside DB. If not - returns empty string.
   * @param recordId - record id
   * @param tenantId - tenant id
   * @return future with record id  {@code recordId}
   */
  Future<String> get(String recordId, String tenantId);
}
