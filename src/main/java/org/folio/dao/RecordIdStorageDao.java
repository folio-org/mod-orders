package org.folio.dao;

import io.vertx.core.Future;

/**
 * DAO which manages processed_records -table
 */
public interface RecordIdStorageDao {

  /**
   * Stores specific recordId inside DB. If it is already exists - failed future with PgException.
   *
   * @param recordId - record id
   * @param tenantId - tenant id
   * @return future with record id that was added {@code recordId}
   */
  Future<String> store(String recordId, String tenantId);
}
