package org.folio.service.dataimport;

import io.vertx.core.Future;

/**
 * Service that manages record id deduplication
 */
public interface IdStorageService {

  /**
   * Stores specific recordId inside DB. If it is already exists - failed future with PgException.
   * @param recordId - record id
   * @param tenantId - tenant id
   * @return future with record id  {@code recordId}
   */
  Future<String> store(String recordId, String tenantId);
}
