package org.folio.dao;

import io.vertx.core.Future;

public interface SequentialOrderIdStorageDao {

  /**
   * Stores the association between specified job execution id {@code jobExecutionId},
   * order sequence number {@code sequenceNo} and order id {@code orderId} and returns the specified {@code orderId}.
   * If the specified {@code jobExecutionId} and {@code sequenceNo} are already associated with orderId {@code orderId},
   * it returns the existing (previously associated) {@code orderId}.
   *
   * @param jobExecutionId - job execution id
   * @param sequenceNo - sequence number
   * @param orderId - order id
   * @param tenantId - tenant id
   * @return future with orderId that associated with sequence number and job execution id
   */
  Future<String> store(String jobExecutionId, Integer sequenceNo, String orderId, String tenantId);
}
