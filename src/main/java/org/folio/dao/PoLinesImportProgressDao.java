package org.folio.dao;

import io.vertx.core.Future;

/**
 * DAO component for tracking po lines processing of particular purchase order during data import
 */
public interface PoLinesImportProgressDao {

  /**
   * Saves total number of po lines needed to be processed during data-import
   * and which belong to particular purchase order with specified {@code orderId}.
   *
   * @param orderId      - purchase order id
   * @param totalPoLines - total number of po lines which belong to particular order
   * @param tenantId     - tenant id
   * @return Future of void
   */
  Future<Void> savePoLinesAmountPerOrder(String orderId, int totalPoLines, String tenantId);
  /**
   * Increases by one a number of po lines which have been processed by specified {@code orderId}.
   *
   * @param orderId  - order id
   * @param tenantId - tenant id
   * @return Future of void
   */
  Future<Void> trackProcessedPoLine(String orderId, String tenantId);

  /**
   * Checks whether all po line for particular order with specified {@code orderId} have been processed.
   *
   * @param orderId  - order id
   * @param tenantId - tenant id
   * @return Future with {@code true} if all po lines for particular order have been processed, otherwise {@code false}
   */
  Future<Boolean> poLinesProcessed(String orderId, String tenantId);
}
