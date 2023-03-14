package org.folio.service.dataimport;

import io.vertx.core.Future;

/**
 * Service for tracking po lines processing of particular purchase order during data import
 */
public interface PoLineImportProgressService {

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
   * Increases by one a number of po lines which have been processed by specified {@code orderId}
   * and checks whether all po lines have been processed for order with specified orderId.
   *
   * @param orderId  - order id
   * @param tenantId - tenant id
   * @return Future with {@code true} if all po lines for have been processed, otherwise Future with {@code false}
   */
  Future<Boolean> trackProcessedPoLine(String orderId, String tenantId);

  /**
   * Checks whether all po line for particular order with specified {@code orderId} have been processed.
   *
   * @param orderId  - order id
   * @param tenantId - tenant id
   * @return Future with {@code true} if all po lines for particular order have been processed, otherwise {@code false}
   */
  Future<Boolean> poLinesProcessed(String orderId, String tenantId);

}
