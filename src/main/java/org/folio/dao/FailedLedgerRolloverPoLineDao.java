package org.folio.dao;

import io.vertx.core.Future;
import org.folio.models.FailedLedgerRolloverPoLineDto;

public interface FailedLedgerRolloverPoLineDao {
  /**
   * Saves the record with information of failed poLine during rollover
   * with specified {@code orderId}.
   *
   * @param dto      - DTO of failed_ledger_rollover_po_line table
   * @param tenantId - tenant id
   * @return Future of void
   */
  Future<Void> saveFailedRolloverRecord(FailedLedgerRolloverPoLineDto dto, String tenantId);
}
