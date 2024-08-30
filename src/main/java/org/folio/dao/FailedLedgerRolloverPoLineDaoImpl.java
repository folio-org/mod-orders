package org.folio.dao;

import static org.folio.dao.util.DbUtils.prepareFullTableName;

import org.folio.dao.util.PostgresClientFactory;
import org.folio.models.FailedLedgerRolloverPoLineDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import io.vertx.core.Future;
import io.vertx.sqlclient.Tuple;

@Repository
public class FailedLedgerRolloverPoLineDaoImpl implements FailedLedgerRolloverPoLineDao {
  private static final String TABLE_NAME = "failed_ledger_rollover_po_line";
  private static final String SAVE_FAILED_LEDGER_ROLLOVER_PO_LINE_SQL =
    "INSERT INTO %s (id, rollover_id, ledger_id, po_line_id, request_body, response_body, status_code, workflow_status) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)";
  private final PostgresClientFactory pgClientFactory;

  @Autowired
  public FailedLedgerRolloverPoLineDaoImpl(PostgresClientFactory pgClientFactory) {
    this.pgClientFactory = pgClientFactory;
  }

  @Override
  public Future<Void> saveFailedRolloverRecord(FailedLedgerRolloverPoLineDto dto, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(SAVE_FAILED_LEDGER_ROLLOVER_PO_LINE_SQL, table);
    Tuple params = Tuple.of(
      dto.id(),
      dto.rolloverId(),
      dto.ledgerId(),
      dto.poLineId(),
      dto.requestBody(),
      dto.responseBody(),
      dto.statusCode(),
      dto.workflow_status()
    );

    return pgClientFactory.createInstance(tenantId)
      .execute(sql, params).mapEmpty();
  }
}
