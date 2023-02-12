package org.folio.dao;

import io.vertx.core.Future;
import io.vertx.sqlclient.Tuple;
import org.folio.dao.util.PostgresClientFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.UUID;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

@Repository
public class PoLinesImportProgressDaoImpl implements PoLinesImportProgressDao {

  private static final String TABLE_NAME = "po_lines_processing_progress";
  private static final String PO_LINES_PROCESSED_FIELD = "po_lines_processed";

  private static final String SAVE_POL_TOTAL_AMOUNT_SQL =
    "INSERT INTO %s (order_id, total_po_lines, processed_po_lines, creation_date) VALUES ($1, $2, 0, $3)";

  private static final String INCREASE_IMPORTED_POL_BY_ORDER_ID_SQL =
    "UPDATE %s SET processed_po_lines = processed_po_lines + 1 WHERE order_id = $1";

  private static final String IS_ALL_POL_IMPORTED_BY_ORDER_ID_SQL =
    "SELECT total_po_lines = processed_po_lines AS po_lines_processed " +
    "FROM %s " +
    "WHERE order_id = $1";

  private final PostgresClientFactory pgClientFactory;

  @Autowired
  public PoLinesImportProgressDaoImpl(PostgresClientFactory pgClientFactory) {
    this.pgClientFactory = pgClientFactory;
  }

  @Override
  public Future<Void> savePoLinesAmountPerOrder(String orderId, int totalPoLines, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(SAVE_POL_TOTAL_AMOUNT_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(orderId), totalPoLines, LocalDateTime.now(ZoneId.of(ZoneOffset.UTC.getId())));

    return pgClientFactory.createInstance(tenantId).execute(sql, params).mapEmpty();
  }

  @Override
  public Future<Void> trackProcessedPoLine(String orderId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(INCREASE_IMPORTED_POL_BY_ORDER_ID_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(orderId));

    return pgClientFactory.createInstance(tenantId).execute(sql, params).mapEmpty();
  }

  @Override
  public Future<Boolean> poLinesProcessed(String orderId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(IS_ALL_POL_IMPORTED_BY_ORDER_ID_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(orderId));

    return pgClientFactory.createInstance(tenantId).execute(sql, params)
      .map(rows -> rows.iterator().next().getBoolean(PO_LINES_PROCESSED_FIELD));
  }

  private String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }

}
