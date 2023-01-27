package org.folio.dao;

import io.vertx.core.Future;
import io.vertx.sqlclient.Tuple;
import org.folio.dao.util.PostgresClientFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.UUID;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

@Repository
public class PoLinesImportProgressDaoImpl implements PoLinesImportProgressDao {

  private static final String TABLE_NAME = "po_lines_import_progress";
  public static final String SAVE_POL_TOTAL_AMOUNT_SQL = "INSERT INTO %s (order_id, total_po_lines) VALUES ($1, $2)";
  public static final String INCREASE_IMPORTED_POL_BY_ORDER_ID_SQL =
    "UPDATE %s SET processed_po_lines = processed_po_lines + 1";
  public static final String IS_ALL_POL_IMPORTED_BY_ORDER_ID_SQL =
    "SELECT total_po_lines = processed_po_lines AS pol_imported " +
    "FROM %s" +
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
    Tuple params = Tuple.of(UUID.fromString(orderId), totalPoLines);

    return pgClientFactory.createInstance(tenantId).execute(sql, params).mapEmpty();
  }

  @Override
  public Future<Void> trackImportedPoLine(String orderId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(INCREASE_IMPORTED_POL_BY_ORDER_ID_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(orderId));

    return pgClientFactory.createInstance(tenantId).execute(sql, params).mapEmpty();
  }

  @Override
  public Future<Boolean> isPoLinesImported(String orderId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(IS_ALL_POL_IMPORTED_BY_ORDER_ID_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(orderId));

    return pgClientFactory.createInstance(tenantId).execute(sql, params)
      .map(rows -> rows.iterator().next().getBoolean("pol_imported"));
  }

  private String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }

}
