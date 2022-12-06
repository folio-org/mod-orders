package org.folio.dao;

import io.vertx.core.Future;
import io.vertx.sqlclient.Tuple;
import org.folio.dao.util.PostgresClientFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.UUID;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

@Repository
public class OrderIdStorageDaoImpl implements IdStorageDao {

  private static final String TABLE_NAME = "records_orders";
  private static final String ORDER_ID_FIELD = "order_id";
  private static final String SQL =
    "WITH input_row(record_id, order_id) AS " +
    "(VALUES ($1, $2)), " +
    "insert_res AS ( " +
    "  INSERT INTO %1$s (record_id, order_id) " +
    "  SELECT * FROM input_row " +
    "  ON CONFLICT (record_id) DO NOTHING " +
    "  RETURNING record_id, order_id " +
    ") " +
    "SELECT record_id, order_id " +
    "FROM insert_res " +
    "UNION ALL " +
    "SELECT t.record_id, t.order_id " +
    "FROM %1$s t, input_row " +
    "WHERE t.record_id = input_row.record_id";
//      "FROM input_row " +
//    "JOIN %s t USING (record_id)";

  private final PostgresClientFactory pgClientFactory;

  @Autowired
  public OrderIdStorageDaoImpl(PostgresClientFactory pgClientFactory) {
    this.pgClientFactory = pgClientFactory;
  }

  @Override
  public Future<String> store(String recordId, String orderId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(SQL, table);
    Tuple params = Tuple.of(UUID.fromString(recordId), UUID.fromString(orderId));

    return pgClientFactory.createInstance(tenantId).execute(sql, params)
      .map(rows -> rows.iterator().next().getValue(ORDER_ID_FIELD).toString());
  }

  private String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }

}
