package org.folio.dao;

import io.vertx.core.Future;
import io.vertx.sqlclient.Tuple;
import org.folio.dao.util.PostgresClientFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.UUID;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

@Repository
public class SequentialOrderIdStorageDaoImpl implements SequentialOrderIdStorageDao {

  private static final String TABLE_NAME = "sequential_order_id";

  private static final String ORDER_ID_FIELD = "order_id";

  private static final String SQL =
    "WITH input_row(job_execution_id, sequential_no, order_id) AS " +
      "(VALUES ($1, $2, $3)), " +
      "insert_res AS ( " +
      "  INSERT INTO %1$s (job_execution_id, sequential_no, order_id) " +
      "  SELECT * FROM input_row " +
      "  ON CONFLICT ON CONSTRAINT sequential_order_pk_constraint DO NOTHING " +
      "  RETURNING job_execution_id, sequential_no, order_id " +
      ") " +
      "SELECT job_execution_id, sequential_no, order_id " +
      "FROM insert_res " +
      "UNION ALL " +
      "SELECT t.job_execution_id, t.sequential_no, t.order_id " +
      "FROM %1$s t, input_row " +
      "WHERE t.job_execution_id = input_row.job_execution_id AND t.sequential_no = input_row.sequential_no";

  private final PostgresClientFactory pgClientFactory;

  @Autowired
  public SequentialOrderIdStorageDaoImpl(PostgresClientFactory pgClientFactory) {
    this.pgClientFactory = pgClientFactory;
  }

  @Override
  public Future<String> store(String jobExecutionId, Integer exponent, String orderId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(SQL, table);
    Tuple params = Tuple.of(UUID.fromString(jobExecutionId), exponent, UUID.fromString(orderId));

    return pgClientFactory.createInstance(tenantId).execute(sql, params)
      .map(rows -> rows.iterator().next().getValue(ORDER_ID_FIELD).toString());
  }

  private String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }

}
