package org.folio.dao;

import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.dao.util.PostgresClientFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.UUID;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

@Repository
public class SequentialOrderIdStorageDaoImpl implements SequentialOrderIdStorageDao {

  private static final Logger LOGGER = LogManager.getLogger();

  private static final String TABLE_NAME = "sequential_order_id";

  private static final String SQL =
    "WITH input_row(job_execution_id, sequential_no, order_id) AS " +
      "(VALUES ($1::uuid, $2::integer, $3::uuid, now())), " +
      "insert_res AS ( " +
      "  INSERT INTO %1$s (job_execution_id, sequential_no, order_id) " +
      "  SELECT job_execution_id::uuid, sequential_no::integer, order_id::uuid FROM input_row " +
      "  ON CONFLICT ON CONSTRAINT sequential_order_pk_constraint DO NOTHING " +
      "  RETURNING job_execution_id::uuid, sequential_no::integer, order_id::uuid " +
      ") " +
      "SELECT order_id::uuid " +
      "FROM insert_res " +
      "UNION ALL " +
      "SELECT t.order_id::uuid " +
      "FROM %1$s t, input_row " +
      "WHERE t.job_execution_id::uuid = input_row.job_execution_id::uuid AND t.sequential_no::integer = input_row.sequential_no::integer";

  private static final String GET_ORDER_ID_SQL =
    "SELECT order_id::uuid FROM %1$s WHERE job_execution_id::uuid = $1 AND sequential_no::integer = $2";

  private static final String INSERT_SQL =
    "INSERT INTO %1$s (job_execution_id, sequential_no, order_id, saved_timestamp) VALUES ($1::uuid, $2::integer, $3::uuid, now())";

  private final PostgresClientFactory pgClientFactory;

  @Autowired
  public SequentialOrderIdStorageDaoImpl(PostgresClientFactory pgClientFactory) {
    this.pgClientFactory = pgClientFactory;
  }

  public Future<String> storeReturn(String jobExecutionId, Integer sequenceNo, String orderId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(SQL, table);
    Tuple params = Tuple.of(UUID.fromString(jobExecutionId), sequenceNo, UUID.fromString(orderId));
    return pgClientFactory.createInstance(tenantId).execute(sql, params).map(rows ->
    {
      if (rows.size() == 0) {
        LOGGER.error("get:: jobExecutionId: {}, sequenceNo: {}, orderId: {}", jobExecutionId, sequenceNo, orderId);
        return get(jobExecutionId, sequenceNo, tenantId).result();
      }
      return rows.iterator().next().getUUID(0).toString();
    });
  }
  public Future<String> store(String jobExecutionId, Integer sequenceNo, String orderId, String tenantId) {
    Promise<RowSet<Row>> promise = Promise.promise();
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String query = String.format(INSERT_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(jobExecutionId), sequenceNo, UUID.fromString(orderId));
    pgClientFactory.createInstance(tenantId).execute(query, params, promise);
    return promise.future().map(insertResult ->
        (insertResult.iterator().hasNext() == true)  ?
          insertResult.iterator().next().getUUID(0).toString() :
          orderId)
      .onFailure(ar -> {
        LOGGER.error("get:: jobExecutionId: {}, sequenceNo: {}, orderId: {}", jobExecutionId, sequenceNo, orderId, ar);
    });
  }

  public Future<String> get(String jobExecutionId, Integer sequenceNo, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(GET_ORDER_ID_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(jobExecutionId), sequenceNo);
    return pgClientFactory.createInstance(tenantId).execute(sql, params).map(rows -> rows.iterator().next().getUUID(0).toString());
  }

  private String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }
}
