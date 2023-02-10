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

import java.time.LocalDateTime;
import java.util.UUID;

import static java.lang.String.format;
import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

@Repository
public class SequentialOrderIdStorageDaoImpl implements SequentialOrderIdStorageDao {

  private static final Logger LOGGER = LogManager.getLogger();

  private static final String TABLE_NAME = "sequential_order_id";

  private static final String SQL =
    "WITH input_row(job_execution_id, sequential_no, order_id, saved_timestamp) AS " +
      "(VALUES ($1, $2, $3, $4)), " +
      "insert_res AS ( " +
      "  INSERT INTO %1$s (job_execution_id, sequential_no, order_id, saved_timestamp) " +
      "  SELECT * FROM input_row " +
      "  ON CONFLICT ON CONSTRAINT sequential_order_pk_constraint DO NOTHING " +
      "  RETURNING job_execution_id, sequential_no, order_id, saved_timestamp " +
      ") " +
      "SELECT order_id " +
      "FROM insert_res " +
      "UNION ALL " +
      "SELECT t.order_id " +
      "FROM %1$s t, input_row " +
      "WHERE t.job_execution_id = input_row.job_execution_id AND t.sequential_no = input_row.sequential_no";

  private final PostgresClientFactory pgClientFactory;

  @Autowired
  public SequentialOrderIdStorageDaoImpl(PostgresClientFactory pgClientFactory) {
    this.pgClientFactory = pgClientFactory;
  }

  @Override
  public Future<String> store(String jobExecutionId, Integer sequenceNo, String orderId, String tenantId) {
    Promise<RowSet<Row>> promise = Promise.promise();
    try {
      LOGGER.trace("store:: jobExecutionId: {}, sequenceNo: {}, orderId: {}", jobExecutionId, sequenceNo, tenantId);
      String query = String.format(SQL, prepareFullTableName(tenantId, TABLE_NAME));
      Tuple params = Tuple.of(UUID.fromString(jobExecutionId), sequenceNo, UUID.fromString(orderId), LocalDateTime.now());
      pgClientFactory.createInstance(tenantId).execute(query, params, promise);
    } catch (Exception e) {
      LOGGER.error("store:: failed to store jobExecutionId: {}, sequenceNo: {}, orderId: {}", jobExecutionId, sequenceNo, tenantId, e);
      promise.fail(e);
    }
    return promise.future().map(resultSet -> resultSet.iterator().next().getUUID(0).toString());
  }

  private String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }
}
