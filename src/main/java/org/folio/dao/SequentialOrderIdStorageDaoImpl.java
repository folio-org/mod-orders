package org.folio.dao;

import static org.folio.dao.util.DbUtils.prepareFullTableName;

import java.util.UUID;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.dao.util.PostgresClientFactory;
import org.springframework.stereotype.Repository;

import io.vertx.core.Future;
import io.vertx.sqlclient.Tuple;

@Log4j2
@RequiredArgsConstructor
@Repository
public class SequentialOrderIdStorageDaoImpl implements SequentialOrderIdStorageDao {

  private static final String TABLE_NAME = "sequential_order_id";

  private static final String SQL =
    " INSERT INTO %1$s (job_execution_id, sequential_no, order_id, saved_timestamp) " +
    " VALUES ($1::uuid, $2::integer, $3::uuid, now()) " +
    " ON CONFLICT ON CONSTRAINT sequential_order_pk_constraint DO UPDATE SET job_execution_id=EXCLUDED.job_execution_id " +
    " RETURNING order_id::uuid";

  private final PostgresClientFactory pgClientFactory;

  @Override
  public Future<String> store(String jobExecutionId, Integer sequenceNo, String orderId, String tenantId) {
    try {
      log.trace("store:: jobExecutionId: {}, sequenceNo: {}, orderId: {}", jobExecutionId, sequenceNo, tenantId);
      var query = SQL.formatted(prepareFullTableName(tenantId, TABLE_NAME));
      var params = Tuple.of(UUID.fromString(jobExecutionId), sequenceNo, UUID.fromString(orderId));
      return pgClientFactory.createInstance(tenantId)
        .execute(query, params)
        .map(resultSet -> resultSet.iterator().next().getUUID(0).toString())
        .onFailure(e -> log.error("store:: failed to store jobExecutionId: {}, sequenceNo: {}, orderId: {}", jobExecutionId, sequenceNo, tenantId, e));
    } catch (Exception e) {
      log.error("store:: failed to store jobExecutionId: {}, sequenceNo: {}, orderId: {}", jobExecutionId, sequenceNo, tenantId, e);
      return Future.failedFuture(e);
    }
  }

}
