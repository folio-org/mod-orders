package org.folio.dao;

import io.vertx.core.Future;
import io.vertx.sqlclient.Tuple;
import org.apache.commons.lang3.StringUtils;
import org.folio.dao.util.PostgresClientFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.UUID;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

@Repository
public class OrderIdStorageDaoImpl implements IdStorageDao {

  private static final String TABLE_NAME = "processed_records";
  private static final String RECORD_ID_FIELD = "record_id";
  private static final String INSERT_SQL =
    "INSERT INTO %1$s (record_id, created_date) VALUES ($1, now()) RETURNING record_id::uuid;";

  private static final String GET_RECORD_ID_SQL =
    "SELECT record_id::uuid FROM %1$s WHERE processed_records.record_id::uuid = $1";

  private final PostgresClientFactory pgClientFactory;

  @Autowired
  public OrderIdStorageDaoImpl(PostgresClientFactory pgClientFactory) {
    this.pgClientFactory = pgClientFactory;
  }

  @Override
  public Future<String> store(String recordId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(INSERT_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(recordId));

    return pgClientFactory.createInstance(tenantId).execute(sql, params)
      .map(rows -> rows.iterator().next().getValue(RECORD_ID_FIELD).toString());
  }

  @Override
  public Future<String> get(String recordId, String tenantId) {
    String table = prepareFullTableName(tenantId, TABLE_NAME);
    String sql = String.format(GET_RECORD_ID_SQL, table);
    Tuple params = Tuple.of(UUID.fromString(recordId));

    return pgClientFactory.createInstance(tenantId).execute(sql, params)
      .map(rows ->
        {
          if (rows.size() == 0) {
            return StringUtils.EMPTY;
          }
          return rows.iterator().next().getValue(RECORD_ID_FIELD).toString();
        });
  }

  private String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }
}
