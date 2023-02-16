package org.folio.dao;

import io.vertx.core.Future;
import io.vertx.sqlclient.Tuple;
import org.folio.dao.util.PostgresClientFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.UUID;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

@Repository
public class OrderRecordIdStorageDaoImpl implements RecordIdStorageDao {

  private static final String TABLE_NAME = "processed_records";
  private static final String RECORD_ID_FIELD = "record_id";
  private static final String INSERT_SQL =
    "INSERT INTO %1$s (record_id, created_date) VALUES ($1, now()) RETURNING record_id::uuid;";

  private final PostgresClientFactory pgClientFactory;

  @Autowired
  public OrderRecordIdStorageDaoImpl(PostgresClientFactory pgClientFactory) {
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

  private String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }
}
