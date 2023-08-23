package org.folio.dao.util;

import static org.folio.rest.persist.PostgresClient.convertToPsqlStandard;

public final class DbUtils {

  private DbUtils() {
  }

  public static String prepareFullTableName(String tenantId, String table) {
    return String.format("%s.%s", convertToPsqlStandard(tenantId), table);
  }
}
