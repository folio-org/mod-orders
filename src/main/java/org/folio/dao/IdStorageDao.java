package org.folio.dao;

import io.vertx.core.Future;

public interface IdStorageDao {

  Future<String> store(String recordId, String entityId, String tenantId);
}
