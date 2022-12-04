package org.folio.service.dataimport;

import io.vertx.core.Future;

public interface IdStorageService {

  Future<String> store(String recordId, String entityId, String tenantId);
}
