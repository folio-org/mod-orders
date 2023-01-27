package org.folio.dao;

import io.vertx.core.Future;

public interface PoLinesImportProgressDao {

  Future<Void> savePoLinesAmountPerOrder(String orderId, int totalPoLines, String tenantId);

  Future<Void> trackImportedPoLine(String orderId, String tenantId);

  Future<Boolean> isPoLinesImported(String orderId, String tenantId);
}
