package org.folio.dao.util;

import io.vertx.core.Vertx;
import org.folio.rest.persist.PostgresClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PostgresClientFactory {

  private Vertx vertx;

  @Autowired
  public PostgresClientFactory(Vertx vertx) {
    this.vertx = vertx;
  }

  /**
   * Creates instance of Postgres Client
   *
   * @param tenantId - tenant id
   * @return Postgres Client
   */
  public PostgresClient createInstance(String tenantId) {
    return PostgresClient.getInstance(vertx, tenantId);
  }
}
