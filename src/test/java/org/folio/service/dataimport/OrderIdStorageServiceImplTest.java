package org.folio.service.dataimport;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.folio.dao.IdStorageDao;
import org.folio.dao.OrderIdStorageDaoImpl;
import org.folio.dao.util.PostgresClientFactory;
import org.folio.di.DiAbstractRestTest;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.UUID;

@RunWith(VertxUnitRunner.class)
public class OrderIdStorageServiceImplTest extends DiAbstractRestTest {

  private PostgresClientFactory pgClientFactory = new PostgresClientFactory(Vertx.vertx());
  private IdStorageDao orderIdStorageDao = new OrderIdStorageDaoImpl(pgClientFactory);
  private IdStorageService orderIdStorageService = new OrderIdStorageServiceImpl(orderIdStorageDao);

  @Test
  public void shouldSaveAndReturnNewRecordId(TestContext context) {
    Async async = context.async();
    String recordId = UUID.randomUUID().toString();
    Future<String> future = orderIdStorageService.store(recordId, TENANT_ID);

    future.onComplete(ar -> {
      context.assertTrue(ar.succeeded());
      context.assertEquals(recordId, ar.result());
      async.complete();
    });
  }

  @Test
  public void shouldNotSaveRecordIdAndReturnExistingByRecordId(TestContext context) {
    Async async = context.async();
    String recordId = UUID.randomUUID().toString();
    Future<String> future = orderIdStorageService.store(recordId, TENANT_ID)
      .compose(v -> orderIdStorageService.store(recordId, TENANT_ID));

    future.onComplete(ar -> {
      context.assertTrue(ar.failed());
      async.complete();
    });
  }

  @Test
  public void shouldReturnSavedRecordId(TestContext context) {
    Async async = context.async();
    String recordId = UUID.randomUUID().toString();
    Future<String> future = orderIdStorageService.store(recordId, TENANT_ID);

    future.onComplete(ar -> {
      context.assertTrue(ar.succeeded());
      context.assertEquals(recordId, ar.result());
      async.complete();
    });
  }
}
