package org.folio;

import org.folio.completablefuture.VertxFutureRepeaterTest;
import org.folio.di.CacheTest;
import org.folio.service.caches.CancelledJobsIdsCacheTest;
import org.folio.service.caches.InventoryCacheTest;
import org.folio.service.caches.JobExecutionTotalRecordsCacheTest;
import org.folio.service.dataimport.OrderIdStorageServiceImplTest;
import org.folio.service.dataimport.handlers.CreateOrderEventHandlerTest;
import org.folio.service.dataimport.handlers.OrderPostProcessingEventHandlerTest;
import org.folio.verticle.CancelledJobExecutionConsumerVerticleTest;
import org.folio.verticle.consumers.DataImportKafkaHandlerTest;
import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses(value = {
  CacheTest.class,
  InventoryCacheTest.class,
  JobExecutionTotalRecordsCacheTest.class,
  CancelledJobsIdsCacheTest.class,
  OrderIdStorageServiceImplTest.class,
  CreateOrderEventHandlerTest.class,
  OrderPostProcessingEventHandlerTest.class,
  VertxFutureRepeaterTest.class,
  CancelledJobExecutionConsumerVerticleTest.class,
  DataImportKafkaHandlerTest.class,
})
public class DataImportTestSuite {
}
