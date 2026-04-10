package org.folio;

import org.folio.completablefuture.VertxFutureRepeaterTest;
import org.folio.di.CacheIT;
import org.folio.service.caches.CancelledJobsIdsCacheTest;
import org.folio.service.caches.InventoryCacheTest;
import org.folio.service.caches.JobExecutionTotalRecordsCacheTest;
import org.folio.service.dataimport.OrderIdStorageServiceImplIT;
import org.folio.service.dataimport.handlers.CreateOrderEventHandlerIT;
import org.folio.service.dataimport.handlers.OrderPostProcessingEventHandlerIT;
import org.folio.verticle.CancelledJobExecutionConsumerVerticleIT;
import org.folio.verticle.consumers.DataImportKafkaHandlerTest;
import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses(
    value = {
      CacheIT.class,
      InventoryCacheTest.class,
      JobExecutionTotalRecordsCacheTest.class,
      CancelledJobsIdsCacheTest.class,
      OrderIdStorageServiceImplIT.class,
      CreateOrderEventHandlerIT.class,
      OrderPostProcessingEventHandlerIT.class,
      VertxFutureRepeaterTest.class,
      CancelledJobExecutionConsumerVerticleIT.class,
      DataImportKafkaHandlerTest.class,
    })
public class DataImportTestSuiteIT {}
