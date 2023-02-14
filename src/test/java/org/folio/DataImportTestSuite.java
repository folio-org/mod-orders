package org.folio;

import org.folio.di.CacheTest;
import org.folio.service.caches.JobExecutionTotalRecordsCacheTest;
import org.folio.service.dataimport.OrderIdStorageServiceImplTest;
import org.folio.service.dataimport.handlers.CreateOrderEventHandlerTest;
import org.folio.service.dataimport.handlers.OrderPostProcessingEventHandlerTest;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
  CacheTest.class,
  JobExecutionTotalRecordsCacheTest.class,
  OrderIdStorageServiceImplTest.class,
  CreateOrderEventHandlerTest.class,
  OrderPostProcessingEventHandlerTest.class
})
public class DataImportTestSuite {
}
