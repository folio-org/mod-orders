package org.folio;

import org.folio.di.CacheTest;
import org.folio.service.dataimport.OrderIdStorageServiceImplTest;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
  CacheTest.class,
  OrderIdStorageServiceImplTest.class
})
public class DataImportTestSuite {
}
