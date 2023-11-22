package org.folio.rest.impl;

import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static org.folio.RestTestUtils.prepareHeaders;
import static org.folio.RestTestUtils.verifyGet;
import static org.folio.TestConfig.X_OKAPI_URL;
import static org.folio.TestConfig.clearServiceInteractions;
import static org.folio.TestConfig.initSpringContext;
import static org.folio.TestConfig.isVerticleNotDeployed;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.folio.ApiTestSuite;
import org.folio.config.ApplicationConfig;
import org.folio.rest.jaxrs.model.ExportHistoryCollection;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

@Disabled
public class ExportHistoryImplTest {

  private static boolean runningOnOwn;
  public static final String EXPORT_HISTORY_ENDPOINT = "/orders/export-history";

  @BeforeAll
  static void before() throws InterruptedException, ExecutionException, TimeoutException {
    if (isVerticleNotDeployed()) {
      ApiTestSuite.before();
      runningOnOwn = true;
    }
    initSpringContext(ApplicationConfig.class);
  }

  @AfterEach
  void afterEach() {
    clearServiceInteractions();
  }

  @AfterAll
  static void after() {
    if (runningOnOwn) {
      ApiTestSuite.after();
    }
  }


  @Test
  void getExportHistorySuccessTest () {
    var headers =  prepareHeaders(X_OKAPI_URL, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10);
    final ExportHistoryCollection exportHistory = verifyGet(EXPORT_HISTORY_ENDPOINT, headers, APPLICATION_JSON, 200)
      .as(ExportHistoryCollection.class);

    assertNotNull(exportHistory);
    assertEquals(2, exportHistory.getExportHistories().size());
  }


}
