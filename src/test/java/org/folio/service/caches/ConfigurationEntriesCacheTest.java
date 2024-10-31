package org.folio.service.caches;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.CopilotGenerated;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.configuration.ConfigurationEntriesService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;

@CopilotGenerated
@ExtendWith(MockitoExtension.class)
public class ConfigurationEntriesCacheTest {

  @Mock
  private RequestContext requestContextMock;
  @Mock
  private ConfigurationEntriesService configurationEntriesServiceMock;
  @InjectMocks
  private ConfigurationEntriesCache configurationEntriesCache;

  @Test
  void shouldLoadConfigurationFromCache() {
    String module = "testModule";
    JsonObject config = new JsonObject().put("key", "value");
    doReturn(Future.succeededFuture(config))
      .when(configurationEntriesServiceMock).loadConfiguration(any(RequestEntry.class), any(RequestContext.class));

    Future<JsonObject> result = configurationEntriesCache.loadConfiguration(module, requestContextMock);

    assertEquals(config, result.result());
  }

  @Test
  void shouldReturnSystemCurrencyFromCache() {
    String currency = "USD";
    doReturn(Future.succeededFuture(currency))
      .when(configurationEntriesServiceMock).getSystemCurrency(any(RequestEntry.class), any(RequestContext.class));

    Future<String> result = configurationEntriesCache.getSystemCurrency(requestContextMock);

    assertEquals(currency, result.result());
  }

  @Test
  void shouldReturnSystemTimeZoneFromCache() {
    String timeZone = "UTC";
    doReturn(Future.succeededFuture(timeZone))
      .when(configurationEntriesServiceMock).getSystemTimeZone(any(RequestEntry.class), any(RequestContext.class));

    Future<String> result = configurationEntriesCache.getSystemTimeZone(requestContextMock);

    assertEquals(timeZone, result.result());
  }

  @Test
  void shouldFailToLoadConfigurationWhenServiceFails() {
    String module = "testModule";
    doReturn(Future.failedFuture(new RuntimeException("Service failure")))
      .when(configurationEntriesServiceMock).loadConfiguration(any(RequestEntry.class), any(RequestContext.class));

    Future<JsonObject> result = configurationEntriesCache.loadConfiguration(module, requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

  @Test
  void shouldFailToReturnSystemCurrencyWhenServiceFails() {
    doReturn(Future.failedFuture(new RuntimeException("Service failure")))
      .when(configurationEntriesServiceMock).getSystemCurrency(any(RequestEntry.class), any(RequestContext.class));

    Future<String> result = configurationEntriesCache.getSystemCurrency(requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

  @Test
  void shouldFailToReturnSystemTimeZoneWhenServiceFails() {
    doReturn(Future.failedFuture(new RuntimeException("Service failure")))
      .when(configurationEntriesServiceMock).getSystemTimeZone(any(RequestEntry.class), any(RequestContext.class));

    Future<String> result = configurationEntriesCache.getSystemTimeZone(requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

}
