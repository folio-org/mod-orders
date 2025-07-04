package org.folio.service.caches;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.CopilotGenerated;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.service.settings.CommonSettingsRetriever;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@CopilotGenerated
@ExtendWith(MockitoExtension.class)
public class CommonSettingsCacheTest {

  @Mock
  private RequestContext requestContextMock;
  @Mock
  private CommonSettingsRetriever commonSettingsRetrieverMock;
  @InjectMocks
  private CommonSettingsCache commonSettingsCache;

  @BeforeEach
  void setUp() {
    commonSettingsCache.init();
  }

  @Test
  void shouldLoadConfigurationFromCache() {
    String module = "testModule";
    JsonObject config = new JsonObject().put("key", "value");

    when(commonSettingsRetrieverMock.loadConfiguration(any(RequestEntry.class), any(RequestContext.class)))
        .thenReturn(Future.succeededFuture(config));

    Future<JsonObject> result = commonSettingsCache.loadConfiguration(module, requestContextMock);

    assertEquals(config, result.result());
  }

  @Test
  void shouldReturnSystemCurrencyFromCache() {
    String currency = "USD";

    when(commonSettingsRetrieverMock.getSystemCurrency(any(RequestEntry.class), any(RequestContext.class)))
        .thenReturn(Future.succeededFuture(currency));

    Future<String> result = commonSettingsCache.getSystemCurrency(requestContextMock);

    assertEquals(currency, result.result());
  }

  @Test
  void shouldReturnSystemTimeZoneFromCache() {
    String timeZone = "UTC";

    when(commonSettingsRetrieverMock.getSystemTimeZone(any(RequestEntry.class), any(RequestContext.class)))
        .thenReturn(Future.succeededFuture(timeZone));

    Future<String> result = commonSettingsCache.getSystemTimeZone(requestContextMock);

    assertEquals(timeZone, result.result());
  }

  @Test
  void shouldFailToLoadConfigurationWhenServiceFails() {
    String module = "testModule";

    when(commonSettingsRetrieverMock.loadConfiguration(any(RequestEntry.class), any(RequestContext.class)))
        .thenReturn(Future.failedFuture(new RuntimeException("Service failure")));

    Future<JsonObject> result = commonSettingsCache.loadConfiguration(module, requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

  @Test
  void shouldFailToReturnSystemCurrencyWhenServiceFails() {

    when(commonSettingsRetrieverMock.getSystemCurrency(any(RequestEntry.class), any(RequestContext.class)))
        .thenReturn(Future.failedFuture(new RuntimeException("Service failure")));

    Future<String> result = commonSettingsCache.getSystemCurrency(requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

  @Test
  void shouldFailToReturnSystemTimeZoneWhenServiceFails() {

    when(commonSettingsRetrieverMock.getSystemTimeZone(any(RequestEntry.class), any(RequestContext.class)))
        .thenReturn(Future.failedFuture(new RuntimeException("Service failure")));

    Future<String> result = commonSettingsCache.getSystemTimeZone(requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

}
