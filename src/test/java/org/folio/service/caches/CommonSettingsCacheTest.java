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

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
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
    // Mock RequestContext headers for cache key generation
    var headers = Map.of(
      "x-okapi-tenant", "test-tenant",
      "x-okapi-user-id", "test-user-id"
    );
    when(requestContextMock.getHeaders()).thenReturn(headers);

    commonSettingsCache.init();
  }

  @Test
  void shouldLoadSettingsFromCache() {
    JsonObject config = new JsonObject().put("key", "value");

    when(commonSettingsRetrieverMock.getLocalSetting(any(RequestEntry.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(config));

    Future<JsonObject> result = commonSettingsCache.loadSettings(requestContextMock);

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
  void shouldFailToLoadSettingsWhenServiceFails() {
    when(commonSettingsRetrieverMock.getLocalSetting(any(RequestEntry.class), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Service failure")));

    Future<JsonObject> result = commonSettingsCache.loadSettings(requestContextMock);

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

  @Test
  void shouldBypassCacheWhenByPassCacheIsTrue() throws Exception {
    // Arrange - Set byPassCache to true using reflection to test bypass behavior
    var byPassCacheField = CommonSettingsCache.class.getDeclaredField("byPassCache");
    byPassCacheField.setAccessible(true);
    byPassCacheField.setBoolean(commonSettingsCache, true);

    var config = new JsonObject().put("testKey", "testValue");
    var currency = "EUR";
    var timeZone = "EST";

    when(commonSettingsRetrieverMock.getLocalSetting(any(RequestEntry.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(config));
    when(commonSettingsRetrieverMock.getSystemCurrency(any(RequestEntry.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(currency));
    when(commonSettingsRetrieverMock.getSystemTimeZone(any(RequestEntry.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(timeZone));

    // Act - Call methods multiple times to verify cache is bypassed
    var settingsResult1 = commonSettingsCache.loadSettings(requestContextMock);
    var settingsResult2 = commonSettingsCache.loadSettings(requestContextMock);
    var currencyResult1 = commonSettingsCache.getSystemCurrency(requestContextMock);
    var currencyResult2 = commonSettingsCache.getSystemCurrency(requestContextMock);
    var timezoneResult1 = commonSettingsCache.getSystemTimeZone(requestContextMock);
    var timezoneResult2 = commonSettingsCache.getSystemTimeZone(requestContextMock);

    // Assert - Verify results are correct
    assertEquals(config, settingsResult1.result());
    assertEquals(config, settingsResult2.result());
    assertEquals(currency, currencyResult1.result());
    assertEquals(currency, currencyResult2.result());
    assertEquals(timeZone, timezoneResult1.result());
    assertEquals(timeZone, timezoneResult2.result());

    // Assert - Verify that retriever methods were called multiple times (proving cache bypass)
    // When byPassCache=true, each call should go directly to the retriever, not use cache
    verify(commonSettingsRetrieverMock, times(2)).getLocalSetting(any(RequestEntry.class), any(RequestContext.class));
    verify(commonSettingsRetrieverMock, times(2)).getSystemCurrency(any(RequestEntry.class), any(RequestContext.class));
    verify(commonSettingsRetrieverMock, times(2)).getSystemTimeZone(any(RequestEntry.class), any(RequestContext.class));
  }
}
