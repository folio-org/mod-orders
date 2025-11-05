package org.folio.service.settings;

import io.vertx.core.Future;
import org.folio.CopilotGenerated;
import org.folio.orders.utils.ResourcePathResolver;
import org.folio.rest.acq.model.settings.CommonSetting;
import org.folio.rest.acq.model.settings.CommonSettingsCollection;
import org.folio.rest.acq.model.settings.Value;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.service.caches.CommonSettingsCache.TENANT_LOCALE_SETTINGS;
import static org.folio.service.settings.CommonSettingsRetriever.CURRENCY_KEY;
import static org.folio.service.settings.CommonSettingsRetriever.TZ_KEY;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@CopilotGenerated
@ExtendWith(MockitoExtension.class)
public class CommonSettingsRetrieverTest {

  @Mock
  private RequestContext requestContextMock;
  @Mock
  private RestClient restClientMock;
  @InjectMocks
  private CommonSettingsRetriever commonSettingsRetriever;

  @Test
  void shouldReturnDefaultCurrencyWhenLocaleSettingsNotPresent() {
    RequestEntry requestEntry = new RequestEntry(resourcesPath(ResourcePathResolver.SETTINGS_ENTRIES));
    CommonSettingsCollection settingsCollection = getLocaleSettings();

    when(restClientMock.get(eq(requestEntry), eq(CommonSettingsCollection.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(settingsCollection));

    Future<String> result = commonSettingsRetriever.getSystemCurrency(requestEntry, requestContextMock);

    assertEquals("USD", result.result());
  }

  @Test
  void shouldReturnConfiguredCurrency() {
    RequestEntry requestEntry = new RequestEntry(resourcesPath(ResourcePathResolver.SETTINGS_ENTRIES));
    CommonSettingsCollection settingsCollection = getLocaleSettings();
    settingsCollection.getItems().getFirst().getValue().withAdditionalProperty("currency", "EUR");

    when(restClientMock.get(eq(requestEntry), eq(CommonSettingsCollection.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(settingsCollection));

    Future<String> result = commonSettingsRetriever.getSystemCurrency(requestEntry, requestContextMock);

    assertEquals("EUR", result.result());
  }

  @Test
  void shouldReturnDefaultTimeZoneWhenLocaleSettingsNotPresent() {
    RequestEntry requestEntry = new RequestEntry(resourcesPath(ResourcePathResolver.SETTINGS_ENTRIES));
    CommonSettingsCollection settingsCollection = new CommonSettingsCollection();

    when(restClientMock.get(eq(requestEntry), eq(CommonSettingsCollection.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(settingsCollection));

    Future<String> result = commonSettingsRetriever.getSystemTimeZone(requestEntry, requestContextMock);

    assertEquals("UTC", result.result());
  }

  @Test
  void shouldReturnConfiguredTimeZone() {
    RequestEntry requestEntry = new RequestEntry(resourcesPath(ResourcePathResolver.SETTINGS_ENTRIES));
    CommonSettingsCollection settingsCollection = getLocaleSettings();
    settingsCollection.getItems().getFirst().getValue().withAdditionalProperty("timezone", "PST");

    when(restClientMock.get(eq(requestEntry), eq(CommonSettingsCollection.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(settingsCollection));

    Future<String> result = commonSettingsRetriever.getSystemTimeZone(requestEntry, requestContextMock);

    assertEquals("PST", result.result());
  }

  @Test
  void shouldFailToReturnSystemCurrencyWhenRestClientFails() {
    RequestEntry requestEntry = new RequestEntry(resourcesPath(ResourcePathResolver.SETTINGS_ENTRIES));

    when(restClientMock.get(eq(requestEntry), eq(CommonSettingsCollection.class), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Service failure")));

    Future<String> result = commonSettingsRetriever.getSystemCurrency(requestEntry, requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

  @Test
  void shouldFailToReturnSystemTimeZoneWhenRestClientFails() {
    RequestEntry requestEntry = new RequestEntry(resourcesPath(ResourcePathResolver.SETTINGS_ENTRIES));

    when(restClientMock.get(eq(requestEntry), eq(CommonSettingsCollection.class), any(RequestContext.class)))
      .thenReturn(Future.failedFuture(new RuntimeException("Service failure")));

    Future<String> result = commonSettingsRetriever.getSystemTimeZone(requestEntry, requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

  private static CommonSettingsCollection getLocaleSettings() {
    return new CommonSettingsCollection().withItems(List.of(new CommonSetting()
      .withKey(TENANT_LOCALE_SETTINGS).withValue(new Value()
        .withAdditionalProperty(CURRENCY_KEY, "USD")
        .withAdditionalProperty(TZ_KEY, "UTC"))));
  }
}
