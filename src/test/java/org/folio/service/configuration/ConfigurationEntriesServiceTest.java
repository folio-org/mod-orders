package org.folio.service.configuration;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.CopilotGenerated;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Config;
import org.folio.rest.jaxrs.model.Configs;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

@CopilotGenerated
@ExtendWith(MockitoExtension.class)
public class ConfigurationEntriesServiceTest {

  @Mock
  private RequestContext requestContextMock;
  @Mock
  private RestClient restClientMock;
  @InjectMocks
  private ConfigurationEntriesService configurationEntriesService;

  @Test
  void shouldLoadConfigurationSuccessfully() {
    RequestEntry requestEntry = new RequestEntry("/configurations/entries");
    JsonObject expectedConfig = new JsonObject().put("key", "value");
    Configs configs = new Configs().withConfigs(List.of(new Config().withConfigName("key").withValue("value")));

    doReturn(Future.succeededFuture(configs))
      .when(restClientMock).get(eq(requestEntry), eq(Configs.class), any(RequestContext.class));

    Future<JsonObject> result = configurationEntriesService.loadConfiguration(requestEntry, requestContextMock);

    assertEquals(expectedConfig, result.result());
  }

  @Test
  void shouldReturnDefaultCurrencyWhenLocaleSettingsNotPresent() {
    RequestEntry requestEntry = new RequestEntry("/configurations/entries");
    JsonObject config = new JsonObject();
    Configs configs = new Configs().withConfigs(List.of());

    doReturn(Future.succeededFuture(configs))
      .when(restClientMock).get(eq(requestEntry), eq(Configs.class), any(RequestContext.class));

    Future<String> result = configurationEntriesService.getSystemCurrency(requestEntry, requestContextMock);

    assertEquals("USD", result.result());
  }

  @Test
  void shouldReturnConfiguredCurrency() {
    RequestEntry requestEntry = new RequestEntry("/configurations/entries");
    JsonObject config = new JsonObject().put("localeSettings", new JsonObject().put("currency", "EUR").encode());
    Configs configs = new Configs().withConfigs(List.of(new Config().withConfigName("localeSettings").withValue(config.getString("localeSettings"))));

    doReturn(Future.succeededFuture(configs))
      .when(restClientMock).get(eq(requestEntry), eq(Configs.class), any(RequestContext.class));

    Future<String> result = configurationEntriesService.getSystemCurrency(requestEntry, requestContextMock);

    assertEquals("EUR", result.result());
  }

  @Test
  void shouldReturnDefaultTimeZoneWhenLocaleSettingsNotPresent() {
    RequestEntry requestEntry = new RequestEntry("/configurations/entries");
    JsonObject config = new JsonObject();
    Configs configs = new Configs().withConfigs(List.of());

    doReturn(Future.succeededFuture(configs))
      .when(restClientMock).get(eq(requestEntry), eq(Configs.class), any(RequestContext.class));

    Future<String> result = configurationEntriesService.getSystemTimeZone(requestEntry, requestContextMock);

    assertEquals("UTC", result.result());
  }

  @Test
  void shouldReturnConfiguredTimeZone() {
    RequestEntry requestEntry = new RequestEntry("/configurations/entries");
    JsonObject config = new JsonObject().put("localeSettings", new JsonObject().put("timezone", "PST").encode());
    Configs configs = new Configs().withConfigs(List.of(new Config().withConfigName("localeSettings").withValue(config.getString("localeSettings"))));

    doReturn(Future.succeededFuture(configs))
      .when(restClientMock).get(eq(requestEntry), eq(Configs.class), any(RequestContext.class));

    Future<String> result = configurationEntriesService.getSystemTimeZone(requestEntry, requestContextMock);

    assertEquals("PST", result.result());
  }

  @Test
  void shouldFailToLoadConfigurationWhenRestClientFails() {
    RequestEntry requestEntry = new RequestEntry("/configurations/entries");

    doReturn(Future.failedFuture(new RuntimeException("Service failure")))
      .when(restClientMock).get(eq(requestEntry), eq(Configs.class), any(RequestContext.class));

    Future<JsonObject> result = configurationEntriesService.loadConfiguration(requestEntry, requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

  @Test
  void shouldFailToReturnSystemCurrencyWhenRestClientFails() {
    RequestEntry requestEntry = new RequestEntry("/configurations/entries");

    doReturn(Future.failedFuture(new RuntimeException("Service failure")))
      .when(restClientMock).get(eq(requestEntry), eq(Configs.class), any(RequestContext.class));

    Future<String> result = configurationEntriesService.getSystemCurrency(requestEntry, requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

  @Test
  void shouldFailToReturnSystemTimeZoneWhenRestClientFails() {
    RequestEntry requestEntry = new RequestEntry("/configurations/entries");

    doReturn(Future.failedFuture(new RuntimeException("Service failure")))
      .when(restClientMock).get(eq(requestEntry), eq(Configs.class), any(RequestContext.class));

    Future<String> result = configurationEntriesService.getSystemTimeZone(requestEntry, requestContextMock);

    assertTrue(result.failed());
    assertEquals(RuntimeException.class, result.cause().getClass());
  }

}
