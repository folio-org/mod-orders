package org.folio.service.consortium;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.acq.model.Setting;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Location;
import org.folio.service.settings.SettingsRetriever;
import org.folio.service.settings.util.SettingKey;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

public class ConsortiumConfigurationServiceTest {

  @InjectMocks
  private ConsortiumConfigurationService consortiumConfigurationService;

  @Mock
  private RestClient restClient;

  @Mock
  private RequestContext requestContext;

  @Mock
  private SettingsRetriever settingsRetriever;

  private AutoCloseable mockitoMocks;

  @BeforeEach
  public void initMocks(){
    mockitoMocks = MockitoAnnotations.openMocks(this);
    consortiumConfigurationService.init();
  }

  @AfterEach
  void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void testCloneRequestContextIfNeeded() {
    // given
    JsonObject consortiumConfiguration = new JsonObject(Map.of(
      "userTenants", new JsonArray(List.of(
        new JsonObject(Map.of("consortiumId", "cid", "centralTenantId", "ctid"))
      ))
    ));
    when(restClient.getAsJsonObject(any(RequestEntry.class), any()))
      .thenReturn(Future.succeededFuture(consortiumConfiguration));

    // when
    Future<RequestContext> future = consortiumConfigurationService.cloneRequestContextIfNeeded(requestContext, new Location().withTenantId("tenantId"));

    // then
    assertEquals("tenantId", future.result().getHeaders().get(XOkapiHeaders.TENANT));
    verify(restClient).getAsJsonObject(any(RequestEntry.class), any());
  }

  @Test
  void overrideContextToCentralTenantIfNeeded_withEmptyConsortiumConfiguration() {
    // given
    JsonObject consortiumConfiguration = new JsonObject(Map.of(
      "userTenants", new JsonArray(List.of())
    ));
    when(restClient.getAsJsonObject(any(RequestEntry.class), any()))
      .thenReturn(Future.succeededFuture(consortiumConfiguration));

    // when
    Future<RequestContext> future = consortiumConfigurationService.overrideContextToCentralTenantIfNeeded(requestContext);

    // then
    assertEquals(requestContext, future.result());
    verify(restClient).getAsJsonObject(any(RequestEntry.class), any());
    verifyNoInteractions(settingsRetriever);
  }

  @Test
  void overrideContextToCentralTenantIdNeeded_withSameTenantIf() {
    // given
    JsonObject consortiumConfiguration = new JsonObject(Map.of(
      "userTenants", new JsonArray(List.of(
        new JsonObject(Map.of("consortiumId", "cid", "centralTenantId", "tenantId"))
      ))
    ));
    when(restClient.getAsJsonObject(any(RequestEntry.class), any()))
      .thenReturn(Future.succeededFuture(consortiumConfiguration));
    when(requestContext.getHeaders()).thenReturn(Map.of(XOkapiHeaders.TENANT, "tenantId"));

    // when
    Future<RequestContext> future = consortiumConfigurationService.overrideContextToCentralTenantIfNeeded(requestContext);

    // then
    assertEquals(requestContext, future.result());
    verify(restClient).getAsJsonObject(any(RequestEntry.class), any());
    verifyNoInteractions(settingsRetriever);
  }

  @Test
  void overrideContextToCentralTenantIdNeeded_withCentralOrderingEnabled() {
    // given
    JsonObject consortiumConfiguration = new JsonObject(Map.of(
      "userTenants", new JsonArray(List.of(
        new JsonObject(Map.of("consortiumId", "cid", "centralTenantId", "centralTenantId"))
      ))
    ));
    when(restClient.getAsJsonObject(any(RequestEntry.class), any()))
      .thenReturn(Future.succeededFuture(consortiumConfiguration));
    when(requestContext.getHeaders()).thenReturn(Map.of(XOkapiHeaders.TENANT, "tenantId"));
    when(settingsRetriever.getSettingByKey(any(SettingKey.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Optional.of(new Setting().withValue("true"))));

    // when
    Future<RequestContext> future = consortiumConfigurationService.overrideContextToCentralTenantIfNeeded(requestContext);

    // then
    assertEquals("centralTenantId", future.result().getHeaders().get(XOkapiHeaders.TENANT));
    verify(restClient).getAsJsonObject(any(RequestEntry.class), any());
    verify(settingsRetriever).getSettingByKey(any(SettingKey.class), any(RequestContext.class));
  }

  @Test
  void overrideContextToCentralTenantIdNeeded_withCentralOrderingDisabled() {
    // given
    JsonObject consortiumConfiguration = new JsonObject(Map.of(
      "userTenants", new JsonArray(List.of(
        new JsonObject(Map.of("consortiumId", "cid", "centralTenantId", "centralTenantId"))
      ))
    ));
    when(restClient.getAsJsonObject(any(RequestEntry.class), any()))
      .thenReturn(Future.succeededFuture(consortiumConfiguration));
    when(requestContext.getHeaders()).thenReturn(Map.of(XOkapiHeaders.TENANT, "tenantId"));
    when(settingsRetriever.getSettingByKey(any(SettingKey.class), any(RequestContext.class)))
      .thenReturn(Future.succeededFuture(Optional.of(new Setting().withValue("false"))));

    // when
    Future<RequestContext> future = consortiumConfigurationService.overrideContextToCentralTenantIfNeeded(requestContext);

    // then
    assertEquals(requestContext, future.result());
    verify(restClient).getAsJsonObject(any(RequestEntry.class), any());
    verify(settingsRetriever).getSettingByKey(any(SettingKey.class), any(RequestContext.class));
  }
}
