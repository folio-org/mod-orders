package org.folio.service.consortium;

import io.vertx.core.Future;
import org.folio.CopilotGenerated;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.rest.core.models.RequestContext;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@CopilotGenerated(model = "Claude Sonnet 4.5")
public class ConsortiumUserTenantServiceTest {

  @InjectMocks private ConsortiumUserTenantService service;
  @Mock private ConsortiumConfigurationService consortiumConfigurationService;
  @Mock private ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;
  @Mock private RequestContext requestContext;

  @Test
  void shouldReturnEmptyListWhenConsortiumConfigurationIsEmpty() {
    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(Optional.empty()));

    var result = service.getUserTenantsIfNeeded(requestContext).result();

    assertTrue(result.isEmpty());
    verify(consortiumConfigurationService).getConsortiumConfiguration(requestContext);
    verify(consortiumConfigurationService, never()).isCentralOrderingEnabled(any());
    verify(consortiumUserTenantsRetriever, never()).getUserTenants(anyString(), anyString(), any());
  }

  @Test
  void shouldReturnEmptyListWhenCentralOrderingIsDisabled() {
    var consortiumId = "consortium-1";
    var centralTenantId = "central-tenant";
    var config = new ConsortiumConfiguration(centralTenantId, consortiumId);

    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(Optional.of(config)));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(false));

    var result = service.getUserTenantsIfNeeded(requestContext).result();

    assertTrue(result.isEmpty());
    verify(consortiumConfigurationService).getConsortiumConfiguration(requestContext);
    verify(consortiumConfigurationService).isCentralOrderingEnabled(any());
    verify(consortiumUserTenantsRetriever, never()).getUserTenants(anyString(), anyString(), any());
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("provideCentralOrderingEnabledValues")
  void shouldHandleCentralOrderingEnabledValues(String testName, Boolean enabled, boolean shouldCallRetriever) {
    var consortiumId = "consortium-1";
    var centralTenantId = "central-tenant";
    var config = new ConsortiumConfiguration(centralTenantId, consortiumId);
    var userTenants = List.of("tenant-1", "tenant-2");

    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(Optional.of(config)));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(enabled));
    if (shouldCallRetriever) {
      when(consortiumUserTenantsRetriever.getUserTenants(consortiumId, centralTenantId, requestContext))
        .thenReturn(Future.succeededFuture(userTenants));
    }

    var result = service.getUserTenantsIfNeeded(requestContext).result();

    if (shouldCallRetriever) {
      assertEquals(userTenants, result);
      verify(consortiumUserTenantsRetriever).getUserTenants(consortiumId, centralTenantId, requestContext);
    } else {
      assertTrue(result.isEmpty());
      verify(consortiumUserTenantsRetriever, never()).getUserTenants(anyString(), anyString(), any());
    }
  }

  static Stream<Arguments> provideCentralOrderingEnabledValues() {
    return Stream.of(
      Arguments.of("Central ordering enabled (true)", true, true),
      Arguments.of("Central ordering disabled (false)", false, false),
      Arguments.of("Central ordering disabled (null)", null, false)
    );
  }

  @Test
  void shouldReturnUserTenantsWhenCentralOrderingIsEnabled() {
    var consortiumId = "consortium-1";
    var centralTenantId = "central-tenant";
    var config = new ConsortiumConfiguration(centralTenantId, consortiumId);
    var userTenants = List.of("tenant-1", "tenant-2", "tenant-3");

    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(Optional.of(config)));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(true));
    when(consortiumUserTenantsRetriever.getUserTenants(consortiumId, centralTenantId, requestContext))
      .thenReturn(Future.succeededFuture(userTenants));

    var result = service.getUserTenantsIfNeeded(requestContext).result();

    assertEquals(userTenants, result);
    verify(consortiumConfigurationService).getConsortiumConfiguration(requestContext);
    verify(consortiumConfigurationService).isCentralOrderingEnabled(any());
    verify(consortiumUserTenantsRetriever).getUserTenants(consortiumId, centralTenantId, requestContext);
  }

  @Test
  void shouldReturnEmptyListWhenGetConsortiumConfigurationFails() {
    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.failedFuture(new RuntimeException("Configuration service error")));

    var result = service.getUserTenantsIfNeeded(requestContext).result();

    assertTrue(result.isEmpty());
    verify(consortiumConfigurationService).getConsortiumConfiguration(requestContext);
  }

  @Test
  void shouldReturnEmptyListWhenIsCentralOrderingEnabledFails() {
    var consortiumId = "consortium-1";
    var centralTenantId = "central-tenant";
    var config = new ConsortiumConfiguration(centralTenantId, consortiumId);

    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(Optional.of(config)));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.failedFuture(new RuntimeException("Central ordering check failed")));

    var result = service.getUserTenantsIfNeeded(requestContext).result();

    assertTrue(result.isEmpty());
    verify(consortiumConfigurationService).getConsortiumConfiguration(requestContext);
    verify(consortiumConfigurationService).isCentralOrderingEnabled(any());
  }

  @Test
  void shouldReturnEmptyListWhenGetUserTenantsFails() {
    var consortiumId = "consortium-1";
    var centralTenantId = "central-tenant";
    var config = new ConsortiumConfiguration(centralTenantId, consortiumId);

    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(Optional.of(config)));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(true));
    when(consortiumUserTenantsRetriever.getUserTenants(consortiumId, centralTenantId, requestContext))
      .thenReturn(Future.failedFuture(new RuntimeException("Failed to retrieve user tenants")));

    var result = service.getUserTenantsIfNeeded(requestContext).result();

    assertTrue(result.isEmpty());
    verify(consortiumConfigurationService).getConsortiumConfiguration(requestContext);
    verify(consortiumConfigurationService).isCentralOrderingEnabled(any());
    verify(consortiumUserTenantsRetriever).getUserTenants(consortiumId, centralTenantId, requestContext);
  }

  @ParameterizedTest(name = "User tenants: {0}")
  @MethodSource("provideUserTenantLists")
  void shouldReturnCorrectUserTenantsForVariousScenarios(List<String> userTenants) {
    var consortiumId = "consortium-1";
    var centralTenantId = "central-tenant";
    var config = new ConsortiumConfiguration(centralTenantId, consortiumId);

    when(consortiumConfigurationService.getConsortiumConfiguration(requestContext))
      .thenReturn(Future.succeededFuture(Optional.of(config)));
    when(consortiumConfigurationService.isCentralOrderingEnabled(any()))
      .thenReturn(Future.succeededFuture(true));
    when(consortiumUserTenantsRetriever.getUserTenants(consortiumId, centralTenantId, requestContext))
      .thenReturn(Future.succeededFuture(userTenants));

    var result = service.getUserTenantsIfNeeded(requestContext).result();

    assertEquals(userTenants, result);
  }

  static Stream<Arguments> provideUserTenantLists() {
    return Stream.of(
      Arguments.of(Collections.emptyList()),
      Arguments.of(List.of("tenant-1")),
      Arguments.of(List.of("tenant-1", "tenant-2")),
      Arguments.of(List.of("tenant-1", "tenant-2", "tenant-3", "tenant-4", "tenant-5"))
    );
  }
}

