package org.folio.service.consortium;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Location;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

@ExtendWith({VertxExtension.class, MockitoExtension.class})
public class ConsortiumConfigurationServiceTest {

  @InjectMocks
  private ConsortiumConfigurationService consortiumConfigurationService;

  @Mock
  private RestClient restClient;

  @Mock
  private RequestContext requestContext;

  @Test
  void testCloneRequestContextIfNeeded(VertxTestContext vertxTestContext) {
    // given
    JsonObject consortiumConfiguration = new JsonObject(Map.of(
      "userTenants", new JsonArray(List.of(
        new JsonObject(Map.of("consortiumId", "cid", "centralTenantId", "ctid"))
      ))
    ));
    Mockito.when(restClient.getAsJsonObject(any(RequestEntry.class), any()))
      .thenReturn(Future.succeededFuture(consortiumConfiguration));

    // when
    Future<RequestContext> future = consortiumConfigurationService.cloneRequestContextIfNeeded(requestContext, new Location().withTenantId("tenantId"));

    // then
    vertxTestContext.assertComplete(future)
      .onComplete(ar -> {
        assertTrue(ar.succeeded());
        assertEquals("tenantId", ar.result().getHeaders().get(XOkapiHeaders.TENANT));
        verify(restClient).getAsJsonObject(any(RequestEntry.class), any());
        vertxTestContext.completeNow();
      });
  }

}
