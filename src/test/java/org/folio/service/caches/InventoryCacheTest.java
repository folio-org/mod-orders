package org.folio.service.caches;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import org.folio.CopilotGenerated;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.inventory.InventoryService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(VertxExtension.class)
@CopilotGenerated
public class InventoryCacheTest {

  private InventoryService inventoryService;
  private InventoryCache inventoryCache;
  private RequestContext requestContext;

  @BeforeEach
  public void setUp() {
    inventoryService = mock(InventoryService.class);
    inventoryCache = new InventoryCache(inventoryService);
    Map<String, String> headers = Map.of("X-Okapi-Tenant", "test-tenant", "user-id", "test-user");
    requestContext = when(mock(RequestContext.class).getHeaders()).thenReturn(headers).getMock();
    // Ensure a Vertx context is available for the cache
    Vertx.vertx().runOnContext(v -> {});
  }

  @Test
  public void shouldReturnEntryIdWhenServiceSucceeds() {
    JsonArray array = new JsonArray().add(new JsonObject().put("id", "expected-id"));
    JsonObject responseJson = new JsonObject().put("identifierTypes", array);
    when(inventoryService.getEntryTypeId(anyString(), anyString(), any())).thenReturn(Future.succeededFuture(responseJson));

    inventoryCache.getEntryId("entryType", "entryTypeValue", requestContext)
      .onComplete(ar -> {
        assertTrue(ar.succeeded());
        JsonObject result = ar.result();
        assertNotNull(result);
        assertEquals("expected-id", result.getJsonArray("identifierTypes").getJsonObject(0).getString("id"));
      });
  }

  @Test
  public void shouldFailWhenServiceFails() {
    when(inventoryService.getEntryTypeId(anyString(), anyString(), any()))
      .thenReturn(Future.failedFuture(new RuntimeException("Service error")));

    inventoryCache.getEntryId("entryType", "entryTypeValue", requestContext)
      .onComplete(ar -> {
        assertTrue(ar.failed());
        assertEquals("Service error", ar.cause().getMessage());
      });
  }
}
