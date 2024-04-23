package org.folio.service.caches;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Map;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.service.inventory.InventoryService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

@ExtendWith(VertxExtension.class)
public class InventoryCacheTest {

  Map<String,String> headers = Map.of();
  RequestContext requestContext = when(mock(RequestContext.class).getHeaders()).thenReturn(headers).getMock();

  @ParameterizedTest
  @CsvSource(textBlock = """
      88888888-8888-8888-8888-888888888888,88888888-8888-8888-8888-888888888888
      ,8261054f-be78-422d-bd51-4ed9f33c3422
      """)
  // https://github.com/folio-org/mod-inventory-storage/blob/v27.1.0/reference-data/identifier-types/isbn.json
  void getIsbnTypeId(String db, String expected, VertxTestContext vtc) {
    inventoryCache(db).getISBNProductTypeId(requestContext)
    .onComplete(vtc.succeeding(id -> {
      assertThat(id, is(expected));
      vtc.completeNow();
    }));
  }

  @ParameterizedTest
  @CsvSource(textBlock = """
      99999999-9999-9999-9999-999999999999,99999999-9999-9999-9999-999999999999
      ,fcca2643-406a-482a-b760-7a7f8aec640e
      """)
  // https://github.com/folio-org/mod-inventory-storage/blob/v27.1.0/reference-data/identifier-types/InvalidIsbn.json
  void getInvalidIsnbTypeId(String db, String expected, VertxTestContext vtc) {
    inventoryCache(db).getInvalidISBNProductTypeId(requestContext)
    .onComplete(vtc.succeeding(id -> {
      assertThat(id, is(expected));
      vtc.completeNow();
    }));
  }

  @Test
  void getProductTypeUuidShouldForwardDbException(VertxTestContext vtc) {
    var inventoryService = mock(InventoryService.class);
    when(inventoryService.getProductTypeUuid(any(), any())).thenReturn(Future.failedFuture("db fail"));
    new InventoryCache(inventoryService).getProductTypeUuid(null, null, requestContext)
    .onComplete(vtc.failing(e -> {
      assertThat(e.getMessage(), is("db fail"));
      vtc.completeNow();
    }));
  }

  InventoryCache inventoryCache(String uuid) {
    var array = new JsonArray();
    if (uuid != null) {
      array.add(new JsonObject().put("id", uuid));
    }
    var json = new JsonObject().put("identifierTypes", array);
    var restClient = mock(RestClient.class);
    when(restClient.getAsJsonObject(anyString(), anyBoolean(), any())).thenReturn(Future.succeededFuture(json));
    var inventoryService = new InventoryService(restClient);
    return new InventoryCache(inventoryService);
  }
}
