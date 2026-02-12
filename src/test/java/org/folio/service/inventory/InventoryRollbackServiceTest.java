package org.folio.service.inventory;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.PoLine;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.HashSet;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
@ExtendWith(VertxExtension.class)
public class InventoryRollbackServiceTest {

  private InventoryRollbackService inventoryRollbackService;

  @Mock
  private RestClient restClient;

  @Mock
  private RequestContext requestContext;

  private PoLine poLine;

  @BeforeEach
  void setUp() {
    inventoryRollbackService = new InventoryRollbackService(restClient);
    poLine = new PoLine();
    poLine.setId("poLineId");
  }

  @Test
  void shouldDeleteInstanceWhenOrphaned(VertxTestContext testContext) {
    // Given
    poLine.setInstanceId("instanceId123");
    JsonObject holdingsResponse = new JsonObject().put("totalRecords", 0);
    when(restClient.getAsJsonObject(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(holdingsResponse));
    when(restClient.delete(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(null));

    // When
    Future<Void> future = inventoryRollbackService.deleteOrphanedInstanceIfNeeded(poLine, new HashSet<>(), requestContext);

    // Then
    future.onComplete(testContext.succeeding(result -> testContext.verify(() -> {
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      verify(restClient, times(1)).delete(any(RequestEntry.class), eq(requestContext));
      testContext.completeNow();
    })));
  }

  @Test
  void shouldNotDeleteInstanceWhenHoldingsExist(VertxTestContext testContext) {
    // Given
    poLine.setInstanceId("instanceId123");
    JsonObject holdingsResponse = new JsonObject().put("totalRecords", 1);
    when(restClient.getAsJsonObject(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(holdingsResponse));

    // When
    Future<Void> future = inventoryRollbackService.deleteOrphanedInstanceIfNeeded(poLine, new HashSet<>(), requestContext);

    // Then
    future.onComplete(testContext.succeeding(result -> testContext.verify(() -> {
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      verify(restClient, never()).delete(any(RequestEntry.class), any(RequestContext.class));
      testContext.completeNow();
    })));
  }

  @Test
  void shouldSkipWhenNoInstanceId(VertxTestContext testContext) {
    // Given
    poLine.setInstanceId(null);

    // When
    Future<Void> future = inventoryRollbackService.deleteOrphanedInstanceIfNeeded(poLine, new HashSet<>(), requestContext);

    // Then
    future.onComplete(testContext.succeeding(result -> testContext.verify(() -> {
      verify(restClient, never()).getAsJsonObject(any(RequestEntry.class), any(RequestContext.class));
      verify(restClient, never()).delete(any(RequestEntry.class), any(RequestContext.class));
      testContext.completeNow();
    })));
  }

  @Test
  void shouldDeleteInstanceWhenItWasCreated(VertxTestContext testContext) {
    // Given
    String instanceId = "instanceId123";
    poLine.setInstanceId(instanceId);
    Set<String> createdInstanceIds = new java.util.HashSet<>();
    createdInstanceIds.add(instanceId);

    JsonObject holdingsResponse = new JsonObject().put("totalRecords", 0);
    when(restClient.getAsJsonObject(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(holdingsResponse));
    when(restClient.delete(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(null));

    // When
    Future<Void> future = inventoryRollbackService.deleteOrphanedInstanceIfNeeded(poLine, createdInstanceIds, requestContext);

    // Then
    future.onComplete(testContext.succeeding(result -> testContext.verify(() -> {
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      verify(restClient, times(1)).delete(any(RequestEntry.class), eq(requestContext));
      testContext.completeNow();
    })));
  }

  @Test
  void shouldNotDeleteInstanceWhenItWasMatched(VertxTestContext testContext) {
    // Given
    String instanceId = "instanceId123";
    poLine.setInstanceId(instanceId);
    Set<String> createdInstanceIds = new java.util.HashSet<>();
    // Instance ID is not in the created set, meaning it was matched to an existing instance

    // When
    Future<Void> future = inventoryRollbackService.deleteOrphanedInstanceIfNeeded(poLine, createdInstanceIds, requestContext);

    // Then
    future.onComplete(testContext.succeeding(result -> testContext.verify(() -> {
      // Should not check holdings or delete since instance was matched, not created
      verify(restClient, never()).getAsJsonObject(any(RequestEntry.class), any(RequestContext.class));
      verify(restClient, never()).delete(any(RequestEntry.class), any(RequestContext.class));
      testContext.completeNow();
    })));
  }

  @Test
  void shouldDeleteDifferentInstanceWhenOnlyOneWasCreated(VertxTestContext testContext) {
    // Given
    String createdInstanceId = "createdInstanceId";
    String matchedInstanceId = "matchedInstanceId";

    Set<String> createdInstanceIds = new java.util.HashSet<>();
    createdInstanceIds.add(createdInstanceId);

    PoLine createdPoLine = new PoLine();
    createdPoLine.setId("createdPoLineId");
    createdPoLine.setInstanceId(createdInstanceId);

    PoLine matchedPoLine = new PoLine();
    matchedPoLine.setId("matchedPoLineId");
    matchedPoLine.setInstanceId(matchedInstanceId);

    JsonObject holdingsResponse = new JsonObject().put("totalRecords", 0);
    when(restClient.getAsJsonObject(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(holdingsResponse));
    when(restClient.delete(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(null));

    // When - process created instance
    Future<Void> createdFuture = inventoryRollbackService.deleteOrphanedInstanceIfNeeded(createdPoLine, createdInstanceIds, requestContext);

    // Then
    createdFuture.onComplete(testContext.succeeding(result -> testContext.verify(() -> {
      // Created instance should be deleted
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      verify(restClient, times(1)).delete(any(RequestEntry.class), eq(requestContext));

      // When - process matched instance
      Future<Void> matchedFuture = inventoryRollbackService.deleteOrphanedInstanceIfNeeded(matchedPoLine, createdInstanceIds, requestContext);

      matchedFuture.onComplete(testContext.succeeding(result2 -> testContext.verify(() -> {
        // Matched instance should not be deleted (still only 1 getAsJsonObject and 1 delete call from before)
        verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
        verify(restClient, times(1)).delete(any(RequestEntry.class), eq(requestContext));
        testContext.completeNow();
      })));
    })));
  }

  @Test
  void shouldRecoverGracefullyWhenDeletionFails(VertxTestContext testContext) {
    // Given
    poLine.setInstanceId("instanceId123");
    JsonObject holdingsResponse = new JsonObject().put("totalRecords", 0);
    when(restClient.getAsJsonObject(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.succeededFuture(holdingsResponse));
    when(restClient.delete(any(RequestEntry.class), eq(requestContext)))
      .thenReturn(Future.failedFuture(new RuntimeException("Delete failed")));

    // When
    Future<Void> future = inventoryRollbackService.deleteOrphanedInstanceIfNeeded(poLine, new HashSet<>(), requestContext);

    // Then - should succeed despite deletion failure (graceful recovery)
    future.onComplete(testContext.succeeding(result -> testContext.verify(() -> {
      verify(restClient, times(1)).getAsJsonObject(any(RequestEntry.class), eq(requestContext));
      verify(restClient, times(1)).delete(any(RequestEntry.class), eq(requestContext));
      testContext.completeNow();
    })));
  }
}
