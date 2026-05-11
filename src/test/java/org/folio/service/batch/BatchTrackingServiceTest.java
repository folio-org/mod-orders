package org.folio.service.batch;

import static io.vertx.core.Future.failedFuture;
import static io.vertx.core.Future.succeededFuture;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import io.vertx.core.Future;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.CopilotGenerated;
import org.folio.rest.acq.model.BatchTracking;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@CopilotGenerated(model = "Claude Opus 4.5")
@ExtendWith({ VertxExtension.class, MockitoExtension.class })
public class BatchTrackingServiceTest {

  @InjectMocks
  private BatchTrackingService batchTrackingService;
  @Mock
  private RestClient restClient;
  @Mock
  private RequestContext requestContext;

  @Test
  void shouldCreateBatchTrackingRecord(VertxTestContext vertxTestContext) {
    // Given
    String batchId = UUID.randomUUID().toString();
    int totalRecords = 10;
    BatchTracking createdBatchTracking = new BatchTracking().withId(batchId).withTotalRecords(totalRecords);

    doReturn(succeededFuture(createdBatchTracking))
      .when(restClient).post(any(RequestEntry.class), any(BatchTracking.class), eq(BatchTracking.class), eq(requestContext));

    // When
    Future<Void> future = batchTrackingService.createBatchTrackingRecord(batchId, totalRecords, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

  @Test
  void shouldReturnSucceededFutureWhenRestClientFails(VertxTestContext vertxTestContext) {
    // Given
    String batchId = UUID.randomUUID().toString();
    int totalRecords = 5;

    doReturn(failedFuture(new RuntimeException("Connection error")))
      .when(restClient).post(any(RequestEntry.class), any(BatchTracking.class), eq(BatchTracking.class), eq(requestContext));

    // When
    Future<Void> future = batchTrackingService.createBatchTrackingRecord(batchId, totalRecords, requestContext);

    // Then
    vertxTestContext.assertComplete(future)
      .onComplete(result -> {
        assertTrue(result.succeeded());
        vertxTestContext.completeNow();
      });
  }

}

