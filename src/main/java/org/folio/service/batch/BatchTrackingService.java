package org.folio.service.batch;

import io.vertx.core.Future;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.acq.model.BatchTracking;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import java.util.HashMap;

import static org.folio.orders.utils.FutureUtils.asFuture;
import static org.folio.orders.utils.ResourcePathResolver.BATCH_TRACKING;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

@Log4j2
@AllArgsConstructor
public class BatchTrackingService {

  private static final String BATCH_TRACKING_HEADER = "X-Batch-Tracking-Id";
  private static final String BATCH_TRACKING_ENDPOINT = resourcesPath(BATCH_TRACKING);

  protected final RestClient restClient;

  public Future<Void> createBatchTrackingRecord(String id, int totalRecords, RequestContext requestContext) {
    var batchTracking = new BatchTracking().withId(id).withTotalRecords(totalRecords);
    var requestEntry = new RequestEntry(BATCH_TRACKING_ENDPOINT);
    return restClient.post(requestEntry, batchTracking, BatchTracking.class, requestContext)
      .map(tracking -> asFuture(() -> populateBatchId(tracking.getId(), requestContext))) // Add the batch tracking ID to the request headers for downstream services to use.
      .onFailure(t -> log.error("Failed to create batch tracking record for batchId: {}", batchTracking.getId(), t))
      .recover(t -> Future.succeededFuture()) // In case of failure, we return a succeeded future to avoid failing the entire batch process.
      .mapEmpty();
  }

  /**
   Populate the batch id in the request headers

   @param requestContext the request context containing the headers
   */
  public static void populateBatchId(String batchId, RequestContext requestContext) {
    var headers = new HashMap<>(requestContext.getHeaders());
    headers.put(BATCH_TRACKING_HEADER, batchId);
    requestContext.withHeaders(headers);
  }

}
