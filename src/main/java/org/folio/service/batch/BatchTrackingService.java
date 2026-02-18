package org.folio.service.batch;

import io.vertx.core.Future;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.BatchTracking;

import static org.folio.orders.utils.ResourcePathResolver.BATCH_TRACKING;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

@Log4j2
@AllArgsConstructor
public class BatchTrackingService {

  public static final String BATCH_TRACKING_HEADER = "X-Batch-Tracking-Id";
  private static final String BATCH_TRACKING_ENDPOINT = resourcesPath(BATCH_TRACKING);

  protected final RestClient restClient;

  public Future<Boolean> createBatchTrackingRecord(String id, int totalRecords, RequestContext requestContext) {
    var batchTracking = new BatchTracking().withId(id).withTotalRecords(totalRecords);
    var requestEntry = new RequestEntry(BATCH_TRACKING_ENDPOINT);
    return restClient.post(requestEntry, batchTracking, BatchTracking.class, requestContext)
      .map(tracking -> requestContext.getHeaders().put(BATCH_TRACKING_HEADER, tracking.getId()))
      .onFailure(t -> log.error("Failed to create batch tracking record for batchId: {}", batchTracking.getId(), t))
      .recover(t -> Future.succeededFuture()); // In case of failure, we return a succeeded future to avoid failing the entire batch process.
  }

}
