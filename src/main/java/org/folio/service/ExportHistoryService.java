package org.folio.service;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.ExportHistoryCollection;

import io.vertx.core.Future;
import lombok.extern.log4j.Log4j2;

@Log4j2
public class ExportHistoryService {

  private final RestClient restClient;
  public static final String EXPORT_HISTORY_STORAGE_ENDPOINT = "/orders-storage/export-history";

  public ExportHistoryService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<ExportHistoryCollection> getExportHistoryByQuery(String query, int offset, int limit,
      RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(EXPORT_HISTORY_STORAGE_ENDPOINT).withQuery(query)
      .withOffset(offset)
      .withLimit(limit);
    return restClient.get(requestEntry, ExportHistoryCollection.class, requestContext);
  }
}
