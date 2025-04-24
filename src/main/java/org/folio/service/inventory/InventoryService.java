package org.folio.service.inventory;

import static org.folio.orders.utils.QueryUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.getFirstObjectFromResponse;
import static org.folio.rest.RestConstants.ID;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;

import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.springframework.stereotype.Component;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

@Component
public class InventoryService {
  private final RestClient restClient;

  public InventoryService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<JsonObject> getEntryTypeId(String entryType, String entryTypeValue, RequestContext requestContext) {
    String endpoint = String.format(INVENTORY_LOOKUP_ENDPOINTS.get(entryType), encodeQuery(entryTypeValue));
    RequestEntry requestEntry = new RequestEntry(endpoint);

    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(entries -> {
        JsonObject result = new JsonObject();
        result.put(entryType, getFirstObjectFromResponse(entries, entryType).getString(ID));
        return result;
      });
  }
}
