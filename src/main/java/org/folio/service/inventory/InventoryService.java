package org.folio.service.inventory;

import static org.folio.orders.utils.HelperUtils.encodeQuery;
import static org.folio.orders.utils.HelperUtils.extractId;
import static org.folio.orders.utils.HelperUtils.getFirstObjectFromResponse;
import static org.folio.rest.RestConstants.ID;
import static org.folio.rest.core.exceptions.ErrorCodes.ISBN_NOT_VALID;
import static org.folio.service.inventory.InventoryUtils.INVENTORY_LOOKUP_ENDPOINTS;

import java.util.Collections;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;
import org.springframework.stereotype.Component;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

@Component
public class InventoryService {
  private static final String IDENTIFIER_TYPES = "identifierTypes";

  private static final Logger logger = LogManager.getLogger(InventoryService.class);
  private final RestClient restClient;

  public InventoryService(RestClient restClient) {
    this.restClient = restClient;

  }

  public Future<String> getProductTypeUuid(String endpoint, RequestContext requestContext) {
    return restClient.getAsJsonObject(endpoint, false, requestContext)
      .compose(identifierTypes -> {
        String identifierTypeId = extractId(getFirstObjectFromResponse(identifierTypes, IDENTIFIER_TYPES));
        return Future.succeededFuture(identifierTypeId);
      });
  }

  public Future<String> convertToISBN13(String isbn, String endpoint, RequestContext requestContext) {
    return restClient.getAsJsonObject(endpoint, false, requestContext)
      .map(json -> json.getString("isbn"))
      .recover(throwable -> {
        logger.error("Can't convert {} to isbn13: {}", endpoint, throwable.getMessage());
        List<Parameter> parameters = Collections.singletonList(new Parameter().withKey("isbn").withValue(isbn));
        throw new HttpException(400, ISBN_NOT_VALID.toError().withParameters(parameters));
      });
  }

  public Future<JsonObject> getEntryTypeId(String entryType, String entryTypeValue, RequestContext requestContext) {
    String endpoint = buildInventoryLookupEndpoint(entryType, encodeQuery(entryTypeValue));
    RequestEntry requestEntry = new RequestEntry(endpoint);

    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(entries -> {
        JsonObject result = new JsonObject();
        result.put(entryType, getFirstObjectFromResponse(entries, entryType).getString(ID));
        return result;
      });
  }

  public String buildInventoryLookupEndpoint(String type, Object... params) {
    return String.format(INVENTORY_LOOKUP_ENDPOINTS.get(type), params);
  }

}
