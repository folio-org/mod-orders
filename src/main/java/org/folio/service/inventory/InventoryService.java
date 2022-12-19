package org.folio.service.inventory;

import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.jaxrs.model.Parameter;

import java.util.Collections;
import java.util.List;

import static org.folio.orders.utils.HelperUtils.extractId;
import static org.folio.orders.utils.HelperUtils.getFirstObjectFromResponse;
import static org.folio.rest.core.exceptions.ErrorCodes.ISBN_NOT_VALID;
import static org.folio.service.inventory.InventoryManager.IDENTIFIER_TYPES;

public class InventoryService {
  private static final Logger logger = LogManager.getLogger(InventoryService.class);
  private final RestClient restClient;

  public InventoryService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<String> getProductTypeUuidByIsbn(RequestContext requestContext) {
    // return id of already retrieved identifier type
    RequestEntry requestEntry = new RequestEntry("/identifier-types")
      .withLimit(1)
      .withQuery("name==ISBN");

    return restClient.getAsJsonObject(requestEntry, requestContext)
      .compose(identifierTypes -> {
        String identifierTypeId = extractId(getFirstObjectFromResponse(identifierTypes, IDENTIFIER_TYPES));
        return Future.succeededFuture(identifierTypeId);
      });
  }

  public Future<String> convertToISBN13(String isbn, RequestContext requestContext) {
    String convertEndpoint = String.format("/isbn/convertTo13?isbn=%s", isbn);
    RequestEntry requestEntry = new RequestEntry(convertEndpoint);

    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(json -> json.getString("isbn"))
      .recover(throwable -> {
        logger.error("Can't convert {} to isbn13: {}", isbn, throwable.getMessage());
        List<Parameter> parameters = Collections.singletonList(new Parameter().withKey("isbn").withValue(isbn));
        throw new HttpException(400, ISBN_NOT_VALID.toError().withParameters(parameters));
      });
  }
}
