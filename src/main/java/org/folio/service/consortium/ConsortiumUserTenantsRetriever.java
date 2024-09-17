package org.folio.service.consortium;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import java.util.List;
import java.util.stream.IntStream;

import static org.folio.orders.utils.RequestContextUtil.getUserIdFromContext;
import static org.folio.orders.utils.ResourcePathResolver.CONSORTIA_USER_TENANTS;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestConstants.PATH_PARAM_PLACE_HOLDER;
import static org.folio.service.pieces.util.UserTenantFields.COLLECTION_USER_TENANTS;
import static org.folio.service.pieces.util.UserTenantFields.TENANT_ID;
import static org.folio.service.pieces.util.UserTenantFields.USER_ID;

public class ConsortiumUserTenantsRetriever {

  private static final String CONSORTIA_USER_TENANTS_ENDPOINT = resourcesPath(CONSORTIA_USER_TENANTS);

  private final RestClient restClient;

  public ConsortiumUserTenantsRetriever(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<List<String>> getUserTenants(String consortiumId, RequestContext requestContext) {
    var userId = getUserIdFromContext(requestContext);
    var url = CONSORTIA_USER_TENANTS_ENDPOINT.replace(PATH_PARAM_PLACE_HOLDER, consortiumId);
    var requestEntry = new RequestEntry(url)
      .withOffset(0)
      .withLimit(Integer.MAX_VALUE)
      .withQueryParameter(USER_ID.getValue(), userId);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(this::extractTenantIds);
  }

  private List<String> extractTenantIds(JsonObject userTenantCollection) {
    var userTenants = userTenantCollection.getJsonArray(COLLECTION_USER_TENANTS.getValue());
    return IntStream.range(0, userTenants.size())
      .mapToObj(userTenants::getJsonObject)
      .map(userTenant -> userTenant.getString(TENANT_ID.getValue()))
      .toList();
  }

}
