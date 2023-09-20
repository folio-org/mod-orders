package org.folio.service.consortium;

import io.vertx.core.Future;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;

import java.util.Optional;

public class ConsortiumConfigurationService {
  private static final Logger logger = LogManager.getLogger(ConsortiumConfigurationService.class);

  private static final String CONSORTIUM_ID_FIELD = "consortiumId";
  private static final String CENTRAL_TENANT_ID_FIELD = "centralTenantId";
  private static final String USER_TENANTS_ARRAY_IDENTIFIER = "userTenants";
  private static final String USER_TENANTS_ENDPOINT = "/user-tenants";

  private final RestClient restClient;

  public ConsortiumConfigurationService(RestClient restClient) {
    this.restClient = restClient;
  }

  public Future<Optional<ConsortiumConfiguration>> getConsortiumConfiguration(RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(USER_TENANTS_ENDPOINT).withLimit(1);
    return restClient.getAsJsonObject(requestEntry, requestContext)
      .map(jsonObject -> jsonObject.getJsonArray(USER_TENANTS_ARRAY_IDENTIFIER))
      .map(userTenants -> {
        if (userTenants.isEmpty()) {
          logger.debug("Central tenant and consortium id not found");
          return Optional.empty();
        }
        String consortiumId = userTenants.getJsonObject(0).getString(CONSORTIUM_ID_FIELD);
        String centralTenantId = userTenants.getJsonObject(0).getString(CENTRAL_TENANT_ID_FIELD);
        logger.debug("Found centralTenantId: {} and consortiumId: {}", centralTenantId, consortiumId);
        return Optional.of(new ConsortiumConfiguration(centralTenantId, consortiumId));
      });
  }
}
