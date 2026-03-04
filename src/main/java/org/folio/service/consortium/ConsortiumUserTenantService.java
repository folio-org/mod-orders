package org.folio.service.consortium;

import io.vertx.core.Future;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang.BooleanUtils;
import org.folio.rest.core.models.RequestContext;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static org.folio.orders.utils.RequestContextUtil.createContextWithNewTenantId;

@Log4j2
@RequiredArgsConstructor
public class ConsortiumUserTenantService {

  private final ConsortiumConfigurationService consortiumConfigurationService;
  private final ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;

  public Future<List<String>> getUserTenantsIfNeeded(RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration -> {
        if (consortiumConfiguration.isEmpty()) {
          return Future.succeededFuture(Collections.emptyList());
        }
        var configuration = consortiumConfiguration.get();

        // Always change to central tenant when it comes to checking if Central Ordering is enabled
        var centralRequestContext = createContextWithNewTenantId(requestContext, configuration.centralTenantId());
        return consortiumConfigurationService.isCentralOrderingEnabled(centralRequestContext)
          .compose(enabled -> {
            if (Objects.isNull(enabled) || BooleanUtils.isFalse(enabled)) {
              log.info("getUserTenantsIfNeeded:: Central ordering is disabled or not configured");
              return Future.succeededFuture(Collections.emptyList());
            }
            return consortiumUserTenantsRetriever.getUserTenants(configuration.consortiumId(), configuration.centralTenantId(), requestContext);
          });
      })
      .recover(throwable -> {
        log.warn("getUserTenantsIfNeeded:: Failed to retrieve user tenants, falling back to current tenant only", throwable);
        return Future.succeededFuture(Collections.emptyList());
      });
  }
}
