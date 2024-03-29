package org.folio.service.consortium;

import io.vertx.core.Context;
import io.vertx.core.Future;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.models.consortium.ConsortiumConfiguration;
import org.folio.models.consortium.SharingInstance;
import org.folio.models.consortium.SharingStatus;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.ConsortiumException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.folio.rest.tools.utils.TenantTool;

import java.util.Map;
import java.util.UUID;

/**
 * The `SharingInstanceService` class manages the creation of shadow instances
 * within a consortium using the `mod-consortia` module.
 * It provides methods for creating shadow instances and sharing them within the consortium.
 * This service is responsible for making REST API calls to the `mod-consortia` module.
 */
public class SharingInstanceService {
  private static final Logger logger = LogManager.getLogger(SharingInstanceService.class);

  private static final String SHARE_INSTANCE_ENDPOINT = "/consortia/{id}/sharing/instances";
  private static final String SHARING_INSTANCE_ERROR = "Error during sharing Instance for sourceTenantId: %s, targetTenantId: %s, instanceIdentifier: %s, error: %s";

  private final RestClient restClient;

  public SharingInstanceService(RestClient restClient) {
    this.restClient = restClient;
  }

  /**
   * Creates a shadow instance and shares it within the consortium.
   *
   * @param instanceId              the unique identifier of the instance to be shared
   * @param consortiumConfiguration the consortium configuration
   * @param requestContext          the request context
   * @return a Future that resolves with the created SharingInstance
   */
  public Future<SharingInstance> createShadowInstance(String instanceId, ConsortiumConfiguration consortiumConfiguration, RequestContext requestContext) {
    SharingInstance sharingInstance = new SharingInstance(UUID.fromString(instanceId),
      consortiumConfiguration.centralTenantId(), TenantTool.tenantId(requestContext.getHeaders()));
    RequestContext consortiaRequestContext = createRequestContextWithUpdatedTenantId(requestContext.getContext(),
      requestContext.getHeaders(), consortiumConfiguration.centralTenantId());
    return shareInstance(consortiumConfiguration.consortiumId(), sharingInstance, consortiaRequestContext);
  }

  private Future<SharingInstance> shareInstance(String consortiumId, SharingInstance sharingInstance, RequestContext requestContext) {
    RequestEntry requestEntry = new RequestEntry(SHARE_INSTANCE_ENDPOINT).withId(consortiumId);
    return restClient.post(requestEntry, sharingInstance, SharingInstance.class, requestContext)
      .compose(response -> {
        if (ObjectUtils.notEqual(SharingStatus.ERROR, response.status())) {
          logger.debug("Successfully sharedInstance with id: {}, sharedInstance: {}", response.instanceIdentifier(), response);
          return Future.succeededFuture(response);
        } else {
          String message = String.format(SHARING_INSTANCE_ERROR, sharingInstance.sourceTenantId(), sharingInstance.targetTenantId(),
            sharingInstance.instanceIdentifier(), sharingInstance.error());
          return Future.failedFuture(new ConsortiumException(message));
        }
      });
  }

  private RequestContext createRequestContextWithUpdatedTenantId(Context context, Map<String, String> headers, String centralTenantId) {
    Map<String, String> modifiedHeaders  = new CaseInsensitiveMap<>(headers);
    modifiedHeaders.put(XOkapiHeaders.TENANT, centralTenantId);
    return new RequestContext(context, modifiedHeaders );
  }

}
