package org.folio.utils;

import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.core.models.RequestContext;

import static org.mockito.ArgumentMatchers.argThat;

public class RequestContextMatcher {

  private static final String FOLIO_SHARED = "folio_shared";

  public static RequestContext matchCentralTenant() {
    return matchTenant(FOLIO_SHARED);
  }

  public static RequestContext matchTenant(String tenantId) {
    return argThat(context -> {
      String contextTenant = context.getHeaders().get(XOkapiHeaders.TENANT);
      return contextTenant == null ? tenantId.equals(FOLIO_SHARED) : contextTenant.equals(tenantId);
    });
  }

}
