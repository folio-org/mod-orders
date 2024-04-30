package org.folio.orders.utils;

import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.core.models.RequestContext;

public class RequestContextUtil {

  private RequestContextUtil() {
  }

  public static RequestContext createContextWithNewTenantId(RequestContext requestContext, String tenantId) {
    var modifiedHeaders = new CaseInsensitiveMap<>(requestContext.getHeaders());
    modifiedHeaders.put(XOkapiHeaders.TENANT, tenantId);
    return new RequestContext(requestContext.getContext(), modifiedHeaders);
  }

}
