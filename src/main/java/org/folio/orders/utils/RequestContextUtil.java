package org.folio.orders.utils;

import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.apache.commons.lang3.StringUtils;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.Location;

import java.util.Map;

public class RequestContextUtil {

  private RequestContextUtil() {
  }

  public static RequestContext cloneRequestContextBasedOnLocation(RequestContext requestContext, Location location) {
    if (StringUtils.isBlank(location.getTenantId())) {
      return requestContext;
    }
    return cloneRequestContextWithTargetTenantId(requestContext, location.getTenantId());
  }

  public static RequestContext cloneRequestContextWithTargetTenantId(RequestContext requestContext, String targetTenantId) {
    Map<String, String> modifiedHeaders = new CaseInsensitiveMap<>(requestContext.getHeaders());
    modifiedHeaders.put(XOkapiHeaders.TENANT, targetTenantId);
    return new RequestContext(requestContext.getContext(), modifiedHeaders);
  }

}
