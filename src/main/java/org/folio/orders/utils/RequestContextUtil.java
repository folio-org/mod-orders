package org.folio.orders.utils;

import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.helper.PurchaseOrderHelper;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.core.models.RequestContext;

public class RequestContextUtil {
  private static final Logger logger = LogManager.getLogger(PurchaseOrderHelper.class);

  private RequestContextUtil() {
  }

  public static RequestContext createContextWithNewTenantId(RequestContext requestContext, String tenantId) {
    if (tenantId == null) {
      return requestContext;
    }
    var modifiedHeaders = new CaseInsensitiveMap<>(requestContext.getHeaders());
    modifiedHeaders.put(XOkapiHeaders.TENANT, tenantId);
    logger.info("Request context has been changes with new tenant: {}", tenantId);
    return new RequestContext(requestContext.getContext(), modifiedHeaders);
  }

}
