package org.folio.service.pieces;

import org.folio.orders.events.handlers.MessageAddress;
import org.folio.rest.core.models.RequestContext;
import java.util.HashMap;
import java.util.Map;
import io.vertx.core.eventbus.DeliveryOptions;
import io.vertx.core.json.JsonObject;
import static org.folio.helper.AbstractHelper.OKAPI_HEADERS;
import static org.folio.orders.utils.HelperUtils.LANG;
import static org.folio.rest.RestConstants.OKAPI_URL;

public class PieceChangeReceiptStatusPublisher {
  public void sendEvent(MessageAddress messageAddress, JsonObject data, RequestContext requestContext) {
    DeliveryOptions deliveryOptions = new DeliveryOptions();

    // Add okapi headers
    Map<String, String> okapiHeaders = requestContext.getHeaders();
    if (okapiHeaders != null) {
      okapiHeaders.forEach(deliveryOptions::addHeader);
    } else {
      Map<String, String> okapiHeadersMap = new HashMap<>();
      JsonObject okapiHeadersObject = data.getJsonObject(OKAPI_HEADERS);
      okapiHeadersMap.put(OKAPI_URL, okapiHeadersObject.getString(OKAPI_URL));
    }

    data.put(LANG, "en");

    requestContext.getContext().owner()
      .eventBus()
      .send(messageAddress.address, data, deliveryOptions);
  }
}
