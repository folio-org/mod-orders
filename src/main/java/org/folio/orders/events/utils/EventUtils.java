package org.folio.orders.events.utils;

import io.vertx.core.json.JsonObject;

public class EventUtils {

  private static final String POL_UPDATE = "poLineIdUpdate";

  public static JsonObject createPoLineUpdateEvent(String poLineId) {
    return JsonObject.of(POL_UPDATE, poLineId);
  }

  public static String getPoLineId(JsonObject eventPayload) {
    return eventPayload.getString(POL_UPDATE);
  }

  private EventUtils() {}

}
