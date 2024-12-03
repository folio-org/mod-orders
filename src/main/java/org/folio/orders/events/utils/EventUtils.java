package org.folio.orders.events.utils;

import io.vertx.core.json.JsonObject;

public class EventUtils {

  public static final String POL_UPDATE_FIELD = "poLineIdUpdate";

  public static JsonObject createPoLineUpdateEvent(String poLineId) {
    return JsonObject.of(POL_UPDATE_FIELD, poLineId);
  }

  public static String getPoLineId(JsonObject eventPayload) {
    return eventPayload.getString(POL_UPDATE_FIELD);
  }

  private EventUtils() {}

}
