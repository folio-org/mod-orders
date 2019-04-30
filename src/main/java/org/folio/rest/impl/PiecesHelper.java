package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.json.JsonObject;
import org.folio.rest.jaxrs.model.Piece;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static org.folio.orders.utils.ResourcePathResolver.PIECES;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;

public class PiecesHelper extends AbstractHelper {

  public PiecesHelper(Map<String, String> okapiHeaders, Context ctx, String lang) {
    super(okapiHeaders, ctx, lang);
  }

  CompletableFuture<Piece> createRecordInStorage(Piece entity) {
    // On success set id of the created entity to piece object and return it back
    return createRecordInStorage(JsonObject.mapFrom(entity), resourcesPath(PIECES)).thenApply(entity::withId);
  }
}
