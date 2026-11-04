package org.folio.models.claiming;

import io.vertx.core.json.JsonObject;

import java.util.List;

/**
 * An export job to be created for one Claims integration.
 *
 * @param config   export config of the integration
 * @param pieceIds ids of the pieces this integration takes
 */
public record ClaimJob(JsonObject config, List<String> pieceIds) {
}
