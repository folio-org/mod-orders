package org.folio.models.claiming;

import io.vertx.core.json.JsonObject;

import java.util.List;

/**
 * An export job to be created for one Claims export config.
 *
 * @param config   the export config
 * @param pieceIds ids of the pieces this config takes
 */
public record ClaimJob(JsonObject config, List<String> pieceIds) {
}
