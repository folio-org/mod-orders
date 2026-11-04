package org.folio.models.claiming;

import org.folio.rest.acq.model.Organization;

/**
 * A piece to be claimed, with the PO line and order values that select its Claims integrations.
 *
 * @param vendor            vendor of the piece's order
 * @param pieceId           id of the piece
 * @param acquisitionMethod acquisition method of the piece's PO line, null if the PO line has none
 */
public record ClaimPiece(Organization vendor, String pieceId, String acquisitionMethod) {
}
