package org.folio.models;

import org.folio.rest.jaxrs.model.ItemsDetail;
import org.folio.rest.jaxrs.model.PiecesDetail;
import org.folio.rest.jaxrs.model.PoLinesDetail;

import java.util.List;

public record HoldingDetailHolder(String holdingId, List<PoLinesDetail> poLines, List<PiecesDetail> pieces, List<ItemsDetail> items) {
}
