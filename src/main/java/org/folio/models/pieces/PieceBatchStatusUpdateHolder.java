package org.folio.models.pieces;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.apache.commons.collections.CollectionUtils;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.PieceCollection;

import java.util.List;

@AllArgsConstructor
public class PieceBatchStatusUpdateHolder extends BasePieceFlowHolder {

  @Getter
  private Piece.ReceivingStatus receivingStatus;
  @Getter
  private Integer claimingInterval;
  @Getter
  private String internalNote;
  @Getter
  private String externalNote;
  @Getter
  private List<Piece> pieces;
  private String poLineId;

  @Override
  public String getOrderLineId() {
    return poLineId;
  }

  @Override
  public String getTitleId() {
    return null;
  }

  public PieceCollection getPieceCollection() {
    return new PieceCollection().withPieces(pieces).withTotalRecords(CollectionUtils.size(pieces));
  }

}
