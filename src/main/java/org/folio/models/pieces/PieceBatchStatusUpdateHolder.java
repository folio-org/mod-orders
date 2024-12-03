package org.folio.models.pieces;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.folio.rest.jaxrs.model.Piece;

import java.util.List;

@AllArgsConstructor
public class PieceBatchStatusUpdateHolder extends BasePieceFlowHolder {

  @Getter
  private Piece.ReceivingStatus receivingStatus;
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

}
