package org.folio.models.pieces;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import org.folio.rest.jaxrs.model.Piece;

import java.util.List;

@AllArgsConstructor
@Getter
@Builder
public class PieceBatchStatusUpdateHolder extends BasePieceFlowHolder {

  private Piece.ReceivingStatus receivingStatus;
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
