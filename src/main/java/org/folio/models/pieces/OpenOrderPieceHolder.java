package org.folio.models.pieces;

import java.util.Collections;
import java.util.List;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;

@RequiredArgsConstructor
@Getter
public class OpenOrderPieceHolder {

  private final Title title;
  private List<Piece> existingPieces = Collections.emptyList();
  private List<Piece> piecesWithLocationToProcess = Collections.emptyList();
  private List<Piece> piecesWithHoldingToProcess = Collections.emptyList();
  private List<Piece> piecesWithChangedLocation = Collections.emptyList();
  private List<Piece> piecesWithoutLocationId = Collections.emptyList();

  public OpenOrderPieceHolder withExistingPieces(List<Piece> existingPieces) {
    this.existingPieces = existingPieces;
    return this;
  }

  public OpenOrderPieceHolder withPiecesWithLocationToProcess(List<Piece> piecesWithLocationToProcess) {
    this.piecesWithLocationToProcess = piecesWithLocationToProcess;
    return this;
  }

  public OpenOrderPieceHolder withPiecesWithHoldingToProcess(List<Piece> piecesWithHoldingToProcess) {
    this.piecesWithHoldingToProcess = piecesWithHoldingToProcess;
    return this;
  }

  public OpenOrderPieceHolder withPiecesWithChangedLocation(List<Piece> piecesWithChangedLocation) {
    this.piecesWithChangedLocation = piecesWithChangedLocation;
    return this;
  }

  public OpenOrderPieceHolder withPiecesWithoutLocationId(List<Piece> piecesWithoutLocationId) {
    this.piecesWithoutLocationId = piecesWithoutLocationId;
    return this;
  }

}
