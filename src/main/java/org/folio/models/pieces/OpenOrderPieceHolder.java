package org.folio.models.pieces;

import java.util.Collections;
import java.util.List;

import org.folio.rest.jaxrs.model.Piece;

public class OpenOrderPieceHolder {
  private final String titleId;
  private List<Piece> existingPieces = Collections.emptyList();
  private List<Piece> piecesWithLocationToProcess = Collections.emptyList();
  private List<Piece> piecesWithHoldingToProcess = Collections.emptyList();
  private List<Piece> piecesWithChangedLocation = Collections.emptyList();
  private List<Piece> piecesWithoutLocationId = Collections.emptyList();

  public OpenOrderPieceHolder(String titleId) {
    this.titleId = titleId;
  }

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

  public List<Piece> getPiecesWithLocationToProcess() {
    return piecesWithLocationToProcess;
  }

  public List<Piece> getPiecesWithHoldingToProcess() {
    return piecesWithHoldingToProcess;
  }

  public List<Piece> getPiecesWithChangedLocation() {
    return piecesWithChangedLocation;
  }

  public List<Piece> getPiecesWithoutLocationId() {
    return piecesWithoutLocationId;
  }

  public List<Piece> getExistingPieces() {
    return existingPieces;
  }

  public String getTitleId() {
    return titleId;
  }
}
