package org.folio.models.pieces;

import one.util.streamex.StreamEx;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.BindPiecesCollection;

import java.util.List;
import java.util.Map;

public class BindPiecesHolder {

  private BindPiecesCollection bindPiecesCollection;
  private Map<String, List<Piece>> piecesGroupedByPoLine;
  private String bindItemId;

  public BindPiecesCollection getBindPiecesCollection() {
    return bindPiecesCollection;
  }

  public BindPiecesHolder withBindPiecesCollection(BindPiecesCollection bindPiecesCollection) {
    this.bindPiecesCollection = bindPiecesCollection;
    return this;
  }

  public Map<String, List<Piece>> getPiecesGroupedByPoLine() {
    return piecesGroupedByPoLine;
  }

  public BindPiecesHolder withPiecesGroupedByPoLine(Map<String, List<Piece>> piecesGroupedByPoLine) {
    this.piecesGroupedByPoLine = piecesGroupedByPoLine;
    return this;
  }

  public String getBindItemId() {
    return bindItemId;
  }

  public BindPiecesHolder withBindItemId(String bindItemId) {
    this.bindItemId = bindItemId;
    return this;
  }

  public StreamEx<Piece> getPieces() {
    return StreamEx.ofValues(piecesGroupedByPoLine).flatMap(List::stream);
  }

  public String getPoLineId() {
    return bindPiecesCollection.getPoLineId();
  }

}
