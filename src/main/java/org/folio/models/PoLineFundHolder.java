package org.folio.models;

import java.util.Objects;

import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.FundDistribution;

public class PoLineFundHolder {
  private final LineFundId id;
  private final PoLine poLine;
  private final FundDistribution fundDistribution;

  public PoLineFundHolder(PoLine poLine, FundDistribution fundDistribution) {
    this.poLine = poLine;
    this.fundDistribution = fundDistribution;
    this.id = new LineFundId(poLine.getId(), fundDistribution.getFundId());
  }

  public LineFundId getId() {
    return id;
  }

  public PoLine getPoLine() {
    return poLine;
  }

  public FundDistribution getFundDistribution() {
    return fundDistribution;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    PoLineFundHolder that = (PoLineFundHolder) o;
    return id.equals(that.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id);
  }
}
