package org.folio.models;

import java.util.Objects;

public class LineFundId {
  private String poLineId;
  private String fundDistributionId;

  public LineFundId() {

  }

  public LineFundId(String poLineId, String fundDistributionId) {
    this.poLineId = poLineId;
    this.fundDistributionId = fundDistributionId;
  }

  public LineFundId withPoLineId(String id) {
    this.poLineId = id;
    return this;
  }

  public LineFundId withFundDistributionId(String id) {
    this.fundDistributionId = id;
    return this;
  }

  public String getPoLineId() {
    return poLineId;
  }

  public String getFundDistributionId() {
    return fundDistributionId;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    LineFundId that = (LineFundId) o;
    return Objects.equals(poLineId, that.poLineId) &&
      Objects.equals(fundDistributionId, that.fundDistributionId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(poLineId, fundDistributionId);
  }

  @Override
  public String toString() {
    return poLineId + "_" + fundDistributionId;
  }
}
