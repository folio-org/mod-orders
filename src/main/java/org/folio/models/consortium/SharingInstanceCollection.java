package org.folio.models.consortium;

import java.util.List;

public class SharingInstanceCollection {
  private List<SharingInstance> sharingInstances;
  private Integer totalRecords;

  public List<SharingInstance> getSharingInstances() {
    return sharingInstances;
  }

  public void setSharingInstances(List<SharingInstance> sharingInstances) {
    this.sharingInstances = sharingInstances;
  }

  public Integer getTotalRecords() {
    return totalRecords;
  }

  public void setTotalRecords(Integer totalRecords) {
    this.totalRecords = totalRecords;
  }
}
