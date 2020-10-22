package org.folio.models;

public class PoLineUpdateHolder {
  private String instanceId;
  private String oldLocationId;
  private String newLocationId;
  private String oldHoldingId;
  private String newHoldingId;

  public PoLineUpdateHolder() {

  }

  public PoLineUpdateHolder(String oldLocationId, String newLocation) {
    this.oldLocationId = oldLocationId;
    this.newLocationId = newLocation;
  }

  public PoLineUpdateHolder withOldLocationId(String oldLocationId) {
    this.oldLocationId = oldLocationId;
    return this;
  }

  public PoLineUpdateHolder withNewLocationId(String newLocationId) {
    this.newLocationId = newLocationId;
    return this;
  }

  public PoLineUpdateHolder withOldHoldingId(String oldHoldingId) {
    this.oldHoldingId = oldHoldingId;
    return this;
  }

  public PoLineUpdateHolder withNewHoldingId(String newHoldingId) {
    this.newHoldingId = newHoldingId;
    return this;
  }

  public PoLineUpdateHolder withInstanceId(String instanceId) {
    this.instanceId = instanceId;
    return this;
  }

  public String getOldLocationId() {
    return oldLocationId;
  }

  public String getNewLocationId() {
    return newLocationId;
  }

  public String getOldHoldingId() {
    return oldHoldingId;
  }

  public String getNewHoldingId() {
    return newHoldingId;
  }

  public String getInstanceId() {
    return instanceId;
  }
}
