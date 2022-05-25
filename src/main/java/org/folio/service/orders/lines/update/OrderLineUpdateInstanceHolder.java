package org.folio.service.orders.lines.update;

import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;

public class OrderLineUpdateInstanceHolder {
  private PoLine storagePoLine;
  private PatchOrderLineRequest patchOrderLineRequest;

  public OrderLineUpdateInstanceHolder withStoragePoLine(PoLine storagePoLine) {
    this.storagePoLine = storagePoLine;
    return this;
  }

  public PoLine getStoragePoLine() {
    return storagePoLine;
  }

  public OrderLineUpdateInstanceHolder withPathOrderLineRequest(PatchOrderLineRequest patchOrderLineRequest) {
    this.patchOrderLineRequest = patchOrderLineRequest;
    return this;
  }

  public PatchOrderLineRequest getPatchOrderLineRequest() {
    return patchOrderLineRequest;
  }
}
