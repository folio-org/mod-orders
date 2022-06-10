package org.folio.models.orders.lines.update;

import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.acq.model.StorageReplaceOrderLineHoldingRefs;
import org.folio.rest.acq.model.StorageReplaceOrderLineInstanceRef;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;

import java.util.ArrayList;

public class OrderLineUpdateInstanceHolder {
  private PoLine storagePoLine;
  private PatchOrderLineRequest patchOrderLineRequest;
  private StoragePatchOrderLineRequest storagePatchOrderLineRequest;

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

  public StoragePatchOrderLineRequest getStoragePatchOrderLineRequest() {
    return storagePatchOrderLineRequest;
  }

  public OrderLineUpdateInstanceHolder withStoragePathOrderLineRequest(StoragePatchOrderLineRequest storagePathOrderLineRequest) {
    this.storagePatchOrderLineRequest = storagePathOrderLineRequest;
    return this;
  }

  public OrderLineUpdateInstanceHolder createStoragePatchOrderLineRequest(StoragePatchOrderLineRequest.PatchOrderLineOperationType patchOrderLineOperationType,
      String newInstanceId) {
    this.storagePatchOrderLineRequest = new StoragePatchOrderLineRequest()
        .withOperation(patchOrderLineOperationType)
        .withReplaceInstanceRef(new StorageReplaceOrderLineInstanceRef()
            .withNewInstanceId(newInstanceId));
    return this;
  }

  public void addHoldingRefsToStoragePatchOrderLineRequest(String holdingId, String newHoldingId) {
    if (this.storagePatchOrderLineRequest.getReplaceInstanceRef().getHoldings() == null) {
      this.storagePatchOrderLineRequest.getReplaceInstanceRef().withHoldings(new ArrayList<>());
    }
    this.storagePatchOrderLineRequest
        .getReplaceInstanceRef()
        .getHoldings()
        .add(new StorageReplaceOrderLineHoldingRefs()
            .withFromHoldingId(holdingId)
            .withToHoldingId(newHoldingId));
  }
}
