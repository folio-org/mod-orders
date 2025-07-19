package org.folio.models.orders.lines.update;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.acq.model.StorageReplaceOrderLineHoldingRefs;
import org.folio.rest.acq.model.StorageReplaceOrderLineInstanceRef;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;

public class OrderLineUpdateInstanceHolder {

  private PoLine storagePoLine;
  private PatchOrderLineRequest patchOrderLineRequest;
  private StoragePatchOrderLineRequest storagePatchOrderLineRequest;
  private final List<String> deletedHoldingIds = new ArrayList<>();

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

  public void addDeletedHoldingId(String holdingId) {
    this.deletedHoldingIds.add(holdingId);
  }

  public PatchOrderLineRequest getPatchOrderLineRequest() {
    return patchOrderLineRequest;
  }

  public StoragePatchOrderLineRequest getStoragePatchOrderLineRequest() {
    return storagePatchOrderLineRequest;
  }

  public List<String> getDeletedHoldingIds() {
    return deletedHoldingIds;
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
