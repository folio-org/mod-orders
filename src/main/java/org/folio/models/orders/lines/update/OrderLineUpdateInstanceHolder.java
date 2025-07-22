package org.folio.models.orders.lines.update;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import lombok.Getter;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest;
import org.folio.rest.acq.model.StoragePatchOrderLineRequest.PatchOrderLineOperationType;
import org.folio.rest.acq.model.StorageReplaceOrderLineHoldingRefs;
import org.folio.rest.acq.model.StorageReplaceOrderLineInstanceRef;
import org.folio.rest.jaxrs.model.PatchOrderLineRequest;
import org.folio.rest.jaxrs.model.PoLine;

@Getter
public class OrderLineUpdateInstanceHolder {

  private PoLine storagePoLine;
  private PatchOrderLineRequest patchOrderLineRequest;
  private StoragePatchOrderLineRequest storagePatchOrderLineRequest;
  private final Set<String> deletedHoldingIds = new HashSet<>();

  public OrderLineUpdateInstanceHolder withStoragePoLine(PoLine storagePoLine) {
    this.storagePoLine = storagePoLine;
    return this;
  }

  public OrderLineUpdateInstanceHolder withPathOrderLineRequest(PatchOrderLineRequest patchOrderLineRequest) {
    this.patchOrderLineRequest = patchOrderLineRequest;
    return this;
  }

  public void createStoragePatchOrderLineRequest(PatchOrderLineOperationType patchOrderLineOperationType, String newInstanceId) {
    this.storagePatchOrderLineRequest = new StoragePatchOrderLineRequest()
      .withOperation(patchOrderLineOperationType)
      .withReplaceInstanceRef(new StorageReplaceOrderLineInstanceRef()
        .withNewInstanceId(newInstanceId));
  }

  public void addHoldingRefsToStoragePatchOrderLineRequest(String holdingId, String newHoldingId) {
    var instanceRef = this.storagePatchOrderLineRequest.getReplaceInstanceRef();
    var holdings = Optional.ofNullable(instanceRef.getHoldings())
      .orElseGet(() -> instanceRef.withHoldings(new ArrayList<>()).getHoldings());
    holdings.add(new StorageReplaceOrderLineHoldingRefs().withFromHoldingId(holdingId).withToHoldingId(newHoldingId));
  }
}
