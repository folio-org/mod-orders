package org.folio.models.orders.lines.update;

import java.util.HashSet;
import java.util.Set;

import lombok.Getter;
import one.util.streamex.StreamEx;
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
    if (storagePatchOrderLineRequest != null) {
      return;
    }
    this.storagePatchOrderLineRequest = new StoragePatchOrderLineRequest()
      .withOperation(patchOrderLineOperationType)
      .withReplaceInstanceRef(new StorageReplaceOrderLineInstanceRef().withNewInstanceId(newInstanceId));
  }

  public void addHoldingRefsToStoragePatchOrderLineRequest(String holdingId, String newHoldingId) {
    this.storagePatchOrderLineRequest.getReplaceInstanceRef().getHoldings()
      .add(new StorageReplaceOrderLineHoldingRefs().withFromHoldingId(holdingId).withToHoldingId(newHoldingId));
  }

  public boolean shouldProcessHolding(String holdingId) {
    // Check if the holding is deleted or found/created that results into storing the holding ref (toHoldingId)
    return !deletedHoldingIds.contains(holdingId) &&
      StreamEx.of(storagePatchOrderLineRequest.getReplaceInstanceRef().getHoldings())
        .map(StorageReplaceOrderLineHoldingRefs::getToHoldingId)
        .noneMatch(holdingId::equals);
  }

}
