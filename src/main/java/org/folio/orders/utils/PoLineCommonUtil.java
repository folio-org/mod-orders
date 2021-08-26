package org.folio.orders.utils;

import org.apache.commons.lang3.ObjectUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import java.util.List;
import java.util.Optional;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.OTHER;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.PHYSICAL_RESOURCE;

public final class PoLineCommonUtil {
  public static final String DASH_SEPARATOR = "-";

  private PoLineCommonUtil() {

  }

  public static void sortPoLinesByPoLineNumber(List<CompositePoLine> poLines) {
    poLines.sort(PoLineCommonUtil::comparePoLinesByPoLineNumber);
  }

  public static boolean isReceiptNotRequired(CompositePoLine.ReceiptStatus receiptStatus) {
    return receiptStatus == CompositePoLine.ReceiptStatus.RECEIPT_NOT_REQUIRED;
  }

  public static boolean inventoryUpdateNotRequired(CompositePoLine compPOL) {
    // in case of "Other" order format check Physical createInventory value only
    if (compPOL.getOrderFormat() == OTHER || compPOL.getOrderFormat() == PHYSICAL_RESOURCE) {
      return isUpdateNotRequiredForPhysical(compPOL);
    } else if (compPOL.getOrderFormat() == ELECTRONIC_RESOURCE) {
      return isUpdateNotRequiredForEresource(compPOL);
    } else {
      return isUpdateNotRequiredForPhysical(compPOL) && isUpdateNotRequiredForEresource(compPOL);
    }
  }

  public static boolean isUpdateNotRequiredForEresource(CompositePoLine compPOL) {
    return compPOL.getEresource() == null || compPOL.getEresource().getCreateInventory() == Eresource.CreateInventory.NONE;
  }

  public static boolean isUpdateNotRequiredForPhysical(CompositePoLine compPOL) {
    return compPOL.getPhysical() == null || compPOL.getPhysical().getCreateInventory() == Physical.CreateInventory.NONE;
  }

  public static boolean isHoldingsUpdateRequired(Eresource eresource, Physical physical) {
    return isHoldingUpdateRequiredForEresource(eresource) || isHoldingUpdateRequiredForPhysical(physical);
  }

  public static boolean isHoldingUpdateRequiredForPhysical(Physical physical) {
    return physical != null && (physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING
      || physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isHoldingUpdateRequiredForEresource(Eresource eresource) {
    return eresource != null && (eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING
      || eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM);
  }

  public static boolean isItemsUpdateRequired(CompositePoLine compPOL) {
    return isItemsUpdateRequiredForPhysical(compPOL) || isItemsUpdateRequiredForEresource(compPOL);
  }

  public static boolean isItemsUpdateRequiredForEresource(CompositePoLine compPOL) {
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(compPOL.getEresource())
      .map(eresource -> eresource.getCreateInventory() == Eresource.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static boolean isItemsUpdateRequiredForPhysical(CompositePoLine compPOL) {
    if (compPOL.getCheckinItems() != null && compPOL.getCheckinItems()) {
      return false;
    }
    return Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM)
      .orElse(false);
  }

  public static boolean isOnlyInstanceUpdateRequired(CompositePoLine compPOL) {
    boolean isPhysicalInstance = Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE)
      .orElse(false);

    boolean isElectronicInstance = Optional.ofNullable(compPOL.getPhysical())
      .map(physical -> physical.getCreateInventory() == Physical.CreateInventory.INSTANCE)
      .orElse(false);

    return isPhysicalInstance || isElectronicInstance;
  }

  public static boolean isHoldingCreationRequiredForLocation(CompositePoLine compPOL, Location location) {
    return (isHoldingUpdateRequiredForPhysical(compPOL.getPhysical()) && ObjectUtils.defaultIfNull(location.getQuantityPhysical(), 0) > 0)
      || (isHoldingUpdateRequiredForEresource(compPOL.getEresource()) && ObjectUtils.defaultIfNull(location.getQuantityElectronic(), 0) > 0);
  }

  private static int comparePoLinesByPoLineNumber(CompositePoLine poLine1, CompositePoLine poLine2) {
    String n1 = poLine1.getPoLineNumber();
    String n2 = poLine2.getPoLineNumber();
    if (n1 == null || n2 == null)
      return 0;
    String poLineNumberSuffix1 = n1.split(DASH_SEPARATOR)[1];
    String poLineNumberSuffix2 = n2.split(DASH_SEPARATOR)[1];
    return Integer.parseInt(poLineNumberSuffix1) - Integer.parseInt(poLineNumberSuffix2);
  }
}
