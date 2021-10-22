package org.folio.orders.utils.validators;

import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.getElectronicCostQuantity;
import static org.folio.orders.utils.HelperUtils.getPhysicalCostQuantity;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForEresource;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForPhysical;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.CompositePoLine.OrderFormat.P_E_MIX;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;

public final class CompositePoLineValidationUtil {

  private CompositePoLineValidationUtil() {

  }

  public static List<Error> validatePoLine(CompositePoLine compPOL) {
    List<Error> errors = new ArrayList<>();
    errors.addAll(validatePackagePoLine(compPOL));

    if (getPhysicalCostQuantity(compPOL) == 0 && getElectronicCostQuantity(compPOL) == 0
      && CollectionUtils.isEmpty(compPOL.getLocations())) {
      return errors;
    }

    errors.addAll(validatePoLineFormats(compPOL));
    errors.addAll(validateLocations(compPOL));
    errors.addAll(validateCostPrices(compPOL));

    return errors;
  }

  public static List<Error> validatePoLineFormats(CompositePoLine compPOL) {
    CompositePoLine.OrderFormat orderFormat = compPOL.getOrderFormat();
    if (orderFormat == P_E_MIX) {
      return validatePoLineWithMixedFormat(compPOL);
    } else if (orderFormat == ELECTRONIC_RESOURCE) {
      return validatePoLineWithElectronicFormat(compPOL);
    } else if (orderFormat == CompositePoLine.OrderFormat.PHYSICAL_RESOURCE) {
      return validatePoLineWithPhysicalFormat(compPOL);
    } else if (orderFormat == CompositePoLine.OrderFormat.OTHER) {
      return validatePoLineWithOtherFormat(compPOL);
    }

    return Collections.emptyList();
  }

  private static List<Error> validatePoLineWithMixedFormat(CompositePoLine compPOL) {

    List<ErrorCodes> errors = new ArrayList<>();
    // The quantity of the physical and electronic resources in the cost must be specified
    if (getPhysicalCostQuantity(compPOL) == 0) {
      errors.add(ErrorCodes.ZERO_COST_PHYSICAL_QTY);
    }
    if (getElectronicCostQuantity(compPOL) == 0) {
      errors.add(ErrorCodes.ZERO_COST_ELECTRONIC_QTY);
    }

    return convertErrorCodesToErrors(compPOL, errors);
  }

  public static Optional<Error> checkMaterialAvailability(CompositePoLine compPOL) {
    boolean isMissing = false;
    if (compPOL.getOrderFormat()
      .equals(ELECTRONIC_RESOURCE)
        || compPOL.getOrderFormat()
          .equals(P_E_MIX)) {
      isMissing = compPOL.getEresource()
        .getCreateInventory() == (Eresource.CreateInventory.INSTANCE_HOLDING_ITEM) && isEmpty(
            compPOL.getEresource()
              .getMaterialType());
    }

    if (!compPOL.getOrderFormat()
      .equals(ELECTRONIC_RESOURCE)) {
      isMissing = isMissing || (compPOL.getPhysical()
        .getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM && isEmpty(
            compPOL.getPhysical()
              .getMaterialType()));
    }

    if (isMissing) {
      return Optional.of(convertErrorCodesToError(compPOL, ErrorCodes.MISSING_MATERIAL_TYPE));
    }
    return Optional.empty();
  }

  public static List<Error> checkMaterialsAvailability(List<CompositePoLine> poLines) {
    if (Objects.nonNull(poLines)) {
      return poLines.stream()
        .map(CompositePoLineValidationUtil::checkMaterialAvailability)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(toList());
    }
    return Collections.emptyList();
  }

  public static List<Error> validateLocations(CompositePoLine compPOL) {
    List<ErrorCodes> errors = new ArrayList<>();
    List<Location> locations = compPOL.getLocations();

    // The total quantity of the physical and electronic resources of all locations must match specified in the cost
    if (isLocationsEresourceQuantityNotValid(compPOL)) {
      errors.add(ErrorCodes.ELECTRONIC_COST_LOC_QTY_MISMATCH);
    }
    if (isLocationsPhysicalQuantityNotValid(compPOL)) {
      errors.add(ErrorCodes.PHYSICAL_COST_LOC_QTY_MISMATCH);
    }

    // The total quantity of any location must exceed 0
    if (locations.stream().anyMatch(location -> HelperUtils.calculateTotalLocationQuantity(location) == 0)) {
      errors.add(ErrorCodes.ZERO_LOCATION_QTY);
    }
    locations.forEach( location -> {
      if (location.getHoldingId() == null && location.getLocationId() == null) {
        errors.add(ErrorCodes.HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR);
      } else if (location.getHoldingId() != null && location.getLocationId() != null) {
        errors.add(ErrorCodes.MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR);
      }
    });
    return convertErrorCodesToErrors(compPOL, errors);
  }

  private static boolean isLocationsPhysicalQuantityNotValid(CompositePoLine compPOL) {
    int physicalQuantity = HelperUtils.getPhysicalLocationsQuantity(compPOL.getLocations());
    return (isHoldingUpdateRequiredForPhysical(compPOL) || physicalQuantity > 0) && (physicalQuantity != getPhysicalCostQuantity(compPOL));
  }

  private static boolean isLocationsEresourceQuantityNotValid(CompositePoLine compPOL) {
    int electronicQuantity = HelperUtils.getElectronicLocationsQuantity(compPOL.getLocations());
    return (isHoldingUpdateRequiredForEresource(compPOL) || electronicQuantity > 0) && (electronicQuantity != getElectronicCostQuantity(compPOL));
  }

  private static List<Error> validatePoLineWithPhysicalFormat(CompositePoLine compPOL) {
    List<ErrorCodes> errors = new ArrayList<>();

    // The quantity of the physical resources in the cost must be specified
    if (getPhysicalCostQuantity(compPOL) == 0) {
      errors.add(ErrorCodes.ZERO_COST_PHYSICAL_QTY);
    }
    // The quantity of the electronic resources in the cost must not be specified
    if (getElectronicCostQuantity(compPOL) > 0) {
      errors.add(ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY);
    }

    return convertErrorCodesToErrors(compPOL, errors);
  }

  private static List<Error> validatePoLineWithElectronicFormat(CompositePoLine compPOL) {
    List<ErrorCodes> errors = new ArrayList<>();

    // The quantity of the electronic resources in the cost must be specified
    if (getElectronicCostQuantity(compPOL) == 0) {
      errors.add(ErrorCodes.ZERO_COST_ELECTRONIC_QTY);
    }
    // The quantity of the physical resources in the cost must not be specified
    if (getPhysicalCostQuantity(compPOL) > 0) {
      errors.add(ErrorCodes.NON_ZERO_COST_PHYSICAL_QTY);
    }

    return convertErrorCodesToErrors(compPOL, errors);
  }

  private static List<Error> validatePoLineWithOtherFormat(CompositePoLine compPOL) {
    return validatePoLineWithPhysicalFormat(compPOL);
  }

  private static List<Error> validateCostPrices(CompositePoLine compLine) {
    List<ErrorCodes> errors = new ArrayList<>();
    Cost cost = compLine.getCost();
    CompositePoLine.OrderFormat orderFormat = compLine.getOrderFormat();
    // Using default value as -1 to avoid null checks
    double unitPrice = defaultIfNull(cost.getListUnitPrice(), -1d);
    if (orderFormat == ELECTRONIC_RESOURCE) {
      if (unitPrice > 0d) {
        errors.add(ErrorCodes.COST_UNIT_PRICE_INVALID);
      }
    } else if (unitPrice < 0d) {
      errors.add(ErrorCodes.COST_UNIT_PRICE_INVALID);
    }

    double unitPriceElectronic = defaultIfNull(cost.getListUnitPriceElectronic(), -1d);
    if (orderFormat == ELECTRONIC_RESOURCE || orderFormat == P_E_MIX) {
      if (unitPriceElectronic < 0d) {
        errors.add(ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID);
      }
    } else if (unitPriceElectronic > 0d) {
      errors.add(ErrorCodes.COST_UNIT_PRICE_ELECTRONIC_INVALID);
    }

    double additionalCost = defaultIfNull(cost.getAdditionalCost(), 0d);
    if (additionalCost < 0d) {
      errors.add(ErrorCodes.COST_ADDITIONAL_COST_INVALID);
    }

    if (isDiscountNotValid(cost)) {
      errors.add(ErrorCodes.COST_DISCOUNT_INVALID);
    }

    return convertErrorCodesToErrors(compLine, errors);
  }

  /**
   * Checks if discount is negative or exceed cost totalPrice
   *
   * @param cost for which discount is checked
   * @return true if cost.discount not valid
   */
  private static boolean isDiscountNotValid(Cost cost) {
    double discount = defaultIfNull(cost.getDiscount(), 0d);
    return (discount < 0d || cost.getDiscountType() == Cost.DiscountType.PERCENTAGE && discount > 100d)
      || (discount > 0d && cost.getDiscountType() == Cost.DiscountType.AMOUNT && calculateEstimatedPrice(cost).isNegative());
  }

  /**
   * The method converts {@link ErrorCodes} elements to {@link Error} adding additionally {@link Parameter} with PO Line number is
   * presents
   *
   * @param compPOL Composite PO Line
   * @param errors  list of static {@link ErrorCodes}
   * @return List of {@link Error} elements
   */
  private static List<Error> convertErrorCodesToErrors(CompositePoLine compPOL, List<ErrorCodes> errors) {
    return errors.stream()
      .map(error -> convertErrorCodesToError(compPOL, error))
      .collect(toList());
  }

  /**
   * The method converts {@link ErrorCodes} elements to {@link Error} adding additionally {@link Parameter} with PO Line number is
   * presents
   *
   * @param compPOL   Composite PO Line
   * @param errorCode static {@link ErrorCodes}
   * @return List of {@link Error} elements
   */
  private static Error convertErrorCodesToError(CompositePoLine compPOL, ErrorCodes errorCode) {
    Error error = errorCode.toError();
    String poLineNumber = compPOL.getPoLineNumber();
    if (StringUtils.isNotEmpty(poLineNumber)) {
      error.getParameters()
        .add(new Parameter().withKey(PO_LINE_NUMBER)
          .withValue(poLineNumber));
    }
    return error;
  }

  private static List<Error> validatePackagePoLine(CompositePoLine compPOL) {
     List<ErrorCodes> errors = new ArrayList<>();

     if (Boolean.TRUE.equals(compPOL.getIsPackage()) && compPOL.getInstanceId() != null) {
       errors.add(ErrorCodes.INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE);
     }

     return convertErrorCodesToErrors(compPOL, errors);
  }
}
