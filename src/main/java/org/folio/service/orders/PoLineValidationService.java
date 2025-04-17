package org.folio.service.orders;

import static org.apache.commons.lang3.ObjectUtils.defaultIfNull;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.PoLineCommonUtil.extractUnaffiliatedLocations;
import static org.folio.orders.utils.PoLineCommonUtil.getElectronicCostQuantity;
import static org.folio.orders.utils.PoLineCommonUtil.getPhysicalCostQuantity;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForEresource;
import static org.folio.orders.utils.PoLineCommonUtil.isHoldingUpdateRequiredForPhysical;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.rest.core.exceptions.ErrorCodes.CREATE_INVENTORY_INCORRECT_FOR_BINDARY_ACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.ORDER_FORMAT_INCORRECT_FOR_BINDARY_ACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.RECEIVING_WORKFLOW_INCORRECT_FOR_BINDARY_ACTIVE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.ELECTRONIC_RESOURCE;
import static org.folio.rest.jaxrs.model.PoLine.OrderFormat.P_E_MIX;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import io.vertx.core.Future;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.core.exceptions.ErrorCodes;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Eresource;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.service.consortium.ConsortiumConfigurationService;
import org.folio.service.consortium.ConsortiumUserTenantsRetriever;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;


public class PoLineValidationService extends BaseValidationService {

  private static final Logger logger = LogManager.getLogger(PoLineValidationService.class);

  private final ExpenseClassValidationService expenseClassValidationService;
  private final ConsortiumConfigurationService consortiumConfigurationService;
  private final ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever;

  public PoLineValidationService(ExpenseClassValidationService expenseClassValidationService,
                                          ConsortiumConfigurationService consortiumConfigurationService,
                                          ConsortiumUserTenantsRetriever consortiumUserTenantsRetriever) {
    this.expenseClassValidationService = expenseClassValidationService;
    this.consortiumConfigurationService = consortiumConfigurationService;
    this.consortiumUserTenantsRetriever = consortiumUserTenantsRetriever;
  }

  public Future<List<Error>> validatePoLine(PoLine poLine, RequestContext requestContext) {
    List<Error> errors = new ArrayList<>(validatePackagePoLine(poLine));
    errors.addAll(validateClaimingConfig(poLine));

    if (getPhysicalCostQuantity(poLine) == 0 && getElectronicCostQuantity(poLine) == 0
      && CollectionUtils.isEmpty(poLine.getLocations())) {
      return Future.succeededFuture(errors);
    }

    return expenseClassValidationService.validateExpenseClasses(List.of(poLine), false, requestContext)
      .map(v -> errors.addAll(validatePoLineFormats(poLine)))
      .map(v -> errors.addAll(validateForBinadryActive(poLine)))
      .map(b -> errors.addAll(validateLocations(poLine)))
      .map(b -> {
        errors.addAll(validateCostPrices(poLine));
        return errors;
      });
  }

  private List<Error> validatePoLineFormats(PoLine poLine) {
    PoLine.OrderFormat orderFormat = poLine.getOrderFormat();
    if (orderFormat == P_E_MIX) {
      return validatePoLineWithMixedFormat(poLine);
    } else if (orderFormat == ELECTRONIC_RESOURCE) {
      return validatePoLineWithElectronicFormat(poLine);
    } else if (orderFormat == PoLine.OrderFormat.PHYSICAL_RESOURCE) {
      return validatePoLineWithPhysicalFormat(poLine);
    } else if (orderFormat == PoLine.OrderFormat.OTHER) {
      return validatePoLineWithOtherFormat(poLine);
    }

    return Collections.emptyList();
  }

  private List<Error> validatePoLineWithMixedFormat(PoLine poLine) {

    List<ErrorCodes> errors = new ArrayList<>();
    // The quantity of the physical and electronic resources in the cost must be specified
    if (getPhysicalCostQuantity(poLine) == 0) {
      errors.add(ErrorCodes.ZERO_COST_PHYSICAL_QTY);
    }
    if (getElectronicCostQuantity(poLine) == 0) {
      errors.add(ErrorCodes.ZERO_COST_ELECTRONIC_QTY);
    }

    return convertErrorCodesToErrors(poLine, errors);
  }

  private Optional<Error> checkMaterialAvailability(PoLine poLine) {
    boolean isMissing = false;
    if (poLine.getOrderFormat()
      .equals(ELECTRONIC_RESOURCE)
      || poLine.getOrderFormat()
      .equals(P_E_MIX)) {
      isMissing = poLine.getEresource()
        .getCreateInventory() == (Eresource.CreateInventory.INSTANCE_HOLDING_ITEM) && isEmpty(
        poLine.getEresource()
          .getMaterialType());
    }

    if (!poLine.getOrderFormat()
      .equals(ELECTRONIC_RESOURCE)) {
      isMissing = isMissing || (poLine.getPhysical()
        .getCreateInventory() == Physical.CreateInventory.INSTANCE_HOLDING_ITEM && isEmpty(
        poLine.getPhysical()
          .getMaterialType()));
    }

    if (isMissing) {
      return Optional.of(convertErrorCodesToError(poLine, ErrorCodes.MISSING_MATERIAL_TYPE));
    }
    return Optional.empty();
  }

  public List<Error> checkMaterialsAvailability(List<PoLine> poLines) {
    if (Objects.nonNull(poLines)) {
      return poLines.stream()
        .map(this::checkMaterialAvailability)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .toList();
    }
    return Collections.emptyList();
  }

  public List<Error> validateLocations(PoLine poLine) {
    List<ErrorCodes> errors = new ArrayList<>();
    List<Location> locations = poLine.getLocations();

    // The total quantity of the physical and electronic resources of all locations must match specified in the cost
    if (isLocationsEresourceQuantityNotValid(poLine)) {
      errors.add(ErrorCodes.ELECTRONIC_COST_LOC_QTY_MISMATCH);
    }
    if (isLocationsPhysicalQuantityNotValid(poLine)) {
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
    return convertErrorCodesToErrors(poLine, errors);
  }

  private boolean isLocationsPhysicalQuantityNotValid(PoLine poLine) {
    int physicalQuantity = HelperUtils.getPhysicalLocationsQuantity(poLine.getLocations());
    return (isHoldingUpdateRequiredForPhysical(poLine) || physicalQuantity > 0) && (physicalQuantity != getPhysicalCostQuantity(poLine));
  }

  private boolean isLocationsEresourceQuantityNotValid(PoLine poLine) {
    int electronicQuantity = HelperUtils.getElectronicLocationsQuantity(poLine.getLocations());
    return (isHoldingUpdateRequiredForEresource(poLine) || electronicQuantity > 0) && (electronicQuantity != getElectronicCostQuantity(poLine));
  }

  private List<Error> validatePoLineWithPhysicalFormat(PoLine poLine) {
    List<ErrorCodes> errors = new ArrayList<>();

    // The quantity of the physical resources in the cost must be specified
    if (getPhysicalCostQuantity(poLine) == 0) {
      errors.add(ErrorCodes.ZERO_COST_PHYSICAL_QTY);
    }
    // The quantity of the electronic resources in the cost must not be specified
    if (getElectronicCostQuantity(poLine) > 0) {
      errors.add(ErrorCodes.NON_ZERO_COST_ELECTRONIC_QTY);
    }

    return convertErrorCodesToErrors(poLine, errors);
  }

  private List<Error> validatePoLineWithElectronicFormat(PoLine poLine) {
    List<ErrorCodes> errors = new ArrayList<>();

    // The quantity of the electronic resources in the cost must be specified
    if (getElectronicCostQuantity(poLine) == 0) {
      errors.add(ErrorCodes.ZERO_COST_ELECTRONIC_QTY);
    }
    // The quantity of the physical resources in the cost must not be specified
    if (getPhysicalCostQuantity(poLine) > 0) {
      errors.add(ErrorCodes.NON_ZERO_COST_PHYSICAL_QTY);
    }

    return convertErrorCodesToErrors(poLine, errors);
  }

  private List<Error> validatePoLineWithOtherFormat(PoLine poLine) {
    return validatePoLineWithPhysicalFormat(poLine);
  }

  private List<Error> validateCostPrices(PoLine poLine) {
    List<ErrorCodes> errors = new ArrayList<>();
    Cost cost = poLine.getCost();
    PoLine.OrderFormat orderFormat = poLine.getOrderFormat();
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

    return convertErrorCodesToErrors(poLine, errors);
  }

  protected List<Error> validateClaimingConfig(PoLine poLine) {
    List<ErrorCodes> errors = checkClaimingConfig(poLine.getClaimingActive(), poLine.getClaimingInterval());
    return convertErrorCodesToErrors(poLine, errors);
  }

  /**
   * Checks if discount is negative or exceed cost totalPrice
   *
   * @param cost for which discount is checked
   * @return true if cost.discount not valid
   */
  private boolean isDiscountNotValid(Cost cost) {
    double discount = defaultIfNull(cost.getDiscount(), 0d);
    return (discount < 0d || cost.getDiscountType() == Cost.DiscountType.PERCENTAGE && discount > 100d)
      || (discount > 0d && cost.getDiscountType() == Cost.DiscountType.AMOUNT && calculateEstimatedPrice(cost).isNegative());
  }

  /**
   * The method converts {@link ErrorCodes} elements to {@link Error} adding additionally {@link Parameter} with PO Line number is
   * presents
   *
   * @param poLine PO Line
   * @param errors  list of errors{@link ErrorCodes}
   * @return List of {@link Error} elements
   */
  private List<Error> convertErrorCodesToErrors(PoLine poLine, List<ErrorCodes> errors) {
    return errors.stream()
      .map(error -> convertErrorCodesToError(poLine, error))
      .toList();
  }

  /**
   * The method converts {@link ErrorCodes} elements to {@link Error} adding additionally {@link Parameter} with PO Line number is
   * presents
   *
   * @param poLine   PO Line
   * @param errorCode Error Code{@link ErrorCodes}
   * @return List of {@link Error} elements
   */
  private Error convertErrorCodesToError(PoLine poLine, ErrorCodes errorCode) {
    Error error = errorCode.toError();
    String poLineNumber = poLine.getPoLineNumber();
    if (StringUtils.isNotEmpty(poLineNumber)) {
      error.getParameters()
        .add(new Parameter().withKey(PO_LINE_NUMBER)
          .withValue(poLineNumber));
    }
    return error;
  }

  private List<Error> validatePackagePoLine(PoLine poLine) {
    List<ErrorCodes> errors = new ArrayList<>();

    if (Boolean.TRUE.equals(poLine.getIsPackage()) && poLine.getInstanceId() != null) {
      errors.add(ErrorCodes.INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE);
    }

    return convertErrorCodesToErrors(poLine, errors);
  }

  protected List<Error> validateForBinadryActive(PoLine poLine) {
    List<Error> errors = new ArrayList<>();
    if (poLine.getDetails() != null && Boolean.TRUE.equals(poLine.getDetails().getIsBinderyActive())) {
      validateOrderFormatForBindaryActive(poLine, errors);
      validateCreateInventoryForBindary(poLine, errors);
      validateReceivingWorkflowForBindary(poLine, errors);
    }
    return errors;
  }

  private void validateOrderFormatForBindaryActive(PoLine poLine, List<Error> errors) {
    var poLineOrderFormat = poLine.getOrderFormat().value();
    if (!Objects.equals(poLineOrderFormat, PoLine.OrderFormat.PHYSICAL_RESOURCE.value())
        && !Objects.equals(poLineOrderFormat, PoLine.OrderFormat.P_E_MIX.value())) {
      var param = new Parameter().withKey("orderFormat").withValue(poLine.getOrderFormat().value());
      var error = ORDER_FORMAT_INCORRECT_FOR_BINDARY_ACTIVE.toError().withParameters(List.of(param));
      errors.add(error);
    }
  }

  private void validateCreateInventoryForBindary(PoLine poLine, List<Error> errors) {
    if (poLine.getPhysical() != null && poLine.getPhysical().getCreateInventory() != Physical.CreateInventory.INSTANCE_HOLDING_ITEM) {
      var param = new Parameter().withKey("createInventory").withValue(poLine.getPhysical().getCreateInventory().value());
      var error = CREATE_INVENTORY_INCORRECT_FOR_BINDARY_ACTIVE.toError().withParameters(List.of(param));
      errors.add(error);
    }
  }

  private void validateReceivingWorkflowForBindary(PoLine poLine, List<Error> errors) {
    if (Boolean.FALSE.equals(poLine.getCheckinItems())) {
      errors.add(RECEIVING_WORKFLOW_INCORRECT_FOR_BINDARY_ACTIVE.toError());
    }
  }

  public Future<Void> validatePurchaseOrderHasPoLines(List<PoLine> poLines) {
    if (CollectionUtils.isEmpty(poLines)) {
      return Future.failedFuture(new HttpException(org.apache.http.HttpStatus.SC_UNPROCESSABLE_ENTITY, ErrorCodes.COMPOSITE_ORDER_MISSING_PO_LINES));
    }
    return Future.succeededFuture();
  }

  public Future<Void> validateUserUnaffiliatedLocations(String poLineId, List<Location> locations, RequestContext requestContext) {
    return getUserTenantsIfNeeded(requestContext)
      .compose(userTenants -> {
        if (CollectionUtils.isEmpty(userTenants)) {
          logger.info("validateUserUnaffiliatedLocationUpdates:: User tenants is empty");
          return Future.succeededFuture();
        }
        var uniqueUnAffiliatedLocations = extractUnaffiliatedLocations(locations, userTenants).stream().map(Location::getTenantId).distinct().toList();
        if (CollectionUtils.isNotEmpty(uniqueUnAffiliatedLocations)) {
          logger.info("validateUserUnaffiliatedLocationUpdates:: User has unaffiliated locations on the POL, poLineId: {}, unique locations: {}", poLineId, uniqueUnAffiliatedLocations);
          return Future.failedFuture(new HttpException(HttpStatus.SC_UNPROCESSABLE_ENTITY, ErrorCodes.LOCATION_UPDATE_WITHOUT_AFFILIATION));
        }
        logger.info("validateUserUnaffiliatedLocationUpdates:: User is affiliated with all {} locations on the POL, poLineId: {}", locations.size(), poLineId);
        return Future.succeededFuture();
      });
  }

  private Future<List<String>> getUserTenantsIfNeeded(RequestContext requestContext) {
    return consortiumConfigurationService.getConsortiumConfiguration(requestContext)
      .compose(consortiumConfiguration ->
        consortiumConfiguration
          .map(configuration -> consortiumConfigurationService.isCentralOrderingEnabled(requestContext)
              .compose(isCentralOrderingEnabled -> isCentralOrderingEnabled
                ? consortiumUserTenantsRetriever.getUserTenants(configuration.consortiumId(), configuration.centralTenantId(), requestContext)
                : Future.succeededFuture()))
          .orElse(Future.succeededFuture())
      );
  }
}
