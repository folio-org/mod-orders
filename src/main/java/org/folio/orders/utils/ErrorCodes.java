package org.folio.orders.utils;

import org.folio.rest.jaxrs.model.Error;

public enum ErrorCodes {

  GENERIC_ERROR_CODE("genericError", "Generic error"),
  PO_NUMBER_ALREADY_EXISTS("poNumberNotUnique", "PO Number already exists"),
  PO_NUMBER_REQUIRED("poNumberRequired", "PO Number is missing"),
  MISSING_ORDER_ID_IN_POL("orderIdRequired", "Purchase order id is missing in PoLine object"),
  ORDER_NOT_FOUND("orderNotFound", "The order cannot be found by provided order id"),
  MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY("idMismatch", "Mismatch between id in path and request body"),
  INCORRECT_ORDER_ID_IN_POL("orderIdMismatch", "Mismatch between order id in the request and storage"),
  POL_LINES_LIMIT_EXCEEDED("polLimitExceeded", "Your FOLIO system is configured to limit the number of PO Lines on each order"),
  MISSING_MATERIAL_TYPE("materialTypeRequired", "The Material Type is required but not available in PO line"),
  PIECE_POL_MISMATCH("piecePolMismatch", "The piece does not correspond to PO line"),
  PIECE_ALREADY_RECEIVED("pieceAlreadyReceived", "The piece record is already received"),
  PIECE_NOT_FOUND("pieceNotFound", "The piece record is not found"),
  PIECE_NOT_RETRIEVED("pieceNotRetrieved", "The piece record is not retrieved"),
  PIECE_UPDATE_FAILED("pieceUpdateFailed", "The piece record failed to be updated"),
  ITEM_CREATION_FAILED("itemCreationFailed", "The item record failed to be created"),
  ITEM_UPDATE_FAILED("itemUpdateFailed", "The item record failed to be updated"),
  ITEM_NOT_RETRIEVED("itemNotRetrieved", "The item record is not retrieved"),
  ZERO_COST_PHYSICAL_QTY("zeroCostQtyPhysical", "Physical cost quantity must be specified"),
  ZERO_COST_ELECTRONIC_QTY("zeroCostQtyElectronic", "Electronic cost quantity must be specified"),
  ZERO_LOCATION_QTY("zeroLocQty", "Physical or electronic location quantity must be specified"),
  NON_ZERO_COST_PHYSICAL_QTY("nonZeroCostQtyPhysical", "Physical cost quantity must not be specified"),
  NON_ZERO_COST_ELECTRONIC_QTY("nonZeroCostQtyElectronic", "Electronic cost quantity must not be specified"),
  PHYSICAL_COST_LOC_QTY_MISMATCH("physicalLocCostQtyMismatch", "PO Line physical quantity and Locations physical quantity do not match"),
  ELECTRONIC_COST_LOC_QTY_MISMATCH("electronicLocCostQtyMismatch", "PO Line electronic quantity and Locations electronic quantity do not match"),
  ORDER_VENDOR_IS_INACTIVE("vendorIsInactive", "Order cannot be open as the associated vendor is inactive"),
  POL_ACCESS_PROVIDER_IS_INACTIVE("accessProviderIsInactive", "Order cannot be open as the associated access provider is inactive"),
  ORDER_VENDOR_NOT_FOUND("vendorNotFound", "Order cannot be open as the associated vendor not found"),
  VENDOR_ISSUE("vendorIssue", "Order cannot be updated due to issues related to associated vendor/access provider"),
  ORDER_OPEN("orderOpen", "Order cannot be modified in Open status"),
  ORDER_CLOSED("orderClosed", "Order cannot be modified in Closed status"),
  POL_ACCESS_PROVIDER_NOT_FOUND("accessProviderNotFound", "Order cannot be open as the associated access provider not found"),
  COST_UNIT_PRICE_INVALID("costUnitPriceInvalid", "Cost's list unit price is invalid"),
  COST_UNIT_PRICE_ELECTRONIC_INVALID("costUnitPriceElectronicInvalid", "Cost's list unit price for Electronic resource(s) is invalid"),
  COST_ADDITIONAL_COST_INVALID("costAdditionalCostInvalid", "Cost's additional cost must be positive number"),
  COST_DISCOUNT_INVALID("costDiscountInvalid", "Cost's discount is invalid"),
  LOC_NOT_PROVIDED("locNotProvided", "Location not provided"),
  ORGANIZATION_NOT_A_VENDOR("organizationNotAVendor", "Order cannot be opened as the associated vendorId belongs to non-vendor organization"),
  MISSING_INSTANCE_TYPE("missingInstanceType", "Instance-type is a required field for creating an Instance in Inventory"),
  MISSING_INSTANCE_STATUS("missingInstanceStatus", "Instance-status is a required field for creating an Instance in Inventory"),
  MISSING_CONTRIBUTOR_NAME_TYPE("missingContributorNameType", "The specified contributor-name-types were not found in inventory-storage"),
  MISSING_LOAN_TYPE("missingLoanType", "Loan-type is a required field for creating an Item in Inventory"),
  ENCUMBRANCE_CREATION_FAILURE("encumbranceCreationFailure", "One or more encumbrance record(s) failed to be created"),
  PROHIBITED_FIELD_CHANGING("protectedFieldChanging","Protected fields can't be modified"),
  ORDER_UNITS_NOT_FOUND("orderAcqUnitsNotFound", "Acquisitions units assigned to order cannot be found"),
  USER_HAS_NO_PERMISSIONS("userHasNoPermission", "User does not have permissions - operation is restricted"),
  USER_HAS_NO_ACQ_PERMISSIONS("userHasNoAcqUnitsPermission", "User does not have permissions to manage acquisition units assignments - operation is restricted"),
  USER_HAS_NO_APPROVAL_PERMISSIONS("userHasNoApprovalPermission", "User does not have permissions to approve order - operation is restricted"),
  USER_HAS_NO_UNOPEN_PERMISSIONS("userHasNoOrderUnopenPermission", "User does not have permissions to move order from open to pending - operation is restricted"),
  APPROVAL_REQUIRED_TO_OPEN("orderApprovalRequired","Approval is required to open order"),
  ISBN_NOT_VALID("invalidISBN", "ISBN value is invalid"),
  FUNDS_NOT_FOUND("fundsNotFound", "The fund records are not found"),
  CURRENT_FISCAL_YEAR_NOT_FOUND("currentFYearNotFound", "Current fiscal year not found for ledger"),
  TITLE_NOT_FOUND("titleNotFound", "Associated title not found for non package PO Line"),
  TITLE_EXIST("titleExist", "The title for poLine already exist"),
  MULTIPLE_NONPACKAGE_TITLES("multipleNonPackageTitles", "Non package PO Line must contain only one title."),
  BUDGET_NOT_FOUND_FOR_TRANSACTION("budgetNotFoundForTransaction", "Budget not found for transaction"),
  LEDGER_NOT_FOUND_FOR_TRANSACTION("ledgerNotFoundForTransaction", "Ledger not found for transaction"),
  BUDGET_IS_INACTIVE("budgetIsInactive", "Cannot create encumbrance from the not active budget"),
  FUND_CANNOT_BE_PAID("fundCannotBePaid", "Fund cannot be paid due to restrictions"),
  MISSING_ONGOING("missingOngoing", "Ongoing field must be present for Ongoing order"),
  ONGOING_NOT_ALLOWED("ongoingNotAllowed", "Ongoing field must be absent for One-time order"),
  INCORRECT_FUND_DISTRIBUTION_TOTAL("incorrectFundDistributionTotal","Fund distribution total must add to 100% or totalPrice"),
  INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE("InstanceIdNotAllowedForPackagePoLine", "Instance id not allowed for package poline"),
  PREFIX_IS_USED("prefixIsUsed", "The prefix cannot be deleted as it is used by one or more orders"),
  SUFFIX_IS_USED("suffixIsUsed", "The suffix cannot be deleted as it is used by one or more orders"),
  PIECES_TO_BE_DELETED("piecesNeedToBeDeleted", "Pieces need to be deleted"),
  PIECES_TO_BE_CREATED("piecesNeedToBeCreated", "Pieces need to be created"),
  REQUEST_FOUND("thereAreRequestsOnItem", "There are requests on item"),
  BUDGET_EXPENSE_CLASS_NOT_FOUND("budgetExpenseClassNotFound", "Budget expense class not found"),
  INACTIVE_EXPENSE_CLASS("inactiveExpenseClass", "Expense class is Inactive"),
  HOLDINGS_BY_INSTANCE_AND_LOCATION_NOT_FOUND("holdingsByInstanceAndLocationNotFoundError", "Holdings by instance id %s and location id %s not found");

  private final String code;
  private final String description;

  ErrorCodes(String code, String description) {
    this.code = code;
    this.description = description;
  }

  public String getDescription() {
    return description;
  }

  public String getCode() {
    return code;
  }

  @Override
  public String toString() {
    return code + ": " + description;
  }

  public Error toError() {
    return new Error().withCode(code).withMessage(description);
  }
}
