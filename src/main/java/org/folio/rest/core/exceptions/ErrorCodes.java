package org.folio.rest.core.exceptions;

import org.folio.rest.jaxrs.model.Error;

public enum ErrorCodes {
  GENERIC_ERROR_CODE("genericError", "Generic error"),
  PO_NUMBER_ALREADY_EXISTS("poNumberNotUnique", "PO Number already exists"),
  PO_NUMBER_REQUIRED("poNumberRequired", "PO Number is missing"),
  PO_NUMBER_PREFIX_REQUIRED("poNumberPrefixRequired", "PO Number must start with Prefix"),
  PO_NUMBER_SUFFIX_REQUIRED("poNumberSuffixRequired", "PO Number must ends with Suffix"),
  MISSING_ORDER_ID_IN_POL("orderIdRequired", "Purchase order id is missing in PoLine object"),
  ORDER_NOT_FOUND("orderNotFound", "The order cannot be found by provided order id"),
  MISMATCH_BETWEEN_ID_IN_PATH_AND_BODY("idMismatch", "Mismatch between id in path and request body"),
  INCORRECT_ORDER_ID_IN_POL("orderIdMismatch", "Mismatch between order id in the request and storage"),
  POL_LINES_LIMIT_EXCEEDED("polLimitExceeded", "Your FOLIO system is configured to limit the number of PO Lines on each order"),
  PO_LINE_ROLLOVER_FAILED("poLineRolloverFailed", "PO line update failed while making rollover"),
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
  MISSING_HOLDINGS_SOURCE_ID("missingHoldingsSourceId", "Holdings source id were not found in inventory-storage"),
  ENCUMBRANCE_CREATION_FAILURE("encumbranceCreationFailure", "One or more encumbrance record(s) failed to be created"),
  PROHIBITED_FIELD_CHANGING("protectedFieldChanging","Protected fields can't be modified"),
  ORDER_UNITS_NOT_FOUND("orderAcqUnitsNotFound", "Acquisitions units assigned to order cannot be found"),
  USER_HAS_NO_PERMISSIONS("userHasNoPermission", "User does not have permissions - operation is restricted"),
  USER_NOT_A_MEMBER_OF_THE_ACQ("userNotAMemberOfTheAcq", "User is not a member of the specified acquisitions group - operation is restricted"),
  USER_HAS_NO_ACQ_PERMISSIONS("userHasNoAcqUnitsPermission", "User does not have permissions to manage acquisition units assignments - operation is restricted"),
  USER_HAS_NO_APPROVAL_PERMISSIONS("userHasNoApprovalPermission", "User does not have permissions to approve order - operation is restricted"),
  USER_HAS_NO_UNOPEN_PERMISSIONS("userHasNoOrderUnopenPermission", "User does not have permissions to move order from open to pending - operation is restricted"),
  USER_HAS_NO_REOPEN_PERMISSIONS("userHasNoOrderReopenPermission", "User does not have permissions to move order from closed to open - operation is restricted"),
  USER_HAS_MISSED_AFFILIATIONS("userHasMissedAffiliations", "User does not have all required affiliations to process - operation is restricted"),
  APPROVAL_REQUIRED_TO_OPEN("orderApprovalRequired","Approval is required to open order"),
  ISBN_NOT_VALID("invalidISBN", "ISBN value is invalid"),
  FUNDS_NOT_FOUND("fundsNotFound", "The fund records are not found"),
  CURRENT_FISCAL_YEAR_NOT_FOUND("currentFYearNotFound", "Current fiscal year not found for ledger"),
  CURRENT_FISCAL_YEAR_ID_NOT_FOUND("currentFYearIdNotFound", "Current fiscal year id not found for POLine"),
  TITLE_NOT_FOUND("titleNotFound", "Associated title not found for PO Line"),
  MULTIPLE_NONPACKAGE_TITLES("multipleNonPackageTitles", "Non package PO Line must contain only one title."),
  BUDGET_NOT_FOUND_FOR_TRANSACTION("budgetNotFoundForTransaction", "Budget not found for transaction"),
  LEDGER_NOT_FOUND_FOR_TRANSACTION("ledgerNotFoundForTransaction", "Ledger not found for transaction"),
  BUDGET_IS_INACTIVE("budgetIsInactive", "Cannot create encumbrance from the not active budget"),
  FUND_CANNOT_BE_PAID("fundCannotBePaid", "Fund cannot be paid due to restrictions"),
  MISSING_ONGOING("missingOngoing", "Ongoing field must be present for Ongoing order"),
  WRONG_ONGOING_SUBSCRIPTION_FIELDS_CHANGED("wrongOngoingSubscriptionFieldsChanged", "Fields [reviewDate, manualRenewal] could not be changed when ongoing subscription = true"),
  WRONG_ONGOING_NOT_SUBSCRIPTION_FIELDS_CHANGED("wrongOngoingNotSubscriptionFieldsChanged", "Fields [interval, renewalDate, reviewPeriod, manualRenewal] could not be changed when ongoing subscription = false"),
  ONGOING_NOT_ALLOWED("ongoingNotAllowed", "Ongoing field must be absent for One-time order"),
  CANNOT_MIX_TYPES_FOR_ZERO_PRICE("cannotMixTypesForZeroPrice", "Fund distribution types cannot be mixed for a price of 0"),
  INCORRECT_FUND_DISTRIBUTION_TOTAL("incorrectFundDistributionTotal","Fund distribution total must add to 100% or totalPrice"),
  INSTANCE_ID_NOT_ALLOWED_FOR_PACKAGE_POLINE("InstanceIdNotAllowedForPackagePoLine", "Instance id not allowed for package poline"),
  PREFIX_IS_USED("prefixIsUsed", "The prefix cannot be deleted as it is used by one or more orders"),
  PREFIX_NOT_FOUND("prefixNotFound", "The prefix is not available"),
  SUFFIX_IS_USED("suffixIsUsed", "The suffix cannot be deleted as it is used by one or more orders"),
  SUFFIX_NOT_FOUND("suffixNotFound", "The suffix is not available"),
  PIECES_TO_BE_DELETED("piecesNeedToBeDeleted", "Pieces need to be deleted"),
  PIECES_TO_BE_CREATED("piecesNeedToBeCreated", "Pieces need to be created"),
  LOCATION_CAN_NOT_BE_MODIFIER_AFTER_OPEN("locationCannotBeModifiedAfterOpen", "Please use the receiving App to update pieces and locations"),
  LOCATION_UPDATE_WITHOUT_AFFILIATION("locationUpdateWithoutAffiliation", "Location must not be updated if the user does not have its corresponding affiliation"),
  REQUEST_FOUND("thereAreRequestsOnItem", "There are requests on item"),
  REQUESTS_ACTION_REQUIRED("requestsActionRequired", "There are circulation requests on items and requestsAction should be populated"),
  PIECES_HAVE_DIFFERENT_RECEIVING_TENANT_IDS("piecesHaveDifferentReceivingTenantIds", "All pieces do not have the same receivingTenantId"),
  PIECES_MUST_HAVE_RECEIVED_STATUS("piecesMustHaveReceivedStatus", "All pieces must have received status in order to be bound"),
  BUDGET_EXPENSE_CLASS_NOT_FOUND("budgetExpenseClassNotFound", "Budget expense class not found"),
  INACTIVE_EXPENSE_CLASS("inactiveExpenseClass", "Expense class is Inactive"),
  HOLDINGS_BY_INSTANCE_AND_LOCATION_NOT_FOUND("holdingsByInstanceAndLocationNotFoundError", "Holdings by instance id %s and location id %s not found"),
  ROLLOVER_PO_LINES_ERROR("rolloverPoLinesError", "Rollover poLines by chunks failed"),
  RETRIEVE_ROLLOVER_ORDER_ERROR("retrieveRolloverOrdersError", "Retrieve rollover order ids by chunks failed"),
  ORDER_RELATES_TO_INVOICE("orderRelatesToInvoice", "This order or order line is linked to Invoice number(s) and can not be deleted"),
  ROLLOVER_NOT_COMPLETED("rolloverNotCompleted", "Rollover has not been completed for some ledgers related to this order"),
  PARTIALLY_RETURNED_COLLECTION("partiallyReturnedCollection", "The number of returned entities is not equal to the number of requested"),
  ERROR_RETRIEVING_PO_LINES("errorRetrievingPoLines", "Error retrieving purchase order lines from storage"),
  ERROR_RETRIEVING_TRANSACTION("errorRetrievingTransactions", "Error retrieving the transactions"),
  HOLDINGS_BY_ID_NOT_FOUND("holdingsByIdNotFoundError", "Holdings with id %s not found"),
  HOLDINGS_ID_AND_LOCATION_ID_IS_NULL_ERROR("locationIdAndHoldingIdAbsentError", "Reference on holding or location must present"),
  MAY_BE_LINK_TO_EITHER_HOLDING_OR_LOCATION_ERROR("mayBeLinkToEitherHoldingOrLocationError", "There may be a link to either the holding or the location"),
  PO_LINE_NOT_FOUND("poLineNotFound", "The purchase order line record is not found"),
  RECEIVING_PROCESS_ENCUMBRANCES_ERROR("receivingProcessEncumbrancesError",
    "Pieces can not be added to or deleted from this Title until all the Fund distributions on the related purchase order line are converted from amounts to percentages."),
  PIECE_HOLDING_REFERENCE_IS_NOT_ALLOWED_ERROR("holdingReferenceIsNotAllowed", "Holding reference is not allowed in the Piece for Pending order. Please set location reference"),
  POSTGRE_SQL_ERROR("pgException", "PostgreSQL exception"),
  PIECE_FORMAT_IS_NOT_VALID_ERROR("pieceFormatIsNotValid", "Piece format %s is not compatible with purchase line %s"),
  PIECE_DISPLAY_ON_HOLDINGS_IS_NOT_CONSISTENT("pieceDisplayOnHoldingsIsNotConsistent", "Display On Holdings could not be set to false when Display To Public is true"),
  PIECE_RELATED_ORDER_DATA_IS_NOT_VALID("pieceRelatedOrderDataIsNotValid", "Adding piece for pending order with synchronized workflow is not allowed"),
  CREATE_PIECE_FOR_PENDING_ORDER_ERROR("createPiecePendingOrderError", "Creating piece for pending order is not possible. Please open order."),
  CREATE_ITEM_FOR_PIECE_IS_NOT_ALLOWED_ERROR("createItemForPieceIsNotAllowedError", "Create item for piece format %s is not allowed. Please check inventory option in the purchase order line %s"),
  NOT_FOUND("notFound", "Not Found"),
  FORBIDDEN_DELETE_SYSTEM_VALUE("forbiddenDeleteSystemValues", "It is forbidden to delete system values"),
  FORBIDDEN_DELETE_USED_VALUE("forbiddenDeleteUsedValue", "Deleting the value used is prohibited"),
  ERROR_REMOVING_INVOICE_LINE_ENCUMBRANCES("errorRemovingInvoiceLineEncumbrances", "Error removing invoice line encumbrance links after deleting the encumbrances: %s"),
  PO_LINE_HAS_RELATED_APPROVED_INVOICE_ERROR("poLineHasRelatedApprovedInvoice", "A related invoice has the APPROVED status, invoice line ids: %s"),
  INSTANCE_INVALID_PRODUCT_ID_ERROR("instanceInvalidProductIdError", "Instance connection could not be changed, the chosen instance contains an invalid Product ID."),
  FUND_LOCATION_RESTRICTION_VIOLATION("fundLocationRestrictionViolation", "One of the locations is restricted to be used by all funds."),
  ENCUMBRANCES_FOR_RE_ENCUMBER_NOT_FOUND("encumbrancesForReEncumberNotFound", "The encumbrances were correctly created during the rollover or have already been updated."),
  CLAIMING_CONFIG_INVALID("claimingConfigInvalid", "Claiming interval should be set and greater than 0 if claiming is active"),
  TEMPLATE_NAME_ALREADY_EXISTS("templateNameNotUnique", "Template name already exists"),
  BARCODE_IS_NOT_UNIQUE("barcodeIsNotUnique", "The barcode already exists. The barcode must be unique"),
  DELETE_WITH_EXPENDED_AMOUNT("deleteWithExpendedAmount", "Cannot delete an encumbrance with an expended amount"),
  INVALID_ROUTING_LIST_FOR_PO_LINE_FORMAT("invalidRoutingListForPoLineFormat", "Cannot create routing list for POL without 'Physical' or 'P/E Mix' order format"),
  ROUTING_LIST_LIMIT_REACHED_FOR_PO_LINE("routingListLimitReachedForPoLine", "Cannot create routing list for POL as the associated lists' amount is not less than Physical copies"),
  PO_LINE_NOT_FOUND_FOR_ROUTING_LIST("poLineNotFoundForRoutingList", "Cannot find a corresponding PO Line with the provided id"),
  ROUTING_LIST_UNIQUE_NAME_VIOLATION("routingListUniqueNameViolation", "Routing list with the same name already exists"),
  ORDER_FORMAT_INCORRECT_FOR_BINDARY_ACTIVE("orderFormatIncorrectForBindaryActive", "When PoLine is bindery active, its format must be 'P/E Mix' or 'Physical Resource'"),
  CREATE_INVENTORY_INCORRECT_FOR_BINDARY_ACTIVE("createInventoryIncorrectForBindaryActive", "When PoLine is bindery active, Create Inventory must be 'Instance, Holding, Item'"),
  RECEIVING_WORKFLOW_INCORRECT_FOR_BINDARY_ACTIVE("receivingWorkflowIncorrectForBindaryActive", "When PoLine is bindery active, its receiving workflow must be set to 'Independent order and receipt quantity'"),
  BIND_ITEM_MUST_INCLUDE_EITHER_HOLDING_ID_OR_LOCATION_ID("bindItemMustIncludeEitherHoldingIdOrLocationId", "During binding pieces, the bindItem object must have either holdingId or locationId field populated"),
  BUDGET_NOT_FOUND_FOR_FISCAL_YEAR("budgetNotFoundForFiscalYear", "Could not find an active budget for a fund with the current fiscal year of another fund in the fund distribution"),
  LAST_PIECE("lastPiece", "The piece cannot be deleted because it is the last piece for the poLine with Receiving Workflow 'Synchronized order and receipt quantity' and cost quantity '1'"),;

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
