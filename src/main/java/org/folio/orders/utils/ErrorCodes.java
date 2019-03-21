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
  ITEM_UPDATE_FAILED("itemUpdateFailed", "The item record failed to be updated"),
  ITEM_NOT_FOUND("itemNotFound", "The item record is not found"),
  ITEM_NOT_RETRIEVED("itemNotRetrieved", "The item record is not retrieved"),
  ZERO_COST_QTY("zeroCostQty", "Physical and electronic cost quantity must be specified"),
  ZERO_COST_PHYSICAL_QTY("zeroCostQtyPhysical", "Physical cost quantity must be specified"),
  ZERO_COST_ELECTRONIC_QTY("zeroCostQtyElectronic", "Electronic cost quantity must be specified"),
  NON_ZERO_COST_PHYSICAL_QTY("nonZeroLocQtyPhysical", "Physical cost quantity must not be specified"),
  NON_ZERO_COST_ELECTRONIC_QTY("nonZeroCostQtyElectronic", "Electronic cost quantity must not be specified"),
  PHYSICAL_LOC_QTY_EXCEEDS_COST("locQtyPhysicalExceedsCost", "Locations physical quantity exceeds cost physical quantity"),
  PHYSICAL_COST_QTY_EXCEEDS_LOC("costQtyPhysicalExceedsLoc", "Cost's physical quantity exceeds locations' physical quantity"),
  ELECTRONIC_LOC_QTY_EXCEEDS_COST("locQtyElectronicExceedsCost", "Locations electronic quantity exceeds cost electronic quantity"),
  ORDER_VENDOR_IS_INACTIVE("vendorIsInactive", "Order cannot be open as the associated vendor is inactive"),
  POL_ACCESS_PROVIDER_IS_INACTIVE("accessProviderIsInactive", "Order cannot be open as the associated access provider is inactive"),
  ORDER_VENDOR_NOT_FOUND("vendorNotFound", "Order cannot be open as the associated vendor not found"),
  ORDER_OPEN("orderOpen", "Order cannot be modified in Open status"),
  ORDER_CLOSED("orderClosed", "Order cannot be modified in Closed status"),
  POL_ACCESS_PROVIDER_NOT_FOUND("accessProviderNotFound", "Order cannot be open as the associated access provider not found"),
  COST_UNIT_PRICE_INVALID("costUnitPriceInvalid", "Cost's list unit price is invalid"),
  COST_UNIT_PRICE_ELECTRONIC_INVALID("costUnitPriceElectronicInvalid", "Cost's list unit price for Electronic resource(s) is invalid"),
  COST_ADDITIONAL_COST_INVALID("costAdditionalCostInvalid", "Cost's list unit price must be positive number"),
  COST_DISCOUNT_INVALID("costDiscountInvalid", "Cost's discount is invalid");

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
