package org.folio.rest.impl;

import io.vertx.core.json.JsonObject;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;

import java.util.Arrays;

import static org.folio.rest.impl.ApiTestBase.*;

public enum ProtectedEntities {

  PIECES("/orders/pieces"),
  ORDER_LINES("/orders/order-lines"),
  ORDERS("/orders/composite-orders");

  public static final String FULL_PROTECTED_USERS_UNIT_ID = "e68c18fc-833f-494e-9a0e-b236eb4b310b";
  public static final String UPDATE_ONLY_UNIT_ID = "aa0ec4e1-782f-45f6-a6f3-8e6b6c00599c";
  public static final String SUPER_USER_UNIT_ID = "0e9525aa-d123-4e4d-9f7e-1b302a97eb90";
  public static final String NON_EXISTED_UNIT_ID = "b548d790-07da-456f-b4ea-7a77c0e34a0f";

  public static final String PO_LINE_FOR_ORDER_WITHOUT_UNITS_ID = "0dd8f1d2-ac2e-4155-a407-72071f6d5f4a";
  public static final String ORDER_WITHOUT_UNITS_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";
  public static final String PO_LINE_FOR_ORDER_WITH_NON_PROTECTED_UNITS_ID = "568f1899-fc55-4956-be18-e47073807acd";
  public static final String ORDER_WITH_NON_PROTECTED_UNITS_ID = "a200e0e1-79f2-4161-81e3-15cc2c8ff9d1";
  public static final String PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID = "c081774b-9ad6-40d8-9527-569a5316f14e";
  public static final String ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID = "d4f3c8e6-0b4a-48ee-bc6e-2d07284bff3b";
  public static final String PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID = "6953bdfb-db69-4e25-9169-71522c11f2a0";
  public static final String ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID = "54876d24-78cf-4ed1-a017-5ca54d94130b";

  private String endpoint;

  ProtectedEntities(String endpoint) {
    this.endpoint = endpoint;
  }

  public String getEndpoint() {
    return endpoint;
  }

  // Returns sample for flow: order haven't units
  public String getSampleForFlowWithoutUnits() {
    switch (this) {
      case PIECES:
        Piece piece = getMinimalContentPiece();
        piece.setPoLineId(PO_LINE_FOR_ORDER_WITHOUT_UNITS_ID);
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId(PO_LINE_FOR_ORDER_WITHOUT_UNITS_ID);
        poLine.setPurchaseOrderId(ORDER_WITHOUT_UNITS_ID);
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList(NON_EXISTED_UNIT_ID, "0f2bb7a2-728f-4e07-9268-082577a7bedb"));
        return JsonObject.mapFrom(compPo).encode();
      default:
        return null;
    }
  }

  // Returns sample for flow: order have units allowed operation
  public String getSampleForFlowWithAllowedUnits() {
    switch (this) {
      case PIECES:
        Piece piece = getMinimalContentPiece();
        piece.setPoLineId(PO_LINE_FOR_ORDER_WITH_NON_PROTECTED_UNITS_ID);
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId(PO_LINE_FOR_ORDER_WITH_NON_PROTECTED_UNITS_ID);
        poLine.setPurchaseOrderId(ORDER_WITH_NON_PROTECTED_UNITS_ID);
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList(SUPER_USER_UNIT_ID, FULL_PROTECTED_USERS_UNIT_ID));
        return JsonObject.mapFrom(compPo).encode();
      default:
        return null;
    }
  }

  // Returns sample for flow: order have units, units protect operation, user is member of order's units
  public String getSampleForProtectedUnitsAndAllowedUserFlow() {
    switch (this) {
      case PIECES:
        Piece piece = getMinimalContentPiece();
        piece.setPoLineId(PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID);
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId(PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID);
        poLine.setPurchaseOrderId(ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID);
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList(FULL_PROTECTED_USERS_UNIT_ID, UPDATE_ONLY_UNIT_ID));
        return JsonObject.mapFrom(compPo).encode();
      default:
        return null;
    }
  }

  // Returns sample for flow: order have units, units protect operation, user isn't member of order's units
  public String getSampleForRestrictedFlow() {
    switch (this) {
      case PIECES:
        Piece piece = getMinimalContentPiece();
        piece.setPoLineId(PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID);
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId(PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID);
        poLine.setPurchaseOrderId(ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID);
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList(FULL_PROTECTED_USERS_UNIT_ID, UPDATE_ONLY_UNIT_ID));
        return JsonObject.mapFrom(compPo).encode();
      default:
        return null;
    }
  }
}
