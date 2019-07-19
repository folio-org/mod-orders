package org.folio.rest.impl;

import io.vertx.core.json.JsonObject;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Location;
import org.folio.rest.jaxrs.model.Physical;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Source;

import java.util.Arrays;

public enum ProtectedEntities {

  PIECES("/orders/pieces"),
  ORDER_LINES("/orders/order-lines"),
  ORDERS("/orders/composite-orders");

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
        piece.setPoLineId("0dd8f1d2-ac2e-4155-a407-72071f6d5f4a");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId("0dd8f1d2-ac2e-4155-a407-72071f6d5f4a");
        poLine.setPurchaseOrderId("1ab7ef6a-d1d4-4a4f-90a2-882aed18af14");
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList("b548d790-07da-456f-b4ea-7a77c0e34a0f", "0f2bb7a2-728f-4e07-9268-082577a7bedb"));
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
        piece.setPoLineId("568f1899-fc55-4956-be18-e47073807acd");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId("568f1899-fc55-4956-be18-e47073807acd");
        poLine.setPurchaseOrderId("a200e0e1-79f2-4161-81e3-15cc2c8ff9d1");
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList("0e9525aa-d123-4e4d-9f7e-1b302a97eb90", "e68c18fc-833f-494e-9a0e-b236eb4b310b"));
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
        piece.setPoLineId("c081774b-9ad6-40d8-9527-569a5316f14e");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId("c081774b-9ad6-40d8-9527-569a5316f14e");
        poLine.setPurchaseOrderId("d4f3c8e6-0b4a-48ee-bc6e-2d07284bff3b");
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList("e68c18fc-833f-494e-9a0e-b236eb4b310b", "aa0ec4e1-782f-45f6-a6f3-8e6b6c00599c"));
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
        piece.setPoLineId("6953bdfb-db69-4e25-9169-71522c11f2a0");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId("6953bdfb-db69-4e25-9169-71522c11f2a0");
        poLine.setPurchaseOrderId("54876d24-78cf-4ed1-a017-5ca54d94130b");
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList("e68c18fc-833f-494e-9a0e-b236eb4b310b", "aa0ec4e1-782f-45f6-a6f3-8e6b6c00599c"));
        return JsonObject.mapFrom(compPo).encode();
      default:
        return null;
    }
  }


  public static Piece getMinimalContentPiece() {
    Piece piece = new Piece();
    piece.setReceivingStatus(Piece.ReceivingStatus.RECEIVED);
    piece.setFormat(Piece.Format.PHYSICAL);
    return piece;
  }

  public static CompositePoLine getMinimalContentCompositePoLine() {
    CompositePoLine poLine = new CompositePoLine();
    poLine.setSource(new Source().withCode("CCC"));
    poLine.setOrderFormat(CompositePoLine.OrderFormat.PHYSICAL_RESOURCE);
    poLine.setAcquisitionMethod(CompositePoLine.AcquisitionMethod.PURCHASE);
    poLine.setPhysical(new Physical().withMaterialType("2d1398ae-e1aa-4c7c-b9c9-15adf8cf6425"));
    poLine.setCost(new Cost().withCurrency("EUR").withQuantityPhysical(1).withListUnitPrice(10.0));
    poLine.setLocations(Arrays.asList(new Location().withLocationId("2a00b0be-1447-42a1-a112-124450991899").withQuantityPhysical(1)));
    poLine.setTitle("Title");
    return poLine;
  }

  public static CompositePurchaseOrder getMinimalContentCompositePurchaseOrder() {
    return new CompositePurchaseOrder()
      .withOrderType(CompositePurchaseOrder.OrderType.ONE_TIME)
      .withVendor("7d232b43-bf9a-4301-a0ce-9e076298632e");
  }
}
