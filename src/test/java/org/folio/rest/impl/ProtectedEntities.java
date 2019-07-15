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

  // Returns sample for flow: order have units, units protect operation, user isn't member of order's units
  public String getSampleForRestrictedFlow() {
    switch (this) {
      case PIECES:
        Piece piece = getMinimalContentPiece();
        piece.setPoLineId("d471d766-8dbb-4609-999a-02681dea6c22");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setId("c2755a78-2f8d-47d0-a218-059a9b7391b4");
        poLine.setPurchaseOrderId("1ab7ef6a-d1d4-4a4f-90a2-882aed18af14");
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList("2e6a170f-ae20-4889-813f-641831e24b84"));
        return JsonObject.mapFrom(compPo).encode();
      default:
        return null;
    }
  }

  // Returns sample for flow: order have units, units protect operation, user is member of order's units
  public String getSampleForFlow201() {
    switch (this) {
      case PIECES:
        Piece piece = getMinimalContentPiece();
        piece.setPoLineId("c2755a78-2f8d-47d0-a218-059a9b7391b4");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setPurchaseOrderId("1ab7ef6a-d1d4-4a4f-90a2-882aed18af14");
        poLine.setId("c2755a78-2f8d-47d0-a218-059a9b7391b4");
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList("6b982ffe-8efd-4690-8168-0c773b49cde1", "aa0ec4e1-782f-45f6-a6f3-8e6b6c00599c"));
        return JsonObject.mapFrom(compPo).encode();
      default:
        return null;
    }
  }

  // Returns sample for flow: order haven't units
  public String getSampleForFlowWithoutUnits() {
    switch (this) {
      case PIECES:
        Piece piece = getMinimalContentPiece();
        piece.setPoLineId("0009662b-8b80-4001-b704-ca10971f175d");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setPurchaseOrderId("9a952cd0-842b-4e71-bddd-014eb128dc8e");
        poLine.setId("0009662b-8b80-4001-b704-ca10971f175d");
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList("5a9fa0b5-b7e2-45b9-8cfb-4d38fad2f9ed"));
        return JsonObject.mapFrom(compPo).encode();
      default:
        return null;
    }
  }

  // Returns sample for flow: order have units allowed operation
  public String getSampleForFlow201WithNonProtectedUnits() {
    switch (this) {
      case PIECES:
        Piece piece = getMinimalContentPiece();
        piece.setPoLineId("8ca5aa90-bfbd-44b3-8ad2-f6f1b7e05337");
        return JsonObject.mapFrom(piece).encode();
      case ORDER_LINES:
        CompositePoLine poLine = getMinimalContentCompositePoLine();
        poLine.setPurchaseOrderId("e5ae4afd-3fa9-494e-a972-f541df9b877e");
        poLine.setId("8ca5aa90-bfbd-44b3-8ad2-f6f1b7e05337");
        return JsonObject.mapFrom(poLine).encode();
      case ORDERS:
        CompositePurchaseOrder compPo = getMinimalContentCompositePurchaseOrder();
        compPo.setAcqUnitIds(Arrays.asList("0e9525aa-d123-4e4d-9f7e-1b302a97eb90"));
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
