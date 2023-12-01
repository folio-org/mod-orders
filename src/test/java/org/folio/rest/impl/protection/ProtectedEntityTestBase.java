package org.folio.rest.impl.protection;

import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.TestUtils.getMinimalContentPiece;
import static org.folio.TestUtils.getMinimalContentTitle;
import static org.folio.TestUtils.getRandomId;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.MockServer.addMockEntry;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasSize;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.folio.rest.impl.MockServer;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Piece;
import org.folio.rest.jaxrs.model.Title;
import org.hamcrest.Matcher;

import io.restassured.http.Header;
import io.vertx.core.json.JsonObject;

public abstract class ProtectedEntityTestBase {

  static final String DELETE_PROTECTED_UNIT = "2e6a170f-ae20-4889-813f-641831e24b84";
  public static final List<String> DELETE_PROTECTED_UNITS = Collections.singletonList(DELETE_PROTECTED_UNIT);
  static final String CREATE_PROTECTED_UNIT = "f6d2cc9d-82ca-437c-a4e6-e5c30323df00";
  public static final List<String> CREATE_PROTECTED_UNITS = Collections.singletonList(CREATE_PROTECTED_UNIT);
  static final String FULL_PROTECTED_USERS_UNIT_ID = "e68c18fc-833f-494e-9a0e-b236eb4b310b";
  public static final List<String> PROTECTED_UNITS = Collections.singletonList(FULL_PROTECTED_USERS_UNIT_ID);
  static final String NOT_PROTECTED_UNIT_ID = "0e9525aa-d123-4e4d-9f7e-1b302a97eb90";
  public static final List<String> NOT_PROTECTED_UNITS = Arrays.asList(NOT_PROTECTED_UNIT_ID, FULL_PROTECTED_USERS_UNIT_ID);
  static final String NON_EXISTENT_UNIT_ID = "b548d790-07da-456f-b4ea-7a77c0e34a0f";
  public static final List<String> NON_EXISTENT_UNITS = Arrays.asList(NON_EXISTENT_UNIT_ID, "0f2bb7a2-728f-4e07-9268-082577a7bedb");

  private static final String USER_IS_NOT_MEMBER_OF_ORDERS_UNITS = "7007ed1b-85ab-46e8-9524-fada8521dfd5";
  static final Header X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER = new Header(OKAPI_USERID_HEADER, USER_IS_NOT_MEMBER_OF_ORDERS_UNITS);

  private static final String USER_IS_MEMBER_OF_ORDER_UNITS = "6b4be232-5ad9-47a6-80b1-8c1acabd6212";
  static final Header X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER = new Header(OKAPI_USERID_HEADER, USER_IS_MEMBER_OF_ORDER_UNITS);

  static void validateNumberOfRequests(int numOfUnitRqs, int numOfMembershipRqs) {
    assertThat(MockServer.getAcqUnitsSearches(), getMatcher(numOfUnitRqs));
    assertThat(MockServer.getAcqMembershipsSearches(), getMatcher(numOfMembershipRqs));
  }

  static Matcher<Collection<?>> getMatcher(Integer value) {
    return value > 0 ? hasSize(value) : empty();
  }

  public CompositePurchaseOrder prepareOrder(List<String> acqUnitsIds, CompositePurchaseOrder.WorkflowStatus workflowStatus) {
    CompositePurchaseOrder po = getMinimalContentCompositePurchaseOrder();
    po.setWorkflowStatus(workflowStatus);
    po.setAcqUnitIds(new ArrayList<>(acqUnitsIds));
    addMockEntry(PURCHASE_ORDER_STORAGE, JsonObject.mapFrom(po));
    return po;
  }

  public CompositePoLine preparePoLine(List<String> acqUnitsIds,
                          CompositePurchaseOrder.WorkflowStatus workflowStatus) {
    CompositePurchaseOrder order = prepareOrder(acqUnitsIds, workflowStatus);
    CompositePoLine poLine = getMinimalContentCompositePoLine(order.getId());
    addMockEntry(PO_LINES_STORAGE, JsonObject.mapFrom(poLine));
    return poLine;
  }

  public Title prepareTitle(List<String> acqUnitIds) {
    Title title = getMinimalContentTitle();
    title.setAcqUnitIds(acqUnitIds);
    addMockEntry(TITLES, JsonObject.mapFrom(title));
    return title;
  }

  public Piece preparePiece(List<String> acqUnitsIds) {
    CompositePoLine poLine = preparePoLine(new ArrayList<>(), CompositePurchaseOrder.WorkflowStatus.OPEN);
    Title title = prepareTitle(acqUnitsIds);
    Piece piece = getMinimalContentPiece(poLine.getId());
    piece.setTitleId(title.getId());
    addMockEntry(PIECES_STORAGE, JsonObject.mapFrom(piece));

    return piece;
  }

  public AcquisitionsUnit prepareTestUnit(boolean isDeleted) {
    String id = getRandomId();
    return new AcquisitionsUnit().withId(id).withName(id).withIsDeleted(isDeleted);
  }
}
