package org.folio.rest.impl.protection;

import io.restassured.http.Header;
import org.folio.rest.impl.ApiTestBase;
import org.folio.rest.impl.MockServer;
import org.hamcrest.Matcher;

import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.nullValue;

public abstract class ProtectedEntityTestBase extends ApiTestBase {

  static final String FULL_PROTECTED_USERS_UNIT_ID = "e68c18fc-833f-494e-9a0e-b236eb4b310b";
  static final String UPDATE_ONLY_UNIT_ID = "aa0ec4e1-782f-45f6-a6f3-8e6b6c00599c";
  static final String SUPER_USER_UNIT_ID = "0e9525aa-d123-4e4d-9f7e-1b302a97eb90";
  static final String NON_EXISTED_UNIT_ID = "b548d790-07da-456f-b4ea-7a77c0e34a0f";

  public static final String PIECE_WITH_PO_LINE_FOR_ORDER_WITHOUT_UNITS_ID = "e4ebae49-f39d-460f-a383-c609da7e07cd";
  public static final String PO_LINE_FOR_ORDER_WITHOUT_UNITS_ID = "0dd8f1d2-ac2e-4155-a407-72071f6d5f4a";
  public static final String ORDER_WITHOUT_UNITS_ID = "1ab7ef6a-d1d4-4a4f-90a2-882aed18af14";

  public static final String PIECE_WITH_PO_LINE_FOR_ORDER_WITH_NON_PROTECTED_UNITS_ID = "dd6c7335-a8e5-440d-86ba-657240867de5";
  public static final String PO_LINE_FOR_ORDER_WITH_NON_PROTECTED_UNITS_ID = "568f1899-fc55-4956-be18-e47073807acd";
  public static final String ORDER_WITH_NON_PROTECTED_UNITS_ID = "a200e0e1-79f2-4161-81e3-15cc2c8ff9d1";

  public static final String PIECE_WITH_PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID = "63bd5119-e177-490b-8eef-dadbb18f4473";
  public static final String PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID = "c081774b-9ad6-40d8-9527-569a5316f14e";
  public static final String ORDER_WITH_PROTECTED_UNITS_ALLOWED_USER_ID = "d4f3c8e6-0b4a-48ee-bc6e-2d07284bff3b";

  public static final String PIECE_WITH_PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID = "67c97a0c-4824-4a3b-9b2c-1b4a4ea80ded";
  public static final String PO_LINE_FOR_ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID = "6953bdfb-db69-4e25-9169-71522c11f2a0";
  public static final String ORDER_WITH_PROTECTED_UNITS_AND_FORBIDDEN_USER_ID = "54876d24-78cf-4ed1-a017-5ca54d94130b";

  private static final String USER_IS_NOT_MEMBER_OF_ORDERS_UNITS = "7007ed1b-85ab-46e8-9524-fada8521dfd5";
  private static final Header X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER = new Header(OKAPI_USERID_HEADER, USER_IS_NOT_MEMBER_OF_ORDERS_UNITS);

  private static final String USER_IS_MEMBER_OF_ORDER_UNITS = "6b4be232-5ad9-47a6-80b1-8c1acabd6212";
  private static final Header X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER = new Header(OKAPI_USERID_HEADER, USER_IS_MEMBER_OF_ORDER_UNITS);

  static final Header[] FORBIDDEN_CREATION_HEADERS = {X_OKAPI_USER_WITH_UNITS_NOT_ASSIGNED_TO_ORDER};
  static final Header[] ALLOWED_CREATION_HEADERS = {X_OKAPI_USER_WITH_UNITS_ASSIGNED_TO_ORDER};

  static void validateNumberOfRequests(int numOfUnitRqs, int numOfMembershipRqs, int numOfAssignmentRqs) {
    assertThat(MockServer.getAcqUnitsSearches(), getMatcher(numOfUnitRqs));
    assertThat(MockServer.getAcqAssignmentSearches(), getMatcher(numOfAssignmentRqs));
    assertThat(MockServer.getAcqMembershipsSearches(), getMatcher(numOfMembershipRqs));
    MockServer.release();
  }

  static Matcher getMatcher(int value) {
    return value > 0 ? hasSize(value) : nullValue();
  }

  public abstract ProtectedOperations[] geProtectedOperations();

  // Returns sample for flow: order has IDs of non-existed units
  public abstract String getSampleForFlowWithNonExistedUnits();

  // Returns sample for flow: order has IDs of existed units that allowed operation
  public abstract String getSampleForFlowWithAllowedUnits();

  // Returns sample for flow: order has IDs of existed units that protect operation, but user is a member of order's units
  public abstract String getSampleForProtectedUnitsAndAllowedUserFlow();

  // Returns sample for flow: order has IDs of existed units that protect operation, but user isn't member of order's units
  public abstract String getSampleForRestrictedFlow();
}
