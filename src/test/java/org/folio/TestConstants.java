package org.folio;

import static org.folio.orders.utils.PermissionsUtil.OKAPI_HEADER_PERMISSIONS;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TOKEN;
import static org.folio.rest.RestVerticle.OKAPI_USERID_HEADER;
import static org.folio.rest.impl.MockServer.BASE_MOCK_DATA_PATH;
import static org.folio.service.orders.AcquisitionsUnitsServiceTest.USER_ID_ASSIGNED_TO_ACQ_UNITS;

import java.util.UUID;

import io.restassured.http.Header;
import io.vertx.core.json.JsonArray;
import org.folio.orders.utils.AcqDesiredPermissions;

public final class TestConstants {

  private TestConstants() {}

  public static final String ORDERS_RECEIVING_ENDPOINT = "/orders/receive";
  public static final String ORDERS_CHECKIN_ENDPOINT = "/orders/check-in";
  public static final String ORDERS_EXPECT_ENDPOINT = "/orders/expect";
  public static final String ORDERS_BIND_ENDPOINT = "/orders/bind-pieces";
  public static final String ORDERS_BIND_ID_ENDPOINT = "/orders/bind-pieces/%s";
  public static final String PIECES_CLAIMING_ENDPOINT = "/pieces/claim";
  public static final String PO_LINE_NUMBER_VALUE = "1";

  public static final String BAD_QUERY = "unprocessableQuery";
  public static final String ID = "id";

  public static final String EXISTED_ID = "763643c2-0f80-4908-b18b-780d3e8cd136";
  public static final String ID_BAD_FORMAT = "123-45-678-90-abc";
  public static final String ID_DOES_NOT_EXIST = "d25498e7-3ae6-45fe-9612-ec99e2700d2f";
  public static final String ID_FOR_INTERNAL_SERVER_ERROR = "168f8a86-d26c-406e-813f-c7527f241ac3";
  public static final String ID_FOR_PIECES_INTERNAL_SERVER_ERROR = "93c5bb58-9429-4fa7-b06d-a829bdf16813";
  public static final String PO_ID_GET_LINES_INTERNAL_SERVER_ERROR = "bad500bb-bbbb-500b-bbbb-bbbbbbbbbbbb";
  public static final String PO_ID_PENDING_STATUS_WITH_PO_LINES = "e5ae4afd-3fa9-494e-a972-f541df9b877e";
  public static final String PO_ID_PENDING_STATUS_WITHOUT_PO_LINES = "50fb922c-3fa9-494e-a972-f2801f1b9fd1";
  public static final String PO_ID_OPEN_STATUS = "c1465131-ed35-4308-872c-d7cdf0afc5f7";
  public static final String PO_WFD_ID_OPEN_STATUS = "a1465131-ed35-4308-872c-d7cdf0afc5f7";
  public static final String PO_ID_CLOSED_STATUS = "07f65192-44a4-483d-97aa-b137bbd96390";
  public static final String PO_CLOSED_STATUS_WITH_ONGOING = "07f65192-44a4-483d-97aa-b137bbd96392";
  public static final String PO_CLOSED_STATUS = "07f65192-44a4-483d-97aa-b137bbd96391";
  public static final String PO_ID_OPEN_TO_BE_CLOSED = "9d56b621-202d-414b-9e7f-5fefe4422ab3";
  public static final String PO_ID_OPEN_TO_CANCEL = "f56c70bc-8a31-4f56-b606-c6d8e597b7c1";
  public static final String PO_LINE_ID_FOR_SUCCESS_CASE = "fca5fa9e-15cb-4a3d-ab09-eeea99b97a47";
  public static final String PO_LINE_ID_WITHOUT_DETAILS = "13c4c3c2-90d9-4090-8694-edae7e79e7ac";
  public static final String PO_LINE_ID_WRONG_EXPENSE_CLASS = "bd8f1901-c768-4bb1-b650-8c12c5f42fd8";
  public static final String MIN_PO_ID = UUID.randomUUID().toString();
  public static final String MIN_PO_LINE_ID = UUID.randomUUID().toString();

  public static final String COMP_ORDER_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "compositeOrders/";

  public static final String X_ECHO_STATUS = "X-Okapi-Echo-Status";
  public static final String EMPTY_CONFIG_TENANT = "config_empty";
  public static final String NON_EXIST_CONTRIBUTOR_NAME_TYPE_TENANT = "nonExistContributorNameType";
  public static final String INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT = "hasCodeLikeInstanceStatus";
  public static final String NON_EXIST_INSTANCE_STATUS_TENANT = "nonExistInstanceStatus";
  public static final String NON_EXIST_INSTANCE_TYPE_TENANT = "nonExistInstanceType";
  public static final String NON_EXIST_LOAN_TYPE_TENANT = "nonExistLoanType";
  public static final String NON_EXIST_HOLDINGS_SOURCE_TENANT = "nonExistHoldingsSource";
  public static final String COMPOSITE_PO_LINES_PREFIX = "compositePoLines[0].";
  public static final String OKAPI_URL = "X-Okapi-Url";
  public static final String OKAPI_TENANT = "X-Okapi-Tenant";

  public static final Header INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT);
  public static final Header NON_EXIST_INSTANCE_STATUS_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_INSTANCE_STATUS_TENANT);
  public static final Header NON_EXIST_INSTANCE_TYPE_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_INSTANCE_TYPE_TENANT);
  public static final Header NON_EXIST_LOAN_TYPE_TENANT_HEADER = new Header(OKAPI_HEADER_TENANT, NON_EXIST_LOAN_TYPE_TENANT);
  public static final Header NON_EXIST_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "ordersimpltest");
  public static final Header X_OKAPI_USER_ID = new Header(OKAPI_USERID_HEADER, "440c89e3-7f6c-578a-9ea8-310dad23605e");
  public static final Header ALL_DESIRED_ACQ_PERMISSIONS_HEADER = new Header(OKAPI_HEADER_PERMISSIONS, new JsonArray(AcqDesiredPermissions.getValuesExceptBypass()).encode());
  public static final Header X_OKAPI_USER_ID_WITH_ACQ_UNITS = new Header(OKAPI_USERID_HEADER, USER_ID_ASSIGNED_TO_ACQ_UNITS);
  public static final Header X_OKAPI_TOKEN = new Header(OKAPI_HEADER_TOKEN, "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJkaWt1X2FkbWluIiwidXNlcl9pZCI6ImJmZTI2MjM0LTMzNjktNTdhYS05ZjhhLWU2ZWVhY2M0YTgzYiIsImlhdCI6MTU4MzE1Nzg5OCwidGVuYW50IjoiZGlrdSJ9.Mk7u4KaCywSuYtBgCT44oGcVC0C8jUMY9KjsUnug48I");
  public static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10 = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_10");
  public static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10_CLAIMS = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_10_claims");
  public static final Header EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1 = new Header(OKAPI_HEADER_TENANT, "test_diku_limit_1");
  public static final Header EXIST_CONFIG_X_OKAPI_TENANT_ECS = new Header(OKAPI_HEADER_TENANT, "consortium");
  public static final Header INVALID_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, "invalid_config");
  public static final Header EMPTY_CONFIG_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, EMPTY_CONFIG_TENANT);
  public static final String PROTECTED_READ_ONLY_TENANT = "protected_read";

  public static final String ACTIVE_ACCESS_PROVIDER_A = "858e80d2-f562-4c54-9934-6e274dee511d";
  public static final String ACTIVE_ACCESS_PROVIDER_B = "d1b79c8d-4950-482f-8e42-04f9aae3cb40";
  public static final String INACTIVE_ACCESS_PROVIDER_A = "f6cd1850-2587-4f6c-b680-9b27ff26d619";
  public static final String INACTIVE_ACCESS_PROVIDER_B = "f64bbcae-e5ea-42b6-8236-55fefed0fb8f";
  public static final String NON_EXIST_ACCESS_PROVIDER_A = "160501b3-52dd-31ec-a0ce-17762e6a9b47";

  public static final String LOCATION_ID = "f34d27c6-a8eb-461b-acd6-5dea81771e70";

  public static final String PIECE_ID = "0f1bb087-72e9-44ce-a145-bfc2e7b005cf";
  public static final String EXISTED_ITEM_ID = "522a501a-56b5-48d9-b28a-3a8f02482d97";

  public static final String PIECE_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  public static final String TILES_PATH = BASE_MOCK_DATA_PATH + "titles/";
  public static final String ID_FOR_TEMPLATE_NAME_ALREADY_EXISTS = "cd0619fb-a628-4d90-be41-df8943e97768";
  public static final String ROUTING_LIST_ID = "eee951de-ea49-400a-96e8-705ae5a1e1e8";
}
