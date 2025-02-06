package org.folio.rest.impl;

import static java.lang.Thread.sleep;
import static java.time.temporal.ChronoUnit.DAYS;
import static java.util.stream.Collectors.toList;
import static javax.ws.rs.core.MediaType.APPLICATION_JSON;
import static javax.ws.rs.core.MediaType.TEXT_PLAIN;
import static javax.ws.rs.core.Response.Status.INTERNAL_SERVER_ERROR;
import static org.apache.commons.lang.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.assertj.core.api.Assertions.fail;
import static org.folio.TestConstants.ACTIVE_ACCESS_PROVIDER_A;
import static org.folio.TestConstants.ACTIVE_ACCESS_PROVIDER_B;
import static org.folio.TestConstants.BAD_QUERY;
import static org.folio.TestConstants.COMP_ORDER_MOCK_DATA_PATH;
import static org.folio.TestConstants.EMPTY_CONFIG_TENANT;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10;
import static org.folio.TestConstants.EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10_CLAIMS;
import static org.folio.TestConstants.ID;
import static org.folio.TestConstants.ID_BAD_FORMAT;
import static org.folio.TestConstants.ID_DOES_NOT_EXIST;
import static org.folio.TestConstants.ID_FOR_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.ID_FOR_PIECES_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.ID_FOR_TEMPLATE_NAME_ALREADY_EXISTS;
import static org.folio.TestConstants.INACTIVE_ACCESS_PROVIDER_A;
import static org.folio.TestConstants.INACTIVE_ACCESS_PROVIDER_B;
import static org.folio.TestConstants.INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT;
import static org.folio.TestConstants.MIN_PO_ID;
import static org.folio.TestConstants.MIN_PO_LINE_ID;
import static org.folio.TestConstants.NON_EXIST_ACCESS_PROVIDER_A;
import static org.folio.TestConstants.NON_EXIST_CONTRIBUTOR_NAME_TYPE_TENANT;
import static org.folio.TestConstants.NON_EXIST_HOLDINGS_SOURCE_TENANT;
import static org.folio.TestConstants.NON_EXIST_INSTANCE_STATUS_TENANT;
import static org.folio.TestConstants.NON_EXIST_INSTANCE_STATUS_TENANT_HEADER;
import static org.folio.TestConstants.NON_EXIST_INSTANCE_TYPE_TENANT;
import static org.folio.TestConstants.NON_EXIST_INSTANCE_TYPE_TENANT_HEADER;
import static org.folio.TestConstants.NON_EXIST_LOAN_TYPE_TENANT;
import static org.folio.TestConstants.NON_EXIST_LOAN_TYPE_TENANT_HEADER;
import static org.folio.TestConstants.OKAPI_TENANT;
import static org.folio.TestConstants.PO_ID_GET_LINES_INTERNAL_SERVER_ERROR;
import static org.folio.TestConstants.PO_LINE_NUMBER_VALUE;
import static org.folio.TestConstants.PROTECTED_READ_ONLY_TENANT;
import static org.folio.TestConstants.X_ECHO_STATUS;
import static org.folio.TestUtils.encodePrettily;
import static org.folio.TestUtils.getMinimalContentCompositePoLine;
import static org.folio.TestUtils.getMinimalContentCompositePurchaseOrder;
import static org.folio.TestUtils.getMinimalOrder;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.TestUtils.getMockData;
import static org.folio.TestUtils.getTitle;
import static org.folio.orders.utils.HelperUtils.COMPOSITE_PO_LINES;
import static org.folio.orders.utils.HelperUtils.DEFAULT_POLINE_LIMIT;
import static org.folio.orders.utils.HelperUtils.FUND_ID;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.QueryUtils.convertIdsToCqlQuery;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_MEMBERSHIPS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITIONS_UNITS;
import static org.folio.orders.utils.ResourcePathResolver.ACQUISITION_METHODS;
import static org.folio.orders.utils.ResourcePathResolver.ALERTS;
import static org.folio.orders.utils.ResourcePathResolver.BUDGETS;
import static org.folio.orders.utils.ResourcePathResolver.CURRENT_BUDGET;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_CREATE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.DATA_EXPORT_SPRING_EXECUTE_JOB;
import static org.folio.orders.utils.ResourcePathResolver.EXPENSE_CLASSES_URL;
import static org.folio.orders.utils.ResourcePathResolver.EXPORT_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.FINANCE_BATCH_TRANSACTIONS;
import static org.folio.orders.utils.ResourcePathResolver.FINANCE_EXCHANGE_RATE;
import static org.folio.orders.utils.ResourcePathResolver.FUNDS;
import static org.folio.orders.utils.ResourcePathResolver.LEDGERS;
import static org.folio.orders.utils.ResourcePathResolver.LEDGER_FY_ROLLOVERS;
import static org.folio.orders.utils.ResourcePathResolver.LEDGER_FY_ROLLOVER_ERRORS;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_INVOICE_RELATIONSHIP;
import static org.folio.orders.utils.ResourcePathResolver.ORDER_TEMPLATES;
import static org.folio.orders.utils.ResourcePathResolver.ORGANIZATION_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PIECES_STORAGE_BATCH;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_BATCH_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.PO_LINE_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PO_NUMBER;
import static org.folio.orders.utils.ResourcePathResolver.PREFIXES;
import static org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.REASONS_FOR_CLOSURE;
import static org.folio.orders.utils.ResourcePathResolver.RECEIVING_HISTORY;
import static org.folio.orders.utils.ResourcePathResolver.REPORTING_CODES;
import static org.folio.orders.utils.ResourcePathResolver.ROUTING_LISTS;
import static org.folio.orders.utils.ResourcePathResolver.SUFFIXES;
import static org.folio.orders.utils.ResourcePathResolver.TAGS;
import static org.folio.orders.utils.ResourcePathResolver.TITLES;
import static org.folio.orders.utils.ResourcePathResolver.TRANSACTIONS_ENDPOINT;
import static org.folio.orders.utils.ResourcePathResolver.USER_TENANTS_ENDPOINT;
import static org.folio.orders.utils.ResourcePathResolver.WRAPPER_PIECES_STORAGE;
import static org.folio.orders.utils.ResourcePathResolver.resourceByIdPath;
import static org.folio.orders.utils.ResourcePathResolver.resourcesPath;
import static org.folio.rest.RestVerticle.OKAPI_HEADER_TENANT;
import static org.folio.rest.acq.model.Piece.ReceivingStatus.LATE;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_IS_INACTIVE;
import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.rest.core.exceptions.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.rest.core.exceptions.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.rest.impl.PoNumberApiTest.EXISTING_PO_NUMBER;
import static org.folio.rest.impl.PoNumberApiTest.NONEXISTING_PO_NUMBER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ACTIVE_VENDOR_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ID_FOR_PRINT_MONOGRAPH_ORDER;
import static org.folio.rest.impl.PurchaseOrdersApiTest.INACTIVE_VENDOR_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ITEMS_NOT_FOUND;
import static org.folio.rest.impl.PurchaseOrdersApiTest.LISTED_PRINT_MONOGRAPH_PATH;
import static org.folio.rest.impl.PurchaseOrdersApiTest.MOD_VENDOR_INTERNAL_ERROR_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.NON_EXIST_VENDOR_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ORDER_DELETE_ERROR_TENANT;
import static org.folio.rest.impl.PurchaseOrdersApiTest.ORGANIZATION_NOT_VENDOR;
import static org.folio.rest.impl.PurchaseOrdersApiTest.PURCHASE_ORDER_ID;
import static org.folio.rest.impl.PurchaseOrdersApiTest.VENDOR_WITH_BAD_CONTENT;
import static org.folio.rest.impl.ReceivingHistoryApiTest.RECEIVING_HISTORY_PURCHASE_ORDER_ID;
import static org.folio.rest.impl.crud.CrudTestEntities.PREFIX;
import static org.folio.rest.impl.crud.CrudTestEntities.REASON_FOR_CLOSURE;
import static org.folio.rest.impl.crud.CrudTestEntities.SUFFIX;
import static org.folio.service.ProtectionService.ACQUISITIONS_UNIT_ID;
import static org.folio.service.inventory.InventoryItemManager.ITEM_PURCHASE_ORDER_LINE_IDENTIFIER;
import static org.folio.service.inventory.InventoryUtils.HOLDINGS_RECORDS;
import static org.folio.service.inventory.InventoryUtils.ITEMS;
import static org.folio.service.inventory.InventoryUtils.REQUESTS;
import static org.folio.service.inventory.InventoryManagerTest.HOLDING_INSTANCE_ID_2_HOLDING;
import static org.folio.service.inventory.InventoryManagerTest.NEW_LOCATION_ID;
import static org.folio.service.inventory.InventoryManagerTest.NON_EXISTED_NEW_HOLDING_ID;
import static org.folio.service.inventory.InventoryManagerTest.OLD_LOCATION_ID;
import static org.folio.service.inventory.InventoryManagerTest.ONLY_NEW_HOLDING_EXIST_ID;

import io.vertx.core.MultiMap;

import java.io.IOException;
import java.nio.file.NoSuchFileException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.ws.rs.core.Response;

import io.vertx.core.json.Json;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.HttpStatus;
import org.folio.Organization;
import org.folio.OrganizationCollection;
import org.folio.helper.BaseHelper;
import org.folio.isbn.IsbnUtil;
import org.folio.rest.RestVerticle;
import org.folio.rest.acq.model.Alert;
import org.folio.rest.acq.model.OrderInvoiceRelationshipCollection;
import org.folio.rest.acq.model.Piece;
import org.folio.rest.acq.model.PieceCollection;
import org.folio.rest.acq.model.ReportingCode;
import org.folio.rest.acq.model.SequenceNumber;
import org.folio.rest.acq.model.SequenceNumbers;
import org.folio.rest.acq.model.Title;
import org.folio.rest.acq.model.TitleCollection;
import org.folio.rest.acq.model.finance.Batch;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.BudgetCollection;
import org.folio.rest.acq.model.finance.BudgetExpenseClassCollection;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.acq.model.finance.ExpenseClassCollection;
import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.FundCollection;
import org.folio.rest.acq.model.finance.Ledger;
import org.folio.rest.acq.model.finance.LedgerCollection;
import org.folio.rest.acq.model.finance.Metadata;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.acq.model.invoice.FundDistribution;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.acq.model.invoice.InvoiceLineCollection;
import org.folio.rest.acq.model.tag.Tag;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.AcquisitionMethod;
import org.folio.rest.jaxrs.model.AcquisitionMethodCollection;
import org.folio.rest.jaxrs.model.AcquisitionsUnit;
import org.folio.rest.jaxrs.model.AcquisitionsUnitCollection;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembership;
import org.folio.rest.jaxrs.model.AcquisitionsUnitMembershipCollection;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.Errors;
import org.folio.rest.jaxrs.model.OrderTemplate;
import org.folio.rest.jaxrs.model.OrderTemplateCollection;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.PoLineCollection;
import org.folio.rest.jaxrs.model.Prefix;
import org.folio.rest.jaxrs.model.PrefixCollection;
import org.folio.rest.jaxrs.model.ProfileSnapshotWrapper;
import org.folio.rest.jaxrs.model.PurchaseOrder;
import org.folio.rest.jaxrs.model.PurchaseOrderCollection;
import org.folio.rest.jaxrs.model.ReasonForClosure;
import org.folio.rest.jaxrs.model.ReasonForClosureCollection;
import org.folio.rest.jaxrs.model.RoutingList;
import org.folio.rest.jaxrs.model.RoutingListCollection;
import org.folio.rest.jaxrs.model.Suffix;
import org.folio.rest.jaxrs.model.SuffixCollection;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Lists;
import com.google.common.collect.Table;

import io.restassured.http.Header;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.http.HttpServer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import one.util.streamex.StreamEx;

public class MockServer {

  private static final Logger logger = LogManager.getLogger();

  // Mock data paths
  public static final String BASE_MOCK_DATA_PATH = "mockdata/";
  private static final String CONTRIBUTOR_NAME_TYPES_PATH = BASE_MOCK_DATA_PATH + "contributorNameTypes/contributorPersonalNameType.json";
  public static final String CONFIG_MOCK_PATH = BASE_MOCK_DATA_PATH + "configurations.entries/%s.json";
  public static final String LOAN_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "loanTypes/";
  public static final String LEDGER_FY_ROLLOVERS_PATH = BASE_MOCK_DATA_PATH + "ledgerFyRollovers/";
  public static final String LEDGER_FY_ROLLOVERS_ERRORS_PATH = BASE_MOCK_DATA_PATH + "ledgerFyRolloverErrors/";
  public static final String LEDGER_FY_ROLLOVERS_PROGRESS_PATH = BASE_MOCK_DATA_PATH + "ledgerFyRolloverProgress/";
  public static final String EXPORT_HISTORY_PATH_TO_SAMPLES = BASE_MOCK_DATA_PATH + "exportHistory/";
  public static final String ITEMS_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "itemsRecords/";
  public static final String INSTANCE_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceTypes/";
  public static final String BUDGET_EXPENSE_CLASSES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "budgetExpenseClasses/budget_expense_classes.json";
  private static final String EXPENSE_CLASSES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "expenseClasses/expense_classes.json";
  public static final String INSTANCE_STATUSES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instanceStatuses/";
  private static final String INSTANCE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "instances/";
  private static final String HOLDINGS_SOURCE_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "holdingsSources/";
  public static final String PIECE_RECORDS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "pieces/";
  public static final String PO_LINES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "lines/";
  public static final String ROUTING_LISTS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "routingLists/";
  public static final String USERS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "users/";
  public static final String TITLES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "titles/";
  private static final String ACQUISITIONS_UNITS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "acquisitionsUnits/units";
  private static final String ORDER_TEMPLATES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "orderTemplates/";
  private static final String RECEIVING_HISTORY_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "receivingHistory/";
  private static final String ORGANIZATIONS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "organizations/";
  public static final String PO_LINES_COLLECTION = PO_LINES_MOCK_DATA_PATH + "po_line_collection.json";
  public static final String PIECES_COLLECTION = PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecordsCollection.json";
  public static final String ORGANIZATION_COLLECTION = ORGANIZATIONS_MOCK_DATA_PATH + "organizations.json";
  private static final String IDENTIFIER_TYPES_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "identifierTypes/";
  private static final String ITEM_REQUESTS_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "itemRequests/";
  public static final String ACQUISITIONS_UNITS_COLLECTION = ACQUISITIONS_UNITS_MOCK_DATA_PATH + "/units.json";
  public static final String ACQUISITIONS_MEMBERSHIPS_COLLECTION = ACQUISITIONS_UNITS_MOCK_DATA_PATH + "/memberships.json";
  public static final String ACQUISITION_METHOD_MOCK_DATA_PATH = BASE_MOCK_DATA_PATH + "acquisitionMethods/";
  public static final String ACQUISITION_METHODS_COLLECTION = ACQUISITION_METHOD_MOCK_DATA_PATH + "acquisitionMethods.json";
  static final String ORDER_TEMPLATES_COLLECTION = ORDER_TEMPLATES_MOCK_DATA_PATH + "/orderTemplates.json";
  private static final String FUNDS_PATH = BASE_MOCK_DATA_PATH + "funds/funds.json";
  public static final String TITLES_PATH = BASE_MOCK_DATA_PATH + "titles/titles.json";
  private static final String ROUTING_LISTS_PATH = BASE_MOCK_DATA_PATH + "routing-lists/routing-lists.json";
  public static final String BUDGETS_PATH = BASE_MOCK_DATA_PATH + "budgets/budgets.json";
  public static final String LEDGERS_PATH = BASE_MOCK_DATA_PATH + "ledgers/ledgers.json";
  public static final String PATCH_ORDER_LINES_REQUEST_PATCH = BASE_MOCK_DATA_PATH + "patchOrderLines/patch.json";
  public static final String ENCUMBRANCE_PATH = BASE_MOCK_DATA_PATH + "encumbrances/valid_encumbrances.json";
  public static final String ENCUMBRANCE_FOR_TAGS_PATH = BASE_MOCK_DATA_PATH + "encumbrances/encumbrance_for_tags_inheritance.json";
  public static final String HOLDINGS_PATH = BASE_MOCK_DATA_PATH + "holdingsRecords/holdingRecords.json";
  public static final String HOLDINGS_OLD_NEW_PATH = BASE_MOCK_DATA_PATH + "holdingsRecords/holdingRecords-old-new.json";
  public static final String LISTED_PRINT_MONOGRAPH_ENCUMBRANCES_PATH = BASE_MOCK_DATA_PATH +
    "encumbrances/encumbrance_for_print_monograph.json";
  public static final String ECS_CONSORTIUM_PURCHASE_ORDER_JSON = BASE_MOCK_DATA_PATH + "ecs/%s/consortium_purchase_order.json";
  public static final String ECS_CONSORTIUM_PO_LINE_JSON = BASE_MOCK_DATA_PATH + "ecs/%s/consortium_po_line.json";
  public static final String ECS_CONSORTIUM_TITLES_JSON = BASE_MOCK_DATA_PATH + "ecs/%s/consortium_titles.json";
  public static final String ECS_CONSORTIUM_PIECES_JSON = BASE_MOCK_DATA_PATH + "ecs/%s/consortium_pieces.json";
  public static final String ECS_UNIVERSITY_INSTANCE_JSON = BASE_MOCK_DATA_PATH + "ecs/%s/university_instance.json";
  public static final String ECS_UNIVERSITY_HOLDINGS_RECORD_JSON = BASE_MOCK_DATA_PATH + "ecs/%s/university_holdings_record.json";
  public static final String ECS_UNIVERSITY_ITEM_JSON = BASE_MOCK_DATA_PATH + "ecs/%s/university_item.json";

  private static final String PENDING_VENDOR_ID = "160501b3-52dd-41ec-a0ce-17762e7a9b47";
  static final String ORDER_ID_WITH_PO_LINES = "ab18897b-0e40-4f31-896b-9c9adc979a87";
  private static final String PIECE_POLINE_CONSISTENT_RECEIPT_STATUS_ID = "7d0aa803-a659-49f0-8a95-968f277c87d7";
  private static final String PIECE_POLINE_CONSISTENCY_404_POLINE_NOT_FOUND_ID = "5b454292-6aaa-474f-9510-b59a564e0c8d";
  public static final String PO_LINES_EMPTY_COLLECTION_ID = "0d729209-6a09-4b78-b9cb-7c225bf0c06c";
  static final String PO_NUMBER_VALUE = "228D126";

  private static final String PO_NUMBER_ERROR_TENANT = "po_number_error_tenant";
  static final String FUND_CANNOT_BE_PAID_TENANT = "Fund cannot be paid tenant";
  static final String BUDGET_IS_INACTIVE_TENANT = "Cannot create encumbrance from the not active budget";
  static final String LEDGER_NOT_FOUND_FOR_TRANSACTION_TENANT = "Ledger not found for transaction";
  static final String BUDGET_NOT_FOUND_FOR_TRANSACTION_TENANT = "Budget not found for transaction";
  static final Header PO_NUMBER_ERROR_X_OKAPI_TENANT = new Header(OKAPI_HEADER_TENANT, PO_NUMBER_ERROR_TENANT);

  private static final String TOTAL_RECORDS = "totalRecords";
  private static final String QUERY = "query";
  public static final String ITEM_RECORDS = "itemRecords";
  private static final String INSTANCE_RECORD = "instanceRecord";
  private static final String HOLDINGS_RECORD = "holdingRecord";
  private static final String CONTRIBUTOR_NAME_TYPES = "contributorNameTypes";
  private static final String INSTANCE_TYPES = "instanceTypes";
  private static final String INSTANCE_STATUSES = "instanceStatuses";
  private static final String IDENTIFIER_TYPES = "identifierTypes";
  private static final String HOLDINGS_SOURCES = "holdingsRecordsSources";
  private static final String ISBN_CONVERT13 = "ISBN13";
  private static final String HOLDING_PERMANENT_LOCATION_ID = "permanentLocationId";
  private static final String ORGANIZATIONS = "organizations";
  static final String LOAN_TYPES = "loantypes";
  static final String IS_DELETED_PROP = "isDeleted";
  static final String ALL_UNITS_CQL = IS_DELETED_PROP + "=*";
  public static final String OLD_HOLDING_ID = "758258bc-ecc1-41b8-abca-f7b610822ffd";
  public static final String NEW_HOLDING_ID = "fcd64ce1-6995-48f0-840e-89ffa2288371";
  public static final String CONFIGS = "configs";
  public static final String JOB_EXECUTIONS = "jobExecutions";
  public static final String IF_EQUAL_STR = "==";
  private static final String ITEM_HOLDINGS_RECORD_ID = "holdingsRecordId";
  public static final String ORDER_ID_DUPLICATION_ERROR_USER_ID = "b711da5e-c84f-4cb3-9978-1d00500e7707";
  public static final String CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL = "8137de83-76c0-4d3e-bf73-416de5e780fa";
  public static final String CONSISTENT_ECS_PURCHASE_ORDER_ID_ELECTRONIC = "01c8d44a-dc73-4bca-a4d1-ef28bdfb9275";
  public static final String CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL_SINGLE_ITEM = "b25f8ef6-04c4-4290-8531-9bbcefeb8c11";
  public static final String CONSISTENT_ECS_PURCHASE_ORDER_ID_PHYSICAL_MULTIPLE_ITEMS = "0c9a0e56-3518-4f0c-bfbb-98cc47b07f6c";
  public static final String MOCK_DATA_LOCATIONS_JSON = "mockdata/locations/locations.json";
  public static final String MOCK_DATA_MATERIAL_TYPES_JSON = "mockdata/material-types/material-types.json";
  public static final String MOCK_DATA_WRAPPER_PIECES_JSON = "mockdata/wrapper-pieces/wrapper-pieces.json";
  public static final String MOCK_DATA_WRAPPER_PIECES_BY_ID_JSON = "mockdata/wrapper-pieces/wrapper-pieces-by-id.json";

  public static Table<String, HttpMethod, List<JsonObject>> serverRqRs = HashBasedTable.create();
  public static HashMap<String, List<String>> serverRqQueries = new HashMap<>();

  private final int port;
  private final Vertx vertx;

  public MockServer(int port) {
    this.port = port;
    this.vertx = Vertx.vertx();
  }

  public static void addMockTitles(List<CompositePoLine> poLines) {
    poLines.stream()
      .filter(line -> !line.getIsPackage() && isNotEmpty(line.getId()))
      .forEach(line -> addMockEntry(TITLES, getTitle(line)));
  }

  public static void addMockOrderData(List<CompositePoLine> poLines) {
    poLines.stream()
      .filter(poLine -> !poLine.getIsPackage() && isNotEmpty(poLine.getId()))
      .forEach(poLine -> {
        addMockEntry(PURCHASE_ORDER_STORAGE, getMinimalOrder(poLine));
        addMockEntry(PO_LINES_STORAGE, poLine);
        addMockEntry(TITLES, getTitle(poLine));
      });
  }

  public static void addMockTitleWithId(CompositePoLine poLine, String titleId) {
    org.folio.rest.jaxrs.model.Title newTitle = getTitle(poLine);
    newTitle.setId(titleId);
    addMockEntry(TITLES, newTitle);
  }

  public void start() throws InterruptedException, ExecutionException, TimeoutException {
    // Setup Mock Server...
    HttpServer server = vertx.createHttpServer();
    Promise<HttpServer> deploymentComplete = Promise.promise();
    server.requestHandler(defineRoutes()).listen(port, result -> {
      if (result.succeeded()) {
        deploymentComplete.complete(result.result());
      } else {
        deploymentComplete.fail(result.cause());
      }
    });
    deploymentComplete.future().toCompletionStage().toCompletableFuture().get(60, TimeUnit.SECONDS);
  }

  public void close() {
    vertx.close(res -> {
      if (res.failed()) {
        logger.error("Failed to shut down mock server", res.cause());
        fail(res.cause().getMessage());
      } else {
        logger.info("Successfully shut down mock server");
      }
    });
  }

  public static List<JsonObject> getPoLineUpdates() {
    return serverRqRs.get(PO_LINES_STORAGE, HttpMethod.PUT);
  }

  public static List<JsonObject> getPoLineBatchUpdates() {
    try {
      sleep(500);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
    return serverRqRs.get(PO_LINES_BATCH_STORAGE, HttpMethod.PUT);
  }

  public static List<JsonObject> getPoLineSearches() {
    return serverRqRs.get(PO_LINES_STORAGE, HttpMethod.GET);
  }

  public static List<JsonObject> getPurchaseOrderRetrievals() {
    return serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.GET);
  }

  public static List<JsonObject> getPurchaseOrderUpdates() {
    return serverRqRs.get(PURCHASE_ORDER_STORAGE, HttpMethod.PUT);
  }

  public static List<JsonObject> getOrganizationSearches() {
    return serverRqRs.get(ORGANIZATION_STORAGE, HttpMethod.SEARCH);
  }

  public static List<JsonObject> getDataExportSpringJobCreations() {
    return serverRqRs.get(DATA_EXPORT_SPRING_CREATE_JOB, HttpMethod.POST);
  }

  public static List<JsonObject> getPieceUpdates() {
    return serverRqRs.get(PIECES_STORAGE, HttpMethod.PUT);
  }

  public static List<JsonObject> getPieceBatchUpdates() {
    return serverRqRs.get(PIECES_STORAGE_BATCH, HttpMethod.PUT);
  }

  public static List<JsonObject> getPieceDeletions() {
    return serverRqRs.get(PIECES_STORAGE, HttpMethod.DELETE);
  }

  public static List<JsonObject> getHoldingsSearches() {
    return serverRqRs.get(HOLDINGS_RECORD, HttpMethod.GET);
  }

  public static List<JsonObject> getCreatedHoldings() {
    return serverRqRs.get(HOLDINGS_RECORD, HttpMethod.POST);
  }

  public static List<JsonObject> getInstancesSearches() {
    return serverRqRs.get(INSTANCE_RECORD, HttpMethod.GET);
  }

  public static List<JsonObject> getCreatedInstances() {
    return serverRqRs.get(INSTANCE_RECORD, HttpMethod.POST);
  }

  public static List<JsonObject> getItemsSearches() {
    return serverRqRs.get(ITEM_RECORDS, HttpMethod.GET);
  }

  public static List<JsonObject> getItemUpdates() {
    return serverRqRs.get(ITEM_RECORDS, HttpMethod.PUT);
  }

  public static List<JsonObject> getItemDeletions() {
    return serverRqRs.get(ITEM_RECORDS, HttpMethod.DELETE);
  }

  public static List<JsonObject> getCreatedItems() {
    return serverRqRs.get(ITEM_RECORDS, HttpMethod.POST);
  }

  public static List<JsonObject> getCreatedPieces() {
    return serverRqRs.get(PIECES_STORAGE, HttpMethod.POST);
  }

  public static List<JsonObject> getCreatedPiecesBatch() {
    return serverRqRs.get(PIECES_STORAGE_BATCH, HttpMethod.POST);
  }

  public static List<JsonObject> getPieceSearches() {
    return serverRqRs.get(PIECES_STORAGE, HttpMethod.GET);
  }

  static List<JsonObject> getTitlesSearches() {
    return serverRqRs.get(TITLES, HttpMethod.GET);
  }

  static List<JsonObject> getUpdatedTitles() {
    return serverRqRs.get(TITLES, HttpMethod.PUT);
  }

  static List<JsonObject> getLoanTypesSearches() {
    return serverRqRs.get(LOAN_TYPES, HttpMethod.GET);
  }

  static List<JsonObject> getInstanceStatusesSearches() {
    return serverRqRs.get(INSTANCE_STATUSES, HttpMethod.GET);
  }

  static List<JsonObject> getContributorNameTypesSearches() {
    return serverRqRs.get(CONTRIBUTOR_NAME_TYPES, HttpMethod.GET);
  }

  static List<JsonObject> getInstanceTypesSearches() {
    return serverRqRs.get(INSTANCE_TYPES, HttpMethod.GET);
  }

  public static List<JsonObject> getAcqUnitsSearches() {
    return getCollectionRecords(getRqRsEntries(HttpMethod.GET, ACQUISITIONS_UNITS));
  }

  public static List<JsonObject> getAcqUnitsRetrievals() {
    return getRecordsByIds(getRqRsEntries(HttpMethod.GET, ACQUISITIONS_UNITS));
  }

  public static List<JsonObject> getAcqMembershipsSearches() {
    return getCollectionRecords(getRqRsEntries(HttpMethod.GET, ACQUISITIONS_MEMBERSHIPS));
  }

  public static List<Transaction> getCreatedEncumbrances() {
    List<JsonObject> jsonObjects = serverRqRs.get(FINANCE_BATCH_TRANSACTIONS, HttpMethod.POST);
    return jsonObjects == null ? Collections.emptyList()
      : jsonObjects.stream()
      .map(json -> json.mapTo(Batch.class))
      .map(Batch::getTransactionsToCreate)
      .flatMap(Collection::stream)
      .toList();
  }

  public static List<Transaction> getUpdatedTransactions() {
    List<JsonObject> jsonObjects = serverRqRs.get(FINANCE_BATCH_TRANSACTIONS, HttpMethod.POST);
    return jsonObjects == null ? Collections.emptyList()
      : jsonObjects.stream()
      .map(json -> json.mapTo(Batch.class))
      .map(Batch::getTransactionsToUpdate)
      .flatMap(Collection::stream)
      .toList();
  }

  private static List<JsonObject> getCollectionRecords(List<JsonObject> entries) {
    return entries.stream()
      .filter(json -> !json.containsKey(ID))
      .collect(toList());
  }

  private static List<JsonObject> getRecordsByIds(List<JsonObject> entries) {
    return entries.stream()
      .filter(json -> json.containsKey(ID))
      .collect(toList());
  }

  public static void release() {
    serverRqRs.clear();
    serverRqQueries.clear();
  }

  public static List<String> getQueryParams(String resourceType) {
    return serverRqQueries.getOrDefault(resourceType, Collections.emptyList());
  }

  public static void addMockEntry(String objName, Object data) {
    addServerRqRsData(HttpMethod.SEARCH, objName, (data instanceof JsonObject) ? (JsonObject) data : JsonObject.mapFrom(data));
  }

  private Optional<JsonObject> getMockEntry(String objName, String id) {
    return getRqRsEntries(HttpMethod.SEARCH, objName).stream()
      .filter(obj -> id.equals(obj.getString(BaseHelper.ID)))
      .findAny();
  }

  private <T> Optional<List<T>> getMockEntries(String objName, Class<T> tClass) {
    List<T> entryList = getRqRsEntries(HttpMethod.SEARCH, objName).stream()
      .map(entries -> entries.mapTo(tClass))
      .collect(toList());
    return Optional.ofNullable(entryList.isEmpty() ? null : entryList);
  }

  public static List<JsonObject> getRqRsEntries(HttpMethod method, String objName) {
    List<JsonObject> entries = serverRqRs.get(objName, method);
    if (entries == null) {
      entries = new ArrayList<>();
    }
    return entries;
  }


  private Router defineRoutes() {
    Router router = Router.router(vertx);
    router.route().handler(BodyHandler.create());
    // POST
    router.post(resourcesPath(PURCHASE_ORDER_STORAGE)).handler(this::handlePostPurchaseOrder);
    router.post("/inventory/instances").handler(this::handlePostInstanceRecord);
    router.post("/item-storage/items").handler(this::handlePostItemStorRecord);
    router.post("/holdings-storage/holdings").handler(this::handlePostHoldingRecord);
    router.post(resourcesPath(PO_LINES_STORAGE)).handler(this::handlePostPOLine);
    router.post(resourcesPath(ALERTS)).handler(ctx -> handlePostGenericSubObj(ctx, ALERTS));
    router.post(resourcesPath(REPORTING_CODES)).handler(ctx -> handlePostGenericSubObj(ctx, REPORTING_CODES));
    router.post(resourcesPath(PIECES_STORAGE)).handler(ctx -> handlePostGenericSubObj(ctx, PIECES_STORAGE));
    router.post(resourcesPath(PIECES_STORAGE_BATCH)).handler(this::handlePostPiecesBatch);
    router.post(resourcesPath(ORDER_TEMPLATES)).handler(ctx -> handlePostGenericSubObj(ctx, ORDER_TEMPLATES));
    router.post(resourcesPath(FINANCE_BATCH_TRANSACTIONS)).handler(this::handleBatchTransactions);
    router.post(resourcesPath(TITLES)).handler(ctx -> handlePostGenericSubObj(ctx, TITLES));
    router.post(resourcesPath(ROUTING_LISTS)).handler(ctx -> handlePostGenericSubObj(ctx, ROUTING_LISTS));
    router.post(resourcesPath(ACQUISITIONS_UNITS)).handler(ctx -> handlePostGenericSubObj(ctx, ACQUISITIONS_UNITS));
    router.post(resourcesPath(ACQUISITION_METHODS)).handler(ctx -> handlePostGenericSubObj(ctx, ACQUISITION_METHODS));
    router.post(resourcesPath(ACQUISITIONS_MEMBERSHIPS)).handler(ctx -> handlePostGenericSubObj(ctx, ACQUISITIONS_MEMBERSHIPS));
    router.post(resourcesPath(REASONS_FOR_CLOSURE)).handler(ctx -> handlePostGenericSubObj(ctx, REASONS_FOR_CLOSURE));
    router.post(resourcesPath(PREFIXES)).handler(ctx -> handlePostGenericSubObj(ctx, PREFIXES));
    router.post(resourcesPath(SUFFIXES)).handler(ctx -> handlePostGenericSubObj(ctx, SUFFIXES));
    router.post(resourcesPath(TAGS)).handler(ctx -> handlePostGenericSubObj(ctx, TAGS));
    router.post(resourcesPath(DATA_EXPORT_SPRING_CREATE_JOB)).handler(ctx -> handlePostGenericSubObj(ctx, DATA_EXPORT_SPRING_CREATE_JOB));
    router.post(resourcesPath(DATA_EXPORT_SPRING_EXECUTE_JOB)).handler(ctx -> handlePostGenericSubObj(ctx, DATA_EXPORT_SPRING_EXECUTE_JOB));
    // GET
    router.get(resourcePath(PURCHASE_ORDER_STORAGE)).handler(this::handleGetPurchaseOrderById);
    router.get(resourcesPath(PURCHASE_ORDER_STORAGE)).handler(this::handleGetPurchaseOrderByQuery);
    router.get("/instance-types").handler(this::handleGetInstanceType);
    router.get("/instance-statuses").handler(this::handleGetInstanceStatus);
    router.get("/inventory/instances").handler(this::handleGetInstanceRecord);
    router.get("/inventory/instances/:id").handler(this::handleGetInstanceRecordById);
    router.get("/item-storage/items").handler(this::handleGetItemRecordsFromStorage);
    router.get("/inventory/items").handler(this::handleGetInventoryItemRecords);
    router.get("/inventory/items/:id").handler(this::handleGetInventoryItemRecordById);
    router.get("/holdings-storage/holdings").handler(this::handleGetHoldingsRecords);
    router.get("/holdings-storage/holdings/:id").handler(this::handleGetHolding);
    router.get("/loan-types").handler(this::handleGetLoanType);
    router.get("/organizations-storage/organizations/:id").handler(this::getOrganizationById);
    router.get("/organizations-storage/organizations").handler(this::handleGetAccessProviders);
    router.get("/identifier-types").handler(this::handleGetIdentifierType);
    router.get("/holdings-sources").handler(this::handleGetHoldingsSource);
    router.get("/circulation/requests").handler(this::handleGetItemRequests);
    router.get(resourcesPath(PO_LINES_STORAGE)).handler(this::handleGetPoLines);
    router.get(resourcePath(PO_LINES_STORAGE)).handler(this::handleGetPoLineById);
    router.get(resourcePath(ALERTS)).handler(ctx -> handleGetGenericSubObj(ctx, ALERTS));
    router.get(resourcePath(REPORTING_CODES)).handler(ctx -> handleGetGenericSubObj(ctx, REPORTING_CODES));
    router.get(resourcesPath(PO_NUMBER)).handler(this::handleGetPoNumber);
    router.get(resourcesPath(PIECES_STORAGE)).handler(this::handleGetPieces);
    router.get(resourcePath(PIECES_STORAGE)).handler(this::handleGetPieceById);
    router.get(resourcesPath(RECEIVING_HISTORY)).handler(this::handleGetReceivingHistory);
    router.get(resourcesPath(PO_LINE_NUMBER)).handler(this::handleGetPoLineNumber);
    router.get("/contributor-name-types").handler(this::handleGetContributorNameTypes);
    router.get(resourcesPath(ACQUISITIONS_UNITS)).handler(this::handleGetAcquisitionsUnits);
    router.get(resourcePath(ACQUISITIONS_UNITS)).handler(this::handleGetAcquisitionsUnit);
    router.get(resourcesPath(ACQUISITIONS_MEMBERSHIPS)).handler(this::handleGetAcquisitionsMemberships);
    router.get(resourcePath(ACQUISITIONS_MEMBERSHIPS)).handler(this::handleGetAcquisitionsMembership);
    router.get("/isbn/convertTo13").handler(this::handleGetIsbnConverter);
    router.get(resourcePath(ORDER_TEMPLATES)).handler(ctx -> handleGetGenericSubObj(ctx, ORDER_TEMPLATES));
    router.get(resourcesPath(ORDER_TEMPLATES)).handler(this::handleGetOrderTemplates);
    router.get("/finance/ledgers/:id/current-fiscal-year").handler(this::handleGetCurrentFiscalYearByLedgerId);
    router.get("/finance/fiscal-years/:id").handler(this::handleGetCurrentFiscalYearByLedgerId);
    router.get("/finance-storage/budget-expense-classes").handler(this::handleGetBudgetExpenseClass);
    router.get(resourcesPath(EXPENSE_CLASSES_URL)).handler(this::handleGetExpenseClasses);
    router.get(resourcesPath(FUNDS)).handler(this::handleGetFunds);
    router.get(resourcePath(FUNDS)).handler(this::handleGetFundById);
    router.get(resourcesPath(BUDGETS)).handler(this::handleGetBudgets);
    router.get(resourcesPath(LEDGERS)).handler(this::handleGetLedgers);
    router.get(resourcesPath(TITLES)).handler(this::handleGetTitles);
    router.get(resourcePath(TITLES)).handler(this::handleGetOrderTitleById);
    router.get(resourcesPath(ROUTING_LISTS)).handler(this::handleGetRoutingLists);
    router.get(resourcePath(ROUTING_LISTS)).handler(this::handleGetRoutingListById);
    router.get(resourcesPath(REASONS_FOR_CLOSURE)).handler(ctx -> handleGetGenericSubObjs(ctx, REASONS_FOR_CLOSURE));
    router.get(resourcesPath(PREFIXES)).handler(ctx -> handleGetGenericSubObjs(ctx, PREFIXES));
    router.get(resourcesPath(SUFFIXES)).handler(ctx -> handleGetGenericSubObjs(ctx, SUFFIXES));
    router.get(resourcePath(REASONS_FOR_CLOSURE)).handler(ctx -> handleGetGenericSubObj(ctx, REASONS_FOR_CLOSURE));
    router.get(resourcePath(PREFIXES)).handler(ctx -> handleGetGenericSubObj(ctx, PREFIXES));
    router.get(resourcePath(SUFFIXES)).handler(ctx -> handleGetGenericSubObj(ctx, SUFFIXES));
    router.get(resourcesPath(TRANSACTIONS_ENDPOINT)).handler(this::handleTransactionGetEntry);
    router.get(resourcesPath(USER_TENANTS_ENDPOINT)).handler(this::handleUserTenantsGetEntry);
    router.get("/finance/funds/:id/budget").handler(this::handleGetBudgetByFinanceId);
    router.get(resourcesPath(FINANCE_EXCHANGE_RATE)).handler(this::handleGetRateOfExchange);
    router.get(resourcesPath(LEDGER_FY_ROLLOVERS)).handler(this::handleGetFyRollovers);
    router.get("/finance/ledger-rollovers-progress").handler(this::handleGetFyRolloverProgress);
    router.get(resourcesPath(LEDGER_FY_ROLLOVER_ERRORS)).handler(this::handleGetFyRolloverErrors);
    router.get(resourcesPath(ORDER_INVOICE_RELATIONSHIP)).handler(this::handleGetOrderInvoiceRelationship);
    router.get(resourcesPath(TAGS)).handler(ctx -> handleGetGenericSubObj(ctx, TAGS));
    router.get("/invoice/invoice-lines").handler(this::handleGetInvoiceLines);
    router.get(resourcesPath(ACQUISITION_METHODS)).handler(this::handleGetAcquisitionMethods);
    router.get(resourcePath(ACQUISITION_METHODS)).handler(this::handleGetAcquisitionMethod);
    router.get(resourcesPath(EXPORT_HISTORY)).handler(this::handleGetExportHistoryMethod);
    router.get("/data-import-profiles/jobProfileSnapshots/:id").handler(this::handleGetJobProfileSnapshotById);
    router.get("/change-manager/jobExecutions/:id").handler(this::handleGetJobExecutionById);
    router.get("/organizations/organizations").handler(this::handleGetOrganizations);
    router.get("/locations").handler(ctx -> handleGetJsonResource(ctx, MOCK_DATA_LOCATIONS_JSON));
    router.get("/material-types").handler(ctx -> handleGetJsonResource(ctx, MOCK_DATA_MATERIAL_TYPES_JSON));
    router.get(resourcesPath(WRAPPER_PIECES_STORAGE)).handler(ctx -> handleGetJsonResource(ctx, MOCK_DATA_WRAPPER_PIECES_JSON));
    router.get(resourcesPath(WRAPPER_PIECES_STORAGE) + "/:id").handler(ctx -> handleGetJsonResource(ctx, MOCK_DATA_WRAPPER_PIECES_BY_ID_JSON));
    // PUT
    router.put(resourcePath(PURCHASE_ORDER_STORAGE)).handler(ctx -> handlePutGenericSubObj(ctx, PURCHASE_ORDER_STORAGE));
    router.put(resourcePath(PO_LINES_STORAGE)).handler(ctx -> handlePutGenericSubObj(ctx, PO_LINES_STORAGE));
    router.put(resourcesPath(PO_LINES_BATCH_STORAGE)).handler(ctx -> handlePutGenericSubObj(ctx, PO_LINES_BATCH_STORAGE));
    router.put(resourcePath(PIECES_STORAGE)).handler(ctx -> handlePutGenericSubObj(ctx, PIECES_STORAGE));
    router.put(resourcesPath(PIECES_STORAGE_BATCH)).handler(this::handlePutPiecesBatch); // without id param
    router.put(resourcePath(REPORTING_CODES)).handler(ctx -> handlePutGenericSubObj(ctx, REPORTING_CODES));
    router.put(resourcePath(ALERTS)).handler(ctx -> handlePutGenericSubObj(ctx, ALERTS));
    router.put("/inventory/items/:id").handler(ctx -> handlePutGenericSubObj(ctx, ITEM_RECORDS));
    router.put("/holdings-storage/holdings/:id").handler(ctx -> handlePutGenericSubObj(ctx, ITEM_RECORDS));
    router.put(resourcePath(ACQUISITIONS_UNITS)).handler(ctx -> handlePutGenericSubObj(ctx, ACQUISITIONS_UNITS));
    router.put(resourcePath(ACQUISITIONS_MEMBERSHIPS)).handler(ctx -> handlePutGenericSubObj(ctx, ACQUISITIONS_MEMBERSHIPS));
    router.put(resourcePath(ORDER_TEMPLATES)).handler(ctx -> handlePutGenericSubObj(ctx, ORDER_TEMPLATES));
    router.put(resourcePath(TITLES)).handler(ctx -> handlePutGenericSubObj(ctx, TITLES));
    router.put(resourcePath(ROUTING_LISTS)).handler(ctx -> handlePutGenericSubObj(ctx, ROUTING_LISTS));
    router.put(resourcePath(REASONS_FOR_CLOSURE)).handler(ctx -> handlePutGenericSubObj(ctx, REASONS_FOR_CLOSURE));
    router.put(resourcePath(PREFIXES)).handler(ctx -> handlePutGenericSubObj(ctx, PREFIXES));
    router.put(resourcePath(SUFFIXES)).handler(ctx -> handlePutGenericSubObj(ctx, SUFFIXES));
    router.put(resourcePath(ACQUISITION_METHODS)).handler(ctx -> handlePutGenericSubObj(ctx, ACQUISITION_METHODS));
    // DELETE
    router.delete(resourcePath(PURCHASE_ORDER_STORAGE)).handler(ctx -> handleDeleteGenericSubObj(ctx, PURCHASE_ORDER_STORAGE));
    router.delete(resourcePath(PO_LINES_STORAGE)).handler(ctx -> handleDeleteGenericSubObj(ctx, PO_LINES_STORAGE));
    router.delete(resourcePath(ALERTS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ALERTS));
    router.delete(resourcePath(REPORTING_CODES)).handler(ctx -> handleDeleteGenericSubObj(ctx, REPORTING_CODES));
    router.delete(resourcePath(PIECES_STORAGE)).handler(ctx -> handleDeleteGenericSubObj(ctx, PIECES_STORAGE));
    router.delete(resourcePath(ACQUISITIONS_UNITS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ACQUISITIONS_UNITS));
    router.delete(resourcePath(ACQUISITION_METHODS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ACQUISITION_METHODS));
    router.delete(resourcePath(ACQUISITIONS_MEMBERSHIPS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ACQUISITIONS_MEMBERSHIPS));
    router.delete(resourcePath(ORDER_TEMPLATES)).handler(ctx -> handleDeleteGenericSubObj(ctx, ORDER_TEMPLATES));
    router.delete(resourcePath(TITLES)).handler(ctx -> handleDeleteGenericSubObj(ctx, TITLES));
    router.delete(resourcePath(ROUTING_LISTS)).handler(ctx -> handleDeleteGenericSubObj(ctx, ROUTING_LISTS));
    router.delete(resourcePath(REASONS_FOR_CLOSURE)).handler(ctx -> handleDeleteGenericSubObj(ctx, REASONS_FOR_CLOSURE));
    router.delete(resourcePath(PREFIXES)).handler(ctx -> handleDeleteGenericSubObj(ctx, PREFIXES));
    router.delete(resourcePath(SUFFIXES)).handler(ctx -> handleDeleteGenericSubObj(ctx, SUFFIXES));
    router.delete("/inventory/items/:id").handler(ctx -> handleDeleteGenericSubObj(ctx, ITEM_RECORDS));
    router.get("/configurations/entries").handler(this::handleConfigurationModuleResponse);
    router.patch(resourcePath(PO_LINES_STORAGE)).handler(this::handlePatchOrderLines);
    return router;
  }

  private void handleGetExportHistoryMethod(RoutingContext ctx) {
    logger.info("handleGetExportHistoryMethod got: {}", ctx.request().path());
    try {
      JsonObject entries = new JsonObject(getMockData(EXPORT_HISTORY_PATH_TO_SAMPLES + "export_history_collection.json"));

      serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
      addServerRqRsData(HttpMethod.GET, "exportHistory", entries);
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(404)
        .end();
    }
  }

  private void handleGetFyRolloverProgress(RoutingContext ctx) {
    logger.info("handleGetFyRolloverProgress got: {}", ctx.request().path());
    try {
      JsonObject entries = new JsonObject(getMockData(LEDGER_FY_ROLLOVERS_PROGRESS_PATH + "ledger_fiscal_year_rollover_progress_collection.json"));

      serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
      addServerRqRsData(HttpMethod.GET, "ledgerFiscalYearRolloverProgress", entries);
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(404)
        .end();
    }
  }

  private JsonObject getTitlesByPoLineIds(List<String> poLineIds) {
    Supplier<List<Title>> getFromFile = () -> {
      try {
        return new JsonObject(getMockData(TITLES_PATH)).mapTo(TitleCollection.class).getTitles();
      } catch (IOException e) {
        return Collections.emptyList();
      }
    };

    List<Title> titles = getMockEntries(TITLES, Title.class).orElseGet(getFromFile);

    if (!poLineIds.isEmpty()) {
      titles.removeIf(item -> !poLineIds.contains(item.getPoLineId()));
    }

    Object record = new TitleCollection().withTitles(titles).withTotalRecords(titles.size());

    return JsonObject.mapFrom(record);
  }

  private JsonObject getRoutingListsByPoLineId(List<String> poLineId) {
    Supplier<List<RoutingList>> getFromFile = () -> {
      try {
        return new JsonObject(getMockData(ROUTING_LISTS_PATH)).mapTo(RoutingListCollection.class).getRoutingLists();
      } catch (IOException e) {
        return Collections.emptyList();
      }
    };

    List<RoutingList> rLists = getMockEntries(ROUTING_LISTS, RoutingList.class).orElseGet(getFromFile);

    if (!poLineId.isEmpty()) {
      rLists.removeIf(item -> !item.getPoLineId().equals(poLineId.get(0)));
    }

    Object record = new RoutingListCollection().withRoutingLists(rLists).withTotalRecords(rLists.size());
    return JsonObject.mapFrom(record);
  }

  private void handleGetFunds(RoutingContext ctx) {
    String query = StringUtils.trimToEmpty(ctx.request().getParam(QUERY));
    addServerRqQuery(FUNDS, query);
    if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        List<String> ids = Collections.emptyList();
        if (query.startsWith("id==")) {
          ids = extractIdsFromQuery(query);
        }

        JsonObject collection = getFundsByIds(ids);
        addServerRqRsData(HttpMethod.GET, FUNDS, collection);

        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(collection.encodePrettily());
      } catch (Exception e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
      }
    }
  }

  private void handleGetFundById(RoutingContext ctx) {
    logger.info("handleGetFundById got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);

    FundCollection funds = getFundsByIds(Collections.singletonList(id))
      .mapTo(FundCollection.class);

    if (funds.getTotalRecords() == 0) {
      serverResponse(ctx, 404, APPLICATION_JSON, id);
    } else {
      JsonObject fund = new JsonObject()
        .put("fund", JsonObject.mapFrom(funds.getFunds().get(0)))
        .put("groupIds", new JsonArray());
      addServerRqRsData(HttpMethod.GET, FUNDS, fund);

      serverResponse(ctx, 200, APPLICATION_JSON, fund.encodePrettily());
    }
  }

  private void handleGetBudgets(RoutingContext ctx) {
    String query = StringUtils.trimToEmpty(ctx.request().getParam(QUERY));
    addServerRqQuery(BUDGETS, query);
    if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        List<String> ids = Collections.emptyList();
        if (query.startsWith("fundId==")) {
          ids = extractfundIdsFromQuery(query);
        }

        JsonObject collection = getBudgetsByFundIds(ids);
        addServerRqRsData(HttpMethod.GET, BUDGETS, collection);

        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(collection.encodePrettily());
      } catch (Exception e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
      }
    }
  }

  private void handleGetLedgers(RoutingContext ctx) {
    String query = StringUtils.trimToEmpty(ctx.request().getParam(QUERY));
    addServerRqQuery(LEDGERS, query);
    if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        List<String> ids = Collections.emptyList();
        if (query.startsWith("id==")) {
          ids = extractIdsFromQuery(query);
        }

        JsonObject collection = getLedgersByIds(ids);
        addServerRqRsData(HttpMethod.GET, LEDGERS, collection);

        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(collection.encodePrettily());
      } catch (Exception e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
      }
    }
  }

  private JsonObject getFundsByIds(List<String> fundIds) {
    Supplier<List<Fund>> getFromFile = () -> {
      try {
        return new JsonObject(getMockData(FUNDS_PATH)).mapTo(FundCollection.class).getFunds();
      } catch (IOException e) {
        return Collections.emptyList();
      }
    };

    List<Fund> funds = getMockEntries(FUNDS, Fund.class).orElseGet(getFromFile);

    if (!fundIds.isEmpty()) {
      funds.removeIf(item -> !fundIds.contains(item.getId()));
    }

    Object record = new FundCollection().withFunds(funds).withTotalRecords(funds.size());

    return JsonObject.mapFrom(record);
  }

  private JsonObject getBudgetsByFundIds(List<String> budgetByFundIds) {
    Supplier<List<Budget>> getFromFile = () -> {
      try {
        return new JsonObject(getMockData(BUDGETS_PATH)).mapTo(BudgetCollection.class).getBudgets();
      } catch (IOException e) {
        return Collections.emptyList();
      }
    };

    List<Budget> budgets = getMockEntries(BUDGETS, Budget.class).orElseGet(getFromFile);

    if (!budgetByFundIds.isEmpty()) {
      budgets.removeIf(item -> !budgetByFundIds.contains(item.getFundId()));
    }

    Object record = new BudgetCollection().withBudgets(budgets).withTotalRecords(budgets.size());


    return JsonObject.mapFrom(record);
  }

  private JsonObject getLedgersByIds(List<String> ledgerByFundIds) {
    Supplier<List<Ledger>> getFromFile = () -> {
      try {
        return new JsonObject(getMockData(LEDGERS_PATH)).mapTo(LedgerCollection.class).getLedgers();
      } catch (IOException e) {
        return Collections.emptyList();
      }
    };

    List<Ledger> ledgers = getMockEntries(LEDGERS, Ledger.class).orElseGet(getFromFile);

    if (!ledgerByFundIds.isEmpty()) {
      ledgers.removeIf(item -> !ledgerByFundIds.contains(item.getId()));
    }

    Object record = new LedgerCollection().withLedgers(ledgers).withTotalRecords(ledgers.size());

    return JsonObject.mapFrom(record);
  }

  private void handleGetBudgetExpenseClass(RoutingContext ctx) {
    logger.info("handleGetBudgetExpenseClass: {}", ctx.request().path());
    try {
      BudgetExpenseClassCollection expenseClassCollection = new JsonObject(getMockData(BUDGET_EXPENSE_CLASSES_MOCK_DATA_PATH))
        .mapTo(BudgetExpenseClassCollection.class);

      JsonObject entries = JsonObject.mapFrom(expenseClassCollection);
      serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
    } catch (IOException exception) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    }
  }

  private void handleGetExpenseClasses(RoutingContext ctx) {
    logger.info("handleGetExpenseClasses: {}", ctx.request().path());
    try {
      ExpenseClassCollection expenseClassCollection = new JsonObject(getMockData(EXPENSE_CLASSES_MOCK_DATA_PATH))
        .mapTo(ExpenseClassCollection.class);

      JsonObject entries = JsonObject.mapFrom(expenseClassCollection);
      serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
    } catch (IOException exception) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    }
  }

  private void handleGetCurrentFiscalYearByLedgerId(RoutingContext ctx) {
    logger.info("handleGetCurrentFiscalYearByLedgerId got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);

    addServerRqRsData(HttpMethod.GET, PO_LINES_STORAGE, new JsonObject().put(ID, id));

    if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else if (ID_DOES_NOT_EXIST.equals(id)) {
      serverResponse(ctx, 404, APPLICATION_JSON, id);
    } else if (id.equals("133a7916-f05e-4df4-8f7f-09eb2a7076d1")) {
      FiscalYear fiscalYear = new FiscalYear();
      fiscalYear.setId("ac2164c7-ba3d-1bc2-a12c-e35ceccbfaf2");
      fiscalYear.setCode("test2020");
      fiscalYear.setName("test");
      fiscalYear.setCurrency("USD");
      fiscalYear.setPeriodStart(Date.from(Instant.now().minus(365, DAYS)));
      fiscalYear.setPeriodEnd(Date.from(Instant.now().plus(365, DAYS)));
      serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(fiscalYear).encodePrettily());
    } else {
      FiscalYear fiscalYear = new FiscalYear();
      fiscalYear.setId(UUID.randomUUID().toString());
      fiscalYear.setCode("test2020");
      fiscalYear.setName("test");
      fiscalYear.setCurrency("USD");
      fiscalYear.setPeriodStart(Date.from(Instant.now().minus(365, DAYS)));
      fiscalYear.setPeriodEnd(Date.from(Instant.now().plus(365, DAYS)));
      serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(fiscalYear).encodePrettily());
    }
  }

  private void handleGetPoLineNumber(RoutingContext ctx) {
    if (PO_NUMBER_ERROR_TENANT.equals(ctx.request().getHeader(OKAPI_HEADER_TENANT))) {
      ctx.response()
        .setStatusCode(500)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end();
    } else {
      SequenceNumbers seqNumbers = new SequenceNumbers();
      List<String> seqNumbersList = new ArrayList<>();
      seqNumbersList.add(PO_LINE_NUMBER_VALUE);
      seqNumbers.setSequenceNumbers(seqNumbersList);
      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(seqNumbers).encodePrettily());
    }
  }

  private void handlePostInstanceRecord(RoutingContext ctx) {
    logger.info("handlePostInstanceRecord got: {}", ctx.body().asString());
    JsonObject body = ctx.body().asJsonObject();
    addServerRqRsData(HttpMethod.POST, INSTANCE_RECORD, body);

    ctx.response()
      .setStatusCode(201)
      .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
      .putHeader(HttpHeaders.LOCATION, ctx.request().absoluteURI() + "/" + UUID.randomUUID())
      .end();
  }

  private void handlePostHoldingRecord(RoutingContext ctx) {
    logger.info("handlePostHoldingsRecord got: {}", ctx.body().asJsonObject());
    JsonObject body = ctx.body().asJsonObject();

    // the case when item creation is expected to fail for particular holding
    String id = body.getString(HOLDING_PERMANENT_LOCATION_ID).equals(ID_FOR_INTERNAL_SERVER_ERROR)
      ? ID_FOR_INTERNAL_SERVER_ERROR
      : UUID.randomUUID().toString();

    body.put(ID, id);
    addServerRqRsData(HttpMethod.POST, HOLDINGS_RECORD, body);
    ctx.response()
      .setStatusCode(201)
      .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
      .putHeader(HttpHeaders.LOCATION, ctx.request().absoluteURI() + "/" + id)
      .end(body.encode());
  }

  private void handlePostItemStorRecord(RoutingContext ctx) {
    String bodyAsString = ctx.body().toString();
    logger.info("handlePostItemRecord got: {}", bodyAsString);

    if (bodyAsString.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      JsonObject bodyAsJson = ctx.body().asJsonObject();
      bodyAsJson.put(ID, UUID.randomUUID().toString());
      addServerRqRsData(HttpMethod.POST, ITEM_RECORDS, bodyAsJson);
      ctx.response()
        .setStatusCode(201)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .putHeader(HttpHeaders.LOCATION, ctx.request().absoluteURI() + "/" + bodyAsJson.getString(ID))
        .end(bodyAsJson.encode());
    }
  }

  private void handleGetInstanceRecord(RoutingContext ctx) {
    logger.info("handleGetInstanceRecord got: {}", ctx.request().path());

    try {
      JsonObject instance;
      if (ctx.request().getParam("query").contains("ocn956625961")) {
        instance = new JsonObject(getMockData(INSTANCE_RECORDS_MOCK_DATA_PATH + "instances.json"));
      } else {
        instance = new JsonObject().put("instances", new JsonArray());
      }
      addServerRqRsData(HttpMethod.GET, INSTANCE_RECORD, instance);
      serverResponse(ctx, 200, APPLICATION_JSON, instance.encodePrettily());
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(404)
        .end();
    }
  }

  private void handleGetInstanceRecordById(RoutingContext ctx) {
    logger.info("handleGetInstanceRecordId got: {}", ctx.request().path());

    try {
      JsonObject instance = new JsonObject(getMockData(INSTANCE_RECORDS_MOCK_DATA_PATH + "instance.json"));
      addServerRqRsData(HttpMethod.GET, INSTANCE_RECORD, instance);
      serverResponse(ctx, 200, APPLICATION_JSON, instance.encodePrettily());
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(404)
        .end();
    }
  }

  private void handleGetHoldingsRecords(RoutingContext ctx) {
    logger.info("handleGetHoldingRecord got: {}", ctx.request().path());
    String queryParam = ctx.queryParam("query").get(0);
    JsonObject holdings;
    try {
      if ((queryParam.contains(OLD_HOLDING_ID) && queryParam.contains(NEW_HOLDING_ID))) {
        holdings = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH));
      } else if (queryParam.contains(OLD_LOCATION_ID) && queryParam.contains(NON_EXISTED_NEW_HOLDING_ID)) {
        List<JsonObject> holdingsList = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH)).getJsonArray("holdingsRecords").stream()
          .map(o -> ((JsonObject) o))
          .filter(holding -> holding.getString("permanentLocationId").equals(OLD_LOCATION_ID))
          .collect(toList());
        holdings = new JsonObject().put("holdingsRecords", new JsonArray(holdingsList));
      } else if (queryParam.contains(OLD_LOCATION_ID) && !queryParam.contains(NON_EXISTED_NEW_HOLDING_ID)) {
        List<JsonObject> holdingsList = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH)).getJsonArray("holdingsRecords").stream()
          .map(o -> ((JsonObject) o))
          .filter(holding -> holding.getString("permanentLocationId").equals(OLD_LOCATION_ID)
            || !holding.getString("permanentLocationId").equals(NON_EXISTED_NEW_HOLDING_ID))
          .collect(toList());
        holdings = new JsonObject().put("holdingsRecords", new JsonArray(holdingsList));
      } else {
        holdings = new JsonObject().put("holdingsRecords", new JsonArray());
      }
      if (queryParam.contains(NEW_LOCATION_ID) && queryParam.contains(ONLY_NEW_HOLDING_EXIST_ID)) {
        List<JsonObject> holdingsList = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH)).getJsonArray("holdingsRecords").stream()
          .map(o -> ((JsonObject) o))
          .filter(holding -> holding.getString("permanentLocationId").equals(NEW_LOCATION_ID))
          .collect(toList());
        holdings = new JsonObject().put("holdingsRecords", new JsonArray(holdingsList));
      }

      if (queryParam.contains(OLD_LOCATION_ID) && queryParam.contains(HOLDING_INSTANCE_ID_2_HOLDING)) {
        List<JsonObject> holdingsList = new JsonObject(getMockData(HOLDINGS_OLD_NEW_PATH)).getJsonArray("holdingsRecords")
          .stream()
          .map(JsonObject::mapFrom)
          .toList();
        List<JsonObject> doubleList = new ArrayList<>(holdingsList);
        doubleList.addAll(holdingsList);
        holdings = new JsonObject().put("holdingsRecords", new JsonArray(doubleList));
      }

      if (queryParam.startsWith("id==") && holdings.getJsonArray(HOLDINGS_RECORDS).isEmpty()) {
        List<String> ids = extractIdsFromQuery(queryParam);
        holdings = getHoldingsByIds(ids);
      }
    } catch (IOException e) {
      holdings = new JsonObject().put("holdingsRecords", new JsonArray());
    }
    addServerRqRsData(HttpMethod.GET, HOLDINGS_RECORD, holdings);
    serverResponse(ctx, 200, APPLICATION_JSON, holdings.encodePrettily());
  }

  private JsonObject getHoldingsByIds(List<String> holdingIds) {
    Supplier<List<JsonObject>> getFromFile = () -> {
      try {
        return new JsonObject(getMockData(HOLDINGS_PATH)).getJsonArray(HOLDINGS_RECORDS).stream()
          .map(obj -> (JsonObject) obj)
          .collect(Collectors.toList());
      } catch (IOException e) {
        return Collections.emptyList();
      }
    };

    List<JsonObject> holdingRecords = getMockEntries(HOLDINGS_RECORD, JsonObject.class).orElseGet(getFromFile);

    if (!holdingIds.isEmpty()) {
      holdingRecords.removeIf(item -> !holdingIds.contains(item.getString(ID)));
    }

    if (holdingRecords.isEmpty() && !holdingIds.isEmpty()) {
      try {
        String mockData = getMockData(HOLDINGS_OLD_NEW_PATH);
        holdingRecords = holdingIds.stream()
          .map(holdingId -> new JsonObject(mockData).getJsonArray("holdingsRecords").getJsonObject(0).put(ID, holdingId))
          .toList();
      } catch (IOException e) {
        holdingRecords = Collections.emptyList();
      }
    }

    Object record = new JsonObject().put("holdingsRecords", new JsonArray(holdingRecords));

    return JsonObject.mapFrom(record);
  }

  private void handleGetHolding(RoutingContext ctx) {
    logger.info("handleGetHolding got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);

    JsonObject holding = new JsonObject();
    holding.put("id", id);
    holding.put("hrid", "ho00000001");
    holding.put("holdingsItems", new JsonArray());

    addServerRqRsData(HttpMethod.GET, HOLDINGS_RECORD, holding);
    serverResponse(ctx, 200, APPLICATION_JSON, holding.encodePrettily());
  }

  private void handleGetItemRecordsFromStorage(RoutingContext ctx) {
    logger.info("handleGetItemRecordsFromStorage got: {}", ctx.request().path());

    // Attempt to find POLine in mock server memory
    List<JsonObject> itemsList = getRqRsEntries(HttpMethod.SEARCH, ITEM_RECORDS);
    JsonObject items;
    if (!itemsList.isEmpty()) {
      items = new JsonObject()
        .put("items", itemsList)
        .put(TOTAL_RECORDS, itemsList.size());
    } else {
      items = new JsonObject().put("items", new JsonArray());
    }
    addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, items);
    serverResponse(ctx, 200, APPLICATION_JSON, items.encodePrettily());
  }

  private void handleGetInventoryItemRecords(RoutingContext ctx) {
    logger.info("handleGetInventoryItemRecords got: {}", ctx.request().path());

    String query = ctx.request().getParam("query");

    addServerRqQuery(ITEM_RECORDS, query);

    if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, new JsonObject());
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else if (query.contains(ITEMS_NOT_FOUND)) {
      JsonObject items = new JsonObject();
      items.put(ITEMS, new JsonArray());
      items.put(TOTAL_RECORDS, 0);
      addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, items);

      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(items.encodePrettily());
    } else {
      try {
        JsonObject items = new JsonObject(getMockData(ITEMS_RECORDS_MOCK_DATA_PATH + "inventoryItemsCollection.json"));
        JsonArray jsonArray = items.getJsonArray(ITEMS);

        if (query.startsWith("id==")) {
          List<String> itemIds = extractIdsFromQuery(query);
          final Iterator<Object> iterator = jsonArray.iterator();
          while (iterator.hasNext()) {
            JsonObject item = (JsonObject) iterator.next();
            if (!itemIds.contains(item.getString(ID))) {
              iterator.remove();
            }
          }
        } else if (query.contains(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER + IF_EQUAL_STR) && query.contains(ITEM_HOLDINGS_RECORD_ID + IF_EQUAL_STR)) {
          int lineIndex = query.indexOf(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER) + ITEM_PURCHASE_ORDER_LINE_IDENTIFIER.length() + 2;
          String purchaseOrderLineIdentifier = query.substring(lineIndex, lineIndex + 36);
          int holdingIndex = query.indexOf(ITEM_HOLDINGS_RECORD_ID) + ITEM_HOLDINGS_RECORD_ID.length() + 2;
          String holdingsRecordId = query.substring(holdingIndex, holdingIndex + 36);
          final Iterator<Object> iterator = jsonArray.iterator();
          while (iterator.hasNext()) {
            JsonObject item = (JsonObject) iterator.next();

            if (!purchaseOrderLineIdentifier.equals(item.getString(ITEM_PURCHASE_ORDER_LINE_IDENTIFIER))
              || !holdingsRecordId.equals(item.getString(ITEM_HOLDINGS_RECORD_ID))) {
              iterator.remove();
            }
          }
        }

        items.put(TOTAL_RECORDS, jsonArray.size());
        addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, items);

        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(items.encodePrettily());
      } catch (Exception e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
      }
    }
  }

  private void handleGetInventoryItemRecordById(RoutingContext ctx) {
    logger.info("handleGetInventoryItemRecordById got: {}", ctx.request().path());

    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);

    if (id.equals(ID_FOR_INTERNAL_SERVER_ERROR)) {
      ctx.response().setStatusCode(500).end();
    } else if (id.equals(ITEMS_NOT_FOUND)) {
      ctx.response().setStatusCode(404).end();
    } else {
      try {
        JsonObject items = new JsonObject(getMockData(ITEMS_RECORDS_MOCK_DATA_PATH + "inventoryItemsCollection.json"));
        JsonArray jsonArray = items.getJsonArray(ITEMS);

        final Iterator<Object> iterator = jsonArray.iterator();
        while (iterator.hasNext()) {
          JsonObject item = (JsonObject) iterator.next();
          if (!id.equals(item.getString(ID))) {
            iterator.remove();
          }
        }

        if (!jsonArray.isEmpty()) {
          items.put(TOTAL_RECORDS, jsonArray.size());
          addServerRqRsData(HttpMethod.GET, ITEM_RECORDS, jsonArray.getJsonObject(0));

          ctx.response().setStatusCode(200).putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
            .end(jsonArray.getJsonObject(0).encodePrettily());
        } else {
          ctx.response().setStatusCode(404).end();
        }
      } catch (Exception e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
      }
    }
  }

  private void handleGetItemRequests(RoutingContext ctx) {
    logger.info("handleGetItemRequests got: {}", ctx.request().path());
    try {
      String itemId = ctx.request().getParam("query").split("and")[0].split("==")[1].trim();
      if (itemId.startsWith("(") && itemId.endsWith(")")) {
        itemId = itemId.substring(1, itemId.length() - 1);
      }
      int limit = Integer.parseInt(ctx.request().getParam("limit"));
      JsonObject entries = new JsonObject(getMockData(ITEM_REQUESTS_MOCK_DATA_PATH + "itemRequests.json"));
      filterByKeyValue("itemId", itemId, entries.getJsonArray(REQUESTS));
      entries.put("totalRecords", entries.getJsonArray(REQUESTS).size());
      if (limit == 0) {
        entries.put("records", new JsonArray());
      }
      serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
      addServerRqRsData(HttpMethod.GET, REQUESTS, entries);
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(500)
        .end();
    }
  }

  private void handleGetLoanType(RoutingContext ctx) {
    logger.info("handleGetLoanType got: {}", ctx.request().path());
    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    try {
      if (NON_EXIST_LOAN_TYPE_TENANT.equals(tenantId)) {
        String body = buildEmptyCollection(LOAN_TYPES);
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, LOAN_TYPES, new JsonObject(body));
      } else {
        // Filter result based on name from query
        String name = ctx.request().getParam("query").split("==")[1];
        JsonObject entries = new JsonObject(getMockData(LOAN_TYPES_MOCK_DATA_PATH + "types.json"));
        filterByKeyValue("name", name, entries.getJsonArray(LOAN_TYPES));

        serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
        addServerRqRsData(HttpMethod.GET, LOAN_TYPES, entries);
      }
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(404)
        .end();
    }
  }

  private void handleGetIdentifierType(RoutingContext ctx) {
    logger.info("handleGetIdentifierType got: {}", ctx.request().path());
    try {
      // Filter result based on name from query
      String query = ctx.request().getParam("query");
      JsonObject entries = new JsonObject(getMockData(IDENTIFIER_TYPES_MOCK_DATA_PATH + "identifierTypes.json"));
      if (query != null) {
        String name = query.split("==")[1];
        filterByKeyValue("name", name, entries.getJsonArray(IDENTIFIER_TYPES));
      }

      serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
      addServerRqRsData(HttpMethod.GET, IDENTIFIER_TYPES, entries);
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(404)
        .end();
    }
  }

  private void handleGetAccessProviders(RoutingContext ctx) {
    logger.info("handleGetAccessProviders got: {}", ctx.request().path());
    String query = ctx.request().getParam("query");
    JsonObject body = null;

    try {
      if (getQuery(ACTIVE_ACCESS_PROVIDER_A, NON_EXIST_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "one_access_provider_not_found.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_B).equals(query)
        || getQuery(ACTIVE_ACCESS_PROVIDER_A, ACTIVE_ACCESS_PROVIDER_A).equals(query)
        || getQuery(ACTIVE_ACCESS_PROVIDER_B, ACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "all_access_providers_active.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "active_inactive_access_providers.json"));
      } else if (getQuery(INACTIVE_ACCESS_PROVIDER_A, INACTIVE_ACCESS_PROVIDER_B).equals(query)
        || getQuery(INACTIVE_ACCESS_PROVIDER_B, INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "all_inactive_access_providers.json"));
      } else if (getQuery(INACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "inactive_vendors.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_A).equals(query)) {
        body = new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "one_access_provider_not_found.json"));
      } else if (getQuery(ACTIVE_ACCESS_PROVIDER_B).equals(query)) {
        body = new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "one_access_providers_active.json"));
      } else if (getQuery(ORGANIZATION_NOT_VENDOR).equals(query)) {
        body = new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "not_vendor.json"));
      } else {
        JsonArray organizations = new JsonArray();

        // Search for Organizations by id
        extractIdsFromQuery(query)
          .stream()
          .map(this::getOrganizationById)
          .filter(Objects::nonNull)
          .forEach(organizations::add);

        if (!organizations.isEmpty()) {
          body = new JsonObject().put(ORGANIZATIONS, organizations);
        }
      }
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(HttpStatus.HTTP_NOT_FOUND.toInt())
        .end();
    }

    if (body != null) {
      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
    } else {
      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, buildEmptyCollection(ORGANIZATIONS));
    }
  }

  private void getOrganizationById(RoutingContext ctx) {
    logger.info("handleGetOrganizationById got: {}", ctx.request().path());
    String vendorId = ctx.request().getParam(ID);
    logger.info("id: {}", vendorId);
    JsonObject body;
    if (NON_EXIST_VENDOR_ID.equals(vendorId)) {
      serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "vendor not found");
    } else if (MOD_VENDOR_INTERNAL_ERROR_ID.equals(vendorId)) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), APPLICATION_JSON, "internal server error, contact administrator");
    } else {
      body = getOrganizationById(vendorId);
      if (body != null) {
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
      } else {
        body = getMockEntry(ORGANIZATION_STORAGE, vendorId).orElse(null);
        if (Objects.nonNull(body)) {
          serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body.encodePrettily());
          return;
        }
        serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "vendor not found");
      }
    }
  }

  private JsonObject getOrganizationById(String organizationId) {
    logger.debug("Searching for organization by id: {}", organizationId);
    JsonObject body;
    try {
      body = switch (organizationId) {
        case ACTIVE_VENDOR_ID -> new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "active_vendor.json"));
        case INACTIVE_VENDOR_ID -> new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "inactive_vendor.json"));
        case PENDING_VENDOR_ID -> new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "pending_vendor.json"));
        case ACTIVE_ACCESS_PROVIDER_B ->
          new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "one_access_providers_active.json"))
            .getJsonArray(ORGANIZATIONS).getJsonObject(0);
        case ORGANIZATION_NOT_VENDOR -> new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "not_vendor.json"));
        case VENDOR_WITH_BAD_CONTENT ->
          new JsonObject(getMockData(ORGANIZATIONS_MOCK_DATA_PATH + "vendor_bad_content.json"));
        default -> null;
      };
    } catch (IOException e) {
      body = null;
    }
    return body;
  }

  private String getQuery(String... accessProviders) {
    return convertIdsToCqlQuery(Arrays.asList(accessProviders));
  }

  private void handleGetInstanceStatus(RoutingContext ctx) {
    logger.info("handleGetInstanceStatus got: {}", ctx.request().path());

    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    try {
      if (NON_EXIST_INSTANCE_STATUS_TENANT.equals(tenantId)) {
        String body = buildEmptyCollection(INSTANCE_STATUSES);
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, INSTANCE_STATUSES, new JsonObject(body));
      } else {
        // Filter result based on code from query
        String code = ctx.request().getParam("query").split("==")[1];
        JsonObject entries = new JsonObject(getMockData(INSTANCE_STATUSES_MOCK_DATA_PATH + "types.json"));
        filterByKeyValue("code", code, entries.getJsonArray(INSTANCE_STATUSES));

        serverResponse(ctx, 200, APPLICATION_JSON, entries.encode());
        addServerRqRsData(HttpMethod.GET, INSTANCE_STATUSES, entries);
      }
    } catch (IOException e) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Mock-server error");
    }
  }

  private void handleGetInstanceType(RoutingContext ctx) {
    logger.info("handleGetInstanceType got: {}", ctx.request().path());

    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    try {
      if (INSTANCE_TYPE_CONTAINS_CODE_AS_INSTANCE_STATUS_TENANT.equals(tenantId)) {
        String body = getMockData(INSTANCE_TYPES_MOCK_DATA_PATH + "temp.json");
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, INSTANCE_TYPES, new JsonObject(body));
      } else if (NON_EXIST_INSTANCE_TYPE_TENANT.equals(tenantId)) {
        String body = buildEmptyCollection(INSTANCE_TYPES);
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, INSTANCE_TYPES, new JsonObject(body));
      } else {
        // Filter result based on code from query
        String code = ctx.request().getParam("query").split("==")[1];
        JsonObject entries = new JsonObject(getMockData(INSTANCE_TYPES_MOCK_DATA_PATH + "types.json"));
        filterByKeyValue("code", code, entries.getJsonArray(INSTANCE_TYPES));

        serverResponse(ctx, 200, APPLICATION_JSON, entries.encode());
        addServerRqRsData(HttpMethod.GET, INSTANCE_TYPES, entries);
      }
    } catch (IOException e) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Mock-server error");
    }
  }

  private void handleGetHoldingsSource(RoutingContext ctx) {
    logger.info("handleGetHoldingsSource got: {}", ctx.request().path());

    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    try {
      if (NON_EXIST_HOLDINGS_SOURCE_TENANT.equals(tenantId)) {
        String body = buildEmptyCollection(HOLDINGS_SOURCES);
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
        addServerRqRsData(HttpMethod.GET, HOLDINGS_SOURCES, new JsonObject(body));
      } else {
        // Filter result based on code from query
        String name = ctx.request().getParam("query").split("==")[1];
        JsonObject entries = new JsonObject(getMockData(HOLDINGS_SOURCE_MOCK_DATA_PATH + "sources.json"));
        filterByKeyValue("name", name, entries.getJsonArray(HOLDINGS_SOURCES));

        serverResponse(ctx, 200, APPLICATION_JSON, entries.encode());
        addServerRqRsData(HttpMethod.GET, HOLDINGS_SOURCES, entries);
      }
    } catch (IOException e) {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Mock-server error");
    }
  }

  private String buildEmptyCollection(String entryType) {
    JsonObject result = new JsonObject();
    result.put(entryType, new JsonArray());
    result.put("totalRecords", 0);
    return result.encodePrettily();
  }

  private void filterByKeyValue(String key, String value, JsonArray entries) {
    Iterator<Object> iterator = entries.iterator();
    while (iterator.hasNext()) {
      JsonObject obj = (JsonObject) iterator.next();
      if (!StringUtils.equals(value, obj.getString(key))) {
        iterator.remove();
      }
    }
  }

  private void handleGetReceivingHistory(RoutingContext ctx) {
    logger.info("handleGetItemsRecords got: {}", ctx.request().path());
    String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    addServerRqQuery(RECEIVING_HISTORY, queryParam);
    try {
      JsonObject receivingHistory;
      if (queryParam.contains(RECEIVING_HISTORY_PURCHASE_ORDER_ID)) {
        receivingHistory = new JsonObject(getMockData(RECEIVING_HISTORY_MOCK_DATA_PATH + "receivingHistory.json"));
      } else if (queryParam.contains(INTERNAL_SERVER_ERROR.getReasonPhrase())) {
        throw new HttpException(500, "Exception in orders-storage module");
      } else if (queryParam.contains(BAD_QUERY)) {
        throw new HttpException(400, "QueryValidationException");
      } else {
        receivingHistory = new JsonObject();
        receivingHistory.put("receivingHistory", new JsonArray());
        receivingHistory.put("totalRecords", 0);
      }
      addServerRqRsData(HttpMethod.GET, RECEIVING_HISTORY, receivingHistory);
      serverResponse(ctx, 200, APPLICATION_JSON, receivingHistory.encodePrettily());
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(404)
        .end();
    } catch (HttpException e) {
      ctx.response()
        .setStatusCode(e.getCode())
        .end();
    }
  }

  private String resourcePath(String subObjName) {
    return resourceByIdPath(subObjName) + ":id";
  }

  private void handleConfigurationModuleResponse(RoutingContext ctx) {
    try {
      List<JsonObject> configEntries = serverRqRs.column(HttpMethod.SEARCH).get(CONFIGS);
      if (configEntries != null && !configEntries.isEmpty()) {
        JsonObject configs = new JsonObject().put(CONFIGS, configEntries);
        serverResponse(ctx, 200, APPLICATION_JSON, configs.encodePrettily());
        return;
      }

      String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT);
      if (PO_NUMBER_ERROR_X_OKAPI_TENANT.getValue().equals(tenant)) {
        tenant = EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10.getValue();
      }
      if (NON_EXIST_INSTANCE_STATUS_TENANT_HEADER.getValue().equals(tenant) ||
        NON_EXIST_INSTANCE_TYPE_TENANT_HEADER.getValue().equals(tenant) ||
        NON_EXIST_LOAN_TYPE_TENANT_HEADER.getValue().equals(tenant)) {
        tenant = EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_1.getValue();
      }
      try {
        serverResponse(ctx, 200, APPLICATION_JSON, getMockData(String.format(CONFIG_MOCK_PATH, tenant)));
      } catch (Exception exc) {
        serverResponse(ctx, 200, APPLICATION_JSON, getMockData(String.format(CONFIG_MOCK_PATH, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10.getValue())));
      }
    } catch (IOException e) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    }
  }

  private void handleDeleteGenericSubObj(RoutingContext ctx, String subObj) {
    String id = ctx.request().getParam(ID);
    String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    addServerRqRsData(HttpMethod.DELETE, subObj, new JsonObject().put(ID, id));
    if (ID_DOES_NOT_EXIST.equals(id)) {
      serverResponse(ctx, 404, TEXT_PLAIN, id);
    }
    if (ID_BAD_FORMAT.equals(id)) {
      serverResponse(ctx, 400, TEXT_PLAIN, id);
    } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id) || ORDER_DELETE_ERROR_TENANT.equals(tenant)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      ctx.response()
        .setStatusCode(204)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end();
    }
  }

  private void handleGetPoLines(RoutingContext ctx) {
    logger.info("handleGetPoLines got: {}?{}", ctx.request().path(), ctx.request().query());

    String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    addServerRqQuery(org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE, queryParam);
    if (queryParam.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else if (queryParam.contains(ID_FOR_INTERNAL_SERVER_ERROR) || queryParam.contains(PO_ID_GET_LINES_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      String poId;
      String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT);
      List<String> polIds;

      if (queryParam.contains(PURCHASE_ORDER_ID)) {
        polIds = Collections.emptyList();
        Matcher matcher = Pattern.compile(".*" + PURCHASE_ORDER_ID + "==(\\S[^)]+).*").matcher(queryParam);
        poId = matcher.find() ? matcher.group(1) : EMPTY;
      } else {
        poId = EMPTY;
        polIds = queryParam.startsWith("id==") ? extractIdsFromQuery(queryParam) : Collections.emptyList();
      }

      List<JsonObject> postedPoLines = getRqRsEntries(HttpMethod.SEARCH, org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE);

      try {
        PoLineCollection poLineCollection = new PoLineCollection();
        if (postedPoLines.isEmpty()) {
          if (poId.equals(ORDER_ID_WITH_PO_LINES) || !polIds.isEmpty()) {
            poLineCollection = new JsonObject(getMockData(PO_LINES_COLLECTION)).mapTo(PoLineCollection.class);

            // Filter PO Lines either by PO id or by expected line ids
            poLineCollection.getPoLines().removeIf(poLine -> polIds.isEmpty() ? !poId.equals(poLine.getPurchaseOrderId()) : !polIds.contains(poLine.getId()));
            poLineCollection.setTotalRecords(poLineCollection.getPoLines().size());
          } else {
            String filePath;
            if (ID_FOR_PRINT_MONOGRAPH_ORDER.equals(poId)) {
              filePath = LISTED_PRINT_MONOGRAPH_PATH;
            } else {
              filePath = String.format("%s%s.json", COMP_ORDER_MOCK_DATA_PATH, poId);
            }
            JsonObject compPO = new JsonObject(getMockData(filePath));
            // Build PoLineCollection to make sure content is valid
            poLineCollection = buildPoLineCollection(tenant, compPO.getJsonArray(COMPOSITE_PO_LINES), poId);
          }
        } else {
          // Attempt to find POLine in mock server memory
          poLineCollection.getPoLines().addAll(postedPoLines.stream()
            .map(jsonObj -> jsonObj.mapTo(PoLine.class))
            .toList());
          poLineCollection.getPoLines().removeIf(poLine -> !polIds.isEmpty() && !polIds.contains(poLine.getId()));
        }

        poLineCollection.setTotalRecords(poLineCollection.getPoLines().size());

        // Update calculated data
        updatePoLineCalculatedData(poLineCollection);

        JsonObject po_lines = JsonObject.mapFrom(poLineCollection);
        logger.info(po_lines.encodePrettily());

        addServerRqRsData(HttpMethod.GET, org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE, po_lines);
        serverResponse(ctx, 200, APPLICATION_JSON, po_lines.encode());
      } catch (NoSuchFileException e) {
        PoLineCollection poLineCollection = new PoLineCollection();

        // Attempt to find POLine in mock server memory
        poLineCollection.getPoLines().addAll(postedPoLines.stream()
          .map(json -> json.mapTo(PoLine.class))
          .filter(line -> poId.equals(line.getPurchaseOrderId()))
          .toList());
        poLineCollection.setTotalRecords(poLineCollection.getPoLines().size());

        JsonObject entries = JsonObject.mapFrom(poLineCollection);
        addServerRqRsData(HttpMethod.GET, org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE, entries);
        serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
      } catch (IOException e) {
        PoLineCollection poLineCollection = new PoLineCollection();
        poLineCollection.setTotalRecords(0);

        JsonObject entries = JsonObject.mapFrom(poLineCollection);
        addServerRqRsData(HttpMethod.GET, org.folio.orders.utils.ResourcePathResolver.PO_LINES_STORAGE, entries);
        serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(poLineCollection).encodePrettily());
      }
    }
  }

  private void updatePoLineCalculatedData(PoLineCollection poLineCollection) {
    poLineCollection.getPoLines().forEach(this::updatePoLineEstimatedPrice);
  }

  private void updatePoLineEstimatedPrice(PoLine line) {
    if (line.getCost() != null) {
      Cost cost = JsonObject.mapFrom(line.getCost()).mapTo(Cost.class);
      line.getCost().setPoLineEstimatedPrice(calculateEstimatedPrice(cost).getNumber().doubleValue());
    }
  }

  private PoLineCollection buildPoLineCollection(String tenant, JsonArray lines, String poId) {
    PoLineCollection result = new PoLineCollection();
    if (lines == null || lines.isEmpty()) {
      result.setTotalRecords(0);
    } else {
      // Transform composite PO Lines to storage representation
      List<PoLine> poLines = lines
        .stream()
        .map(JsonObject::mapFrom)
        .map(line -> {
          replaceObjectsByIds(line, ALERTS, REPORTING_CODES);
          return line.mapTo(PoLine.class);
        })
        .map(line -> line.withPurchaseOrderId(StringUtils.isNotEmpty(poId) ? poId : UUID.randomUUID().toString()))
        .collect(Collectors.toList());

      // Set PO Line number if empty
      for (PoLine line : poLines) {
        if (StringUtils.isEmpty(line.getPoLineNumber())) {
          line.setPoLineNumber(PO_NUMBER_VALUE + "-1");
        }
      }

      result.setPoLines(poLines);

      if (EMPTY_CONFIG_TENANT.equals(tenant)) {
        result.setTotalRecords(Integer.parseInt(DEFAULT_POLINE_LIMIT));
      } else {
        result.setTotalRecords(lines.size());
      }
    }
    return result;
  }

  private void replaceObjectsByIds(JsonObject line, String... property) {
    for (String prop : property) {
      JsonArray objs = (JsonArray) line.remove(prop);
      if (objs != null) {
        line.put(prop, new JsonArray(objs.stream()
          .map(o -> JsonObject.mapFrom(o).getString(ID))
          .filter(Objects::nonNull)
          .collect(Collectors.toList())));
      }
    }
  }

  private void handleGetPoLineById(RoutingContext ctx) {
    logger.info("handleGetPoLineById: got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);

    addServerRqRsData(HttpMethod.GET, PO_LINES_STORAGE, new JsonObject().put(ID, id));

    if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else if (MIN_PO_LINE_ID.equals(id)) {
      serverResponse(ctx, 200, APPLICATION_JSON, encodePrettily(getMinimalContentCompositePoLine()));
    } else {
      try {
        // Attempt to find POLine in mock server memory
        JsonObject pol = getMockEntry(PO_LINES_STORAGE, id).orElse(null);

        // If previous step has no result then attempt to find POLine in stubs
        if (pol == null) {
          PoLine poLine = new JsonObject(getMockData(String.format("%s%s.json", PO_LINES_MOCK_DATA_PATH, id))).mapTo(PoLine.class);
          updatePoLineEstimatedPrice(poLine);
          pol = JsonObject.mapFrom(poLine);
        }

        serverResponse(ctx, 200, APPLICATION_JSON, pol.encodePrettily());
      } catch (IOException e) {
        serverResponse(ctx, 404, APPLICATION_JSON, id);
      }
    }
  }

  private void serverResponse(RoutingContext ctx, int statusCode, String contentType, String body) {
    ctx.response()
      .setStatusCode(statusCode)
      .putHeader(HttpHeaders.CONTENT_TYPE, contentType)
      .end(body);
  }

  private void handleGetGenericSubObj(RoutingContext ctx, String subObj) {
    logger.info("got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);

    JsonObject data = new JsonObject().put(ID, id);
    addServerRqRsData(HttpMethod.GET, subObj, data);

    if (ID_DOES_NOT_EXIST.equals(id)) {
      serverResponse(ctx, 404, APPLICATION_JSON, id);
    }
    if (ID_BAD_FORMAT.equals(id)) {
      serverResponse(ctx, 400, APPLICATION_JSON, id);
    } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(data.encodePrettily());
    }
  }

  private void handleGetGenericSubObjs(RoutingContext ctx, String subObj) {
    logger.info("handleGetGenericSubObjs got: {}", ctx.request().path());

    String query = StringUtils.trimToEmpty(ctx.request().getParam(QUERY));
    addServerRqQuery(subObj, query);

    if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        JsonObject collection;
        if (REASONS_FOR_CLOSURE.equals(subObj)) {
          ReasonForClosureCollection reasonsCollection = new ReasonForClosureCollection();
          List<ReasonForClosure> reasons = Lists.newArrayList(REASON_FOR_CLOSURE.getTestSample().mapTo(ReasonForClosure.class));
          collection = JsonObject.mapFrom(reasonsCollection.withReasonsForClosure(reasons).withTotalRecords(reasons.size()));
        } else if (PREFIXES.equals(subObj)) {
          PrefixCollection prefixCollection = new PrefixCollection();
          List<Prefix> prefixes = Lists.newArrayList(PREFIX.getTestSample().mapTo(Prefix.class));
          collection = JsonObject.mapFrom(prefixCollection.withPrefixes(prefixes).withTotalRecords(prefixes.size()));
        } else if (SUFFIXES.equals(subObj)) {
          SuffixCollection suffixCollection = new SuffixCollection();
          List<Suffix> suffixes = Lists.newArrayList(SUFFIX.getTestSample().mapTo(Suffix.class));
          collection = JsonObject.mapFrom(suffixCollection.withSuffixes(suffixes).withTotalRecords(suffixes.size()));
        } else {
          collection = new JsonObject();
        }

        addServerRqRsData(HttpMethod.GET, subObj, collection);
        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(collection.encodePrettily());
      } catch (Exception e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
      }
    }
  }

  private void handleGetPieceById(RoutingContext ctx) {
    logger.info("handleGetPiecesById got: {}", ctx.request().path());
    String pieceId = ctx.request().getParam(ID);
    logger.info("id: {}", pieceId);
    try {
      if (ID_DOES_NOT_EXIST.equals(pieceId)) {
        serverResponse(ctx, 404, APPLICATION_JSON, pieceId);
      } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(pieceId)) {
        serverResponse(ctx, 500, APPLICATION_JSON, pieceId);
      } else {
        // Attempt to find POLine in mock server memory
        JsonObject data = getMockEntry(PIECES_STORAGE, pieceId).orElse(null);
        if (data == null) {
          if (PIECE_POLINE_CONSISTENCY_404_POLINE_NOT_FOUND_ID.equals(pieceId)) {
            data = new JsonObject(getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-poline-not-exists-5b454292-6aaa-474f-9510-b59a564e0c8d.json"));
          } else if (PIECE_POLINE_CONSISTENT_RECEIPT_STATUS_ID.equals(pieceId)) {
            data = new JsonObject(getMockData(PIECE_RECORDS_MOCK_DATA_PATH + "pieceRecord-received-consistent-receipt-status-5b454292-6aaa-474f-9510-b59a564e0c8d2.json"));
          } else {
            // Load piece by id
            data = new JsonObject(getMockData(PIECE_RECORDS_MOCK_DATA_PATH + String.format("pieceRecord-%s.json", pieceId)));
          }
        }
        logger.info("handleGetPieceById, data: {}", data.encodePrettily());
        serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, data.encodePrettily());
      }
    } catch (IOException e) {
      serverResponse(ctx, 404, APPLICATION_JSON, pieceId);
    }
  }

  private void handleGetPieces(RoutingContext ctx) {
    String requestPath = ctx.request().path();
    String requestQuery = ctx.request().getParam("query");
    MultiMap requestHeaders = ctx.request().headers();
    logger.info("handleGetPieces requestPath: {}, requestQuery: {}, requestHeaders: {}", requestPath, requestQuery, requestHeaders);
    if (Objects.nonNull(requestQuery) && requestQuery.contains(ID_FOR_PIECES_INTERNAL_SERVER_ERROR)) {
      logger.info("handleGetPieces (with internal server error)");
      addServerRqRsData(HttpMethod.GET, PIECES_STORAGE, new JsonObject());
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      PieceCollection pieces;
      logger.info("handleGetPieces (all records), pieces already present: {}", getMockEntries(PIECES_STORAGE, Piece.class).isPresent());
      // Prevent loading of unnecessary pieces from other tests for tenant that was made specifically for Claiming
      if (getMockEntries(PIECES_STORAGE, Piece.class).isPresent()) {
        var isClaimingTenant = requestHeaders.contains(OKAPI_TENANT, EXIST_CONFIG_X_OKAPI_TENANT_LIMIT_10_CLAIMS.getValue(), false);
        logger.info("handleGetPieces (all records), isClaimingTenant: {}", isClaimingTenant);
        try {
          List<Piece> piecesList = getMockEntries(PIECES_STORAGE, Piece.class).get();
          if (!isClaimingTenant) {
            pieces = new PieceCollection().withPieces(piecesList);
          } else {
            pieces = new PieceCollection().withPieces(piecesList.stream().filter(piece -> piece.getReceivingStatus() == LATE).toList());
          }
          pieces.setTotalRecords(pieces.getPieces().size());
        } catch (Exception e) {
          throw new IllegalStateException(String.format("Cannot retrieved mock data for requestPath: %s, requestQuery: %s", requestPath, requestQuery));
        }
      } else {
        try {
          if (requestQuery.contains("poLineId==")) {
            logger.info("handleGetPieces (by poLineId)");
            List<String> conditions = StreamEx
              .split(requestQuery, " or ")
              .flatMap(s -> StreamEx.split(s, " and "))
              .toList();

            String polId = EMPTY;
            String status = EMPTY;
            for (String condition : conditions) {
              if (condition.startsWith("poLineId")) {
                polId = condition.split("poLineId==")[1];
              } else if (condition.startsWith("receivingStatus")) {
                status = condition.split("receivingStatus==")[1];
              }
            }
            logger.info("poLineId: {}", polId);
            logger.info("receivingStatus: {}", status);

            String path = PIECE_RECORDS_MOCK_DATA_PATH + String.format("pieceRecords-%s.json", polId);
            pieces = new JsonObject(getMockData(path)).mapTo(PieceCollection.class);
            // Filter piece records by receiving status
            if (StringUtils.isNotEmpty(status)) {
              Piece.ReceivingStatus receivingStatus = Piece.ReceivingStatus.fromValue(status);
              pieces.getPieces().removeIf(piece -> receivingStatus != piece.getReceivingStatus());
            }
          } else if (requestQuery.contains("id==")) {
            logger.info("handleGetPieces (by id)");
            pieces = new JsonObject(getMockData(PIECES_COLLECTION)).mapTo(PieceCollection.class);
            List<String> pieceIds = extractIdsFromQuery(requestQuery);
            pieces.getPieces().removeIf(piece -> !pieceIds.contains(piece.getId()));
            // fix consistency with titles: the piece's title id should be the same as one of the titles ids
            // returned for the piece's po line
            pieces.getPieces().forEach(piece -> {
              String poLineId = piece.getPoLineId();
              List<Title> titlesForPoLine = getTitlesByPoLineIds(List.of(poLineId)).mapTo(TitleCollection.class).getTitles();
              if (!titlesForPoLine.isEmpty() && titlesForPoLine.stream().noneMatch(title -> title.getId().equals(piece.getTitleId()))) {
                piece.setTitleId(titlesForPoLine.get(0).getId());
              }
            });
          } else {
            logger.info("handleGetPieces (with empty piece collection)");
            pieces = new PieceCollection();
          }

          pieces.setTotalRecords(pieces.getPieces().size());
        } catch (Exception e) {
          logger.info("handleGetPieces (with empty piece collection on exception)");
          pieces = new PieceCollection();
          pieces.setTotalRecords(0);
        }
      }

      JsonObject data = JsonObject.mapFrom(pieces);
      addServerRqRsData(HttpMethod.GET, PIECES_STORAGE, data);
      logger.info("handleGetPieces, data: {}", data.encodePrettily());

      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(data.encodePrettily());
    }
  }

  private void handleGetTitles(RoutingContext ctx) {
    String query = StringUtils.trimToEmpty(ctx.request().getParam(QUERY));
    addServerRqQuery(TITLES, query);
    if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        List<String> ids = Collections.emptyList();
        if (query.contains("poLineId==")) {
          ids = extractValuesFromQuery("poLineId", query);
        }

        JsonObject collection = getTitlesByPoLineIds(ids);
        addServerRqRsData(HttpMethod.GET, TITLES, collection);

        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(collection.encodePrettily());
      } catch (Exception e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
      }
    }
  }

  private void handleGetRoutingListById(RoutingContext ctx) {
    logger.info("handleGetRoutingListById got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);

    addServerRqRsData(HttpMethod.GET, ROUTING_LISTS, new JsonObject().put(ID, id));

    if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        // Attempt to find title in mock server memory
        JsonObject existantTitle = getMockEntry(ROUTING_LISTS, id).orElse(null);

        // If previous step has no result then attempt to find title in stubs
        if (existantTitle == null) {
          RoutingList title = new JsonObject(getMockData(String.format("%s%s.json", ROUTING_LISTS_MOCK_DATA_PATH, id))).mapTo(RoutingList.class);
          existantTitle = JsonObject.mapFrom(title);
        }

        serverResponse(ctx, 200, APPLICATION_JSON, existantTitle.encodePrettily());
      } catch (IOException e) {
        serverResponse(ctx, 404, APPLICATION_JSON, id);
      }
    }
  }

  private void handleGetRoutingLists(RoutingContext ctx) {
    String query = StringUtils.trimToEmpty(ctx.request().getParam(QUERY));
    addServerRqQuery(ROUTING_LISTS, query);
    if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        List<String> ids = Collections.emptyList();
        if (query.contains("poLineId==")) {
          ids = extractValuesFromQuery("poLineId", query);
        }

        JsonObject collection = getRoutingListsByPoLineId(ids);
        addServerRqRsData(HttpMethod.GET, ROUTING_LISTS, collection);

        ctx.response()
          .setStatusCode(200)
          .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
          .end(collection.encodePrettily());
      } catch (Exception e) {
        serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
      }
    }
  }


  private List<String> extractIdsFromQuery(String query) {
    return extractValuesFromQuery(ID, query);
  }

  private List<String> extractfundIdsFromQuery(String query) {
    return extractValuesFromQuery(FUND_ID, query);
  }


  private List<String> extractValuesFromQuery(String fieldName, String query) {
    Matcher matcher = Pattern.compile(".*" + fieldName + "==\\(?([^)]+).*").matcher(query);
    if (matcher.find()) {
      return StreamEx.split(matcher.group(1), " or ").toList();
    } else {
      return Collections.emptyList();
    }
  }

  private void handlePutGenericSubObj(RoutingContext ctx, String subObj) {
    logger.info("handlePutGenericSubObj got: PUT {}", ctx.request().path());
    String id = ctx.request().getParam(ID);

    addServerRqRsData(HttpMethod.PUT, subObj, ctx.body().asJsonObject());

    if (ID_DOES_NOT_EXIST.equals(id)) {
      serverResponse(ctx, 404, APPLICATION_JSON, id);
    }
    if (ID_BAD_FORMAT.equals(id)) {
      serverResponse(ctx, 400, APPLICATION_JSON, id);
    } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id) || ctx.body().asString().contains("500500500500")) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      ctx.response()
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .setStatusCode(204)
        .end();
    }
  }

  private void handlePutPiecesBatch(RoutingContext ctx) {
    logger.info("handlePutPiecesBatch got: PUT {}", ctx.request().path());
    JsonObject body = ctx.body().asJsonObject();
    addServerRqRsData(HttpMethod.PUT, PIECES_STORAGE_BATCH, body);
    ctx.response().putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
      .setStatusCode(204)
      .end();
  }

  private static void addServerRqRsData(HttpMethod method, String objName, JsonObject data) {
    List<JsonObject> entries = serverRqRs.get(objName, method);
    if (entries == null) {
      entries = new ArrayList<>();
    }
    entries.add(data);
    serverRqRs.put(objName, method, entries);
  }

  private void addServerRqQuery(String objName, String query) {
    serverRqQueries.computeIfAbsent(objName, key -> new ArrayList<>())
      .add(query);
  }

  private void handleGetPurchaseOrderById(RoutingContext ctx) {
    logger.info("handleGetPurchaseOrderById got: GET {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);
    try {
      // Attempt to find PO in mock server memory
      JsonObject po = getMockEntry(PURCHASE_ORDER_STORAGE, id).orElse(null);
      // If previous step has no result then attempt to find PO in stubs
      if (po == null) {
        if (MIN_PO_ID.equals(id)) {
          CompositePurchaseOrder compPO = getMinimalContentCompositePurchaseOrder();
          compPO.setCompositePoLines(null);
          compPO.setTotalItems(null);
          compPO.setTotalEstimatedPrice(null);
          serverResponse(ctx, 200, APPLICATION_JSON, encodePrettily(compPO));
          return;
        }

        String filePath;
        if (ID_FOR_PRINT_MONOGRAPH_ORDER.equals(id)) {
          filePath = LISTED_PRINT_MONOGRAPH_PATH;
        } else {
          filePath = String.format("%s%s.json", COMP_ORDER_MOCK_DATA_PATH, id);
        }
        po = new JsonObject(getMockData(filePath));
        po.remove(COMPOSITE_PO_LINES);
        po.remove("totalEstimatedPrice");
        po.remove("totalItems");
        // Validate the content against schema
        org.folio.rest.acq.model.PurchaseOrder order = po.mapTo(org.folio.rest.acq.model.PurchaseOrder.class);
        order.setId(id);
        po = JsonObject.mapFrom(order);
      }
      if (po.getString("orderType") == null) {
        po.put("orderType", org.folio.rest.acq.model.PurchaseOrder.OrderType.ONE_TIME.value());
      }
      po.remove(COMPOSITE_PO_LINES);
      po.remove("totalEstimatedPrice");
      po.remove("totalItems");
      addServerRqRsData(HttpMethod.GET, PURCHASE_ORDER_STORAGE, po);
      serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(ID_FOR_INTERNAL_SERVER_ERROR.equals(id) ? 500 : 404)
        .end(id);
    }
  }

  private void handleGetPurchaseOrderByQuery(RoutingContext ctx) {
    String query = StringUtils.substringAfter(ctx.request().absoluteURI(), "query=");
    addServerRqQuery(org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE, query);

    JsonObject po = new JsonObject();
    PurchaseOrderCollection orderCollection = new PurchaseOrderCollection();

    // Attempt to find POLine in mock server memory
    List<JsonObject> postedOrders = serverRqRs.column(HttpMethod.SEARCH).get(org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE);

    if (postedOrders != null) {
      orderCollection
        .withPurchaseOrders(
          postedOrders.stream()
            .peek(order -> order.remove(COMPOSITE_PO_LINES))
            .map(order -> order.mapTo(PurchaseOrder.class))
            .collect(Collectors.toList()))
        .withTotalRecords(orderCollection.getPurchaseOrders().size());
      po = JsonObject.mapFrom(orderCollection);
      po.remove(COMPOSITE_PO_LINES);
      po.remove("totalEstimatedPrice");
      po.remove("totalItems");

      addServerRqRsData(HttpMethod.GET, org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE, po);
    } else {
      if (query.contains(BAD_QUERY)) {
        serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
      } else if (query.contains(ID_FOR_INTERNAL_SERVER_ERROR)) {
        serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
      } else {
        addServerRqRsData(HttpMethod.GET, org.folio.orders.utils.ResourcePathResolver.PURCHASE_ORDER_STORAGE, po);
        Matcher matcher = Pattern.compile(".*poNumber==(\\S[^)]+).*").matcher(query);
        final String poNumber = matcher.find() ? matcher.group(1) : EMPTY;
        switch (poNumber) {
          case EXISTING_PO_NUMBER -> po.put(TOTAL_RECORDS, 1);
          case NONEXISTING_PO_NUMBER, EMPTY -> po.put(TOTAL_RECORDS, 0);
          default ->
            //modify later as needed
            po.put(TOTAL_RECORDS, 0);
        }
      }
    }
    serverResponse(ctx, 200, APPLICATION_JSON, po.encodePrettily());
  }

  private void handlePostPurchaseOrder(RoutingContext ctx) {
    logger.info("handlePostPurchaseOrder got: {}", ctx.body().asString());
    if (ORDER_ID_DUPLICATION_ERROR_USER_ID.equals(ctx.request().getHeader(RestVerticle.OKAPI_USERID_HEADER))) {
      JsonObject body = ctx.body().asJsonObject();
      Error error = new Error()
        .withMessage(String.format("duplicate key value violates unique constraint \\\"purchase_order_pkey\\\": Key (id)=(%s) already exists.", body.getString(ID)))
        .withCode("generic error");
      serverResponse(ctx, 500, APPLICATION_JSON, Json.encodePrettily(error));
      return;
    }
    JsonObject body = ctx.body().asJsonObject();
    if (isEmpty(body.getString(ID))) {
      body.put(ID, UUID.randomUUID().toString());
    }

    org.folio.rest.acq.model.PurchaseOrder po = body.mapTo(org.folio.rest.acq.model.PurchaseOrder.class);
    addServerRqRsData(HttpMethod.POST, PURCHASE_ORDER_STORAGE, body);
    addServerRqRsData(HttpMethod.SEARCH, PURCHASE_ORDER_STORAGE, body);
    ctx.response()
      .setStatusCode(201)
      .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
      .end(JsonObject.mapFrom(po).encodePrettily());
  }

  private void handlePostGenericSubObj(RoutingContext ctx, String subObj) {
    logger.info("handlePostGenericSubObj got: {}", ctx.body().asString());

    String echoStatus = ctx.request().getHeader(X_ECHO_STATUS);

    int status = 201;
    String respBody = "";
    String contentType = APPLICATION_JSON;

    if (echoStatus != null) {
      try {
        status = Integer.parseInt(echoStatus);
      } catch (NumberFormatException e) {
        logger.error("Exception parsing " + X_ECHO_STATUS, e);
      }
    }
    ctx.response().setStatusCode(status);

    JsonObject body = null;
    switch (status) {
      case 201 -> {
        contentType = APPLICATION_JSON;
        if (ctx.body().asJsonObject() != null) {
          body = JsonObject.mapFrom(ctx.body().asJsonObject().mapTo(getSubObjClass(subObj)));
          if (StringUtils.isEmpty(body.getString(ID))) {
            body.put(ID, UUID.randomUUID().toString());
          }
          respBody = body.encodePrettily();
        } else {
          respBody = EMPTY;
        }
      }
      case 400 -> respBody = "Unable to add -- malformed JSON at 13:3";
      case 403 -> respBody = "Access requires permission: foo.bar.baz";
      case 500 -> respBody = INTERNAL_SERVER_ERROR.getReasonPhrase();
    }

    if (Objects.nonNull(body) && ID_FOR_TEMPLATE_NAME_ALREADY_EXISTS.equals(body.getString(ID))) {
      Errors errors = new Errors();
      List<Error> errorList = new ArrayList<>();

      errorList.add(new Error()
        .withCode("422")
        .withMessage("lower(f_unaccent(jsonb ->> 'templateName'::text)) value already exists in table order_templates: " + body.getString("templateName")));
      errors.withErrors(errorList);

      serverResponse(ctx, 422, APPLICATION_JSON, JsonObject.mapFrom(errors).encodePrettily());
    }

    addServerRqRsData(HttpMethod.POST, subObj, body);
    serverResponse(ctx, status, contentType, respBody);
  }

  private void handleBatchTransactions(RoutingContext ctx) {
    logger.info("handleBatchTransactions got: {}", ctx.request().path());

    String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT);

    if (INTERNAL_SERVER_ERROR.getReasonPhrase().equals(tenant)) {
      serverResponse(ctx, 500, TEXT_PLAIN, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());

    } else if (FUND_CANNOT_BE_PAID_TENANT.equals(tenant)) {
      Errors errors = new Errors();
      List<Error> errorList = new ArrayList<>();
      errorList.add(new Error().withCode(FUND_CANNOT_BE_PAID.getCode()).withMessage(FUND_CANNOT_BE_PAID.getDescription()));
      errors.withErrors(errorList);
      serverResponse(ctx, 422, APPLICATION_JSON, JsonObject.mapFrom(errors).encodePrettily());

    } else if (BUDGET_IS_INACTIVE_TENANT.equals(tenant)) {
      Errors errors = new Errors();
      List<Error> errorList = new ArrayList<>();
      errorList.add(new Error().withCode(BUDGET_IS_INACTIVE.getCode()).withMessage(BUDGET_IS_INACTIVE.getDescription()));
      errors.withErrors(errorList);
      serverResponse(ctx, 422, APPLICATION_JSON, JsonObject.mapFrom(errors).encodePrettily());

    } else if (LEDGER_NOT_FOUND_FOR_TRANSACTION_TENANT.equals(tenant)) {
      Errors errors = new Errors();
      List<Error> errorList = new ArrayList<>();
      errorList.add(new Error().withCode(LEDGER_NOT_FOUND_FOR_TRANSACTION.getCode()).withMessage(LEDGER_NOT_FOUND_FOR_TRANSACTION.getDescription()));
      errors.withErrors(errorList);
      serverResponse(ctx, 422, APPLICATION_JSON, JsonObject.mapFrom(errors).encodePrettily());

    } else if (BUDGET_NOT_FOUND_FOR_TRANSACTION_TENANT.equals(tenant)) {
      Errors errors = new Errors();
      List<Error> errorList = new ArrayList<>();
      errorList.add(new Error().withCode(BUDGET_NOT_FOUND_FOR_TRANSACTION.getCode()).withMessage(BUDGET_NOT_FOUND_FOR_TRANSACTION.getDescription()));
      errors.withErrors(errorList);
      serverResponse(ctx, 422, APPLICATION_JSON, JsonObject.mapFrom(errors).encodePrettily());

    } else {
      addServerRqRsData(HttpMethod.POST, FINANCE_BATCH_TRANSACTIONS, ctx.body().asJsonObject());
      serverResponse(ctx, 204, TEXT_PLAIN, "");
    }
  }

  private void handleTransactionGetEntry(RoutingContext ctx) {
    try {
      String query = ctx.request().params().get("query");
      String body;
      if (query.equals("id==(1e42ac94-8fba-4245-aa99-35af60108588)")) {
        body = getMockData(ENCUMBRANCE_FOR_TAGS_PATH);
      } else if (query.equals("id==(eb506834-6c70-4239-8d1a-6414a5b08015 or eb506834-6c70-4239-8d1a-6414a5b08ac3 or 0466cb77-0344-43c6-85eb-0a64aa2934e5)")) {
        body = getMockData(LISTED_PRINT_MONOGRAPH_ENCUMBRANCES_PATH);
      } else if (query.equals("id==(9333fd47-4d9b-5bfc-afa3-3f2a49d4adb1 or 109ecaa0-207d-5ebd-89f2-1fda1ae9108c)")) {
        Transaction transaction1 = new Transaction()
          .withId(UUID.randomUUID().toString())
          .withFromFundId("fb7b70f1-b898-4924-a991-0e4b6312bb5f")
          .withEncumbrance(new Encumbrance()
            .withSourcePurchaseOrderId("d6966317-96c7-492f-8df6-dc6c19554452")
            .withSourcePoLineId("a6edc906-2f9f-5fb2-a373-efac406f0ef2")
            .withStatus(Encumbrance.Status.UNRELEASED))
          .withMetadata(new Metadata());
        Transaction transaction2 = new Transaction()
          .withId(UUID.randomUUID().toString())
          .withFromFundId("fb7b70f1-b898-4924-a991-0e4b6312bb5f")
          .withEncumbrance(new Encumbrance()
            .withSourcePurchaseOrderId("d6966317-96c7-492f-8df6-dc6c19554452")
            .withSourcePoLineId("a6edc906-2f9f-5fb2-a373-efac406f0ef2")
            .withStatus(Encumbrance.Status.UNRELEASED))
          .withMetadata(new Metadata());
        TransactionCollection transactionCollection = new TransactionCollection().withTransactions(List.of(transaction1, transaction2)).withTotalRecords(2);
        body = JsonObject.mapFrom(transactionCollection).encodePrettily();
      } else if (query.contains("encumbrance.sourcePoLineId == 50fb5514-cdf1-11e8-a8d5-f2801f1b9fd1")) {
        // for testReopenOrderUnreleasesEncumbrancesUnlessInvoiceLineHasReleaseEncumbrance
        Transaction transaction1 = new Transaction()
          .withId(UUID.randomUUID().toString())
          .withFromFundId("fb7b70f1-b898-4924-a991-0e4b6312bb5f")
          .withEncumbrance(new Encumbrance()
            .withSourcePurchaseOrderId("477f9ca8-b295-11eb-8529-0242ac130003")
            .withSourcePoLineId("50fb5514-cdf1-11e8-a8d5-f2801f1b9fd1")
            .withStatus(Encumbrance.Status.RELEASED))
          .withMetadata(new Metadata());
        List<Transaction> transactions = List.of(transaction1);
        TransactionCollection transactionCollection = new TransactionCollection().withTransactions(transactions).withTotalRecords(1);
        body = JsonObject.mapFrom(transactionCollection).encodePrettily();
      } else if (query.contains("id==(9333fd47-4d9b-5bfc-afa3-3f2a49d4adb1)")) {
        TransactionCollection transactionCollection = new JsonObject(getMockData(ENCUMBRANCE_PATH)).mapTo(TransactionCollection.class);
        transactionCollection.getTransactions().get(0).withId("9333fd47-4d9b-5bfc-afa3-3f2a49d4adb1");
        body = JsonObject.mapFrom(transactionCollection).encodePrettily();
      } else if (query.contains("awaitingPayment.encumbranceId")) {
        TransactionCollection transactionCollection = new TransactionCollection().withTotalRecords(0);
        body = JsonObject.mapFrom(transactionCollection).encodePrettily();
      } else {
        body = getMockData(ENCUMBRANCE_PATH);
      }
      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
      addServerRqRsData(HttpMethod.GET, TRANSACTIONS_ENDPOINT, new JsonObject(body));
    } catch (IOException e) {
      logger.error("handleTransactionGetEntry error", e);
    }
  }

  private void handleUserTenantsGetEntry(RoutingContext ctx) {
    String body = new JsonObject().put("userTenants", Collections.emptyList()).encodePrettily();
    serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
    addServerRqRsData(HttpMethod.GET, USER_TENANTS_ENDPOINT, new JsonObject(body));
  }

  private Class<?> getSubObjClass(String subObj) {
    return switch (subObj) {
      case ALERTS -> Alert.class;
      case REPORTING_CODES -> ReportingCode.class;
      case PIECES_STORAGE -> Piece.class;
      case ACQUISITIONS_UNITS -> AcquisitionsUnit.class;
      case ACQUISITION_METHODS -> AcquisitionMethod.class;
      case ACQUISITIONS_MEMBERSHIPS -> AcquisitionsUnitMembership.class;
      case ORDER_TEMPLATES -> OrderTemplate.class;
      case TITLES -> Title.class;
      case REASONS_FOR_CLOSURE -> ReasonForClosure.class;
      case PREFIXES -> Prefix.class;
      case SUFFIXES -> Suffix.class;
      case TAGS -> Tag.class;
      case DATA_EXPORT_SPRING_CREATE_JOB, DATA_EXPORT_SPRING_EXECUTE_JOB -> Object.class;
      default -> {
        fail("The sub-object is unknown");
        yield null;
      }
    };
  }

  private void handlePostPOLine(RoutingContext ctx) {
    logger.info("handlePostPOLine got poLine: {}", ctx.body().asString());
    JsonObject body = ctx.body().asJsonObject();
    org.folio.rest.acq.model.PoLine pol = body.mapTo(org.folio.rest.acq.model.PoLine.class);

    if (pol.getId() == null) {
      pol.setId(UUID.randomUUID().toString());
    }

    if (ID_FOR_INTERNAL_SERVER_ERROR.equals(pol.getPurchaseOrderId())) {
      ctx.response()
        .setStatusCode(500)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end();
    } else {
      if (!pol.getIsPackage() && getMockEntries(TITLES, Title.class).orElseGet(Collections::emptyList).isEmpty()) {
        addMockEntry(TITLES, new Title().withId(UUID.randomUUID().toString()).withPoLineId(pol.getId()).withTitle(pol.getTitleOrPackage()));
      }
      ctx.response()
        .setStatusCode(201)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(pol).encodePrettily());
    }

    addServerRqRsData(HttpMethod.POST, PO_LINES_STORAGE, body);
    addServerRqRsData(HttpMethod.SEARCH, PO_LINES_STORAGE, body);
  }

  private void handlePostPiecesBatch(RoutingContext ctx) {
    logger.info("handlePostPiecesBatch got: {}", ctx.body().asString());
    JsonObject body = ctx.body().asJsonObject();
    PieceCollection pieceCollection = body.mapTo(PieceCollection.class);

    pieceCollection.getPieces().forEach(piece -> {
      if (piece.getId() == null) {
        piece.setId(UUID.randomUUID().toString());
      }
    });

    if (pieceCollection.getPieces().stream().anyMatch(piece -> ID_FOR_INTERNAL_SERVER_ERROR.equals(piece.getPoLineId()))) {
      ctx.response()
        .setStatusCode(500)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end();
    } else {
      pieceCollection.getPieces().forEach(piece -> addMockEntry(PIECES_STORAGE, piece));
      ctx.response()
        .setStatusCode(201)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(pieceCollection).encodePrettily());
    }

    addServerRqRsData(HttpMethod.POST, PIECES_STORAGE_BATCH, body);
  }

  private void handleGetPoNumber(RoutingContext ctx) {
    if (PO_NUMBER_ERROR_TENANT.equals(ctx.request().getHeader(OKAPI_HEADER_TENANT))) {
      ctx.response()
        .setStatusCode(500)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end();
    } else {
      SequenceNumber seqNumber = new SequenceNumber();
      seqNumber.setSequenceNumber(PO_NUMBER_VALUE);
      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(JsonObject.mapFrom(seqNumber).encodePrettily());
    }
  }

  private void handleGetContributorNameTypes(RoutingContext ctx) {
    String queryParam = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    String tenantId = ctx.request().getHeader(OKAPI_HEADER_TENANT);

    if (NON_EXIST_CONTRIBUTOR_NAME_TYPE_TENANT.equals(tenantId)) {
      String body = buildEmptyCollection(CONTRIBUTOR_NAME_TYPES);

      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
      addServerRqRsData(HttpMethod.GET, CONTRIBUTOR_NAME_TYPES, new JsonObject(body));
    } else if (StringUtils.isEmpty(queryParam) || queryParam.startsWith("id==")) {
      List<String> contributorNameTypeIds = extractIdsFromQuery(queryParam);

      JsonObject contributorNameTypeCollection = getMockAsJson(CONTRIBUTOR_NAME_TYPES_PATH);
      if (!StringUtils.isEmpty(queryParam)) {
        List<JsonObject> contributorNameTypes = contributorNameTypeCollection.getJsonArray(CONTRIBUTOR_NAME_TYPES)
          .stream()
          .map(o -> ((JsonObject) o))
          .filter(contributorNameType -> contributorNameTypeIds.contains(contributorNameType.getString(ID)))
          .collect(Collectors.toList());

        contributorNameTypeCollection.put(CONTRIBUTOR_NAME_TYPES, contributorNameTypes);
      }

      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, contributorNameTypeCollection.encodePrettily());
      addServerRqRsData(HttpMethod.GET, CONTRIBUTOR_NAME_TYPES, contributorNameTypeCollection);
    } else {
      serverResponse(ctx, HttpStatus.HTTP_INTERNAL_SERVER_ERROR.toInt(), TEXT_PLAIN, "Illegal query");
    }

  }

  Supplier<List<AcquisitionsUnit>> getAcqUnitsFromFile = () -> {
    try {
      return new JsonObject(getMockData(ACQUISITIONS_UNITS_COLLECTION)).mapTo(AcquisitionsUnitCollection.class)
        .getAcquisitionsUnits();
    } catch (IOException e) {
      return Collections.emptyList();
    }
  };

  private void handleGetAcquisitionsUnits(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsUnits got: {}", ctx.request().path());
    String tenant = ctx.request().getHeader(OKAPI_HEADER_TENANT);
    String query = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    addServerRqQuery(ACQUISITIONS_UNITS, query);

    AcquisitionsUnitCollection units = new AcquisitionsUnitCollection();
    if (!PROTECTED_READ_ONLY_TENANT.equals(tenant)) {
      units.setAcquisitionsUnits(getMockEntries(ACQUISITIONS_UNITS, AcquisitionsUnit.class).orElseGet(getAcqUnitsFromFile));
    }

    if (query.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else {

      if (!query.contains(ALL_UNITS_CQL)) {
        List<Boolean> isDeleted = extractValuesFromQuery(IS_DELETED_PROP, query).stream()
          .map(Boolean::valueOf)
          .toList();
        if (!isDeleted.isEmpty()) {
          units.getAcquisitionsUnits().removeIf(unit -> !isDeleted.contains(unit.getIsDeleted()));
        }
      }

      if (query.contains("name==")) {
        List<String> names = extractValuesFromQuery("name", query);
        if (!names.isEmpty()) {
          units.getAcquisitionsUnits().removeIf(unit -> !names.contains(unit.getName()));
        }
      }

      if (query.contains("id==")) {
        List<String> ids = extractIdsFromQuery(query);
        if (!ids.isEmpty()) {
          units.getAcquisitionsUnits().removeIf(unit -> !ids.contains(unit.getId()));
        }
      }

      JsonObject data = JsonObject.mapFrom(units.withTotalRecords(units.getAcquisitionsUnits().size()));
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_UNITS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    }
  }

  private void handleGetAcquisitionsUnit(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsUnit got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);

    AcquisitionsUnitCollection units;
    try {
      units = new JsonObject(getMockData(ACQUISITIONS_UNITS_COLLECTION)).mapTo(AcquisitionsUnitCollection.class);
    } catch (IOException e) {
      units = new AcquisitionsUnitCollection();
    }

    AcquisitionsUnit acquisitionsUnit = units.getAcquisitionsUnits()
      .stream()
      .filter(unit -> unit.getId().equals(id))
      .findAny()
      .orElse(null);

    if (acquisitionsUnit != null) {
      JsonObject data = JsonObject.mapFrom(acquisitionsUnit);
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_UNITS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    } else {
      serverResponse(ctx, 404, TEXT_PLAIN, id);
    }
  }

  private void handleGetAcquisitionsMemberships(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsMemberships got: {}", ctx.request().path());

    String query = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    if (query.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else {
      Matcher userIdMatcher = Pattern.compile(".*userId==(\\S+).*").matcher(query);
      final String userId = userIdMatcher.find() ? userIdMatcher.group(1) : EMPTY;

      AcquisitionsUnitMembershipCollection memberships;
      try {
        memberships = new JsonObject(getMockData(ACQUISITIONS_MEMBERSHIPS_COLLECTION)).mapTo(AcquisitionsUnitMembershipCollection.class);
      } catch (IOException e) {
        memberships = new AcquisitionsUnitMembershipCollection();
      }

      if (StringUtils.isNotEmpty(userId)) {
        memberships.getAcquisitionsUnitMemberships().removeIf(membership -> !membership.getUserId().equals(userId));
        List<String> acquisitionsUnitIds = extractValuesFromQuery(ACQUISITIONS_UNIT_ID, query);
        if (!acquisitionsUnitIds.isEmpty()) {
          memberships.getAcquisitionsUnitMemberships().removeIf(membership -> !acquisitionsUnitIds.contains(membership.getAcquisitionsUnitId()));
        }
      }

      JsonObject data = JsonObject.mapFrom(memberships.withTotalRecords(memberships.getAcquisitionsUnitMemberships().size()));
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_MEMBERSHIPS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    }
  }

  private void handleGetAcquisitionsMembership(RoutingContext ctx) {
    logger.info("handleGetAcquisitionsMembership got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);

    AcquisitionsUnitMembershipCollection memberships;
    try {
      memberships = new JsonObject(getMockData(ACQUISITIONS_MEMBERSHIPS_COLLECTION)).mapTo(AcquisitionsUnitMembershipCollection.class);
    } catch (IOException e) {
      memberships = new AcquisitionsUnitMembershipCollection();
    }

    AcquisitionsUnitMembership acquisitionsUnitMembership = memberships.getAcquisitionsUnitMemberships()
      .stream()
      .filter(membership -> membership.getId().equals(id))
      .findAny()
      .orElse(null);

    if (acquisitionsUnitMembership != null) {
      JsonObject data = JsonObject.mapFrom(acquisitionsUnitMembership);
      addServerRqRsData(HttpMethod.GET, ACQUISITIONS_MEMBERSHIPS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    } else {
      serverResponse(ctx, 404, TEXT_PLAIN, id);
    }
  }

  private void handleGetAcquisitionMethod(RoutingContext ctx) {
    logger.info("handleGetAcquisitionMethod got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);

    AcquisitionMethodCollection acquisitionMethodCollection;
    try {
      acquisitionMethodCollection = new JsonObject(getMockData(ACQUISITION_METHODS_COLLECTION)).mapTo(AcquisitionMethodCollection.class);
    } catch (IOException e) {
      acquisitionMethodCollection = new AcquisitionMethodCollection();
    }

    AcquisitionMethod acquisitionMethod = acquisitionMethodCollection.getAcquisitionMethods()
      .stream()
      .filter(acqMethod -> acqMethod.getId().equals(id))
      .findAny()
      .orElse(null);

    if (acquisitionMethod != null) {
      JsonObject data = JsonObject.mapFrom(acquisitionMethod);
      addServerRqRsData(HttpMethod.GET, ACQUISITION_METHODS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    } else {
      serverResponse(ctx, 404, TEXT_PLAIN, id);
    }
  }

  private void handleGetAcquisitionMethods(RoutingContext ctx) {
    logger.info("handleGetAcquisitionMethods got: {}", ctx.request().path());

    String query = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    if (query.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else {
      Matcher sourceMatcher = Pattern.compile(".*source==(\\S+).*").matcher(query);
      final String acquisitionMethodSource = sourceMatcher.find() ? sourceMatcher.group(1) : EMPTY;

      AcquisitionMethodCollection acquisitionMethodCollection;
      try {
        acquisitionMethodCollection = new JsonObject(getMockData(ACQUISITION_METHODS_COLLECTION)).mapTo(AcquisitionMethodCollection.class);
      } catch (IOException e) {
        acquisitionMethodCollection = new AcquisitionMethodCollection();
      }

      if (StringUtils.isNotEmpty(acquisitionMethodSource)) {
        acquisitionMethodCollection.getAcquisitionMethods().removeIf(acquisitionMethod -> !acquisitionMethod.getSource().value().equals(acquisitionMethodSource));
        List<String> acquisitionsMethodSource = extractValuesFromQuery("Source", query);
        if (!acquisitionsMethodSource.isEmpty()) {
          acquisitionMethodCollection.getAcquisitionMethods().removeIf(acquisitionMethod -> !acquisitionsMethodSource.contains(acquisitionMethod.getSource().value()));
        }
      }

      JsonObject data = JsonObject.mapFrom(acquisitionMethodCollection.withTotalRecords(acquisitionMethodCollection.getAcquisitionMethods().size()));
      addServerRqRsData(HttpMethod.GET, ACQUISITION_METHODS, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    }
  }

  private void handleGetIsbnConverter(RoutingContext ctx) {
    logger.info("handleGetIsbnConverter got: {}", ctx.request().path());
    String isbn = ctx.request()
      .getParam("isbn");
    JsonObject data = new JsonObject();
    if (IsbnUtil.isValid13DigitNumber(isbn)) {
      data.put("isbn", isbn);
      addServerRqRsData(HttpMethod.GET, ISBN_CONVERT13, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    } else if (IsbnUtil.isValid10DigitNumber(isbn)) {
      data.put("isbn", IsbnUtil.convertTo13DigitNumber(isbn));
      addServerRqRsData(HttpMethod.GET, ISBN_CONVERT13, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    } else {
      serverResponse(ctx, 400, TEXT_PLAIN, String.format("ISBN value %s is invalid", isbn));
    }
  }

  private void handleGetOrderTemplates(RoutingContext ctx) {
    logger.info("handleGetOrderTemplates got: {}", ctx.request().path());

    String query = StringUtils.trimToEmpty(ctx.request().getParam("query"));
    addServerRqQuery(ORDER_TEMPLATES, query);

    if (query.contains(BAD_QUERY)) {
      serverResponse(ctx, 400, APPLICATION_JSON, Response.Status.BAD_REQUEST.getReasonPhrase());
    } else {
      Matcher templateCodeMatcher = Pattern.compile(".*templateCode==(\\S+).*").matcher(query);
      final String templateCode = templateCodeMatcher.find() ? templateCodeMatcher.group(1) : EMPTY;

      OrderTemplateCollection templates;

      try {
        templates = new JsonObject(getMockData(ORDER_TEMPLATES_COLLECTION)).mapTo(OrderTemplateCollection.class);
      } catch (IOException e) {
        templates = new OrderTemplateCollection();
      }

      if (StringUtils.isNotEmpty(templateCode)) {
        templates.getOrderTemplates().removeIf(template -> !template.getTemplateCode().equals(templateCode));
        List<String> templateCodes = extractValuesFromQuery("templateCode", query);
        if (!templateCodes.isEmpty()) {
          templates.getOrderTemplates().removeIf(template -> !templateCodes.contains(template.getTemplateCode()));
        }
      }

      JsonObject data = JsonObject.mapFrom(templates.withTotalRecords(templates.getOrderTemplates().size()));
      addServerRqRsData(HttpMethod.GET, ORDER_TEMPLATES, data);
      serverResponse(ctx, 200, APPLICATION_JSON, data.encodePrettily());
    }
  }

  private void handleGetOrderTitleById(RoutingContext ctx) {
    logger.info("handleGetOrderTitleById got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);

    addServerRqRsData(HttpMethod.GET, TITLES, new JsonObject().put(ID, id));

    if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      try {
        // Attempt to find title in mock server memory
        JsonObject existantTitle = getMockEntry(TITLES, id).orElse(null);

        // If previous step has no result then attempt to find title in stubs
        if (existantTitle == null) {
          Title title = new JsonObject(getMockData(String.format("%s%s.json", TITLES_MOCK_DATA_PATH, id))).mapTo(Title.class);
          existantTitle = JsonObject.mapFrom(title);
        }

        serverResponse(ctx, 200, APPLICATION_JSON, existantTitle.encodePrettily());
      } catch (IOException e) {
        serverResponse(ctx, 404, APPLICATION_JSON, id);
      }
    }
  }

  private void handleGetBudgetByFinanceId(RoutingContext ctx) {
    logger.info("handleGetInvoiceDocumentById got: GET {}", ctx.request().path());
    String fundId = ctx.request().getParam("id");

    JsonObject collection = getBudgetsByFundIds(Collections.singletonList(fundId));
    BudgetCollection budgetCollection = collection.mapTo(BudgetCollection.class);
    if (budgetCollection.getTotalRecords() > 0) {
      Budget budget = budgetCollection.getBudgets().get(0);
      JsonObject budgetJson = JsonObject.mapFrom(budget);
      addServerRqRsData(HttpMethod.GET, CURRENT_BUDGET, budgetJson);

      ctx.response()
        .setStatusCode(200)
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .end(budgetJson.encodePrettily());
    } else {
      ctx.response()
        .setStatusCode(404)
        .end();
    }
  }

  private void handleGetRateOfExchange(RoutingContext ctx) {
    logger.info("handleGetRateOfExchange: {}", ctx.request().path());
    String fromParam = StringUtils.trimToEmpty(ctx.request().getParam("from"));
    String toParam = StringUtils.trimToEmpty(ctx.request().getParam("to"));
    ExchangeRate exchangeRate = new ExchangeRate().withExchangeRate(1.0d).withFrom(fromParam).withTo(toParam);
    addServerRqRsData(HttpMethod.GET, FINANCE_EXCHANGE_RATE, JsonObject.mapFrom(exchangeRate));
    serverResponse(ctx, 200, APPLICATION_JSON, JsonObject.mapFrom(exchangeRate).encodePrettily());
  }

  private void handleGetFyRollovers(RoutingContext ctx) {
    logger.info("handleGetFyRollovers got: {}", ctx.request().path());
    try {
      JsonObject entries = new JsonObject(getMockData(LEDGER_FY_ROLLOVERS_PATH + "ledger_fiscal_year_rollover_collection.json"));

      serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
      addServerRqRsData(HttpMethod.GET, "ledgerFiscalYearRollovers", entries);

    } catch (IOException e) {
      String body = buildEmptyCollection("ledgerFiscalYearRollovers");
      serverResponse(ctx, HttpStatus.HTTP_OK.toInt(), APPLICATION_JSON, body);
      addServerRqRsData(HttpMethod.GET, "ledgerFiscalYearRollovers", new JsonObject(body));
    }
  }

  private void handleGetFyRolloverErrors(RoutingContext ctx) {
    logger.info("handleGetFyRolloverErrors got: {}", ctx.request().path());
    try {
      JsonObject entries = new JsonObject(getMockData(LEDGER_FY_ROLLOVERS_ERRORS_PATH + "ledger_fiscal_year_rollover_error_collection.json"));

      serverResponse(ctx, 200, APPLICATION_JSON, entries.encodePrettily());
      addServerRqRsData(HttpMethod.GET, "ledgerFiscalYearRolloverErrors", entries);
    } catch (IOException e) {
      ctx.response()
        .setStatusCode(404)
        .end();
    }
  }

  private void handleGetOrderInvoiceRelationship(RoutingContext ctx) {
    logger.info("handleGetOrderInvoiceRelationship got: {}", ctx.request().path());
    JsonObject emptyCollection = JsonObject.mapFrom(new OrderInvoiceRelationshipCollection().withTotalRecords(0));

    serverResponse(ctx, 200, APPLICATION_JSON, emptyCollection.encodePrettily());
    addServerRqRsData(HttpMethod.GET, "orderInvoiceRelationship", emptyCollection);
  }

  private void handleGetInvoiceLines(RoutingContext ctx) {
    // for testReopenOrderUnreleasesEncumbrancesUnlessInvoiceLineHasReleaseEncumbrance
    String query = ctx.request().params().get("query");

    String poLineId1 = "50fb5514-cdf1-11e8-a8d5-f2801f1b9fd1";
    String poLineId2 = "4d3d5eb0-b32a-11eb-8529-0242ac130003";
    String poLineId3 = "c0d08448-347b-418a-8c2f-5fb50248d67e";
    String poLineId4 = "a6edc906-2f9f-5fb2-a373-efac406f0ef2";
    InvoiceLineCollection invoiceLineCollection;
    if (query.equals("poLineId == " + poLineId1 + " and releaseEncumbrance == true")) {
      invoiceLineCollection = new InvoiceLineCollection().withInvoiceLines(Collections.emptyList()).withTotalRecords(0);
    } else if (query.equals("poLineId == " + poLineId2 + " and releaseEncumbrance == true")) {
      InvoiceLine invoiceLine = new InvoiceLine()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId2)
        .withReleaseEncumbrance(true);
      invoiceLineCollection = new InvoiceLineCollection().withInvoiceLines(List.of(invoiceLine)).withTotalRecords(1);
    } else if (query.contains(poLineId4)) {
      InvoiceLine invoiceLine = new InvoiceLine()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId2)
        .withFundDistributions(List.of(new FundDistribution().withCode("HIST")
          .withFundId("fb7b70f1-b898-4924-a991-0e4b6312bb5f").withEncumbrance("9333fd47-4d9b-5bfc-afa3-3f2a49d4adb1")
          .withDistributionType(FundDistribution.DistributionType.PERCENTAGE).withValue(80.0)))
        .withInvoiceLineStatus(InvoiceLine.InvoiceLineStatus.APPROVED)
        .withReleaseEncumbrance(true);
      invoiceLineCollection = new InvoiceLineCollection().withInvoiceLines(List.of(invoiceLine)).withTotalRecords(1);
    } else if (query.contains("poLineId==")) {
      if (query.contains(poLineId3)) {
        InvoiceLine invoiceLine = new InvoiceLine()
          .withId(UUID.randomUUID().toString())
          .withPoLineId(poLineId2)
          .withInvoiceLineStatus(InvoiceLine.InvoiceLineStatus.PAID)
          .withReleaseEncumbrance(true);
        invoiceLineCollection = new InvoiceLineCollection().withInvoiceLines(List.of(invoiceLine)).withTotalRecords(1);
      } else {
        invoiceLineCollection = new InvoiceLineCollection();
      }
    } else if (query.matches("poLineId(.*)")) {
      InvoiceLine invoiceLine = new InvoiceLine()
        .withId(UUID.randomUUID().toString())
        .withPoLineId(poLineId2)
        .withInvoiceLineStatus(InvoiceLine.InvoiceLineStatus.OPEN)
        .withInvoiceId(UUID.randomUUID().toString())
        .withReleaseEncumbrance(true);
      invoiceLineCollection = new InvoiceLineCollection().withInvoiceLines(List.of(invoiceLine)).withTotalRecords(1);
    } else {
      serverResponse(ctx, HttpStatus.HTTP_NOT_FOUND.toInt(), APPLICATION_JSON, "invoice line not found");
      return;
    }
    JsonObject jo = JsonObject.mapFrom(invoiceLineCollection);
    serverResponse(ctx, 200, APPLICATION_JSON, jo.encodePrettily());
    addServerRqRsData(HttpMethod.GET, "invoiceLines", jo);
  }

  private void handlePatchOrderLines(RoutingContext ctx) {
    logger.info("handlePatchOrderLines got: PATCH {}", ctx.request().path());
    String id = ctx.request().getParam(ID);

    addServerRqRsData(HttpMethod.PATCH, MockServer.PATCH_ORDER_LINES_REQUEST_PATCH, ctx.body().asJsonObject());

    if (ID_DOES_NOT_EXIST.equals(id)) {
      serverResponse(ctx, 404, APPLICATION_JSON, id);
    }
    if (ID_BAD_FORMAT.equals(id)) {
      serverResponse(ctx, 400, APPLICATION_JSON, id);
    } else if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id) || ctx.body().asString().contains("500500500500")) {
      serverResponse(ctx, 500, APPLICATION_JSON, INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      ctx.response()
        .putHeader(HttpHeaders.CONTENT_TYPE, APPLICATION_JSON)
        .setStatusCode(204)
        .end();
    }
  }

  private void handleGetJobProfileSnapshotById(RoutingContext ctx) {
    logger.info("handleGetJobProfileSnapshotById got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);
    if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      Optional<JsonObject> jobProfileSnapshotOptional = getMockEntry("jobProfileSnapshots", id);
      if (jobProfileSnapshotOptional.isPresent()) {
        // validate content against schema
        JsonObject jobProfileSnapshot = jobProfileSnapshotOptional.get();
        ProfileSnapshotWrapper profileSnapshotClassSchema = jobProfileSnapshot.mapTo(ProfileSnapshotWrapper.class);
        profileSnapshotClassSchema.setId(id);
        serverResponse(ctx, 200, APPLICATION_JSON, Json.encodePrettily(profileSnapshotClassSchema));
      } else {
        serverResponse(ctx, 404, APPLICATION_JSON, id);
      }
    }
  }

  private void handleGetJobExecutionById(RoutingContext ctx) {
    logger.info("handleGetJobExecutionById got: {}", ctx.request().path());
    String id = ctx.request().getParam(ID);
    logger.info("id: {}", id);
    if (ID_FOR_INTERNAL_SERVER_ERROR.equals(id)) {
      serverResponse(ctx, 500, APPLICATION_JSON, Response.Status.INTERNAL_SERVER_ERROR.getReasonPhrase());
    } else {
      Optional<JsonObject> jobExecutionOptional = getMockEntry(JOB_EXECUTIONS, id);
      if (jobExecutionOptional.isPresent()) {
        serverResponse(ctx, 200, APPLICATION_JSON, jobExecutionOptional.get().encodePrettily());
      } else {
        serverResponse(ctx, 404, APPLICATION_JSON, id);
      }
    }
  }

  private void handleGetOrganizations(RoutingContext ctx) {
    logger.info("handleGetOrganizations:: got: {}", ctx.request().path());
    Optional<List<Organization>> organizationsOptional = getMockEntries("organizations", Organization.class);
    if (organizationsOptional.isPresent()) {
      List<Organization> organizations = organizationsOptional.get();
      OrganizationCollection organizationsClassSchema = new OrganizationCollection().withOrganizations(organizations).withTotalRecords(organizations.size());
      serverResponse(ctx, 200, APPLICATION_JSON, Json.encodePrettily(organizationsClassSchema));
    } else {
      serverResponse(ctx, 404, APPLICATION_JSON, null);
    }
  }

  private void handleGetJsonResource(RoutingContext ctx, String path) {
    logger.info("handleGetJsonResource:: got: {}", ctx.request().path());
    JsonObject resource = getMockAsJson(path);
    if (!resource.isEmpty()) {
      serverResponse(ctx, 200, APPLICATION_JSON, resource.encodePrettily());
    } else {
      serverResponse(ctx, 404, APPLICATION_JSON, null);
    }
  }
}
