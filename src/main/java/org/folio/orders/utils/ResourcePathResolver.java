package org.folio.orders.utils;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class ResourcePathResolver {

  private ResourcePathResolver() {
  }

  public static final String PO_LINES = "poLines";
  public static final String PO_NUMBER = "poNumber";
  public static final String VENDOR_ID = "vendor";
  public static final String PO_LINE_NUMBER = "poLineNumber";

  public static final String ALERTS = "alerts";
  public static final String ACQUISITIONS_UNITS = "acquisitionsUnits";
  public static final String ACQUISITIONS_MEMBERSHIPS = "acquisitionsMemberships";
  public static final String REPORTING_CODES = "reportingCodes";
  public static final String PURCHASE_ORDER = "purchaseOrder";
  public static final String PIECES = "pieces";
  public static final String RECEIVING_HISTORY = "receiving-history";
  public static final String RECEIPT_STATUS = "receiptStatus";
  public static final String PAYMENT_STATUS = "paymentStatus";
  public static final String SEARCH_ORDERS = "searchOrders";
  public static final String ORDER_LINES = "orderLines";
  public static final String ORDER_TEMPLATES = "orderTemplates";
  public static final String TITLES = "titles";
  public static final String ENCUMBRANCES = "finance.encumbrances";
  public static final String FUNDS = "finance.funds";
  public static final String BUDGETS = "finance.budgets";
  public static final String ORDER_TRANSACTION_SUMMARIES = "finance.order-summaries";

  private static final Map<String, String> SUB_OBJECT_ITEM_APIS;
  private static final Map<String, String> SUB_OBJECT_COLLECTION_APIS;

  static {
    Map<String, String> apis = new HashMap<>();
    apis.put(ALERTS, "/orders-storage/alerts");
    apis.put(ACQUISITIONS_UNITS, "/acquisitions-units-storage/units");
    apis.put(ACQUISITIONS_MEMBERSHIPS, "/acquisitions-units-storage/memberships");
    apis.put(REPORTING_CODES, "/orders-storage/reporting-codes");
    apis.put(PO_LINES, "/orders-storage/po-lines");
    apis.put(PO_NUMBER, "/orders-storage/po-number");
    apis.put(PURCHASE_ORDER, "/orders-storage/purchase-orders");
    apis.put(PIECES, "/orders-storage/pieces");
    apis.put(RECEIVING_HISTORY, "/orders-storage/receiving-history");
    apis.put(PO_LINE_NUMBER, "/orders-storage/po-line-number");
    apis.put(SEARCH_ORDERS, "/orders-storage/orders");
    apis.put(ORDER_LINES, "/orders-storage/order-lines");
    apis.put(ORDER_TEMPLATES, "/orders-storage/order-templates");
    apis.put(ENCUMBRANCES, "/finance/encumbrances");
    apis.put(FUNDS, "/finance/funds");
    apis.put(BUDGETS, "/finance/budgets");
    apis.put(ORDER_TRANSACTION_SUMMARIES, "/finance/order-transaction-summaries");
    apis.put(TITLES, "/orders-storage/titles");

    SUB_OBJECT_COLLECTION_APIS = Collections.unmodifiableMap(apis);
    SUB_OBJECT_ITEM_APIS = Collections.unmodifiableMap(
      apis.entrySet()
          .stream()
          .collect(Collectors.toMap(Map.Entry::getKey, v -> v.getValue() + "/"))
    );
  }

  public static String resourcesPath(String field) {
    return SUB_OBJECT_COLLECTION_APIS.get(field);
  }

  public static String resourceByIdPath(String field) {
    return SUB_OBJECT_ITEM_APIS.get(field);
  }

  public static String resourceByIdPath(String field, String id) {
    return SUB_OBJECT_ITEM_APIS.get(field) + id;
  }
}
