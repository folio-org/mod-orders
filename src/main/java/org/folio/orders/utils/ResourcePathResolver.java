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
  public static final String ORDER_TEMPLATES = "orderTemplates";
  public static final String TITLES = "titles";
  public static final String ENCUMBRANCES = "finance.encumbrances";
  public static final String FUNDS = "finance.funds";
  public static final String BUDGETS = "finance.budgets";
  public static final String LEDGERS = "finance.ledgers";
  public static final String ORDER_TRANSACTION_SUMMARIES = "finance.order-summaries";
  public static final String REASONS_FOR_CLOSURE = "configuration.reasons-for-closure";
  public static final String PREFIXES = "configuration.prefixes";
  public static final String SUFFIXES = "configuration.suffixes";
  public static final String TRANSACTIONS_ENDPOINT = "finance.transactions";
  public static final String FINANCE_RELEASE_ENCUMBRANCE = "finance.release-encumbrance";
  public static final String BUDGET_EXPENSE_CLASSES = "finance-storage.budget-expense-classes";
  public static final String CURRENT_BUDGET = "finance.current-budgets";
  public static final String FINANCE_EXCHANGE_RATE = "finance/exchange-rate";
  public static final String EXPENSE_CLASSES_URL = "finance.expense-classes";
  public static final String CONFIGURATION_ENTRIES = "configurations.entries";
  public static final String LEDGER_FY_ROLLOVERS = "finance.ledger-rollovers";
  public static final String LEDGER_FY_ROLLOVER_ERRORS = "finance.ledger-rollovers-errors";
  public static final String ORDER_INVOICE_RELATIONSHIP = "order-invoice-relationship";
  public static final String TAGS = "tags";


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
    apis.put(ORDER_TEMPLATES, "/orders-storage/order-templates");
    apis.put(ENCUMBRANCES, "/finance/encumbrances");
    apis.put(FUNDS, "/finance/funds");
    apis.put(BUDGETS, "/finance/budgets");
    apis.put(LEDGERS, "/finance/ledgers");
    apis.put(ORDER_TRANSACTION_SUMMARIES, "/finance/order-transaction-summaries");
    apis.put(TITLES, "/orders-storage/titles");
    apis.put(REASONS_FOR_CLOSURE, "/orders-storage/configuration/reasons-for-closure");
    apis.put(PREFIXES, "/orders-storage/configuration/prefixes");
    apis.put(SUFFIXES, "/orders-storage/configuration/suffixes");
    apis.put(TRANSACTIONS_ENDPOINT, "/finance/transactions");
    apis.put(FINANCE_RELEASE_ENCUMBRANCE, "/finance/release-encumbrance");
    apis.put(BUDGET_EXPENSE_CLASSES, "/finance-storage/budget-expense-classes");
    apis.put(CURRENT_BUDGET, "/finance/funds/%s/budget");
    apis.put(FINANCE_EXCHANGE_RATE, "/finance/exchange-rate");
    apis.put(EXPENSE_CLASSES_URL, "/finance/expense-classes");
    apis.put(CONFIGURATION_ENTRIES, "/configurations/entries");
    apis.put(LEDGER_FY_ROLLOVERS, "/finance/ledger-rollovers");
    apis.put(LEDGER_FY_ROLLOVER_ERRORS, "/finance/ledger-rollovers-errors");
    apis.put(ORDER_INVOICE_RELATIONSHIP, "/orders-storage/order-invoice-relns");
    apis.put(TAGS, "/tags");

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
