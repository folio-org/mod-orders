package org.folio.orders.utils;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class SubObjects {

  private SubObjects() {
  }

  public static final String PO_LINES = "po_lines";

  public static final String ADJUSTMENT = "adjustment";
  public static final String ALERTS = "alerts";
  public static final String CLAIMS = "claims";
  public static final String COST = "cost";
  public static final String DETAILS = "details";
  public static final String ERESOURCE = "eresource";
  public static final String FUND_DISTRIBUTION = "fund_distribution";
  public static final String LOCATION = "location";
  public static final String PHYSICAL = "physical";
  public static final String RENEWAL = "renewal";
  public static final String REPORTING_CODES = "reporting_codes";
  public static final String SOURCE = "source";
  public static final String VENDOR_DETAIL = "vendor_detail";

  private static final Map<String, String> SUB_OBJECT_ITEM_APIS;
  private static final Map<String, String> SUB_OBJECT_COLLECTION_APIS;

  static {
    Map<String, String> apis = new HashMap<>();
    apis.put(ADJUSTMENT, "/adjustment");
    apis.put(ALERTS, "/alert");
    apis.put(CLAIMS, "/claim");
    apis.put(COST, "/cost");
    apis.put(DETAILS, "/details");
    apis.put(ERESOURCE, "/eresource");
    apis.put(FUND_DISTRIBUTION, "/orders-storage/fund_distributions");
    apis.put(LOCATION, "/location");
    apis.put(PHYSICAL, "/physical");
    apis.put(RENEWAL, "/renewal");
    apis.put(REPORTING_CODES, "/reporting_code");
    apis.put(SOURCE, "/source");
    apis.put(VENDOR_DETAIL, "/vendor_detail");
    apis.put(PO_LINES, "/po_line");

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
