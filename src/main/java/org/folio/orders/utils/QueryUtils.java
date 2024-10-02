package org.folio.orders.utils;

import one.util.streamex.StreamEx;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class QueryUtils {

  private static final String CQL_COMBINE_OPERATOR = ") %s (";
  private static final String CQL_MATCH_STRICT = "%s==%s";
  private static final String CQL_MATCH = "%s=%s";
  private static final String CQL_PREFIX = "(";
  private static final String CQL_SUFFIX = ")";
  private static final String CQL_UNDEFINED_FIELD_EXPRESSION = "cql.allRecords=1 NOT %s=\"\"";
  private static final Pattern CQL_SORT_BY_PATTERN = Pattern.compile("(.*)(\\ssortBy\\s.*)", Pattern.CASE_INSENSITIVE);

  public static String encodeQuery(String query) {
    return URLEncoder.encode(query, StandardCharsets.UTF_8);
  }

  public static String buildQuery(String query) {
    return StringUtils.isEmpty(query) ? StringUtils.EMPTY : "&query=" + encodeQuery(query);
  }

  /**
   * Combines multiple CQL expressions using the specified logical operator. For example:<br>
   * Call: <code>combineCqlExpressions("and", "field1==value1", "field2==value2")</code><br>
   * Result: <code>(field1==value1) and (field2==value2)</code>
   *
   * @param operator    The logical operator to combine the expressions (e.g., "and", "or").
   * @param expressions The CQL expressions to combine.
   * @return A single CQL query string combining the provided expressions with the specified operator.
   */
  public static String combineCqlExpressions(String operator, String... expressions) {
    if (ArrayUtils.isEmpty(expressions)) {
      return StringUtils.EMPTY;
    }
    var sorting = StringUtils.EMPTY;
    // Check whether last expression contains sorting query. If it does, extract it to be added in the end of the resulting query
    Matcher matcher = CQL_SORT_BY_PATTERN.matcher(expressions[expressions.length - 1]);
    if (matcher.find()) {
      expressions[expressions.length - 1] = matcher.group(1);
      sorting = matcher.group(2);
    }

    var suffix = CQL_SUFFIX + sorting;
    var delimiter = String.format(CQL_COMBINE_OPERATOR, operator);
    return StreamEx.of(expressions)
      .filter(StringUtils::isNotBlank)
      .joining(delimiter, CQL_PREFIX, suffix);
  }

  /**
   * Converts a collection of IDs to a CQL query string using the specified ID field.
   *
   * @param ids     The collection of IDs to be converted.
   * @param idField The field name to be used in the CQL query.
   * @return A CQL query string representing the IDs.
   */
  public static String convertIdsToCqlQuery(Collection<String> ids, String idField) {
    return convertFieldListToCqlQuery(ids, idField, true);
  }

  /**
   * Converts a collection of IDs to a CQL query string using the default ID field.
   *
   * @param ids The collection of IDs to be converted.
   * @return A CQL query string representing the IDs.
   */
  public static String convertIdsToCqlQuery(Collection<String> ids) {
    return convertFieldListToCqlQuery(ids, CommonFields.ID.getValue(), true);
  }

  /**
   * Transform list of values for some property to CQL query using 'or' operation
   *
   * @param values      list of field values
   * @param fieldName   the property name to search by
   * @param strictMatch indicates whether strict match mode (i.e. ==) should be used or not (i.e. =)
   * @return String representing CQL query to get records by some property values
   */
  public static String convertFieldListToCqlQuery(Collection<String> values, String fieldName, boolean strictMatch) {
    var prefix = String.format(strictMatch ? CQL_MATCH_STRICT : CQL_MATCH, fieldName, CQL_PREFIX);
    return StreamEx.of(values).joining(" or ", prefix, CQL_SUFFIX);
  }

  public static String convertTagListToCqlQuery(Collection<String> values, String fieldName, boolean strictMatch) {
    var prefix = String.format(strictMatch ? CQL_MATCH_STRICT : CQL_MATCH, fieldName, CQL_PREFIX) + "\"";
    return StreamEx.of(values).joining("\" or \"", prefix, "\"" + CQL_SUFFIX);
  }

  public static String getCqlExpressionForFieldNullValue(String fieldName) {
    return String.format(CQL_UNDEFINED_FIELD_EXPRESSION, fieldName);
  }

}
