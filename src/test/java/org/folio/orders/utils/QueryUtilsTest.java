package org.folio.orders.utils;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Collection;
import java.util.List;

import static org.folio.orders.utils.QueryUtils.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class QueryUtilsTest {

  @Test
  void testReturnEncodedQuery() {
    var query = "test query";
    var encodedQuery = encodeQuery(query);
    assertEquals(encodedQuery, "test+query");
  }

  @Test
  void testBuildQuery() {
    var query = "test query";
    var result = buildQuery(query);
    assertEquals(result, "&query=test+query");
  }

  @Test
  void testBuildQueryReturnEmptyWhenQueryIsEmpty() {
    var query = "";
    var result = buildQuery(query);
    assertEquals(result, StringUtils.EMPTY);
  }

  @ParameterizedTest
  @ValueSource(strings = {"and", "or"})
  void testCombineCqlExpressions(String operator) {
    var result = combineCqlExpressions(operator, "field1==value1", "field2==value2");
    assertEquals(result, "(field1==value1) " + operator + " (field2==value2)");
  }

  @Test
  void testCombineCqlExpressionsReturnEmptyWhenNoExpressionsProvided() {
    var result = combineCqlExpressions("and");
    assertEquals(result, StringUtils.EMPTY);
  }

  @Test
  void testConvertIdsToCqlQuery() {
    var ids = List.of("id1", "id2");
    var result = convertIdsToCqlQuery(ids);
    assertEquals(result, "id==(id1 or id2)");
  }

  @Test
  void testConvertIdsToCqlQuerySpecifyField() {
    var ids = List.of("id1", "id2");
    var result = convertIdsToCqlQuery(ids, "idField");
    assertEquals(result, "idField==(id1 or id2)");
  }

  @ParameterizedTest
  @CsvSource(value = {"true,==", "false,="}, delimiterString = ",")
  void testConvertFieldListToCqlQuery(boolean strictMatch, String expectedOperator) {
    var values = List.of("value1", "value2");
    var result = convertFieldListToCqlQuery(values, "fieldName", strictMatch);
    assertEquals(result, "fieldName" + expectedOperator + "(value1 or value2)");
  }

  @ParameterizedTest
  @CsvSource(value = {"true,==", "false,="}, delimiterString = ",")
  void testConvertTagListToCqlQueryWithStrictMatch(boolean strictMatch, String expectedOperator) {
    Collection<String> values = List.of("tag1", "tag2");
    String result = convertTagListToCqlQuery(values, "tagField", strictMatch);
    assertEquals(result, "tagField" + expectedOperator + "(\"tag1\" or \"tag2\")");
  }

  @Test
  void testGetCqlExpressionForFieldNullValue() {
    var result = getCqlExpressionForFieldNullValue("fieldName");
    assertEquals(result, "cql.allRecords=1 NOT fieldName=\"\"");
  }

}
