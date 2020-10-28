package org.folio.orders.utils;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.isEmptyOrNullString;

import org.junit.jupiter.api.Test;

public class HelperUtilsTest {

  @Test
  public void testShouldReturnEmptyString(){
    String act = HelperUtils.combineCqlExpressions("");
    assertThat(act, isEmptyOrNullString());
  }
}
