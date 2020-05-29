package org.folio.orders.utils;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.isEmptyOrNullString;

public class HelperUtilsTest {

  @Test
  public void testShouldReturnEmptyString(){
    String act = HelperUtils.combineCqlExpressions("");
    assertThat(act, isEmptyOrNullString());
  }
}
