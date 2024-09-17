package org.folio.helper;

import org.folio.rest.jaxrs.model.Cost;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class PurchaseOrderLineHelperTest {

  private static Stream<Arguments> testHasAlteredExchangeRateArgs() {
    return Stream.of(
      Arguments.of(false, 0.7d, 0.7d),
      Arguments.of(false, null, null),
      Arguments.of(true, 0.7d, 0.8d),
      Arguments.of(true, null, 0.8d),
      Arguments.of(true, 0.7d, null)
    );
  }

  @ParameterizedTest
  @MethodSource("testHasAlteredExchangeRateArgs")
  void testHasAlteredExchangeRate(boolean expected, Double oldExchangeRate, Double newExchangeRate) {
    var currencyCode = "AUD";
    var oldCost = new Cost().withCurrency(currencyCode).withExchangeRate(newExchangeRate);
    var newCost = new Cost().withCurrency(currencyCode).withExchangeRate(oldExchangeRate);
    assertEquals(expected, PurchaseOrderLineHelper.hasAlteredExchangeRate(oldCost, newCost));
  }
}
