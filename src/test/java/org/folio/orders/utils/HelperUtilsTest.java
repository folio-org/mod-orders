package org.folio.orders.utils;

import static org.folio.service.exchange.ExchangeRateProviderResolver.RATE_KEY;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import javax.money.convert.ConversionQuery;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.PoLine;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.UUID;

public class HelperUtilsTest {

  @Test
  public void testShouldReturnEmptyString(){
    String act = HelperUtils.combineCqlExpressions("");
    assertThat(act, isEmptyOrNullString());
  }

  @Test
  public void testShouldReturnConversionQueryWithRateKeyForGetConversionQueryWithExchangeRate(){
    ConversionQuery conversionQuery = HelperUtils.getConversionQuery(2.0d, "USD", "EUR");
    assertThat(conversionQuery.get(RATE_KEY, Double.class), is(2.0d));
    assertThat(conversionQuery.getBaseCurrency().getCurrencyCode(), is("USD"));
    assertThat(conversionQuery.getCurrency().getCurrencyCode(), is("EUR"));
  }

  @Test
  public void testShouldBuildQueryWithoutExchangeRate(){
    String systemCurrency = "USD";
    Cost costOneTime = new Cost().withListUnitPrice(595d).withQuantityPhysical(1).withCurrency("EUR").withPoLineEstimatedPrice(595d);
    PoLine poLineOneTime = new PoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(UUID.randomUUID().toString()).withCost(costOneTime);
    ConversionQuery actQuery = HelperUtils.buildConversionQuery(poLineOneTime, systemCurrency);
    assertEquals(actQuery.getCurrency().getCurrencyCode(), systemCurrency);
    assertNull(actQuery.get(RATE_KEY, Double.class));
  }

  @Test
  public void testShouldBuildQueryWithExchangeRate(){
    String systemCurrency = "USD";
    Cost costOneTime = new Cost().withListUnitPrice(595d).withQuantityPhysical(1).withExchangeRate(2d).withCurrency("EUR").withPoLineEstimatedPrice(595d);
    PoLine poLineOneTime = new PoLine().withId(UUID.randomUUID().toString()).withPurchaseOrderId(UUID.randomUUID().toString()).withCost(costOneTime);
    ConversionQuery actQuery = HelperUtils.buildConversionQuery(poLineOneTime, systemCurrency);

    assertEquals(actQuery.getCurrency().getCurrencyCode(), systemCurrency);
    assertEquals(Double.valueOf(2d), actQuery.get(RATE_KEY, Double.class));
  }
}
