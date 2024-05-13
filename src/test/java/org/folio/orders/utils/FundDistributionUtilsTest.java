package org.folio.orders.utils;

import static java.util.Collections.singletonList;
import static org.folio.rest.core.exceptions.ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.List;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

public class FundDistributionUtilsTest {

  @ParameterizedTest
  @CsvSource(value = {
    "90:percentage:93:percentage:3:incorrectFundDistributionTotal:3.60",
    "90:amount:50:amount:35:incorrectFundDistributionTotal:5.00",
    "10:amount:10::::",
    "20:amount:10:amount:10::",
    "10:percentage:100::::",
    "10:percentage:50:percentage:50::",
    "9.99:percentage:50:percentage:50::",
    "10:amount:5:percentage:50::",
    "0:amount:0::::",
    "0:amount:0:amount:0::",
    "0:percentage:100::::",
    "0:percentage:50:percentage:50::",
    "10:amount:10:amount:10:incorrectFundDistributionTotal:-10.00",
    "10:percentage:20:percentage:50:incorrectFundDistributionTotal:3.00",
    "10:amount:5:percentage:60:incorrectFundDistributionTotal:-1.00",
    "0:amount:0:percentage:100:cannotMixTypesForZeroPrice:",
    "0:percentage:0:amount:0:cannotMixTypesForZeroPrice:",
    "0:amount:10:amount:10:incorrectFundDistributionTotal:0.00",
    "0:percentage:10:percentage:10:incorrectFundDistributionTotal:0.00",
    "47.98:percentage:100:percentage:100:incorrectFundDistributionTotal:-47.98"
  }, delimiter = ':')
  void testValidateFundDistributionTotal(Double estimatedPrice, String fd1Type, Double fd1Value, String fd2Type,
      Double fd2Value, String errorCode, String remainingAmount) {

    Cost cost = new Cost()
      .withPoLineEstimatedPrice(estimatedPrice);

    ArrayList<FundDistribution> fdList = new ArrayList<>();
    fdList.add(new FundDistribution().withDistributionType(DistributionType.fromValue(fd1Type)).withValue(fd1Value));
    if (fd2Value != null)
      fdList.add(new FundDistribution().withDistributionType(DistributionType.fromValue(fd2Type)).withValue(fd2Value));

    CompositePoLine poLine = new CompositePoLine()
      .withCost(cost)
      .withFundDistribution(fdList);
    List<CompositePoLine> compositePoLines = singletonList(poLine);

    if (errorCode == null) {
      assertDoesNotThrow(() -> FundDistributionUtils.validateFundDistributionTotal(compositePoLines));
    } else {
      HttpException exception = assertThrows(HttpException.class, () ->
        FundDistributionUtils.validateFundDistributionTotal(compositePoLines));
      assertEquals(exception.getError().getCode(), errorCode);
      if (exception.getError().getCode().equals(INCORRECT_FUND_DISTRIBUTION_TOTAL.getCode())) {
        assertEquals(remainingAmount, exception.getError().getParameters().get(0).getValue());
      }
    }
  }

}
