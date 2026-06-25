package org.folio.orders.utils;

import static java.util.Collections.singletonList;
import static org.folio.rest.core.exceptions.ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL;
import static org.folio.rest.core.exceptions.ErrorCodes.FISCAL_YEAR_DISTRIBUTION_COUNT_MISMATCH;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.FiscalYearDistribution;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.PaymentTerms;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

public class FundDistributionUtilsTest {

  @ParameterizedTest(name = "{0}")
  @MethodSource("noThrowCases")
  void shouldNotThrow_whenValidatePrepaymentTermConditionBypassed(String scenario, PoLine poLine) {
    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  static Stream<Arguments> noThrowCases() {
    return Stream.of(
      Arguments.of("multiYearPayment is false",
        new PoLine()
          .withMultiYearPayment(false)
          .withPaymentTerms(new PaymentTerms()
            .withTotalPrice(100.0)
            .withPrepaymentTerm(3)
            .withStartingFiscalYearId(UUID.randomUUID().toString())
            .withFiscalYearDistributions(List.of(
              new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString())
            )))),
      Arguments.of("paymentTerms is null",
        new PoLine()
          .withMultiYearPayment(true)
          .withPaymentTerms(null)),
      Arguments.of("prepaymentTerm is null",
        new PoLine()
          .withMultiYearPayment(true)
          .withPaymentTerms(new PaymentTerms()
            .withTotalPrice(100.0)
            .withPrepaymentTerm(null)
            .withStartingFiscalYearId(UUID.randomUUID().toString()))),
      Arguments.of("prepaymentTerm is zero",
        new PoLine()
          .withMultiYearPayment(true)
          .withPaymentTerms(new PaymentTerms()
            .withTotalPrice(100.0)
            .withPrepaymentTerm(0)
            .withStartingFiscalYearId(UUID.randomUUID().toString()))),
      Arguments.of("prepaymentTerm is negative",
        new PoLine()
          .withMultiYearPayment(true)
          .withPaymentTerms(new PaymentTerms()
            .withTotalPrice(100.0)
            .withPrepaymentTerm(-1)
            .withStartingFiscalYearId(UUID.randomUUID().toString()))),
      Arguments.of("fiscalYearDistributions count equals prepaymentTerm",
        new PoLine()
          .withMultiYearPayment(true)
          .withPaymentTerms(new PaymentTerms()
            .withTotalPrice(100.0)
            .withPrepaymentTerm(2)
            .withStartingFiscalYearId(UUID.randomUUID().toString())
            .withFiscalYearDistributions(List.of(
              new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString()),
              new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString())
            ))))
    );
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("throwCases")
  void shouldThrow_whenFiscalYearDistributionCountMismatch(
    String scenario, int prepaymentTerm, Integer distributionCount,
    String expectedTermParam, String expectedCountParam) {

    PaymentTerms paymentTerms = new PaymentTerms()
      .withTotalPrice(100.0)
      .withPrepaymentTerm(prepaymentTerm)
      .withStartingFiscalYearId(UUID.randomUUID().toString());

    if (distributionCount != null) {
      List<FiscalYearDistribution> distributions = new ArrayList<>();
      for (int i = 0; i < distributionCount; i++) {
        distributions.add(new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString()));
      }
      paymentTerms.withFiscalYearDistributions(distributions);
    }

    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(paymentTerms);

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    assertEquals(422, exception.getCode());
    assertEquals(FISCAL_YEAR_DISTRIBUTION_COUNT_MISMATCH.getCode(), exception.getError().getCode());
    var parameters = exception.getError().getParameters();
    assertEquals(2, parameters.size());
    assertEquals(FundDistributionUtils.PREPAYMENT_TERM_PARAM, parameters.get(0).getKey());
    assertEquals(expectedTermParam, parameters.get(0).getValue());
    assertEquals(FundDistributionUtils.FISCAL_YEAR_DISTRIBUTION_COUNT_PARAM, parameters.get(1).getKey());
    assertEquals(expectedCountParam, parameters.get(1).getValue());
  }

  static Stream<Arguments> throwCases() {
    return Stream.of(
      Arguments.of("more distributions than prepaymentTerm", 2, 3, "2", "3"),
      Arguments.of("fewer distributions than prepaymentTerm", 3, 2, "3", "2"),
      Arguments.of("empty distributions list", 2, 0, "2", "0"),
      Arguments.of("no distributions (null list)", 1, null, "1", "0"),
      Arguments.of("prepaymentTerm=3 distributions=1", 3, 1, "3", "1")
    );
  }


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

    PoLine poLine = new PoLine()
      .withCost(cost)
      .withFundDistribution(fdList);
    List<PoLine> poLines = singletonList(poLine);

    if (errorCode == null) {
      assertDoesNotThrow(() -> FundDistributionUtils.validateFundDistributionTotal(poLines));
    } else {
      HttpException exception = assertThrows(HttpException.class, () ->
        FundDistributionUtils.validateFundDistributionTotal(poLines));
      assertEquals(exception.getError().getCode(), errorCode);
      if (exception.getError().getCode().equals(INCORRECT_FUND_DISTRIBUTION_TOTAL.getCode())) {
        assertEquals(remainingAmount, exception.getError().getParameters().get(0).getValue());
      }
    }
  }

}
