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

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.FiscalYearDistribution;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.folio.rest.jaxrs.model.PaymentTerms;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

public class FundDistributionUtilsTest {

  @Test
  void shouldNotThrow_whenMultiYearPaymentFalse() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(false)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(3)
        .withStartingFiscalYearId(UUID.randomUUID().toString())
        .withFiscalYearDistributions(List.of(
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString())
        )));

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void shouldNotThrow_whenPaymentTermsNull() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(null);

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void shouldNotThrow_whenPrepaymentTermNull() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(null)
        .withStartingFiscalYearId(UUID.randomUUID().toString()));

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void shouldNotThrow_whenPrepaymentTermZero() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(0)
        .withStartingFiscalYearId(UUID.randomUUID().toString()));

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void shouldNotThrow_whenPrepaymentTermNegative() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(-1)
        .withStartingFiscalYearId(UUID.randomUUID().toString()));

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void shouldNotThrow_whenExactlyRequiredFundDistributions() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(2)
        .withStartingFiscalYearId(UUID.randomUUID().toString())
        .withFiscalYearDistributions(List.of(
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString()),
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString())
        )));

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void shouldThrow_whenMoreFundDistributionsThanPrepaymentTerm() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(2)
        .withStartingFiscalYearId(UUID.randomUUID().toString())
        .withFiscalYearDistributions(List.of(
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString()),
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString()),
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString())
        )));

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    assertEquals(422, exception.getCode());
    assertEquals(FISCAL_YEAR_DISTRIBUTION_COUNT_MISMATCH.getCode(), exception.getError().getCode());
  }

  @Test
  void shouldThrow_whenFewerFundDistributionsThanPrepaymentTerm() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(3)
        .withStartingFiscalYearId(UUID.randomUUID().toString())
        .withFiscalYearDistributions(List.of(
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString()),
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString())
        )));

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    assertEquals(422, exception.getCode());
    assertEquals(FISCAL_YEAR_DISTRIBUTION_COUNT_MISMATCH.getCode(), exception.getError().getCode());
  }

  @Test
  void shouldThrow_whenNoFundDistributionsInPaymentTerms() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(2)
        .withStartingFiscalYearId(UUID.randomUUID().toString())
        .withFiscalYearDistributions(List.of()));

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    assertEquals(422, exception.getCode());
    assertEquals(FISCAL_YEAR_DISTRIBUTION_COUNT_MISMATCH.getCode(), exception.getError().getCode());
  }

  @Test
  void shouldThrow_whenPaymentTermsFundDistributionsIsEmpty() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(1)
        .withStartingFiscalYearId(UUID.randomUUID().toString()));

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    assertEquals(422, exception.getCode());
    assertEquals(FISCAL_YEAR_DISTRIBUTION_COUNT_MISMATCH.getCode(), exception.getError().getCode());
  }

  @Test
  void shouldThrowOnFirstViolation_whenValidatingListWithMixedPoLines() {
    PoLine violatingPoLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(3)
        .withStartingFiscalYearId(UUID.randomUUID().toString())
        .withFiscalYearDistributions(List.of(
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString())
        )));
    PoLine validPoLine = new PoLine()
      .withMultiYearPayment(false);

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validateFundDistributionForMultiYear(List.of(violatingPoLine, validPoLine)));

    assertEquals(422, exception.getCode());
    assertEquals(FISCAL_YEAR_DISTRIBUTION_COUNT_MISMATCH.getCode(), exception.getError().getCode());
  }

  @Test
  void shouldIncludePrepaymentTermAndCountInErrorParameters_whenThrowingValidationError() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPaymentTerms(new PaymentTerms()
        .withTotalPrice(100.0)
        .withPrepaymentTerm(3)
        .withStartingFiscalYearId(UUID.randomUUID().toString())
        .withFiscalYearDistributions(List.of(
          new FiscalYearDistribution().withFiscalYearId(UUID.randomUUID().toString())
        )));

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    var parameters = exception.getError().getParameters();
    assertEquals(2, parameters.size());
    assertEquals(FundDistributionUtils.PREPAYMENT_TERM_PARAM, parameters.get(0).getKey());
    assertEquals("3", parameters.get(0).getValue());
    assertEquals(FundDistributionUtils.FISCAL_YEAR_DISTRIBUTION_COUNT_PARAM, parameters.get(1).getKey());
    assertEquals("1", parameters.get(1).getValue());
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
