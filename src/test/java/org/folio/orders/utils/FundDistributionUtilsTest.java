package org.folio.orders.utils;

import static java.util.Collections.singletonList;
import static org.folio.rest.core.exceptions.ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL;
import static org.folio.rest.core.exceptions.ErrorCodes.PREPAYMENT_TERM_EXCEEDS_FISCAL_YEARS;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.FundDistribution.DistributionType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

public class FundDistributionUtilsTest {

  @Test
  void testValidatePrepaymentTerm_ShouldNotThrowWhenMultiYearPaymentFalse() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(false)
      .withPrepaymentTerm(3)
      .withFundDistribution(List.of(
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(100.0)
      ));

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void testValidatePrepaymentTerm_ShouldNotThrowWhenPrepaymentTermNull() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(null)
      .withFundDistribution(List.of());

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void testValidatePrepaymentTerm_ShouldNotThrowWhenPrepaymentTermZero() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(0)
      .withFundDistribution(List.of());

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void testValidatePrepaymentTerm_ShouldNotThrowWhenPrepaymentTermNegative() {
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(-1)
      .withFundDistribution(List.of());

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void testValidatePrepaymentTerm_ShouldNotThrowWhenExactlyRequiredDistinctFiscalYears() {
    String fyId1 = UUID.randomUUID().toString();
    String fyId2 = UUID.randomUUID().toString();
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(2)
      .withFundDistribution(List.of(
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(50.0).withFiscalYearId(fyId1),
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(50.0).withFiscalYearId(fyId2)
      ));

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void testValidatePrepaymentTerm_ShouldNotThrowWhenMoreDistinctFiscalYearsThanRequired() {
    String fyId1 = UUID.randomUUID().toString();
    String fyId2 = UUID.randomUUID().toString();
    String fyId3 = UUID.randomUUID().toString();
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(2)
      .withFundDistribution(List.of(
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(34.0).withFiscalYearId(fyId1),
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(33.0).withFiscalYearId(fyId2),
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(33.0).withFiscalYearId(fyId3)
      ));

    assertDoesNotThrow(() -> FundDistributionUtils.validatePrepaymentTerm(poLine));
  }

  @Test
  void testValidatePrepaymentTerm_ShouldThrowWhenFewerDistinctFiscalYearsThanPrepaymentTerm() {
    String fyId1 = UUID.randomUUID().toString();
    String fyId2 = UUID.randomUUID().toString();
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(3)
      .withFundDistribution(List.of(
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(50.0).withFiscalYearId(fyId1),
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(50.0).withFiscalYearId(fyId2)
      ));

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    assertEquals(422, exception.getCode());
    assertEquals(PREPAYMENT_TERM_EXCEEDS_FISCAL_YEARS.getCode(), exception.getError().getCode());
  }

  @Test
  void testValidatePrepaymentTerm_ShouldThrowWhenNoFiscalYearIdInFundDistributions() {
    // fund distributions exist but have no fiscalYearId; count is 0
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(2)
      .withFundDistribution(List.of(
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(100.0)
      ));

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    assertEquals(422, exception.getCode());
    assertEquals(PREPAYMENT_TERM_EXCEEDS_FISCAL_YEARS.getCode(), exception.getError().getCode());
  }

  @Test
  void testValidatePrepaymentTerm_ShouldThrowWhenDuplicateFiscalYearIdResultsInInsufficientDistinctCount() {
    // two fund distributions share the same fiscalYearId, distinct count is 1
    String sharedFyId = UUID.randomUUID().toString();
    PoLine poLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(2)
      .withFundDistribution(List.of(
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(50.0).withFiscalYearId(sharedFyId),
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(50.0).withFiscalYearId(sharedFyId)
      ));

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validatePrepaymentTerm(poLine));

    assertEquals(422, exception.getCode());
    assertEquals(PREPAYMENT_TERM_EXCEEDS_FISCAL_YEARS.getCode(), exception.getError().getCode());
  }

  @Test
  void testValidateFundDistributionForMultiYear_ShouldThrowOnFirstViolationWithMixedList() {
    String fyId1 = UUID.randomUUID().toString();
    PoLine violatingPoLine = new PoLine()
      .withMultiYearPayment(true)
      .withPrepaymentTerm(3)
      .withFundDistribution(List.of(
        new FundDistribution().withFundId(UUID.randomUUID().toString()).withDistributionType(DistributionType.PERCENTAGE).withValue(100.0).withFiscalYearId(fyId1)
      ));
    PoLine validPoLine = new PoLine()
      .withMultiYearPayment(false)
      .withFundDistribution(List.of());

    var exception = assertThrows(HttpException.class,
      () -> FundDistributionUtils.validateFundDistributionForMultiYear(List.of(violatingPoLine, validPoLine)));

    assertEquals(422, exception.getCode());
    assertEquals(PREPAYMENT_TERM_EXCEEDS_FISCAL_YEARS.getCode(), exception.getError().getCode());
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
