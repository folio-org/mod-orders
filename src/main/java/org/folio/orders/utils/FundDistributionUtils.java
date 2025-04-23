package org.folio.orders.utils;

import static java.math.RoundingMode.HALF_EVEN;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_MIX_TYPES_FOR_ZERO_PRICE;
import static org.folio.rest.core.exceptions.ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL;
import static org.folio.rest.jaxrs.model.FundDistribution.DistributionType.AMOUNT;
import static org.folio.rest.jaxrs.model.FundDistribution.DistributionType.PERCENTAGE;

import java.math.BigDecimal;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.rest.jaxrs.model.Cost;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;

import com.google.common.collect.Lists;

public final class FundDistributionUtils {

  public static final String REMAINING_AMOUNT_FIELD = "remainingAmount";

  private static final BigDecimal ZERO_REMAINING_AMOUNT = BigDecimal.ZERO.setScale(2, HALF_EVEN);
  private static final BigDecimal ONE_HUNDRED_PERCENT = BigDecimal.valueOf(100);

  private FundDistributionUtils() {

  }

  public static void validateFundDistributionTotal(List<PoLine> poLines) {
    for (PoLine cPoLine : poLines) {
      validateFundDistributionForPoLine(cPoLine.getCost(), cPoLine.getFundDistribution());
    }
  }

  public static void validateFundDistributionForPoLine(Cost cost, List<FundDistribution> fundDistributions) {
    if (cost.getPoLineEstimatedPrice() != null && CollectionUtils.isNotEmpty(fundDistributions)) {
      Double poLineEstimatedPrice = cost.getPoLineEstimatedPrice();
      if (poLineEstimatedPrice == 0d) {
        validateZeroPrice(fundDistributions);
        return;
      }
      BigDecimal remainingPercent = ONE_HUNDRED_PERCENT;

      for (FundDistribution fundDistribution : fundDistributions) {

        FundDistribution.DistributionType dType = fundDistribution.getDistributionType();
        if (dType == PERCENTAGE) {
          remainingPercent = remainingPercent.subtract(BigDecimal.valueOf(fundDistribution.getValue()));
        } else {
          Double value = fundDistribution.getValue();
          BigDecimal percentageValue = BigDecimal.valueOf(value)
            .divide(BigDecimal.valueOf(poLineEstimatedPrice), 15, HALF_EVEN)
            .movePointRight(2);
          remainingPercent = remainingPercent.subtract(percentageValue);
        }
      }
      checkRemainingPercentMatchesToZero(remainingPercent, poLineEstimatedPrice);
    }
  }

  public static void validateZeroPrice(List<FundDistribution> fdList) {
    FundDistribution.DistributionType firstFdType = fdList.get(0).getDistributionType();
    if (fdList.stream().skip(1).anyMatch(fd -> fd.getDistributionType() != firstFdType))
      throw new HttpException(422, CANNOT_MIX_TYPES_FOR_ZERO_PRICE);
    if (firstFdType == AMOUNT) {
      for (FundDistribution fd : fdList) {
        if (fd.getValue() != 0)
          throwExceptionWithIncorrectAmount(ZERO_REMAINING_AMOUNT);
      }
    } else {
      BigDecimal remainingPercent = ONE_HUNDRED_PERCENT;
      for (FundDistribution fd : fdList) {
        remainingPercent = remainingPercent.subtract(BigDecimal.valueOf(fd.getValue()));
      }
      checkRemainingPercentMatchesToZero(remainingPercent, 0d);
    }
  }

  public static boolean isFundDistributionsPresent(List<PoLine> poLines) {
    return poLines.stream().mapToLong(poLine -> poLine.getFundDistribution().size()).sum() >= 1;
  }

  private static void checkRemainingPercentMatchesToZero(BigDecimal remainingPercent, Double poLineEstimatedPrice) {
    BigDecimal epsilon = BigDecimal.valueOf(1e-10);
    if (remainingPercent.abs().compareTo(epsilon) > 0) {
      throwExceptionWithIncorrectAmount(remainingPercent, poLineEstimatedPrice);
    }
  }

  private static void throwExceptionWithIncorrectAmount(BigDecimal remainingPercent, Double poLineEstimatedPrice) {
    BigDecimal total = BigDecimal.valueOf(poLineEstimatedPrice);
    BigDecimal remainingAmount = remainingPercent.multiply(total).divide(ONE_HUNDRED_PERCENT, 2, HALF_EVEN);

    throwExceptionWithIncorrectAmount(remainingAmount);
  }

  private static void throwExceptionWithIncorrectAmount(BigDecimal remainingAmount) {
    throw new HttpException(422, INCORRECT_FUND_DISTRIBUTION_TOTAL, Lists.newArrayList(new Parameter()
      .withKey(REMAINING_AMOUNT_FIELD)
      .withValue(remainingAmount.toString())));
  }
}
