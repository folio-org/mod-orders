package org.folio.orders.utils;

import static java.math.RoundingMode.HALF_EVEN;
import static org.folio.rest.core.exceptions.ErrorCodes.CANNOT_MIX_TYPES_FOR_ZERO_PRICE;
import static org.folio.rest.core.exceptions.ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL;
import static org.folio.rest.jaxrs.model.FundDistribution.DistributionType.AMOUNT;
import static org.folio.rest.jaxrs.model.FundDistribution.DistributionType.PERCENTAGE;

import java.math.BigDecimal;
import java.util.List;

import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;

public final class FundDistributionUtils {

  private FundDistributionUtils() {

  }

  public static void validateFundDistributionTotal(List<CompositePoLine> compositePoLines) {
    for (CompositePoLine cPoLine : compositePoLines) {

      if (cPoLine.getCost().getPoLineEstimatedPrice() != null && !cPoLine.getFundDistribution().isEmpty()) {
        Double poLineEstimatedPrice = cPoLine.getCost().getPoLineEstimatedPrice();
        if (poLineEstimatedPrice == 0d) {
          validateZeroPrice(cPoLine.getFundDistribution());
          continue;
        }
        BigDecimal remainingPercent = BigDecimal.valueOf(100);

        for (FundDistribution fundDistribution : cPoLine.getFundDistribution()) {

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
        BigDecimal epsilon = BigDecimal.valueOf(1e-10);
        if (remainingPercent.abs().compareTo(epsilon) > 0) {
          throw new HttpException(422, INCORRECT_FUND_DISTRIBUTION_TOTAL);
        }
      }
    }
  }

  public static void validateZeroPrice(List<FundDistribution> fdList) {
    FundDistribution.DistributionType firstFdType = fdList.get(0).getDistributionType();
    if (fdList.stream().skip(1).anyMatch(fd -> fd.getDistributionType() != firstFdType))
      throw new HttpException(422, CANNOT_MIX_TYPES_FOR_ZERO_PRICE);
    if (firstFdType == AMOUNT) {
      for (FundDistribution fd : fdList) {
        if (fd.getValue() != 0)
          throw new HttpException(422, INCORRECT_FUND_DISTRIBUTION_TOTAL);
      }
    } else {
      double percentTotal = 0;
      for (FundDistribution fd : fdList)
        percentTotal += fd.getValue();
      if (percentTotal != 100)
        throw new HttpException(422, INCORRECT_FUND_DISTRIBUTION_TOTAL);
    }
  }

  public static boolean isFundDistributionsPresent(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().mapToLong(compositePoLine -> compositePoLine.getFundDistribution().size()).sum() >= 1;
  }
}
