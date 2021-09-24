package org.folio.orders.utils;

import static java.math.RoundingMode.HALF_EVEN;
import static org.folio.rest.core.exceptions.ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL;

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
          BigDecimal remainingPercent = BigDecimal.valueOf(100);

          for (FundDistribution fundDistribution : cPoLine.getFundDistribution()) {

            FundDistribution.DistributionType dType = fundDistribution.getDistributionType();
            if (dType == FundDistribution.DistributionType.PERCENTAGE) {
              // convert percent to amount
              remainingPercent = remainingPercent.subtract(BigDecimal.valueOf(fundDistribution.getValue()));
            } else {
              Double value = fundDistribution.getValue();
              BigDecimal percentageValue = BigDecimal.valueOf(value)
                  .divide(BigDecimal.valueOf(poLineEstimatedPrice), 15, HALF_EVEN)
                  .movePointRight(2);
              remainingPercent = remainingPercent.subtract(percentageValue);
            }
          }
          if (remainingPercent.compareTo(BigDecimal.ZERO) != 0) {
            throw new HttpException(422, INCORRECT_FUND_DISTRIBUTION_TOTAL);
          }
        }
      }
    }

  public static boolean isFundDistributionsPresent(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().mapToLong(compositePoLine -> compositePoLine.getFundDistribution().size()).sum() >= 1;
  }
}
