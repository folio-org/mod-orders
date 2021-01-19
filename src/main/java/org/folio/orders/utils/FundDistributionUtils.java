package org.folio.orders.utils;

import org.folio.orders.rest.exceptions.HttpException;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import javax.money.MonetaryAmount;
import java.util.List;

import static org.folio.orders.utils.ErrorCodes.INCORRECT_FUND_DISTRIBUTION_TOTAL;

public final class FundDistributionUtils {

    private FundDistributionUtils() {

    }

    public static void validateFundDistributionTotal(List<CompositePoLine> compositePoLines) {
      for (CompositePoLine cPoLine : compositePoLines) {

        if (cPoLine.getCost().getPoLineEstimatedPrice() != null && !cPoLine.getFundDistribution().isEmpty()) {
          Double poLineEstimatedPrice = cPoLine.getCost().getPoLineEstimatedPrice();
          String currency = cPoLine.getCost().getCurrency();
          MonetaryAmount remainingAmount = Money.of(poLineEstimatedPrice, currency);

          for (FundDistribution fundDistribution : cPoLine.getFundDistribution()) {
            FundDistribution.DistributionType dType = fundDistribution.getDistributionType();
            Double value = fundDistribution.getValue();
            MonetaryAmount amountValueMoney = Money.of(value, currency);

            if (dType == FundDistribution.DistributionType.PERCENTAGE) {
              // convert percent to amount
              amountValueMoney = Money.of(poLineEstimatedPrice, currency).with(MonetaryOperators.percent(value));
            }

            remainingAmount = remainingAmount.subtract(amountValueMoney);
          }
          if (!remainingAmount.isZero()) {
            throw new HttpException(422, INCORRECT_FUND_DISTRIBUTION_TOTAL);
          }
        }
      }
    }


  public static boolean isFundDistributionsPresent(List<CompositePoLine> compositePoLines) {
    return compositePoLines.stream().mapToLong(compositePoLine -> compositePoLine.getFundDistribution().size()).sum() >= 1;
  }
}
