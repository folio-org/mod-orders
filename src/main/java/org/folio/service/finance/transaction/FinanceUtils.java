package org.folio.service.finance.transaction;

import lombok.experimental.UtilityClass;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryFunctions;

import javax.money.CurrencyUnit;
import javax.money.MonetaryAmount;
import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

@UtilityClass
public class FinanceUtils {

  public static double calculateEncumbranceTotalAmount(Transaction transaction) {
    Encumbrance encumbrance = transaction.getEncumbrance();
    return encumbrance == null ? 0 : encumbrance.getAmountExpended() - encumbrance.getAmountCredited();
  }

  public static double sumAmounts(List<Transaction> list, Function<Encumbrance, Double> getAmount) {
    return list.stream()
      .map(encumbrance -> getAmount.apply(encumbrance.getEncumbrance()))
      .map(BigDecimal::valueOf)
      .reduce(BigDecimal.ZERO, BigDecimal::add).doubleValue();
  }

  public static double calculateEncumbranceEffectiveAmount(double initialAmount,
                                                           double expended,
                                                           double credited,
                                                           double awaitingPayment,
                                                           CurrencyUnit fiscalYearCurrency) {
    return calculateEncumbranceEffectiveAmount(
      Money.of(initialAmount, fiscalYearCurrency),
      Money.of(expended, fiscalYearCurrency),
      Money.of(credited, fiscalYearCurrency),
      Money.of(awaitingPayment, fiscalYearCurrency),
      fiscalYearCurrency
    ).getNumber().doubleValue();
  }

  public static MonetaryAmount calculateEncumbranceEffectiveAmount(MonetaryAmount initialAmount,
                                                                   MonetaryAmount expended,
                                                                   MonetaryAmount credited,
                                                                   MonetaryAmount awaitingPayment,
                                                                   CurrencyUnit fiscalYearCurrency) {
    return MonetaryFunctions.max().apply(
      initialAmount.subtract(expended).add(credited).subtract(awaitingPayment),
      Money.zero(fiscalYearCurrency)
    );
  }

}
