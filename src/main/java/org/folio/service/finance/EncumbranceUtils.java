package org.folio.service.finance;

import lombok.experimental.UtilityClass;
import org.folio.rest.acq.model.finance.Transaction;

import java.util.List;

@UtilityClass
public class EncumbranceUtils {

  public static List<Transaction> collectAllowedTransactionsForUnrelease(List<Transaction> transactions) {
    return transactions.stream().filter(EncumbranceUtils::allowTransactionToUnrelease).toList();
  }

  public static boolean allowTransactionToUnrelease(Transaction transaction) {
    var encumbrance = transaction.getEncumbrance();
    return encumbrance.getAmountExpended() == 0
      && encumbrance.getAmountCredited() == 0
      && encumbrance.getAmountAwaitingPayment() == 0;
  }
}
