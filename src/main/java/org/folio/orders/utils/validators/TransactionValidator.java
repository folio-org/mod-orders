package org.folio.orders.utils.validators;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.jaxrs.model.Parameter;

import java.util.Collections;

import static org.folio.rest.core.exceptions.ErrorCodes.DELETE_WITH_EXPENDED_AMOUNT;

public class TransactionValidator {
  private static final Logger logger = LogManager.getLogger(TransactionValidator.class);

  private TransactionValidator() {

  }

  public static void validateEncumbranceForDeletion(Transaction transaction) {
    String transactionId = transaction.getId();
    Encumbrance encumbrance = transaction.getEncumbrance();
    if (encumbrance != null && encumbrance.getAmountExpended() > 0) {
      logger.info("Tried to delete transaction {} but it has an expended amount.", transactionId) ;
      Parameter parameter = new Parameter().withKey("id").withValue(transactionId);
      throw new HttpException(422, DELETE_WITH_EXPENDED_AMOUNT.toError().withParameters(Collections.singletonList(parameter)));
    }
  }

}
