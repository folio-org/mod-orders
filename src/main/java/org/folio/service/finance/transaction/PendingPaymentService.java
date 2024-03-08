package org.folio.service.finance.transaction;

public class PendingPaymentService extends AbstractTransactionManagingService {

  public PendingPaymentService(TransactionService transactionService) {
    super(transactionService);
  }

  @Override
  protected String getEncumbranceIdentifierQuery() {
    return "awaitingPayment.encumbranceId";
  }

}
