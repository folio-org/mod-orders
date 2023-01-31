package org.folio.service.finance.transaction;

import org.folio.rest.core.RestClient;

public class PendingPaymentService extends AbstractTransactionManagingService {

  public PendingPaymentService(TransactionService transactionService, RestClient restClient) {
    super(transactionService, restClient);
  }

  @Override
  protected String getEncumbranceIdentifierQuery() {
    return "awaitingPayment.encumbranceId";
  }

}
