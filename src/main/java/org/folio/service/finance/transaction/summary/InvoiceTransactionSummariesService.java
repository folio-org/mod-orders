package org.folio.service.finance.transaction.summary;

import org.folio.rest.acq.model.finance.InvoiceTransactionSummary;
import org.folio.rest.core.RestClient;

public class InvoiceTransactionSummariesService extends AbstractTransactionSummariesService<InvoiceTransactionSummary> {

  private static final String SUMMARY_NAME = "invoice";

  public InvoiceTransactionSummariesService(RestClient restClient) {
    super(restClient);
  }

  public static InvoiceTransactionSummary buildInvoiceTransactionsSummary(String invoiceId, int size) {
    return new InvoiceTransactionSummary().withId(invoiceId)
        .withNumPendingPayments(size).withNumPaymentsCredits(size);
  }

  @Override
  protected String getId(InvoiceTransactionSummary summary) {
    return summary.getId();
  }

  @Override
  protected int getTransactionNumber(InvoiceTransactionSummary summary) {
    return summary.getNumPaymentsCredits() + summary.getNumPendingPayments();
  }

  @Override
  protected String getSummaryName() {
    return SUMMARY_NAME;
  }

  @Override
  protected Class<InvoiceTransactionSummary> getClassT() {
    return InvoiceTransactionSummary.class;
  }

}
