package org.folio.service.finance.transaction;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.HttpStatus;
import org.folio.completablefuture.FolioVertxCompletableFuture;
import org.folio.models.EncumbranceRelationsHolder;
import org.folio.models.EncumbrancesProcessingHolder;
import org.folio.orders.rest.exceptions.HttpException;
import org.folio.orders.utils.ErrorCodes;
import org.folio.orders.utils.HelperUtils;
import org.folio.rest.acq.model.finance.Encumbrance;
import org.folio.rest.acq.model.finance.Tags;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.finance.TransactionCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.Error;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.service.invoice.InvoiceService;
import org.javamoney.moneta.function.MonetaryOperators;

import javax.money.MonetaryAmount;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;

import static java.util.stream.Collectors.toList;
import static org.folio.orders.utils.ErrorCodes.BUDGET_IS_INACTIVE;
import static org.folio.orders.utils.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.ErrorCodes.FUND_CANNOT_BE_PAID;
import static org.folio.orders.utils.ErrorCodes.LEDGER_NOT_FOUND_FOR_TRANSACTION;
import static org.folio.orders.utils.HelperUtils.calculateEstimatedPrice;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

public class EncumbranceService {
//String a = "{ \"orderType\": \"ListedPrintMonograph\", \"mappings\": [ { \"field\": \"ACQUISITION_METHOD\", \"dataSource\": { \"default\": \"Purchase At Vendor System\" } }, { \"field\": \"APPROVED\", \"dataSource\": { \"default\": \"true\", \"translation\": \"toBoolean\", \"translateDefault\": true } }, { \"field\": \"CLAIMED\", \"dataSource\": { \"default\": \"true\", \"translation\": \"toBoolean\", \"translateDefault\": true } }, { \"field\": \"COLLECTION\", \"dataSource\": { \"default\": \"false\", \"translation\": \"toBoolean\", \"translateDefault\": true } }, { \"field\": \"CONTRIBUTOR\", \"dataSource\": { \"from\": \"//datafield[@tag='100']/*\", \"combinator\": \"concat\" } }, { \"field\": \"CONTRIBUTOR_NAME_TYPE\", \"dataSource\": { \"default\": \"Personal name\", \"translation\": \"lookupContributorNameTypeId\", \"translateDefault\": true } }, { \"field\": \"CURRENCY\", \"dataSource\": { \"from\": \"//ListPrice/Currency\", \"default\": \"USD\" } }, { \"field\": \"DATE_ORDERED\", \"dataSource\": { \"from\": \"//OrderPlaced\", \"translation\": \"toDate\" } }, { \"field\": \"EXPENSE_CLASS\", \"dataSource\": { \"from\": \"//LocalData[Description='LocalData4']/Value\", \"translation\": \"lookupExpenseClassId\" } }, { \"field\": \"FUND_ID\", \"dataSource\": { \"from\": \"//FundCode\", \"translation\": \"lookupFundId\" } }, { \"field\": \"FUND_CODE\", \"dataSource\": { \"from\": \"//FundCode\" } }, { \"field\": \"FUND_PERCENTAGE\", \"dataSource\": { \"default\": \"100\", \"translation\": \"toDouble\", \"translateDefault\": true } }, { \"field\": \"VENDOR_INSTRUCTIONS\", \"dataSource\": { \"from\": \"//OrderNotes\", \"default\" : \"N/A\" } }, { \"field\": \"LIST_UNIT_PRICE\", \"dataSource\": { \"from\": \"//ListPrice/Amount\", \"default\": \"0\", \"translation\": \"toDouble\", \"translateDefault\": true } }, { \"field\": \"LOCATION\", \"dataSource\": { \"from\": \"//Location\", \"default\": \"*\", \"translation\": \"lookupLocationId\", \"translateDefault\": true } }, { \"field\": \"MANUAL_PO\", \"dataSource\": { \"default\": \"false\", \"translation\": \"toBoolean\", \"translateDefault\": true } }, { \"field\": \"MATERIAL_TYPE\", \"dataSource\": { \"from\": \"//LocalData[Description='LocalData1']/Value\", \"default\" : \"unspecified\", \"translation\": \"lookupMaterialTypeId\", \"translateDefault\": true } }, { \"field\": \"ORDER_TYPE\", \"dataSource\": { \"default\": \"One-Time\" } }, { \"field\": \"PO_LINE_ORDER_FORMAT\", \"dataSource\": { \"default\": \"Physical Resource\" } }, { \"field\": \"PO_LINE_PAYMENT_STATUS\", \"dataSource\": { \"default\": \"Awaiting Payment\" } }, { \"field\": \"PO_LINE_RECEIPT_STATUS\", \"dataSource\": { \"default\": \"Awaiting Receipt\" } }, { \"field\": \"PRODUCT_ID\", \"dataSource\": { \"from\": \"//datafield[@tag='020']/subfield[@code='a']\", \"translation\": \"truncateISBNQualifier\" } }, { \"field\": \"PRODUCT_ID_TYPE\", \"dataSource\": { \"default\": \"ISBN\", \"translation\": \"lookupProductIdType\", \"translateDefault\": true } }, { \"field\": \"PRODUCT_QUALIFIER\", \"dataSource\": { \"from\": \"//datafield[@tag='020']/subfield[@code='q']\", \"defaultMapping\": { \"dataSource\": { \"from\": \"//datafield[@tag='020']/subfield[@code='a']\", \"translation\": \"separateISBNQualifier\" } } } }, { \"field\": \"PUBLICATION_DATE\", \"dataSource\": { \"from\": \"//datafield[@tag='260']/subfield[@code='c']\" } }, { \"field\": \"PUBLISHER\", \"dataSource\": { \"from\": \"//datafield[@tag='260']/subfield[@code='b']\" } }, { \"field\": \"QUANTITY_PHYSICAL\", \"dataSource\": { \"from\": \"//Quantity\", \"default\": \"1\", \"translation\": \"toInteger\" } }, { \"field\": \"RECEIVING_NOTE\", \"dataSource\": { \"from\": \"//LocalData[Description='LocalData2']/Value\" } }, { \"field\": \"REQUESTER\", \"dataSource\": { \"from\": \"//LocalData[Description='LocalData3']/Value\" } }, { \"field\": \"SOURCE\", \"dataSource\": { \"default\": \"API\" } }, { \"field\": \"TITLE\", \"dataSource\": { \"from\": \"//datafield[@tag='245']/*\", \"combinator\": \"concat\" } }, { \"field\": \"VENDOR\", \"dataSource\": { \"default\": \"GOBI\", \"translation\": \"lookupOrganization\", \"translateDefault\": true } }, { \"field\": \"VENDOR_ACCOUNT\", \"dataSource\": { \"from\": \"//SubAccount\", \"default\": \"0\" } }, { \"field\": \"VENDOR_REF_NO\", \"dataSource\": { \"from\": \"//YBPOrderKey\" } }, { \"field\": \"VENDOR_REF_NO_TYPE\", \"dataSource\": { \"default\": \"Vendor order reference number\" } }, { \"field\": \"WORKFLOW_STATUS\", \"dataSource\": { \"default\": \"Open\" } } ] }"
  private static final String ENCUMBRANCE_CRITERIA = "transactionType==Encumbrance";
  public static final String AND = " and ";
  public static final String FUND_CODE = "fundCode";
  public static final String EXPENSE_CLASS_NAME = "expenseClassName";

  private final TransactionService transactionService;
  private final TransactionSummariesService transactionSummariesService;
  private final InvoiceService invoiceService;

  public EncumbranceService(TransactionService transactionService,
                            TransactionSummariesService transactionSummariesService,
                            InvoiceService invoiceService) {
    this.transactionService = transactionService;
    this.transactionSummariesService = transactionSummariesService;
    this.invoiceService = invoiceService;
  }


  public CompletableFuture<Void> createOrUpdateEncumbrances(EncumbrancesProcessingHolder holder, RequestContext requestContext) {

    if (holder.getAllEncumbrancesQuantity() == 0)
      return CompletableFuture.completedFuture(null);
    return transactionSummariesService.createOrUpdateOrderTransactionSummary(holder, requestContext)
        .thenCompose(v -> createEncumbrances(holder.getEncumbrancesForCreate(), requestContext))
        .thenCompose(v -> releaseEncumbrances(holder.getEncumbrancesForRelease(), requestContext))
        .thenCompose(v -> unreleaseEncumbrances(holder.getEncumbrancesForUnrelease(), requestContext))
        .thenCompose(v -> updateEncumbrances(holder, requestContext))
        .thenCompose(v -> deleteEncumbrances(holder.getEncumbrancesForDelete(), requestContext)); // thenAccept
  }

  public CompletableFuture<Void> createEncumbrances(List<EncumbranceRelationsHolder> relationsHolders, RequestContext requestContext) {
    return FolioVertxCompletableFuture.allOf(requestContext.getContext(), relationsHolders.stream()
            .map(holder -> transactionService.createTransaction(holder.getNewEncumbrance(), requestContext)
                    .thenAccept(transaction -> holder.getFundDistribution().setEncumbrance(transaction.getId()))
                    .exceptionally(fail -> {
                      checkCustomTransactionError(fail);
                      throw new CompletionException(fail);
                    })
            )
            .toArray(CompletableFuture[]::new)
    );
  }

  public CompletionStage<Void> updateEncumbrancesOrderStatus(CompositePurchaseOrder compPo, RequestContext requestContext) {
    return getOrderUnreleasedEncumbrances(compPo.getId(), requestContext)
            .thenCompose(encumbrs -> {
              if (isEncumbrancesOrderStatusUpdateNeeded(compPo.getWorkflowStatus(), encumbrs)) {
                return transactionSummariesService.updateOrderTransactionSummary(compPo.getId(), encumbrs.size(), requestContext)
                        .thenApply(v -> {
                          syncEncumbrancesOrderStatus(compPo.getWorkflowStatus(), encumbrs);
                          return encumbrs;
                        })
                        .thenCompose(transactions -> transactionService.updateTransactions(transactions, requestContext));
              }
              return CompletableFuture.completedFuture(null);
            });
  }

  private void syncEncumbrancesOrderStatus(CompositePurchaseOrder.WorkflowStatus workflowStatus,
                                           List<Transaction> encumbrances) {
    Encumbrance.OrderStatus orderStatus = Encumbrance.OrderStatus.fromValue(workflowStatus.value());
    encumbrances.forEach(encumbrance -> encumbrance.getEncumbrance().setOrderStatus(orderStatus));
  }


  private boolean isEncumbrancesOrderStatusUpdateNeeded(CompositePurchaseOrder.WorkflowStatus orderStatus, List<Transaction> encumbrs) {
    return !CollectionUtils.isEmpty(encumbrs) && !orderStatus.value().equals(encumbrs.get(0).getEncumbrance().getOrderStatus().value());
  }


  private CompletionStage<Void> updateEncumbrances(EncumbrancesProcessingHolder holder, RequestContext requestContext) {
    List<Transaction> encumbrances = holder.getEncumbrancesForUpdate().stream()
            .map(EncumbranceRelationsHolder::getNewEncumbrance)
            .collect(toList());
    return updateEncumbrances(encumbrances, requestContext);
  }

  public CompletableFuture<List<Transaction>> getOrderUnreleasedEncumbrances(String orderId, RequestContext requestContext) {
    return transactionService.getTransactions(buildNonReleasedEncumbranceOrderQuery(orderId), 0, Integer.MAX_VALUE, requestContext)
              .thenApply(TransactionCollection::getTransactions);
  }

  public CompletableFuture<List<Transaction>> getOrderEncumbrancesToUnrelease(CompositePurchaseOrder compPO,
                                                                              Map<String, List<CompositePoLine>> mapFiscalYearWithCompPOLines,
                                                                              RequestContext requestContext) {
    // Check invoice line's releaseEncumbrance value for each order line
    List<CompletableFuture<List<Transaction>>> futures =
      compPO.getCompositePoLines()
        .stream()
        .map(poLine -> getPoLineEncumbrancesToUnrelease(poLine, mapFiscalYearWithCompPOLines, requestContext))
        .collect(toList());
    return HelperUtils.collectResultsOnSuccess(futures)
      .thenApply(listOfLists -> listOfLists.stream().flatMap(List::stream).collect(toList()));
  }

  public CompletableFuture<List<Transaction>> getPoLineUnreleasedEncumbrances(String poLineId, RequestContext requestContext) {
    return transactionService.getTransactions(buildNonReleasedEncumbranceByPoLineQuery(poLineId), 0, Integer.MAX_VALUE, requestContext)
      .thenApply(TransactionCollection::getTransactions);
  }

  public CompletableFuture<List<Transaction>> getEncumbrancesByIds(List<String> transactionIds, RequestContext requestContext) {
    return transactionService.getTransactionsByIds(transactionIds, requestContext);
  }

  public CompletableFuture<List<Transaction>> getCurrentPoLinesEncumbrances(List<CompositePoLine> poLines, String fiscalYearId, RequestContext requestContext) {
    String searchCriteria = "fiscalYearId==" + fiscalYearId;
    List<String> poLineIds = poLines.stream().map(CompositePoLine::getId).collect(toList());
    return transactionService.getTransactionsByPoLinesIds(poLineIds, searchCriteria, requestContext);
  }

  public CompletableFuture<List<List<Transaction>>> getEncumbrancesByPoLinesFromCurrentFy(
                             Map<String, List<CompositePoLine>> poLinesByFy, RequestContext requestContext) {
    return collectResultsOnSuccess(poLinesByFy.entrySet()
      .stream()
      .map(entry -> getCurrentPoLinesEncumbrances(entry.getValue(), entry.getKey(), requestContext))
      .collect(toList()));
  }

  public void updateEncumbrance(FundDistribution fundDistribution, CompositePoLine poLine, Transaction trEncumbrance) {
    MonetaryAmount estimatedPrice = calculateEstimatedPrice(poLine.getCost());
    trEncumbrance.setAmount(calculateAmountEncumbered(fundDistribution, estimatedPrice));
    trEncumbrance.setExpenseClassId(fundDistribution.getExpenseClassId());
    if (Objects.nonNull(poLine.getTags())) {
      trEncumbrance.setTags(new Tags().withTagList(poLine.getTags().getTagList()));
    }
    Encumbrance encumbrance = trEncumbrance.getEncumbrance();
    encumbrance.setStatus(Encumbrance.Status.UNRELEASED);
    encumbrance.setInitialAmountEncumbered(trEncumbrance.getAmount());
  }

  public CompletableFuture<Void> releaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    encumbrances.forEach(transaction -> transaction.getEncumbrance().setStatus(Encumbrance.Status.RELEASED));
    return transactionService.updateTransactions(encumbrances, requestContext);
  }

  public CompletableFuture<Void> unreleaseEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    encumbrances.forEach(transaction -> transaction.getEncumbrance().setStatus(Encumbrance.Status.UNRELEASED));
    return transactionService.updateTransactions(encumbrances, requestContext);
  }

  public CompletableFuture<Void> updateEncumbrances(List<Transaction> transactions, RequestContext requestContext) {
    return transactionService.updateTransactions(transactions, requestContext);
  }

  public CompletableFuture<Void> deleteEncumbrances(List<Transaction> encumbrances, RequestContext requestContext) {
    return transactionService.deleteTransactions(encumbrances, requestContext);
  }

  public CompletableFuture<Void> deletePoLineEncumbrances(String lineId, RequestContext requestContext) {
    return getPoLineUnreleasedEncumbrances(lineId, requestContext)
      .thenCompose(encumbrances -> transactionService.deleteTransactions(encumbrances, requestContext));
  }

  public CompletableFuture<Void> deleteOrderEncumbrances(String orderId, RequestContext requestContext) {
    return getOrderUnreleasedEncumbrances(orderId, requestContext)
      .thenCompose(encumbrances -> transactionService.deleteTransactions(encumbrances, requestContext));
  }

  public static double calculateAmountEncumbered(FundDistribution distribution, MonetaryAmount estimatedPrice) {
    if (distribution.getDistributionType() == FundDistribution.DistributionType.PERCENTAGE) {
      return estimatedPrice.with(MonetaryOperators.percent(distribution.getValue()))
        .with(MonetaryOperators.rounding())
        .getNumber()
        .doubleValue();
    }
    return distribution.getValue();
  }

  private String buildNonReleasedEncumbranceOrderQuery(String orderId) {
    return ENCUMBRANCE_CRITERIA
                + AND + "encumbrance.sourcePurchaseOrderId==" + orderId
                    + AND + "encumbrance.status <> " + Encumbrance.Status.RELEASED;
  }

  private String buildNonReleasedEncumbranceByPoLineQuery(String polineId) {
    return ENCUMBRANCE_CRITERIA
      + AND + "encumbrance.sourcePoLineId==" + polineId
      + AND + "encumbrance.status <> " + Encumbrance.Status.RELEASED;
  }

  private String buildReleasedEncumbranceByPoLineQuery(String poLineId, String fiscalYearId) {
    return ENCUMBRANCE_CRITERIA
      + AND + "encumbrance.sourcePoLineId == " + poLineId
      + AND + "encumbrance.status == " + Encumbrance.Status.RELEASED
      + AND + "encumbrance.fiscalYearId == " + fiscalYearId;
  }

  private CompletableFuture<List<Transaction>> getPoLineEncumbrancesToUnrelease(CompositePoLine poLine,
                                                                                Map<String, List<CompositePoLine>> mapFiscalYearWithCompPOLines,
                                                                                RequestContext requestContext) {
    final String[] currentFiscalYearId = new String[1];
    mapFiscalYearWithCompPOLines.entrySet()
      .stream()
      .map(entry ->
      {
        for (CompositePoLine pLine : entry.getValue()) {
          if (poLine.equals(pLine)) {
            currentFiscalYearId[0] = entry.getKey();
            break;
          }
        }
        return null;
      });
    if (currentFiscalYearId[0] == null) {
        Error error = ErrorCodes.CURRENT_FISCAL_YEAR_ID_NOT_FOUND.toError();
        List<Parameter> parameters = Collections.singletonList(new Parameter().withKey("poLineNumber")
                                           .withValue(poLine.getPoLineNumber()));
      throw new HttpException(404, error.withParameters(parameters));
    }
    return hasNotInvoiceLineWithReleaseEncumbrance(poLine, requestContext)
      .thenCompose(hasNotInvoiceLineWithReleaseEncumbrance -> {
        if (hasNotInvoiceLineWithReleaseEncumbrance)
          return CompletableFuture.completedFuture(Collections.emptyList());
        return transactionService.getTransactions(
          buildReleasedEncumbranceByPoLineQuery(poLine.getId(), currentFiscalYearId[0]), 0, Integer.MAX_VALUE, requestContext)
            .thenApply(TransactionCollection::getTransactions);
      });
  }

  private CompletableFuture<Boolean> hasNotInvoiceLineWithReleaseEncumbrance(CompositePoLine poLine, RequestContext requestContext) {
    return invoiceService.retrieveInvoiceLines("poLineId == " + poLine.getId() + AND + "releaseEncumbrance == true", requestContext)
      .thenApply(invoiceLines -> !invoiceLines.isEmpty())
      .exceptionally(t -> {
        Throwable cause = t.getCause();
        // ignore 404 errors as mod-invoice may be unavailable
        if (cause instanceof HttpException && ((HttpException)cause).getCode() == HttpStatus.HTTP_NOT_FOUND.toInt())
          return false;
        throw t instanceof CompletionException ? (CompletionException) t : new CompletionException(cause);
      });
  }

  protected void checkCustomTransactionError(Throwable fail) {
    if (fail.getCause().getMessage().contains(BUDGET_NOT_FOUND_FOR_TRANSACTION.getDescription())) {
      throw new CompletionException(new HttpException(422, BUDGET_NOT_FOUND_FOR_TRANSACTION));
    } else if (fail.getCause().getMessage().contains(LEDGER_NOT_FOUND_FOR_TRANSACTION.getDescription())) {
      throw new CompletionException(new HttpException(422, LEDGER_NOT_FOUND_FOR_TRANSACTION));
    } else if (fail.getCause().getMessage().contains(BUDGET_IS_INACTIVE.getDescription())) {
      throw new CompletionException(new HttpException(422, BUDGET_IS_INACTIVE));
    } else if (fail.getCause().getMessage().contains(FUND_CANNOT_BE_PAID.getDescription())) {
      throw new CompletionException(new HttpException(422, FUND_CANNOT_BE_PAID));
    } else {
      throw new CompletionException(fail.getCause());
    }
  }

}
