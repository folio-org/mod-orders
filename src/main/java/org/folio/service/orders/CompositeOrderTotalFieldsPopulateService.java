package org.folio.service.orders;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.lang3.StringUtils;
import org.folio.models.CompositeOrderRetrieveHolder;
import org.folio.rest.acq.model.finance.ExchangeRate;
import org.folio.rest.acq.model.finance.Transaction;
import org.folio.rest.acq.model.invoice.Invoice;
import org.folio.rest.acq.model.invoice.InvoiceLine;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.PoLine;
import org.folio.service.caches.CommonSettingsCache;
import org.folio.service.exchange.CacheableExchangeRateService;
import org.folio.service.exchange.CustomExchangeRateProvider;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.transaction.TransactionService;
import org.folio.service.invoice.InvoiceLineService;
import org.folio.service.invoice.InvoiceService;
import org.javamoney.moneta.Money;
import org.javamoney.moneta.function.MonetaryOperators;

import io.vertx.core.Future;

import javax.money.convert.ConversionQuery;
import javax.money.convert.CurrencyConversion;

import static java.util.Collections.emptyMap;
import static java.util.stream.Collectors.toMap;
import static org.folio.orders.utils.HelperUtils.buildConversionQuery;
import static org.folio.orders.utils.HelperUtils.collectResultsOnSuccess;

@Log4j2
public class CompositeOrderTotalFieldsPopulateService implements CompositeOrderDynamicDataPopulateService {

  private final TransactionService transactionService;
  private final InvoiceService invoiceService;
  private final InvoiceLineService invoiceLineService;
  private final FiscalYearService fiscalYearService;
  private final CommonSettingsCache commonSettingsCache;
  private final CacheableExchangeRateService cacheableExchangeRateService;

  public CompositeOrderTotalFieldsPopulateService(TransactionService transactionService, InvoiceService invoiceService,
                                                  InvoiceLineService invoiceLineService, FiscalYearService fiscalYearService,
                                                  CommonSettingsCache commonSettingsCache, CacheableExchangeRateService cacheableExchangeRateService) {
    this.transactionService = transactionService;
    this.invoiceService = invoiceService;
    this.invoiceLineService = invoiceLineService;
    this.fiscalYearService = fiscalYearService;
    this.commonSettingsCache = commonSettingsCache;
    this.cacheableExchangeRateService = cacheableExchangeRateService;
  }

  @Override
  public Future<CompositeOrderRetrieveHolder> populate(CompositeOrderRetrieveHolder holder, RequestContext requestContext) {
    // We need to fetch invoices, invoice lines and transactions in order to calculate total fields
    // totalEncumbered = sum of transactions amounts
    // totalExpended = sum of invoice lines positive total values
    // totalCredited = absolute sum of invoice lines negative total values
    var query = "transactionType==Encumbrance AND encumbrance.sourcePurchaseOrderId==%s AND fiscalYearId==%s"
      .formatted(holder.getOrderId(), holder.getFiscalYearId());
    return invoiceService.getInvoicesByOrderId(holder.getOrderId(), requestContext)
      .compose(invoices -> getCurrentFiscalYearIds(invoices, holder.getFiscalYearId(), requestContext)
        .compose(fiscalYears -> getInvoiceLinesByInvoiceIds(invoices, fiscalYears, requestContext))
        .map(invoiceLines -> filterInvoiceLinesWithPoLines(invoiceLines, holder.getOrder().getPoLines()))
        .map(invoiceLines -> groupInvoiceLinesByInvoices(invoices, invoiceLines))
        .compose(invoiceToInvoiceLinesMap -> transactionService.getTransactions(query, requestContext)
          .compose(transactions -> getExchangeRates(invoices, transactions, requestContext)
            .map(exchangeRateMaps -> populateTotalFields(holder, invoiceToInvoiceLinesMap,
              transactions, exchangeRateMaps)))))
      .onFailure(t -> log.error("Failed to populate order's '{}' total fields with fiscal year '{}' from invoice lines",
        holder.getOrderId(), holder.getFiscalYearId(), t));
  }

  private Future<Set<String>> getCurrentFiscalYearIds(List<Invoice> invoices, String fiscalYearId, RequestContext requestContext) {
    if (StringUtils.isNotBlank(fiscalYearId)) {
      return Future.succeededFuture(Set.of(fiscalYearId));
    }
    return collectResultsOnSuccess(invoices.stream()
      .map(Invoice::getFiscalYearId)
      .filter(Objects::nonNull)
      .map(invoiceFiscalYearId -> fiscalYearService.getCurrentFYForSeriesByFYId(invoiceFiscalYearId, requestContext)).toList())
      .map(fiscalYearsIds -> fiscalYearsIds.stream().filter(Objects::nonNull).collect(Collectors.toSet()));
  }

  private Future<List<InvoiceLine>> getInvoiceLinesByInvoiceIds(List<Invoice> invoices, Set<String> fiscalYearIds, RequestContext requestContext) {
    return collectResultsOnSuccess(invoices.stream()
      .filter(invoice -> invoice.getFiscalYearId() != null && fiscalYearIds.contains(invoice.getFiscalYearId()))
      .map(invoice -> invoiceLineService.getInvoiceLinesByInvoiceIdAndStatus(invoice.getId(), InvoiceLine.InvoiceLineStatus.PAID, requestContext)).toList())
      .map(invoiceLinesLists -> invoiceLinesLists.stream().flatMap(List::stream).toList());
  }

  private List<InvoiceLine> filterInvoiceLinesWithPoLines(List<InvoiceLine> invoiceLines, List<PoLine> poLines) {
    Set<String> poLineIds = poLines.stream().map(PoLine::getId).collect(Collectors.toCollection(HashSet::new));
    return invoiceLines.stream().filter(invoiceLine -> poLineIds.contains(invoiceLine.getPoLineId())).toList();
  }

  private Map<Invoice, List<InvoiceLine>> groupInvoiceLinesByInvoices(List<Invoice> invoices, List<InvoiceLine> invoiceLines) {
    return invoices.stream()
      .collect(toMap(invoice -> invoice, invoice -> invoiceLines.stream()
        .filter(invoiceLine -> invoiceLine.getInvoiceId().equals(invoice.getId()))
        .collect(Collectors.toList())));
  }

  private static class ExchangeRateMaps {
    public Map<Invoice, ExchangeRate> invoiceRates;
    public Map<Transaction, ExchangeRate> encumbranceRates;
  }

  private Future<ExchangeRateMaps> getExchangeRates(List<Invoice> invoices, List<Transaction> transactions,
      RequestContext requestContext) {
    return commonSettingsCache.getSystemCurrency(requestContext)
      .compose(systemCurrency -> getInvoiceExchangeRates(invoices, systemCurrency, requestContext)
        .compose(invoiceRates -> getEncumbranceExchangeRates(transactions, systemCurrency,
            requestContext)
          .map(encumbranceRates -> {
            ExchangeRateMaps maps = new ExchangeRateMaps();
            maps.invoiceRates = invoiceRates;
            maps.encumbranceRates = encumbranceRates;
            return maps;
          })
      ));
  }

  private Future<Map<Invoice, ExchangeRate>> getInvoiceExchangeRates(List<Invoice> invoices, String systemCurrency,
      RequestContext requestContext) {
    if (invoices.isEmpty()) {
      return Future.succeededFuture(emptyMap());
    }
    var exchangeRatesFutures = new ArrayList<Future<ExchangeRate>>();
    invoices.forEach(invoice -> exchangeRatesFutures.add(cacheableExchangeRateService.getExchangeRate(
      invoice.getCurrency(), systemCurrency, invoice.getExchangeRate(), requestContext)));
    return collectResultsOnSuccess(exchangeRatesFutures)
      .map(exchangeRates -> IntStream.range(0, invoices.size())
        .boxed()
        .collect(toMap(invoices::get, exchangeRates::get)));
  }

  private Future<Map<Transaction, ExchangeRate>> getEncumbranceExchangeRates(List<Transaction> transactions,
      String systemCurrency, RequestContext requestContext) {
    if (transactions.isEmpty()) {
      return Future.succeededFuture(emptyMap());
    }
    List<String> currencies = transactions.stream()
      .map(Transaction::getCurrency)
      .distinct()
      .toList();
    var exchangeRatesFutures = new ArrayList<Future<ExchangeRate>>();
    currencies.forEach(currency -> exchangeRatesFutures.add(cacheableExchangeRateService.getExchangeRate(
      currency, systemCurrency, null, requestContext)));
    return collectResultsOnSuccess(exchangeRatesFutures)
      .map(exchangeRates -> IntStream.range(0, transactions.size())
        .boxed()
        .collect(toMap(transactions::get, i -> exchangeRates.get(currencies.indexOf(transactions.get(i).getCurrency())))));
  }

  private CompositeOrderRetrieveHolder populateTotalFields(CompositeOrderRetrieveHolder holder,
      Map<Invoice, List<InvoiceLine>> invoiceToInvoiceLinesMap, List<Transaction> transactions,
      ExchangeRateMaps exchangeRateMaps) {
    return holder
      .withTotalEncumbered(getTotalAmountSum(transactions, exchangeRateMaps.encumbranceRates))
      .withTotalExpended(getTotalAmountSum(invoiceToInvoiceLinesMap, total -> Double.max(total, 0),
        exchangeRateMaps.invoiceRates))
      .withTotalCredited(getTotalAmountSum(invoiceToInvoiceLinesMap, total -> Double.min(total, 0),
        exchangeRateMaps.invoiceRates));
  }

  private double getTotalAmountSum(List<Transaction> transactions, Map<Transaction, ExchangeRate> encumbranceRates) {
    return getTotalAmount(transactions, Transaction::getCurrency, Transaction::getAmount, encumbranceRates::get);
  }

  private double getTotalAmountSum(Map<Invoice, List<InvoiceLine>> invoiceToInvoiceLinesMap,
      Function<Double, Double> amountMapper, Map<Invoice, ExchangeRate> invoiceRates) {
    return StreamEx.of(invoiceToInvoiceLinesMap.entrySet())
      .mapToDouble(entry -> getTotalAmount(entry.getValue(),
        invoiceLine -> entry.getKey().getCurrency(),
        invoiceLine -> Math.abs(amountMapper.apply(invoiceLine.getTotal())),
        invoiceLine -> invoiceRates.get(entry.getKey())))
      .sum();
  }

  private <T> double getTotalAmount(List<T> entities, Function<T, String> currencyExtractor,
      Function<T, Double> amountExtractor, Function<T, ExchangeRate> exchangeRateExtractor) {
    return StreamEx.of(entities)
      .map(entity -> {
        String currency = currencyExtractor.apply(entity);
        ExchangeRate exchangeRate = exchangeRateExtractor.apply(entity);
        Money amount = Money.of(amountExtractor.apply(entity), currency);
        if (currency.equals(exchangeRate.getTo())) {
          return amount;
        } else {
          var provider = new CustomExchangeRateProvider(exchangeRate.getOperationMode());
          ConversionQuery query = buildConversionQuery(currency, exchangeRate.getTo(), exchangeRate.getExchangeRate());
          CurrencyConversion conversion = provider.getCurrencyConversion(query);
          return amount.with(conversion);
        }
      })
      .reduce(Money::add)
      .map(amount -> amount.with(MonetaryOperators.rounding()).getNumber().doubleValue())
      .orElse(0d);
  }

}
