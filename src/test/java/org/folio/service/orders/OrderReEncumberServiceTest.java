package org.folio.service.orders;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.helper.PurchaseOrderHelperTest.ORDER_PATH;
import static org.folio.rest.impl.ApiTestBase.getMockAsJson;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.Collections;
import java.util.UUID;

import org.folio.rest.acq.model.finance.FiscalYear;
import org.folio.rest.acq.model.finance.Fund;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverError;
import org.folio.rest.acq.model.finance.LedgerFiscalYearRolloverErrorCollection;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePurchaseOrder;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRollover;
import org.folio.rest.jaxrs.model.LedgerFiscalYearRolloverCollection;
import org.folio.service.finance.EncumbranceService;
import org.folio.service.finance.FiscalYearService;
import org.folio.service.finance.FundService;
import org.folio.service.finance.RolloverErrorService;
import org.folio.service.finance.RolloverRetrieveService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class OrderReEncumberServiceTest {

    @InjectMocks
    private OrderReEncumberService orderReEncumberService;

    @Mock
    private CompositePurchaseOrderService compositePurchaseOrderService;
    @Mock
    private FundService fundService;
    @Mock
    private RolloverErrorService rolloverErrorService;
    @Mock
    private RolloverRetrieveService rolloverRetrieveService;
    @Mock
    private FiscalYearService fiscalYearService;

    @Mock
    private RequestContext requestContext;


    @BeforeEach
    public void initMocks(){
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testPopulateNeedReEncumberField() {
        // given
             EncumbranceService encumbranceService = mock(EncumbranceService.class, CALLS_REAL_METHODS);
        FundService fundService = mock(FundService.class, CALLS_REAL_METHODS);

        CompositePurchaseOrder order = getMockAsJson(ORDER_PATH).mapTo(CompositePurchaseOrder.class);

        Fund sampleFund = new Fund()
                .withLedgerId(UUID.randomUUID().toString())
                .withFundStatus(Fund.FundStatus.ACTIVE)
                .withCode(UUID.randomUUID().toString());

        LedgerFiscalYearRolloverCollection ledgerFiscalYearRollover = new LedgerFiscalYearRolloverCollection()
                .withLedgerFiscalYearRollovers(Collections.singletonList(new LedgerFiscalYearRollover()))
                .withTotalRecords(1);

        LedgerFiscalYearRolloverErrorCollection ledgerFiscalYearRolloverErrors = new LedgerFiscalYearRolloverErrorCollection()
                .withLedgerFiscalYearRolloverErrors(Collections.singletonList(new LedgerFiscalYearRolloverError()));

        // LedgerFiscalYearRolloverErrorCollection is not empty. Expected "needReEncumber" = true
        doReturn(completedFuture(sampleFund)).when(fundService).retrieveFundById(any(), any());
        doReturn(completedFuture(new FiscalYear())).when(fiscalYearService).getCurrentFiscalYear(any(), any());
        doReturn(completedFuture(ledgerFiscalYearRollover)).when(rolloverRetrieveService).getLedgerFyRollovers(any(), any(), any());
        doReturn(completedFuture(ledgerFiscalYearRolloverErrors)).when(rolloverErrorService).getLedgerFyRolloverErrors(any(), any(), any());

        CompositePurchaseOrder compOrder = orderReEncumberService.populateNeedReEncumberFlag(order, requestContext).join();
        assertTrue(compOrder.getNeedReEncumber());

        // LedgerFiscalYearRolloverErrorCollection is empty. Expected "needReEncumber" = false
        doReturn(completedFuture(new LedgerFiscalYearRolloverErrorCollection())).when(rolloverErrorService).getLedgerFyRolloverErrors(any(), any(), any());
        compOrder = orderReEncumberService.populateNeedReEncumberFlag(order, requestContext).join();
        assertFalse(compOrder.getNeedReEncumber());

        // LedgerFyRollover not exists. Expected "needReEncumber" = false
        doReturn(completedFuture(new LedgerFiscalYearRolloverCollection())).when(rolloverRetrieveService).getLedgerFyRollovers(any(), any(), any());
        compOrder = orderReEncumberService.populateNeedReEncumberFlag(order, requestContext).join();
        assertFalse(compOrder.getNeedReEncumber());

    }
}
