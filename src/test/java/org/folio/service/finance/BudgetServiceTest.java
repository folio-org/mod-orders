package org.folio.service.finance;

import org.folio.rest.acq.model.finance.Budget;
import org.folio.service.finance.EncumbranceService;
import org.junit.jupiter.api.Test;

import java.util.UUID;
import java.util.concurrent.CompletionException;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class BudgetServiceTest {

    @Test
    public void testShouldReturnActiveBudgetForFund() {
        EncumbranceService encumbranceService = new EncumbranceService();
        Budget budget = encumbranceService.getActiveBudgetByFundId(ACTIVE_BUDGET).join();
        assertNotNull(budget);
    }

    @Test
    public void testShouldThrowExceptionIfActiveBudgetForFund() {
        assertThrows(CompletionException.class, () -> {
            EncumbranceService encumbranceService = new EncumbranceService();
            Budget budget = encumbranceService.getActiveBudgetByFundId(UUID.randomUUID().toString())
                    .join();
            assertNotNull(budget);
        });
    }
}
