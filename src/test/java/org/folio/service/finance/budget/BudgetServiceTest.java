package org.folio.service.finance.budget;

import io.vertx.core.Future;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.exceptions.HttpException;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.UUID;

import static org.folio.rest.core.exceptions.ErrorCodes.BUDGET_NOT_FOUND_FOR_TRANSACTION;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;

public class BudgetServiceTest {
  private AutoCloseable mockitoMocks;
  private BudgetService budgetService;
  @Mock
  private RequestContext requestContext;
  @Mock
  private RestClient restClient;

  @BeforeEach
  void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
    budgetService = new BudgetService(restClient);
  }

  @AfterEach
  void resetMocks() throws Exception {
    mockitoMocks.close();
  }

  @Test
  @DisplayName("Test getBudgets failure")
  void testGetBudgetsFailure() {
    // Given
    doReturn(Future.failedFuture(new HttpException(404, "not found")))
      .when(restClient).get(any(RequestEntry.class), eq(Budget.class), eq(requestContext));
    String fundId = UUID.randomUUID().toString();

    // When
    Future<List<Budget>> future = budgetService.getBudgets(List.of(fundId), requestContext);

    // Then
    assertTrue(future.failed());
    HttpException ex = (HttpException)future.cause();
    assertThat(ex.getErrors().getErrors(), hasSize(1));
    assertEquals(BUDGET_NOT_FOUND_FOR_TRANSACTION.getCode(), ex.getError().getCode());
    assertEquals(fundId, ex.getError().getParameters().get(0).getValue());
  }

}
