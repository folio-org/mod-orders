package org.folio.service.finance.budget;

import io.vertx.core.Future;
import org.folio.rest.acq.model.finance.Budget;
import org.folio.rest.acq.model.finance.BudgetCollection;
import org.folio.rest.core.RestClient;
import org.folio.rest.core.models.RequestContext;
import org.folio.rest.core.models.RequestEntry;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;

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
  @DisplayName("Test getBudgetsByQuery")
  void testGetBudgetsByQuery() {
    // Given
    Budget budget = new Budget();
    BudgetCollection budgetCollection = new BudgetCollection().withBudgets(List.of(budget));
    doReturn(Future.succeededFuture(budgetCollection))
      .when(restClient).get(any(RequestEntry.class), eq(BudgetCollection.class), eq(requestContext));

    // When
    Future<List<Budget>> future = budgetService.getBudgetsByQuery("", requestContext);

    // Then
    assertTrue(future.succeeded());
    assertThat(future.result(), hasSize(1));
    assertEquals(budget, future.result().get(0));
  }

}
