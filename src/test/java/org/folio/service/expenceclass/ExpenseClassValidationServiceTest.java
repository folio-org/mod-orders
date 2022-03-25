package org.folio.service.expenceclass;

import static java.util.concurrent.CompletableFuture.completedFuture;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.COMP_PO_LINES_MOCK_DATA_PATH;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.folio.rest.core.models.RequestContext;
import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.folio.service.finance.expenceclass.ExpenseClassValidationService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("ExpenseClassValidationService class ")
public class ExpenseClassValidationServiceTest {
  static final String ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE = "c0d08448-347b-418a-8c2f-5fb50248d67e";

  @Test
  @DisplayName("Should not throw DuplicateKeyException")
  void testShouldNotThrowDuplicateKeyException() {
    CompositePoLine compositePoLine = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE)
      .mapTo(CompositePoLine.class);

    compositePoLine.getFundDistribution().clear();

    var fd = new FundDistribution().withFundId(UUID.randomUUID()
      .toString())
      .withCode("ANY")
      .withExpenseClassId(UUID.randomUUID().toString())
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D);

    compositePoLine.getFundDistribution().add(fd);

    List<CompositePoLine> compositePoLineList = new ArrayList<>();

    compositePoLineList.add(compositePoLine);
    compositePoLineList.add(compositePoLine);

    ExpenseClassValidationService expenseClassValidationService = mock(ExpenseClassValidationService.class,CALLS_REAL_METHODS);
    RequestContext requestContext = new RequestContext(null,null);

    doReturn(completedFuture(null)).when(expenseClassValidationService).checkExpenseClassIsActiveByFundDistribution(any(), anyBoolean(), any());

    CompletableFuture<Void> response = expenseClassValidationService.validateExpenseClasses(compositePoLineList, true, requestContext);
    response.join();

    Assertions.assertTrue(response.isDone());

  }
}
