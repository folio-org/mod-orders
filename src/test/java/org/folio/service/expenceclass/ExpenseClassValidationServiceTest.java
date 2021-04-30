package org.folio.service.expenceclass;

import org.folio.rest.jaxrs.model.CompositePoLine;
import org.folio.rest.jaxrs.model.FundDistribution;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.function.Function;

import static java.util.stream.Collectors.toMap;
import static org.folio.TestUtils.getMockAsJson;
import static org.folio.rest.impl.PurchaseOrderLinesApiTest.COMP_PO_LINES_MOCK_DATA_PATH;

@DisplayName("ExpenseClassValidationService class ")
public class ExpenseClassValidationServiceTest {
  static final String ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE = "c0d08448-347b-418a-8c2f-5fb50248d67e";

  @Test
  @DisplayName("should not throw DuplicateKeyException")
  void testShouldNotThrowDuplicateKeyException() {
    CompositePoLine compositePoLine = getMockAsJson(COMP_PO_LINES_MOCK_DATA_PATH, ANOTHER_PO_LINE_ID_FOR_SUCCESS_CASE)
      .mapTo(CompositePoLine.class);

    compositePoLine.getFundDistribution()
      .clear();

    var fd = new FundDistribution().withFundId(UUID.randomUUID()
      .toString())
      .withCode("ANY")
      .withExpenseClassId(UUID.randomUUID()
        .toString())
      .withDistributionType(FundDistribution.DistributionType.PERCENTAGE)
      .withValue(100D);

    compositePoLine.getFundDistribution()
      .add(fd);

    List<CompositePoLine> compositePoLineList = new ArrayList<>();

    compositePoLineList.add(compositePoLine);
    compositePoLineList.add(compositePoLine);

    Map<FundDistribution, String> expenseClassesByFundId = compositePoLineList.stream()
      .flatMap(poLine -> poLine.getFundDistribution()
        .stream())
      .distinct()
      .filter(fundDistribution -> Objects.nonNull(fundDistribution.getExpenseClassId()))
      .collect(toMap(Function.identity(), FundDistribution::getExpenseClassId));

    Assertions.assertEquals(1, expenseClassesByFundId.size());

  }
}
